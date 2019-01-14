library(tidyverse)
library(visNetwork)

source("./src/customer/customer.r")


Network_Graph <- function(last_q = "2018-Q2", this_q = "2018-Q3", percentage_view = FALSE, input_data) {
  
  ## PARAMS------------------------------------------------------------------------
  assertthat::assert_that(inherits(input_data, "preprocessed_df_cust"))
  #Imediately evaluate paramaters to safeguard shiny app issues caused by lazy evaluation 
  last_q <- last_q
  this_q <- this_q
  percentage_view <- percentage_view 
  input_data <- input_data
  

  ## DATA ------------------------------------------------------------------------
  preprocess_for_network <- function(df_cust) {
    assertthat::assert_that(inherits(df_cust, "preprocessed_df_cust"))
    
    df_cust <- df_cust %>% filter(owns_product == 1) %>%
      filter(quarter %in% c(last_q, this_q)) %>%
      mutate(
        product_super_group = recode(
          product_super_group,
          nx_ivm = "N",
          metasploit = "M",
          idr = "I",
          app_sec = "A",
          insightops = "S",
          komand = "K"
        )
      ) %>%
      group_by(internal_id, company_name, quarter) %>%
      summarize(
        label = glue::collapse(product_super_group, sep = ","),
        n_prod = sum(owns_product, na.rm = TRUE)
      ) %>%
      ungroup()
    
    all_cust_quarter_combo <-
      expand.grid(
        internal_id = unique(df_cust$internal_id),
        quarter = unique(df_cust$quarter)
      )
    
    df_cust <- df_cust %>% full_join(all_cust_quarter_combo) %>%
      mutate(label = ifelse(is.na(label), "none", label),
             n_prod = ifelse(is.na(n_prod), 0, n_prod))
    
    df_cust <- df_cust %>%
      group_by(internal_id) %>%
      mutate(change = n_prod[quarter == this_q] - n_prod[quarter == last_q]) %>%
      ungroup()
    
    class(df_cust) <- c(class(df_cust), "network_graph_data")
    
    df_cust
  }
  
  
  data <- input_data %>% preprocess_for_network()
  
  ## NODES & EDGES ------------------------------------------------------------------------
  make_nodes <- function(df_cust) {
    assertthat::assert_that(inherits(df_cust, "network_graph_data"))
    cust_labels <-
      df_cust %>% select(label, n_prod) %>% distinct() %>% pull(label)
    cust_nprods <-
      df_cust %>% select(label, n_prod) %>% distinct() %>% pull(n_prod)
    
    nodes_shape_map <-
      c("star", "circle", "square", "triangle", rep("diamond", 4)) #0, 1, 2, >2 products
    
    nodes <- tibble(label = cust_labels,
                    group = paste0(cust_nprods, " Products")) %>%
      arrange(label) %>%
      mutate(id = label,
             shape = rep("circle", n()))
    
    class(nodes) <- append(class(nodes), "network_nodes")
    nodes
  }
  
  
  make_edges <-
    function(df_cust,
             from_filter = NULL,
             to_filter = NULL,
             bool_op = `|`,
             nodes) {
      
      assertthat::assert_that(inherits(df_cust, "network_graph_data"))
      assertthat::assert_that(inherits(bool_op, "function"))
      assertthat::assert_that(inherits(nodes, "network_nodes"))
      
      df_last_q <- df_cust %>% filter(quarter == last_q) %>%
        select(internal_id, from = label)
      
      df_this_q <- df_cust %>% filter(quarter == this_q) %>%
        select(internal_id, to = label, change)
      
      if (is.null(from_filter)) {
        from_filter <- unique(df_last_q$from)
      }
      if (is.null(to_filter)) {
        to_filter <- unique(df_this_q$to)
      }
      
      
      edges <- df_last_q %>%
        left_join(df_this_q) %>%
        group_by(from) %>%
        mutate(from_total = n()) %>%
        ungroup() 
      
      
      edges <- edges  %>% 
        group_by(from, to, change) %>% 
        summarize( 
          value_perc = round(n() / mean(from_total), 2),
          value_n = n(),
          from_total = mean(from_total)) %>%
        ungroup() %>%
        #  filter(from != to) %>%
        filter(bool_op(from %in% from_filter, to %in% to_filter)) 
       
      
      confidence_interval <- purrr::map2_chr(.x = edges$value_n, .y = edges$from_total, ~{
        confidence_interval <- prop.test(.x, .y, correct = FALSE)
        confidence_interval <- paste(round(confidence_interval$conf.int[1],3), 
                                     round(confidence_interval$conf.int[2], 3), sep = ",")
      })
      
      edges <- edges %>% mutate(confidence_interval = confidence_interval)
      
      if(percentage_view){
        edges <- edges %>% mutate(value = value_perc)
      }
      
      else{
        edges <- edges %>% mutate(value = value_n)
        
      }
      
      edges <- edges %>%
        mutate(color = case_when(change == 0 ~ "grey",
                                 change > 0 ~ "green",
                                 change < 0 ~ "red")) %>%
        select(-change)
      #to %in% label_Filter
      
      edges <- edges %>%
        left_join(nodes, by = c("from" = "label")) %>%
        mutate(from = id) %>%
        select(-id) %>%
        left_join(nodes, by = c("to" = "label")) %>%
        mutate(to = id) %>%
        select(-id)
      
      if (percentage_view) {
        edges <- edges %>%
          mutate(label = paste0(round(100 * value), "%"))
      }
      
      else{
        edges <- edges %>%
          mutate(label = value)
      }
      
      edges
    }
  
  
  ## NETWORK------------------------------------------------------------------------
  draw_network <-
    function(df_cust,
             from_filter = NULL,
             to_filter = NULL,
             bool_op = `|`,
             title = "Full Flow") {
      assertthat::assert_that(inherits(df_cust, "network_graph_data"))
      assertthat::assert_that(inherits(bool_op, "function"))
      
      nodes <- make_nodes(df_cust)
      
      
      edges <-
        make_edges(df_cust, from_filter, to_filter, bool_op, nodes)
      nodes <- nodes %>% filter(id %in% c(edges$from, edges$to))
      
      title = paste0("<p>",
                     title,
                     "<br>",
                     paste0(last_q, " \u2192 ", this_q),
                     "</p>")
      
      visNetwork(nodes, edges,
                 main = title,
                 height = "100%",
                 width = "100%") %>%
        visEdges(arrows = "to") %>%
        visLayout(improvedLayout = TRUE, randomSeed = 123) %>%
        visOptions(
          highlightNearest = list(
            enabled = TRUE,
            degree = 0,
            labelOnly = TRUE,
            hover = TRUE
          ),
          nodesIdSelection = list(enabled = TRUE, values = nodes$id %>% sort()),
          selectedBy = "group"
        ) %>%
        visLegend()
    }
  
  
  draw_new_network <- function(df_cust) {
    assertthat::assert_that(inherits(df_cust, "network_graph_data"))
    draw_network(
      df_cust,
      from_filter = "none",
      bool_op = `&`,
      title = "New Customer"
    )
  }
  
  
  draw_churn_network <- function(df_cust) {
    assertthat::assert_that(inherits(df_cust, "network_graph_data"))
    draw_network(df_cust,
                 to_filter = "none",
                 bool_op = `&`,
                 title = "Customer Churn")
  }
  
  
  draw_downgrade_network <- function(df_cust, label_regex = NULL) {
    #@params: label_regex can be vector of regexes to match
    assertthat::assert_that(inherits(df_cust, "network_graph_data"))
    
    if (is.null(label_regex)) {
      label_regex <- c(unique(df_cust$label), "none")
    }
    data_class <-
      class(df_cust) #need to make copy because dplyr loses user defined classes.
    
    df_cust <- df_cust %>%
      filter(change < 0, label != "none") %>%
      group_by(internal_id) %>%
      mutate(label_united = glue::collapse(label, sep = ",")) %>%
      ungroup() %>%
      filter(map(label_regex, ~ str_detect(label_united, .x)) %>% reduce(`|`)) %>%
      select(-label_united)
    
    class(df_cust) <- data_class
    draw_network(df_cust, title = "Product Churn")
  }
  
  
  draw_cross_sell_network <- function(df_cust, label_regex = NULL) {
    #@params: label_regex can be vector of regexes to match
    assertthat::assert_that(inherits(df_cust, "network_graph_data"))
    data_class <- class(df_cust)
    
    if (is.null(label_regex)) {
      label_regex <- c(unique(df_cust$label), "none")
    }
    df_cust <- df_cust %>%
      filter(change > 0,
             label != "none") %>%
      group_by(internal_id) %>%
      mutate(label_united = paste(label, sep = ",")) %>%
      ungroup() %>%
      filter(map(label_regex, ~ str_detect(label_united, .x)) %>% reduce(`|`)) %>%
      select(-label_united)
    
    class(df_cust) <- data_class
    draw_network(df_cust, title = "Cross-Sell")
  }
  
  
  draw_all <- function(df_cust, label_regex = NULL) {
    list(
      draw_new_network,
      draw_churn_network,
      draw_cross_sell_network,
      draw_downgrade_network
    ) %>%
      purrr::walk( ~ print(.x(df_cust)))
  }
  
  draw_transition_matrix <- function(df_cust) {
    edges <- make_edges(df_cust, nodes = make_nodes(df_cust)) 
    plot = edges %>% 
       filter(from_total >= 10,
              value_n >= 2) %>% 
        mutate(from = forcats::fct_reorder(from, value_n, .fun = sum),
                            to = forcats::fct_reorder(to, value_n, .fun = sum)) %>%
      mutate(tooltip = row.names(.)) %>%
      ggplot(aes(x = to, y = from, fill = value_n, color = confidence_interval, label = value, tooltip = tooltip)) + 
      geom_tile(width = 0.9, height = 0.9) + 
      geom_text() + 
      ggiraph::geom_tile_interactive() %>%
      viridis::scale_fill_viridis() + 
      scale_x_discrete(position = "top") +
      theme_classic() +
      theme(
        axis.text.x = element_text(hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        legend.position = "none"
      ) +
      ylab("From") +
      xlab("To") 
    
   # plot <- plotly::ggplotly(plot, source = "network_transition_matrix") %>%
    #  plotly::layout(dragmode = "select")
  }
  
  
 structure(list(
    data = data,
    draw_network = purrr::partial(draw_network, df_cust = data),
    draw_new_network = purrr::partial(draw_new_network, df_cust = data),
    draw_churn_network = purrr::partial(draw_churn_network, df_cust = data),
    draw_cross_sell_network = purrr::partial(draw_cross_sell_network, df_cust = data),
    draw_downgrade_network = purrr::partial(draw_downgrade_network, df_cust = data),
    draw_transition_matrix = purrr::partial(draw_transition_matrix, df_cust = data)
  ), 
  class = "network_graph")
}


#quarters <- list(c("2015-Q3", "2016-Q3"), c("2016-Q3", "2017-Q3"), c("2017-Q3", "2018-Q3"))
#quarters = list(c("2017-Q3", "2018-Q3"))

plot_network_time_series <- function(quarters, type = NULL, percentage_view = FALSE) {
  networks <- list()
  
  for (i in seq_along(quarters)) {
    networks[[i]] <- Network_Graph(quarters[[i]][1], quarters[[i]][2], percentage_view = percentage_view)
  }
  
  if(is.null(type)){
    for (N in networks) {
      print(N$draw_network(N$data))
    }
    return()
  }
  
  
  if (type == "new") {
    for (N in networks) {
      print(N$draw_new_network(N$data))
    }
    return()
  }
  
  if (type == "churn") {
    for (N in networks) {
      print(N$draw_churn_network(N$data))
    }
    return()
  }
  
  if (type == "cross_sell") {
    for (N in networks) {
      print(N$draw_cross_sell_network(N$data))
    }
    return()
  }
  
  if (type == "downgrade") {
    for (N in networks) {
      print(N$draw_downgrade_network(N$data))
    }
    return()
  }
}