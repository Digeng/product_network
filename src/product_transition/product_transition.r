library(tidyverse)

## PRODUCT CHANGE TABLE-----------------------------------------------------------------------
Product_Transition <- function(input_data) {
  assertthat::assert_that(inherits(input_data, "preprocessed_df_cust"))
  
  ## PRODUCT CHANGE TABLE HELPERS------------------------------------------------------------------------
  create_product_count_matrix <-
    function(df_cust,
             start = "2018-Q2",
             end = "2018-Q3") {
     
     # browser()
      quarter_list <- c(start = start, end = end)
      
      df_cust <- df_cust %>%
        mutate(product_super_group = as.character(product_super_group)) %>%
        filter(quarter %in% quarter_list) %>%
        group_by(internal_id, quarter) %>%
        summarize(n_prod = sum(owns_product[product_super_group != "services"], na.rm = TRUE)) %>%
        ungroup() 
      
      #churned customers do not exist as rows in end period. We create rows for these customers for end period in order to calculate QoQ changes. 
      churned_ids <-
        setdiff(df_cust[df_cust$quarter == start &
                          df_cust$n_prod > 0,]$internal_id,
                df_cust[df_cust$quarter == end,]$internal_id)
      
      churned_cust_df <-
        expand.grid(
          internal_id = churned_ids,
          quarter = end,
          n_prod = 0
        )
      
      df_cust %>%
        rbind(churned_cust_df) %>%
        group_by(internal_id) %>%
        mutate(n_prod_lag = lag(n_prod, order_by = quarter, default = 0)) %>%
        ungroup() 
    }
  
  create_product_count_matrix_memo <-
    memoise::memoise(create_product_count_matrix)
 

  convert_table_to_dataframe <- function(tab) {
    assertthat::assert_that(inherits(tab, "table"))
    
    df_table <- tab %>% as.data.frame.matrix() %>%
      as.data.frame() %>%
      mutate(n_prod_start = rownames(.) %>% as.numeric()) %>%
      select(n_prod_start, everything())
    
    class(df_table) <- append(class(df_table), "df_table")
    
    df_table
  }
  
  
  ## PUBLIC------------------------------------------------------------------------
   create_product_change_table <-
    function(
      df_cust,
      product_filter =  c("app_sec",
                          "idr",
                          "insightops",
                          "komand",
                          "metasploit",
                          "nx_ivm",
                          "services"),
      start = "2018-Q2",
      end = "2018-Q3") {
      
      product_customers <- df_cust %>%
        filter(product_super_group %in% product_filter) %>%
        group_by(internal_id) %>%
        summarize(keep = ifelse(any(owns_product == 1), 1, 0)) %>%
        ungroup() %>%
        filter(keep == 1) %>%
        pull(internal_id)
      
      df_cust <- df_cust %>%
        filter(internal_id %in% product_customers) %>%
        create_product_count_matrix_memo(
          start = start,
          end = end
        )
      
      #browser()
      df_cust <- df_cust %>% filter(quarter == end)
      tab <-
        table(df_cust$n_prod_lag, df_cust$n_prod - df_cust$n_prod_lag) %>%
        convert_table_to_dataframe() 
    }
  
  
  create_product_change_table_memo <-
    memoise::memoise(create_product_change_table)
  

  
  create_table_plot <- function(df_table) {
    
    color_map <- c(
      white = "#ffffff",
      light_green = "#ccffcc",
      dark_green = "#009900",
      pink = "#ffb3da",
      red = "#ff0000"
    )
    
    df_table <- df_table %>%
      gather(key = n_prod_start) %>%
      setNames(c("n_prod_start", "change", "value")) %>%
      mutate(change = as.numeric(change)) %>%
      mutate(
        color = case_when(
          value > 0 & change != 0 & n_prod_start == -change ~ "red",
          value > 0 & n_prod_start == 0 & change > 0 ~ "dark_green",
          value > 0 & n_prod_start > 0 &
            change > 0 ~ "light_green",
          value > 0 & n_prod_start > 0 &
            change < 0 ~ "pink",
          TRUE ~ "white"
        )
      ) %>%
      mutate(color = factor(color, names(color_map)))
    
    plot <- df_table %>%
      mutate(
        n_prod_start = factor(n_prod_start, n_prod_start %>% unique() %>% sort() %>% rev()),
        change = factor(change, change %>% unique() %>% sort())
      ) %>%
      ggplot(aes(
        y = n_prod_start,
        x = change,
        fill = color,
        label = value,
        key = row.names(df_table)
      )) +
      geom_tile() +
      geom_text() +
      scale_fill_manual(values = color_map) +
      scale_x_discrete(position = "top") +
      theme_classic() +
      theme(
        axis.text.x = element_text(hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        legend.position = "none"
      ) +
      ylab("Starting # Products") +
      xlab("Flow")
    
    plot <- plotly::ggplotly(plot, source = "transition_table") %>%
      plotly::layout(dragmode = "select")
    
    list(data = df_table, plot = plot)
  }
  
  
  structure(list(create_product_change_table = purrr::partial(create_product_change_table_memo, df_cust = input_data), 
       create_table_plot = create_table_plot), 
       class = "product_transition")
}
