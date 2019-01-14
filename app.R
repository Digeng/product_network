
source("./src/network/network_graph.r")
source("./src/customer/customer.r")
source("./src/product_transition/product_transition.r")
library(shiny)
library(plotly)


## HELPER FUNCTIONS ------------------------------------------------------------------------
set_attribute <- function(x, default) {
  if (is.null(x)) {
    x <- default
  }
  
  x
}

prettify_string <- function(my_string) {
  str_to_title(str_replace_all(my_string, "_", " "))
}

uniq_sort <- purrr::compose(sort, unique)

## DEFAULTS ------------------------------------------------------------------------
#DATA <-
 # feather::read_feather("./data/customer/preprocessed_cust_data2.feather")
DATA <- feather::read_feather("./data/customer/preprocessed_waterfall.feather")

class(DATA) <- append(class(DATA), "preprocessed_df_cust")

GROUP_CHOICES <- c("segment", "reporting_division", "director_level_team", 
                   "csm_tier", "cohort", "cohort_quarter", "company_name", 
                   "platform_waterfall_type", "internal_id") %>% 
  intersect(colnames(DATA))
  
QUARTER_CHOICES <- uniq_sort(DATA[["quarter"]])


## UI ------------------------------------------------------------------------
ui <- fluidPage(
  includeCSS("app.css"),
  titlePanel("Product Flow"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      #shinyWidgets::prettyRadioButtons("dataset", 
       #                                "Dataset",
        #                               choices = c("Customer Trending", "Waterfall"), 
         #                              status = "success", 
          #                             inline = TRUE),
      
      shinyWidgets::sliderTextInput(
        "start_quarter",
        "Start Quarter",
         choices = QUARTER_CHOICES,
        selected = QUARTER_CHOICES[length(QUARTER_CHOICES) - 2]
      ),
      
      shinyWidgets::sliderTextInput(
        "end_quarter",
        "End Quarter",
        choices = QUARTER_CHOICES,
        selected = QUARTER_CHOICES[length(QUARTER_CHOICES) - 1]
      ),

      shinyWidgets::pickerInput(
        "group", 
        "Group", 
        choices = GROUP_CHOICES,
        choicesOpt = list(content = prettify_string(GROUP_CHOICES)),
        selected = NULL
      ),
      
      actionButton("add", "", icon = icon("plus")),
    
      hr(),

      uiOutput("group_boxes"),
      
      actionButton("remove", "", icon = icon("minus")),
      
      hr(),
      
      actionButton("update", "Update")
    ),
    
    mainPanel(
      height = 12, 
      tabsetPanel(
      id = "tabset",

      tabPanel("Matrix",
               hr(),
               fluidRow(column(8, plotlyOutput("product_change_table"))), 
               hr(),
               fluidRow(column(4, htmlOutput("summary_table")))),
      tabPanel(
        "Network",
        visNetworkOutput("network"),
        shinyWidgets::prettyCheckbox("percentage_view", "%", icon = icon("check"),
                                     bigger = TRUE, status = "success", outline = TRUE, 
                                     fill = TRUE, animation = "smooth"),
        ggiraph::ggiraphOutput("network_transition_matrix")

      ),
      tabPanel("Node", 
               dataTableOutput("edges_table")
            ),
      tabPanel(id = "block", "Block", 
               plotOutput("scoresPlot"))
    ))
  ), 
  includeScript("./js/app.js"),
  includeScript("./js/main.js")
)


## SERVER ------------------------------------------------------------------------
server <- function(input, output, session) {
  
  ## UI ELEMENTS ------------------------------------------------------------------------

  col_vars <- list()
  
  observeEvent(input$add, {
    #store input$group's value in col_vars list without triggering reactivity
    group_reactiveValue <- reactiveValues(temp = input$group)
    col_vars <<-
      append(col_vars, reactiveValuesToList(group_reactiveValue) %>% unlist()) %>%
      unique()
    
    choices <- setdiff(GROUP_CHOICES, col_vars %>% unlist())
    shinyWidgets::updatePickerInput(session, "group", "Group", 
                      choices = choices,
                      choicesOpt = list(content = choices %>% 
                                          prettify_string()))
  })
  
  observeEvent(input$remove, { 
    col_vars[length(col_vars)] <<- NULL 
    choices <- setdiff(GROUP_CHOICES, col_vars %>% unlist())
    shinyWidgets::updatePickerInput(session, "group", "Group", 
                      choices = choices,
                        choicesOpt = list(content = choices %>% 
                                            prettify_string()))
    })
  
  add_or_remove <- reactive({
    paste0(input$add, input$remove)
  })
  
  group_boxes <- reactive({
    add_or_remove()
    
    lapply(col_vars, function(grp) {
      selectizeInput(
        grp,
        prettify_string(grp),
        choices = uniq_sort(DATA[[grp]]),
        multiple = TRUE, 
        width = "250px"
      )
    })
  })
  
  
  output$group_boxes <- renderUI({
    group_boxes()
  })
  
  ## DATA------------------------------------------------------------------------
  data <- reactive({
    input$update
    
    isolate({
      col_vars_list <- col_vars
      data_temp <- DATA
      
      for (i in seq_along(col_vars_list)) {
        col_name <- col_vars_list[[i]]
        if (!is.null(input[[col_name]])) {
          data_temp <-
            data_temp[data_temp[[col_name]] %in% input[[col_name]], ]
        }
      }
      
      class(data_temp) <-
        append(class(data_temp), "preprocessed_df_cust") %>% unique()
      data_temp
    })
  })
  
  
  ## FLOW TABLE ------------------------------------------------------------------------
  table_plot <- reactive({
    input$update
    isolate({
      pt <- Product_Transition(data())
      pt$create_table_plot(
        pt$create_product_change_table(
          start = input$start_quarter,
          end = input$end_quarter
        )
      )
    })
  })
  
  output$summary_table <- renderText({
      data_temp <- table_plot()$data
      
      n_cust_adds <-
        sum(data_temp[data_temp$color == "dark_green", ]$value)
      n_cust_churn <- sum(data_temp[data_temp$color == "red", ]$value)
      n_cust_cross <-
        sum(data_temp[data_temp$color == "light_green", ]$value)
      n_cust_prod_churn <-
        sum(data_temp[data_temp$color == "pink", ]$value)
      
      n_cust_start <- sum(data_temp$value) - n_cust_adds
      n_cust_end <- n_cust_start + n_cust_adds - n_cust_churn
      
      df_summary <- tribble(
        ~ Stat,
        ~ Value,
        "Starting Product Customers",
        n_cust_start,
        "Ending Product Customers",
        n_cust_end,
        "New Customer Adds",
        n_cust_adds,
        "Customer Churn",
        n_cust_churn,
        "Product Cross-sell",
        n_cust_cross,
        "Product Cancel",
        n_cust_prod_churn
      ) %>%
        mutate(Percentage = round(Value / Value[Stat == "Starting Product Customers"], 3) %>%
                 scales::percent())
      
      knitr::kable(df_summary,
                   booktabs = T,
                   align = c("l", "c", "c")) %>%
        kableExtra::kable_styling(
          font_size = 15,
          bootstrap_options = c("condensed"),
          full_width = FALSE
        ) %>%
        kableExtra::row_spec(3, color = "white", background = "#009900") %>%
        kableExtra::row_spec(4, color = "white", background = "#ff0000") %>%
        kableExtra::row_spec(5, background = "#ccffcc") %>%
        kableExtra::row_spec(6, background = "#ffb3da") 
  })
  
  
  output$product_change_table <- renderPlotly({
      title_font <-  list(family = "Arial, sans-serif",
                          size = 22,
                          color = "black")
      
      tick_font <-  list(family = "Arial, sans-serif",
                         size = 18,
                         color = "black")
      
      x_lab <- list(
        title = "Flow",
        titlefont = title_font,
        showticklabels = TRUE,
        side = "top",
        tickfont = tick_font
        # tickangle = 45
      )
      
      y_lab <-
        list(
          title = "Starting #Products",
          titlefont = title_font,
          showticklabels = TRUE,
          tickfont = tick_font
          #tickangle = 45
        )
      
      plot_margin <- list(l = 60, t = 60)
      
      
      print(
        table_plot()$plot %>%
          plotly::layout(
            plot_bgcolor = "#F0F0F0",
            margin = plot_margin,
            xaxis = x_lab,
            yaxis = y_lab
          )
      )
  })
  
  
  ## NETWORK GRAPH ------------------------------------------------------------------------
  network <-
    reactive(
      Network_Graph(
        input$start_quarter,
        input$end_quarter,
        percentage_view = input$percentage_view,
        data()
      )
    )
  
  table_color_network_map <- reactive({
    my_mapper <-
      list(
        red = network()$draw_churn_network,
        pink = network()$draw_downgrade_network,
        dark_green = network()$draw_new_network,
        light_green = network()$draw_cross_sell_network,
        white = network()$draw_network
      )
    
    vis_event_partial <- purrr::partial(visEvents,
                                        selectEdge = "function(properties) {
                                                          Shiny.onInputChange('from_edge',  this.body.data.edges._data[properties.edges[0]].from);
                                                          Shiny.onInputChange('to_edge',  this.body.data.edges._data[properties.edges[0]].to);
                                                      }" 
                                        )
    
    map(my_mapper, ~ compose(vis_event_partial, .x)) %>% 
      setNames(names(my_mapper))
  })
  
  
  network_viz <- reactive({
      plotly_data <- plotly::event_data("plotly_click", source = "transition_table")
      row_id <- plotly_data$key %>% unlist()
     
      if (is.null(plotly_data)) {
        table_color_network_map()[["white"]]()
      }
      
      else {
        color <- table_plot()$data[row_id, ]$color %>%
          as.character()
        table_color_network_map()[[color]]()
      }
    })
  
  observeEvent(plotly::event_data("plotly_click"), {
    updateTabsetPanel(session, "tabset", selected = "Network")
  })
  
  output$network <- renderVisNetwork({
    print(network_viz())
  })
  
  output$network_transition_matrix <- renderPlot({
    title_font <-  list(family = "Arial, sans-serif",
                        size = 22,
                        color = "black")
    
    tick_font <-  list(family = "Arial, sans-serif",
                       size = 14,
                       color = "black")
    
    x_lab <- list(
      title = "Transition Matrix",
      titlefont = title_font,
      showticklabels = TRUE,
      side = "top",
      tickfont = tick_font,
      tickangle = 90
    )
    
    y_lab <-
      list(
        title = "From",
        titlefont = title_font,
        showticklabels = TRUE,
        tickfont = tick_font
      )
    
    plot_margin <- list(l = 60, t = 60)
   # print(network()$draw_transition_matrix() %>%
    #        plotly::layout(
     #           plot_bgcolor = "#F0F0F0",
      #          margin = plot_margin,
       #         xaxis = x_lab,
        #        yaxis = y_lab
         #     ))
    
   # print(network()$draw_transition_matrix())
    ggiraph::ggiraph(code = {print(network()$draw_transition_matrix())})
  })


  ## EDGES TABLE ------------------------------------------------------------------------
  output$edges_table <- renderDataTable({
    if (!is.null(input$from_edge) & !is.null(input$to_edge)) {
      network()$data %>% filter((internal_id %in% internal_id[label == input$from_edge & quarter == input$start_quarter]) & 
                                   (internal_id %in% internal_id[label == input$to_edge & quarter == input$end_quarter])) %>%
        select(-n_prod) %>%
        spread(key = quarter, value = label) %>%
        mutate(internal_id = 
                 paste0("<a href=", "'https://system.na1.netsuite.com/app/common/entity/custjob.nl?id=",
                        internal_id, "'>", 
                        internal_id, 
                        "</a>"))
    }
  }, escape = FALSE)
  
  
  observeEvent(input$from_edge, {
    updateTabsetPanel(session, "tabset", selected = "Node")
  })
  

  output$scoresPlot <- renderPlot({
    if (!is.null(input$scores)) {
       plot <- data.frame(block = seq(length(unlist(input$scores))), 
                          scores = unlist(input$scores)) %>% 
       ggplot(aes(x = block, y = scores)) +  
       geom_area(fill = "#00cc69") + 
       theme_classic() + 
       theme(legend.position = "none")
       
       print(plot)
    }
  })
}
  
shinyApp(ui = ui, server = server)
