server <- function(input, output, session) {
  tab_list <- NULL
  
  showNotification("Created by Jittirat Pushsondok", duration = NULL, type = "message")
  
  # Use a reactive() function to prepare the base SQL query that all the elements in the dashboard
  # will use. The reactive() allows us to evaluate the input variables
 
  
  base_electric <- reactive({
    res <- res112Ex %>%
      filter(ItemCode == input$sale) 
      
    
    
    
    if (input$month != 99) res <- filter(res, month == input$month)
    res
    
    
    
    
  })
  
  
  
 
  # Montly/daily trend (server) -------------------------------------
  output$group_totals <- renderD3({
    grouped <- ifelse(input$month != 99, expr(day), expr(month))
    
    
    
    res <- base_electric() %>%
      
      group_by(!!grouped) %>%
      
      tally() %>%
      collect() %>%
      
      
      
      mutate(
        y = n,
        x = !!grouped
      ) %>%
      select(x, y)
    
    if (input$month == 99) {
      res <- res %>%
        inner_join(
          tibble(x = 1:12, label = substr(month.name, 1, 3)),
          by = "x"
        )
    } else {
      res <- res %>%
        mutate(label = x)
    }
    r2d3(res, "col_plot.js")
  })
  
  # Top customers (server) -------------------------------------------
  output$top_customers<- renderD3({
    # The following code runs inside the database
    base_electric() %>%
      group_by(Customer, cust_name) %>%
      tally() %>%
      
      collect() %>%
      arrange(desc(n)) %>%
      head(10) %>%
      arrange(cust_name) %>%
      mutate(cust_name=str_sub(cust_name,1, 30)) %>%
      
      rename(
        x = Customer,
        y = n,
        label = cust_name
      ) %>%
      r2d3("bar_plot.js")
  })
  
  
  # Get details (server) --------------------------------------------
  get_details <- function(customer = NULL, day = NULL) {
    # Create a generic details function that can be called
    # by different dashboard events
    
    res <- base_electric() 
    
   
    if (!is.null(customer)) res <- filter(res, Customer == as.character(customer))
    if (!is.null(day)) res <- filter(res, day == !!as.integer(day))
    
    res %>%
      head(100) %>% 
      select(
        month, day, Customer, cust_name, Quantity, UnitPrice
        
      ) %>%
      collect() %>%
      mutate(month = month.name[as.integer(month)]) 
    
    
    
  }
  
  # Month/Day column click (server) ---------------------------------
  observeEvent(input$column_clicked != "", {
    if (input$month == "99") {
      updateSelectInput(session, "month", selected = input$column_clicked)
    } else {
      day <- input$column_clicked
      month <- input$month
      tab_title <- paste(
        input$sale, "-", month.name[as.integer(month)], "-", day
      )
      
      
      if (!(tab_title %in% tab_list)) {
        appendTab(
          inputId = "tabs",
          tabPanel(
            tab_title,
            DT::renderDataTable(
              get_details(day = day)
              
              
              
              
              
            )
            
          )
        )
        tab_list <<- c(tab_list, tab_title)
      }
      updateTabsetPanel(session, "tabs", selected = tab_title)
    }
  },
  ignoreInit = TRUE
  )
  
  
  # Bar clicked (server) --------------------------------------------
  observeEvent(input$bar_clicked, {
    customer <- input$bar_clicked
    month <- input$month
    tab_title <- paste(
      input$sale, "-", customer,
      if (month != 99) {
        paste("-", month.name[as.integer(month)])
      }
    )
    if (!(tab_title %in% tab_list)) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          tab_title,
          DT::renderDataTable(
            get_details(customer = customer)
          )
        )
      )
      
      tab_list <<- c(tab_list, tab_title)
    }
    updateTabsetPanel(session, "tabs", selected = tab_title)
  })
  
  # Remote tabs (server) --------------------------------------------
  observeEvent(input$remove, {
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~ removeTab("tabs", .x))
    tab_list <<- NULL
    
    
    
  })
}
