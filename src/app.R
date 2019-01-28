# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(cowplot)
library(shinydashboard)
library(shinycssloaders)
plot_data_column = function (column, data, x){
  data %>% ggplot(aes_string(x, column)) +
    geom_bar(fill="#CC79A7", colour="black",stat='identity') +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() + labs(y= column, x= "US Cities") +
    ggtitle("Number of crimes per city")
}
crime_csv <- read.csv("ucr_crime_1975_2015_filtered.csv", stringsAsFactors = FALSE)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Compare safest cities in the US", titleWidth = 350),
  dashboardSidebar(
    width = 350,
    dropdownButton(inputId = "cityInput", 
                   label = "Select the Cities:",
                   circle = FALSE,
                   actionButton(inputId = "all", label = "(Un)select all but one"),
                   checkboxGroupInput(inputId = "check2", 
                                      label = "Choose", 
                                      choices = c(unique(crime_csv$department_name), "Top Ten"),
                                      selected = "Top Ten ")),
    sliderInput("yearInput", "Select your desired year range:",
                min = 1975, max = 2013, value = c(1975, 2013)),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Description", tabName = "description")
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard", 
              fluidRow(
                # infoBoxes with fill=TRUEfluidRow(
                infoBoxOutput("totalBox"),
                infoBoxOutput("progressBox2"),
                infoBoxOutput("approvalBox2")
              )
              ,
              fluidRow(
                box(
                  title = "Compare your choice of cities", status = "info", solidHeader = TRUE,
                  collapsible = FALSE,
                  withSpinner(plotOutput("crime_hom", height =300, width = 400)),
                  withSpinner(plotOutput("crime_rape", height =300, width = 400)),
                  withSpinner(plotOutput("crime_rob", height =300, width = 400)),
                  withSpinner(plotOutput("crime_assault", height =300, width = 400))
                  
                ),
                box(
                  title = "Top ten cities with minimum crimes", status = "warning", solidHeader = TRUE,
                  collapsible = FALSE,
                  withSpinner(plotOutput("crime_hist", height =300, width = 450))
                ),
                box(
                  title = "Top ten cities with maximum crimes", status = "warning", solidHeader = TRUE,
                  collapsible = FALSE,
                  withSpinner(plotOutput("crime_hist_top_ten", height =300, width = 450))
                )
              )
      ),
      tabItem("description" , titleWidth = 450 ,
              fluidRow(
                box(
                  title = "Description", height =650, width = 12, status = "info", solidHeader = TRUE,
                  collapsible = FALSE, 
                  tags$h2("This app is intended to show which cities in the US are the safest."),
                  tags$h2('Filters that you can explore:'),
                  tags$ol(
                    tags$li( 'Use the drop-down menu on the side pannel to choose which cities to you want to compare.', style="font-size:25px"),
                    tags$li( 'Use the slider on the left to choose the range of years you want to look at.', style="font-size:25px")),
                  
                  tags$h2('The dashboard contains:'),
                  tags$ol(
                    tags$li(" The four graphs on the 'Compare your choice of cities' dashbox. These graphs get updated based on your selections.", style="font-size:25px"),
                    tags$li("The graph on right shows the top ten cities with minimum crimes. (Note- This graph remains static for the user to compare it with the filtered graph on the right)", style="font-size:25px"),
                    tags$li(" The graph on right shows the top ten cities with maximum crimes. (Note- This graph remains static for the user to compare it with the filtered graph on the right)", style="font-size:25px"),
                    tags$li(" At the top of the Dashboard, there are two value boxes which show the numerical figures for all the crimes for the selected cities and time period.  
                            There is also a box that displays the safest city in terms of overall violent crime for the time period.", style="font-size:25px")
                    )
                  ) # end of box
                
                ) # end of fluidRow
      ) # end of tab Item
      
      ),  # end of tabItems
    
    
    tags$head(tags$style(
      "#check2 input[type='checkbox']+span{ 
      color: black;} "
    )
    )
    ) # end of dashboardBody
    )
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  crime_filtered <- reactive(
    if (is.null(input$check2) || input$check2 == "Top Ten") {
      
      crime_csv_filtered <- crime_csv %>% 
        filter(year >= input$yearInput[1],
               year <= input$yearInput[2]) %>%
        group_by(department_name) %>% 
        summarize(violent_crime = sum(violent_crime),
                  homs_sum = sum(homs_sum),
                  rape_sum = sum(rape_sum),
                  rob_sum = sum(rob_sum),
                  agg_ass_sum = sum(agg_ass_sum)) %>% 
        mutate(department_name = fct_reorder(department_name, violent_crime, desc = FALSE))
      top_cities <- crime_csv_filtered %>% 
        arrange(violent_crime) %>% 
        slice(1:10)
      
      crime_csv_filtered <- crime_csv_filtered %>% filter(department_name %in% top_cities$department_name)
      crime_csv_filtered <- droplevels(crime_csv_filtered)
      crime_csv_filtered <- crime_csv_filtered %>% 
        mutate(department_name = fct_rev(fct_reorder(department_name, violent_crime, desc = TRUE))) %>% 
        arrange(violent_crime)
      crime_csv_filtered
    } else {
      
      crime_csv %>% 
        filter(year >= input$yearInput[1],
               year <= input$yearInput[2],
               department_name %in% input$check2) %>%
        group_by(department_name) %>% 
        summarize(violent_crime = sum(violent_crime),
                  homs_sum = sum(homs_sum),
                  rape_sum = sum(rape_sum),
                  rob_sum = sum(rob_sum),
                  agg_ass_sum = sum(agg_ass_sum)) %>% 
        mutate(department_name = fct_rev(fct_reorder(department_name, violent_crime, desc = FALSE)))
    }
  )
  
  # Select all / Unselect all
  observeEvent(input$all, ignoreNULL = FALSE, {
    if (is.null(input$check2) || input$check2 == "Top Ten") {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", selected = "Top Ten"
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", selected = "Albuquerque, N.M."
      )
    }
  })
  
  
  crime_total <- crime_csv %>% 
    group_by(department_name) %>% 
    summarize(violent_crime = sum(violent_crime)) %>% 
    mutate(department_name = fct_reorder(department_name, violent_crime, desc = FALSE))
  top_cities_total <- crime_total %>% 
    arrange(violent_crime) %>% 
    slice(1:10)
  
  crime_total <- crime_total %>% filter(department_name %in% top_cities_total$department_name)
  crime_total <- droplevels(crime_total)
  crime_total <- crime_total %>% 
    mutate(department_name = fct_rev(fct_reorder(department_name, violent_crime, desc = TRUE))) %>% 
    arrange(violent_crime)
  
  output$crime_hist <- renderPlot(
    crime_total %>%
      ggplot(aes(department_name, violent_crime))
    + geom_bar(fill="#E6C81C", colour="white",stat='identity')
    + scale_y_continuous(labels = scales::comma)
    + coord_flip() + labs(y= "Number of crimes", x= "US Cities") + ggtitle("Number of crimes per city")
  )
  
  
  crime_total_top_ten <- crime_csv %>% 
    group_by(department_name) %>% 
    summarize(violent_crime = sum(violent_crime)) %>% 
    mutate(department_name = fct_reorder(department_name, violent_crime, desc = TRUE))
  top_cities_total <- crime_total_top_ten %>% 
    arrange(desc(violent_crime)) %>% 
    slice(1:10)
  
  crime_total_top_ten <- crime_total_top_ten %>% filter(department_name %in% top_cities_total$department_name)
  crime_total_top_ten <- droplevels(crime_total_top_ten)
  crime_total_top_ten <- crime_total_top_ten %>% 
    mutate(department_name = fct_reorder(department_name, violent_crime, desc = TRUE)) %>% 
    arrange(violent_crime)
  
  output$crime_hist_top_ten <- renderPlot(
    crime_total_top_ten %>%
      ggplot(aes(department_name, violent_crime))
    + geom_bar(fill="#E6C81C", colour="white",stat='identity')
    + scale_y_continuous(labels = scales::comma)
    + coord_flip() + labs(y= "Number of crimes", x= "US Cities") + ggtitle("Number of crimes per city")
  )
  
  update_plot = function() {
    output$crime_hom <- renderPlot(
      crime_filtered() %>% 
        mutate(department_name = fct_rev(fct_reorder(department_name, homs_sum, desc = TRUE))) %>% 
        arrange(homs_sum) %>% 
        ggplot(aes(department_name, homs_sum))
      + geom_bar(fill="mediumaquamarine", colour="white",stat='identity')
      + scale_y_continuous(labels = scales::comma)
      + coord_flip()
      + labs(y= "Number of Homicides", x= "US Cities") 
      + ggtitle("Number of Homicides per city")   
    )
    output$crime_rape <- renderPlot(
      crime_filtered() %>% 
        mutate(department_name = fct_rev(fct_reorder(department_name, rape_sum, desc = TRUE))) %>% 
        arrange(rape_sum) %>% 
        ggplot(aes(department_name, rape_sum))
      + geom_bar(fill="mediumaquamarine", colour="white",stat='identity')
      + scale_y_continuous(labels = scales::comma)
      + coord_flip() + labs(y= "Number of Rapes", x= "US Cities") + ggtitle("Number of Rapes per city")   
    )
    output$crime_rob <- renderPlot(
      crime_filtered() %>% 
        mutate(department_name = fct_rev(fct_reorder(department_name, rob_sum, desc = TRUE))) %>% 
        arrange(rob_sum) %>% 
        ggplot(aes(department_name, rob_sum))
      + geom_bar(fill="mediumaquamarine", colour="white",stat='identity')
      + scale_y_continuous(labels = scales::comma)
      + coord_flip() + labs(y= "Number of Robberies", x= "US Cities") + ggtitle("Number of Robberies per city")   
    )
    output$crime_assault <- renderPlot(
      crime_filtered() %>%
        mutate(department_name = fct_rev(fct_reorder(department_name, agg_ass_sum, desc = TRUE))) %>% 
        arrange(agg_ass_sum) %>% 
        ggplot(aes(department_name, agg_ass_sum))
      + geom_bar(fill="mediumaquamarine", colour="white",stat='identity')
      + scale_y_continuous(labels = scales::comma)
      + coord_flip() + labs(y= "Number of Aggravated Assaults", x= "US Cities") + ggtitle("Number of Assaults per city")   
    )
  } # end of function
  
  observeEvent(eventExpr = input$check2, ignoreNULL = FALSE, {
    update_plot()
  })
  
  observeEvent(eventExpr = input$checkGroup, ignoreNULL = FALSE, {
    update_plot()
  })
  
  total_crimes <- reactive(
    sum(crime_filtered()$violent_crime)
  )
  
  avg_crimes <- reactive(
    total_crimes() / nrow(crime_filtered())
  )
  
  safest_city <- reactive(
    crime_filtered <- crime_csv %>% 
      filter(year >= input$yearInput[1],
             year <= input$yearInput[2]) %>%
      group_by(department_name) %>% 
      summarize(violent_crime = sum(violent_crime)) %>% 
      mutate(department_name = fct_reorder(department_name, violent_crime, desc = FALSE)) %>% 
      arrange(violent_crime) %>% 
      slice(1)
  )
  
  old_crime <- reactive(
    crime_filtered() %>% 
      filter(year == input$yearInput[1])
  )
  
  new_crime <- reactive(
    crime_filtered() %>% 
      filter(year == input$yearInput[2])
  )
  
  output$totalBox <- renderInfoBox({
    infoBox(
      "Total reported crimes", total_crimes(), fill = TRUE
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Average number of crimes", round(avg_crimes()), icon = icon("list"),
      color = "blue", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Safest City over Selected Time Frame" , safest_city()$department_name ,icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue", fill = TRUE
    )
  })
  
}
# Run the shiny app
shinyApp(ui=ui, server=server)