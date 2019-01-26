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
  dashboardHeader(title = "Compare safest cities in US"),
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
                infoBox("Total reported crimes", 24203949, fill = TRUE),
                infoBoxOutput("progressBox2"),
                infoBoxOutput("approvalBox2")
              )
              ,
              fluidRow(
                box(
                  title = "Compare your choice of cities", status = "info", solidHeader = TRUE,
                  collapsible = FALSE,
                  plotOutput("crime_hom", height =300, width = 400),
                  plotOutput("crime_rape", height =300, width = 400),
                  plotOutput("crime_rob", height =300, width = 400),
                  plotOutput("crime_assault", height =300, width = 400)
                  
                ),
                box(
                  title = "Top ten cities with minimum crimes", status = "warning", solidHeader = TRUE,
                  collapsible = FALSE,
                  plotOutput("crime_hist", height =300, width = 350)
                ),
                box(
                  title = "Top ten cities with maximum crimes", status = "warning", solidHeader = TRUE,
                  collapsible = FALSE,
                  plotOutput("crime_hist_top_ten", height =300, width = 450)
                )
              )
      ),
      tabItem("description",
              fluidRow(
                box(
                  title = "Description", status = "info", solidHeader = TRUE,
                  collapsible = FALSE, 
                  tags$h4("This app is intended to show which cities in the US are the safest."),
                  tags$h5('Filters that you can explore:'),
                 tags$ol(
                   tags$li( 'Use the drop-down menu on the side pannel to choose which cities to you want to compare.'),
                   tags$li( 'Use the slider on the left to choose the range of years you want to look at.')),
                  
                tags$h5('The dashboard contains:'),
                tags$ol(
                  tags$li(" The four graphs on the 'Compare your choice of cities' dashbox. These graphs get updated based on your selections."),
                  tags$li("The graph on right shows the top ten cities with minimum crimes. (Note- This graph remains static for the user to compare it with the filtered graph on the right)"),
                  tags$li(" The graph on right shows the top ten cities with maximum crimes. (Note- This graph remains static for the user to compare it with the filtered graph on the right)"),
                  tags$li(" At the top of the Dashboard, there are three value boxes which show the numerical figures for all the crimes in the total year range.(Note- This remains static for the user to broswer over the actual figures.")
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
        ggplot(aes(department_name, homs_sum))
      + geom_bar(fill="mediumaquamarine", colour="white",stat='identity')
      + scale_y_continuous(labels = scales::comma)
      + coord_flip()
      + labs(y= "Number of Homicides", x= "US Cities") 
      + ggtitle("Number of Homicides per city")   
    )
    output$crime_rape <- renderPlot(
      crime_filtered() %>% 
        ggplot(aes(department_name, rape_sum))
      + geom_bar(fill="mediumaquamarine", colour="white",stat='identity')
      + scale_y_continuous(labels = scales::comma)
      + coord_flip() + labs(y= "Number of Rapes", x= "US Cities") + ggtitle("Number of Rapes per city")   
    )
    output$crime_rob <- renderPlot(
      crime_filtered() %>% 
        ggplot(aes(department_name, rob_sum))
      + geom_bar(fill="mediumaquamarine", colour="white",stat='identity')
      + scale_y_continuous(labels = scales::comma)
      + coord_flip() + labs(y= "Number of Robberies", x= "US Cities") + ggtitle("Number of Robberies per city")   
    )
    output$crime_assault <- renderPlot(
      crime_filtered() %>% 
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
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Average number of crimes", 9004, icon = icon("list"),
      color = "blue", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Decrease in overall crime" ,"0.4%",icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue", fill = TRUE
    )
  })
  
}
# Run the shiny app
shinyApp(ui=ui, server=server)