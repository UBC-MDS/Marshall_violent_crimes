# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#  http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(cowplot)
library(shinydashboard)


plot_data_column = function (column, data, x){
  data %>% ggplot(aes_string(x, column)) +
    geom_bar(fill="#CC79A7", colour="black",stat='identity') +
    coord_flip() + labs(y= column, x= "US Cities") +
    ggtitle("Number of crimes per city")
}

crime_csv <- read.csv("ucr_crime_1975_2015_filtered.csv", stringsAsFactors = FALSE)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "US crime rate analysis"),
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
    checkboxGroupInput("checkGroup", label = "Select type of crime:",
                       choices = c("Homicide" = "homs_sum",
                                   "Rape" = "rape_sum",
                                   "Robbery" = "rob_sum",
                                   "Aggravated Assault" = "agg_ass_sum" ),
                       selected = 1),
    sliderInput("yearInput", "Select your desired year range:",
                min = 1975, max = 2013, value = c(1975, 2013))
    
  ),
  dashboardBody(
    # infoBoxes with fill=TRUEfluidRow(
    infoBox("Total crime", 12000, fill = TRUE),
    infoBoxOutput("progressBox2"),
    infoBoxOutput("approvalBox2")
    ,
    fluidRow(
      box(
        title = "Top ten cities with maximum crime", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("crime_hist", height =700)
      )
    )
  ),
  tags$head(tags$style(
    "#check2 input[type='checkbox']+span{ 
    color: black;} "
  )
  )
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
        mutate(department_name = fct_reorder(department_name, violent_crime, desc = TRUE))
      top_cities <- crime_csv_filtered %>% 
        arrange(desc(violent_crime)) %>% 
        slice(1:10)
      
      crime_csv_filtered <- crime_csv_filtered %>% filter(department_name %in% top_cities$department_name)
      crime_csv_filtered <- droplevels(crime_csv_filtered)
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
        mutate(department_name = fct_reorder(department_name, violent_crime, desc = TRUE))
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
  observeEvent(eventExpr = input$checkGroup, ignoreNULL = FALSE, {
    if (is.null(input$checkGroup)) {
      
      output$crime_hist <- renderPlot(
        crime_filtered() %>%
          ggplot(aes(department_name, violent_crime))
        + geom_bar(fill="mediumaquamarine", colour="white",stat='identity')
        + coord_flip() + labs(y= "Number of crimes", x= "US Cities") + ggtitle("Number of crimes per city")
      )    
    } else {
      
      plots <- list()
      plots <- lapply(input$checkGroup, plot_data_column, data = crime_filtered(), x="department_name")
      
      output$crime_hist <- renderPlot(
        plot_grid(plotlist=plots)
      )
    }
  })
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
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