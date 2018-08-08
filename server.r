##################################################
# A Shiny app for creating custom SHEF reports.  #
##################################################

## Load packages -----
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(openxlsx)

## Load data -----
df <- read.xlsx("SHEF_State_by_State_Wave_Charts_FY17.xlsx")

## Clean data -----
colnames(df) <- df[1, ] ## Rename columns
df <- df[-1, ]          ## Drop first row (column names)
df$`Fiscal Year` <- as.numeric(df$`Fiscal Year`)
df$`Net Public FTE Enrollment` <- round(as.numeric(df$`Net Public FTE Enrollment`), 
                                        digits = 2)
df$`Educational Appropriations per FTE, Constant Dollars` <- round(as.numeric(df$`Educational Appropriations per FTE, Constant Dollars`), 
                                                                   digits = 2)
df$`Net Tuition per FTE, Constant Dollars` <- round(as.numeric(df$`Net Tuition per FTE, Constant Dollars`), 
                                                    digits = 2)
df$`Student Share (Net Tuition as a Proportion of Total Educational Revenues)` <- round(as.numeric(df$`Student Share (Net Tuition as a Proportion of Total Educational Revenues)`), 
                                                                                        digits = 2)

## Make choices -----
default_state <- "Nevada"
state_choices <- unique(sort(df$State))
wue_choices <- c("Alaska",
                 "Arizona",
                 "California",
                 "Colorado",
                 "Hawaii",
                 "Idaho",
                 "Montana",
                 "New Mexico",
                 "North Dakota",
                 "Oregon",
                 "South Dakota",
                 "Utah",
                 "Washington",
                 "Wyoming",
                 "US")

## Beginning of server -----
shinyServer(function(input, output, session) {
  
  dataset <- reactive({
    df <- df[df$State %in% input$states, ]
  })
  
  ## Beginning of main body -----
  output$mainbody <- renderUI({
    fluidPage(
      
      theme = "mystyle.css",

      br(), br(),
      br(), br(),
      titlePanel("State Higher Education Finance (SHEF)"), br(),
      h4("FY 1992-2017"), br(),
      br(), br(),
      br(), br(), 
      
      ## Sidebar and Subsetting Options -----
      sidebarLayout(
        sidebarPanel(
          ## Subsetting options 
          selectInput(inputId = "states",
                      label = h5("States"),
                      choices = list("Nevada" = default_state,
                                    "Western Undergraduate Exchange (WUE)" = wue_choices
                                    ),
                      multiple = TRUE,
                      selected = default_state
                      ),
          br(), br()
        ), 
        ## End of side bar
        
        ## Main Panel -----
        mainPanel(
          output$plot1 <- renderPlot(
          ggplot(dataset(), 
                aes(x = `Fiscal Year`, 
                    y = `Net Public FTE Enrollment`, 
                    fill = input$states)) +
            geom_point() +
            geom_line() +
            scale_x_continuous(breaks = seq(1992, 2017, by = 1)) +
            labs(title = "Net Public FTE Enrollment",
                  x = "Fiscal Year",
                  y = "") +
            expand_limits(y = 0) +
            theme_classic() +
            theme(legend.position = "bottom")
          ),
          
          br(), hr(), br(),
          
          output$plot2 <- renderPlot(
            ggplot(dataset(), 
                   aes(x = `Fiscal Year`, 
                       y = `Educational Appropriations per FTE, Constant Dollars`, 
                       fill = input$states)) +
              geom_point() +
              geom_line() +
              scale_x_continuous(breaks = seq(1992, 2017, by = 1)) +
              labs(title = "Educational Appropriations per FTE",
                   x = "Fiscal Year",
                   y = "Constant Dollars") +
              expand_limits(y = 0) +
              theme_classic() +
              theme(legend.position = "bottom")
          ),
        
          br(), hr(), br(),
          
          output$plot3 <- renderPlot(
            ggplot(dataset(), 
                   aes(x = `Fiscal Year`, 
                       y = `Net Tuition per FTE, Constant Dollars`, 
                       fill = input$states)) +
              geom_point() +
              geom_line() +
              scale_x_continuous(breaks = seq(1992, 2017, by = 1)) +
              labs(title = "Net Tuition per FTE",
                   x = "Fiscal Year",
                   y = "Constant Dollars") +
              expand_limits(y = 0) +
              theme_classic() +
              theme(legend.position = "bottom")
          ),
          
          br(), hr(), br(),
          
          output$plot4 <- renderPlot(
            ggplot(dataset(), 
                   aes(x = `Fiscal Year`, 
                       y = `Student Share (Net Tuition as a Proportion of Total Educational Revenues)`,
                       fill = input$states)) +
              geom_point() +
              geom_line() +
              scale_x_continuous(breaks = seq(1992, 2017, by = 1)) +
              labs(title = "Student Share",
                   x = "Fiscal Year",
                   y = "") +
              expand_limits(y = 0) +
              theme_classic() +
              theme(legend.position = "bottom")
          )
        ) ## End of Main panel
      ) ## End of Sidebar Layout
    ) ## End of Fluid Page
  }) ## End of Main body
})

## End of server -----
