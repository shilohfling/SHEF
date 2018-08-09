##################################################
# A Shiny app for creating custom SHEF reports.  #
##################################################

## Load packages -----
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(openxlsx)

## Load sources -----
source("ggplot2_formatter.R")

## Load data -----
df <- read.xlsx("SHEF_State_by_State_Wave_Charts_FY17.xlsx")

## Clean data -----
colnames(df) <- df[1, ] ## Rename columns
df <- df[-1, ]          ## Drop first row (column names)
df$`Fiscal Year` <- as.numeric(df$`Fiscal Year`)
df$`Net Public FTE Enrollment` <- round(as.numeric(df$`Net Public FTE Enrollment`), 2)
df$`Educational Appropriations per FTE, Constant Dollars` <- round(as.numeric(df$`Educational Appropriations per FTE, Constant Dollars`), 2)
df$`Net Tuition per FTE, Constant Dollars` <- round(as.numeric(df$`Net Tuition per FTE, Constant Dollars`), 2)
df$`Student Share (Net Tuition as a Proportion of Total Educational Revenues)` <- round(as.numeric(df$`Student Share (Net Tuition as a Proportion of Total Educational Revenues)`), 2)

## Rescale Net Public FTE Enrollment
for(i in 1:length(df$`Net Public FTE Enrollment`)) {
  if(df$`Fiscal Year`[i] == 1992) {
    base <- df$`Net Public FTE Enrollment`[i]
  }
  df$Rescale[i] <- round((df$`Net Public FTE Enrollment`[i] / base), 2)
}

## Make choices -----
default_state <- "Nevada"
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
                 "Wyoming")
usa <- "US"
state_choices <- df[!(df$State) %in% wue_choices, ] %>%
                  .[!(.$State) %in% usa, ] %>%
                  .[!(.$State) %in% default_state, ]
state_choices <- unique(state_choices$State) %>%
                  sort(.)

## Aesthetics -----
color <- c(
            ## Default State
            "Nevada" = "#60ba9c",
            ## Overall USA
            "US" = "#115edb",
            ## WUE States
            "Alaska"         = "#666666", "Arizona"       = "#666666", "California"     = "#666666", "Colorado"       = "#666666",
            "Hawaii"         = "#666666", "Idaho"         = "#666666", "Montana"        = "#666666", "New Mexico"     = "#666666",
            "North Dakota"   = "#666666", "Oregon"        = "#666666", "South Dakota"   = "#666666", "Utah"           = "#666666",
            "Washington"     = "#666666", "Wyoming"       = "#666666",
            ## All other States
            "Alabama"        = "#000000", "Arkansas"      = "#000000", "Connecticut"    = "#000000", "Delaware"       = "#000000", 
            "Florida"        = "#000000", "Georgia"       = "#000000", "Illinois"       = "#000000", "Indiana"        = "#000000", 
            "Iowa"           = "#000000", "Kansas"        = "#000000", "Kentucky"       = "#000000", "Louisiana"      = "#000000", 
            "Maine"          = "#000000", "Maryland"      = "#000000", "Massachusetts"  = "#000000", "Michigan"       = "#000000", 
            "Minnesota"      = "#000000", "Mississippi"   = "#000000", "Missouri"       = "#000000", "Nebraska"       = "#000000", 
            "New Hampshire"  = "#000000", "New Jersey"    = "#000000", "New York"       = "#000000", "North Carolina" = "#000000", 
            "Ohio"           = "#000000", "Oklahoma"      = "#000000", "Pennsylvania"   = "#000000", "Rhode Island"   = "#000000", 
            "South Carolina" = "#000000", "Tennessee"     = "#000000", "Texas"          = "#000000", "Vermont"        = "#000000", 
            "Virginia"       = "#000000", "Washington DC" = "#000000", "West Virginia"  = "#000000", "Wisconsin"      = "#000000"
)
bold.text <- element_text(face = "bold")

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
                      choices = list(default_state,
                                    "Overall United States" = usa,
                                    "Western Undergraduate Exchange (WUE)" = wue_choices,
                                    "Other States" = state_choices
                                    ),
                      multiple = TRUE,
                      selected = default_state
                      ),
          br(), br()
        ), 
        ## End of side bar
        
        ## Main Panel -----
        mainPanel(
          ## First plot
          output$plot1 <- renderPlot(
          ggplot(dataset(), 
                aes(x = `Fiscal Year`, 
                    y = Rescale, 
                    color = State,
                    shape = State)) +
            geom_point() +
            geom_line() +
            scale_x_continuous(breaks = seq(1992, 2017, by = 2)) +
            scale_color_manual(values = color) +
            scale_shape_manual(values = c(0:25)) +
            labs(title = "Net Public FTE Enrollment",
                  x = "Fiscal Year",
                  y = "") +
            guides(fill = guide_legend(title = "States")) +
            expand_limits(y = 0) +
            theme_classic() +
            theme(title = bold.text,
                  axis.title = bold.text,
                  axis.text.x = bold.text,
                  axis.text.y = bold.text) +
            theme(legend.position = "bottom")
          ),
          
          br(), hr(), br(),
          ## Second Plot
          output$plot2 <- renderPlot(
            ggplot(dataset(), 
                   aes(x = `Fiscal Year`, 
                       y = `Educational Appropriations per FTE, Constant Dollars`, 
                       color = State,
                       shape = State)) +
              geom_point() +
              geom_line() +
              scale_x_continuous(breaks = seq(1992, 2017, by = 2)) +
              scale_y_continuous(labels = human_usd) +
              scale_color_manual(values = color) +
              scale_shape_manual(values = c(0:25)) +
              labs(title = "Educational Appropriations per FTE",
                   x = "Fiscal Year",
                   y = "Constant Dollars") +
              guides(fill = guide_legend(title = "States")) +
              expand_limits(y = 0) +
              theme_classic() +
              theme(title = bold.text,
                    axis.title = bold.text,
                    axis.text.x = bold.text,
                    axis.text.y = bold.text) +
              theme(legend.position = "bottom")
          ),
        
          br(), hr(), br(),
          ## Third plot
          output$plot3 <- renderPlot(
            ggplot(dataset(), 
                   aes(x = `Fiscal Year`, 
                       y = `Net Tuition per FTE, Constant Dollars`, 
                       color = State,
                       shape = State)) +
              geom_point() +
              geom_line() +
              scale_x_continuous(breaks = seq(1992, 2017, by = 2)) +
              scale_y_continuous(labels = human_usd) +
              scale_color_manual(values = color) +
              scale_shape_manual(values = c(0:25)) +
              labs(title = "Net Tuition per FTE",
                   x = "Fiscal Year",
                   y = "Constant Dollars") +
              guides(fill = guide_legend(title = "States")) +
              expand_limits(y = 0) +
              theme_classic() +
              theme(title = bold.text,
                    axis.title = bold.text,
                    axis.text.x = bold.text,
                    axis.text.y = bold.text) +
              theme(legend.position = "bottom")
          ),
          
          br(), hr(), br(),
          ## Fourth plot
          output$plot4 <- renderPlot(
            ggplot(dataset(), 
                   aes(x = `Fiscal Year`, 
                       y = `Student Share (Net Tuition as a Proportion of Total Educational Revenues)`,
                       color = State,
                       shape = State)) +
              geom_point() +
              geom_line() +
              scale_x_continuous(breaks = seq(1992, 2017, by = 2)) +
              scale_color_manual(values = color) +
              scale_shape_manual(values = c(0:25)) +
              labs(title = "Student Share",
                   x = "Fiscal Year",
                   y = "") +
              guides(fill = guide_legend(title = "States")) +
              expand_limits(y = 0) +
              theme_classic() +
              theme(title = bold.text,
                    axis.title = bold.text,
                    axis.text.x = bold.text,
                    axis.text.y = bold.text) +
              theme(legend.position = "bottom") 
          )
        ) ## End of Main panel
      ) ## End of Sidebar Layout
    ) ## End of Fluid Page
  }) ## End of Main body
})

## End of server -----
