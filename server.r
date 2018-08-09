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
color <- c("Nevada" = "60ba9c")
for(i in 1:length(df$State)) {
  if(df$State[i] == default_state) {
      color[i] <- "#60ba9c"
    } else if (df$State[i] %in% wue_choices) {
      df$Color[i] <- "#666666"
    } else if(df$State[i] == usa) {
      df$Color[i] <- "#115edb"
    } else {
      df$Color[i] <- "#000000"
    }
}

# concat <- c()
# for (i in 1:length(unique(df$State))) {
#   concat[i] <- paste("\"", df$State[i], "\"", " = ", "\"", df$Color[i], "\"")
# }

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
            scale_color_manual(values = concat) +
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
              scale_color_manual(values = df$Color) +
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
              scale_color_manual(values = df$Color) +
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
              scale_color_manual(values = df$Color) +
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
