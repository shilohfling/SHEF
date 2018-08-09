##################################################
# A Shiny app for creating custom SHEF reports.  #
##################################################

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