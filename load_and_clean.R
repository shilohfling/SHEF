##################################################
# A Shiny app for creating custom SHEF reports.  #
##################################################

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

## Rescale function
# rescale_df <- function(x, y) {
#       for(i in 1:length(x)) {
#         if(df$`Fiscal Year`[i] == 1992) {
#           base <- x[i]
#         }
#         x[i] <- round((x[i] / base), 2)
#       }
# }
# 
# df$rescale_fte <- rescale_df(df$`Net Public FTE Enrollment`, df$rescale_fte)
# df$RescaleStudentShare <- rescale_df(df$`Student Share (Net Tuition as a Proportion of Total Educational Revenues)`)

## Rescale Net Public FTE Enrollment
for(i in 1:length(df$`Net Public FTE Enrollment`)) {
  if(df$`Fiscal Year`[i] == 1992) {
    base <- df$`Net Public FTE Enrollment`[i]
  }
  df$RescalePublFTE[i] <- round((df$`Net Public FTE Enrollment`[i] / base), 2)
}

## Rescale Educational Appropriations per FTE
for(i in 1:length(df$`Educational Appropriations per FTE, Constant Dollars`)) {
  if(df$`Fiscal Year`[i] == 1992) {
    base <- df$`Educational Appropriations per FTE, Constant Dollars`[i]
  }
  df$RescaleEducApp[i] <- round((df$`Educational Appropriations per FTE, Constant Dollars`[i] / base), 2)
}

## Rescale Net Tuition per FTE
for(i in 1:length(df$`Net Tuition per FTE, Constant Dollars`)) {
  if(df$`Fiscal Year`[i] == 1992) {
    base <- df$`Net Tuition per FTE, Constant Dollars`[i]
  }
  df$RescaleNetTuition[i] <- round((df$`Net Tuition per FTE, Constant Dollars`[i] / base), 2)
}

## Rescale Student Share
for(i in 1:length(df$`Student Share (Net Tuition as a Proportion of Total Educational Revenues)`)) {
  if(df$`Fiscal Year`[i] == 1992) {
    base <- df$`Student Share (Net Tuition as a Proportion of Total Educational Revenues)`[i]
  }
  df$RescaleStudentShare[i] <- round((df$`Student Share (Net Tuition as a Proportion of Total Educational Revenues)`[i] / base), 2)
}


