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

## Rescale Net Public FTE Enrollment
for(i in 1:length(df$`Net Public FTE Enrollment`)) {
  if(df$`Fiscal Year`[i] == 1992) {
    base <- df$`Net Public FTE Enrollment`[i]
  }
  df$Rescale[i] <- round((df$`Net Public FTE Enrollment`[i] / base), 2)
}




