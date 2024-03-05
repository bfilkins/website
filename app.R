# Title: app.R script for website example charts
# Author: Brendan Filkins
# Date: 2024-2-25
# Notes:

# rsconnect::setAccountInfo(name='greenstatedata', token='C5D549B8A327628C42A4462F020528AD', secret='WTfEIsRkYvXZnr5ZGT51mTuaBaw/2nyU2bqlqA84')
# rsconnect::deployApp()

# Load Libraries
source("src/libraries.R")

# Define themes
source("src/theme.R")

# make charts (uncomment to refresh)
# source("src/wordcloud.R")
# source(")

# load charts
source("src/load_charts.R")

# Define UI for app ####

ui = fluidPage(
  useShinyjs(),
  theme = my_theme,
  # tags$style(
  #   type = "text/css",
  #   paste0(
  #     "body{background-image: linear-gradient(to right top,", bg_color, ", ", cool_winter_theme$light_gray, ", ", bg_color,", ", bg_color, ");}
  #     .transparentBackgroundColorMixin(@alpha,@color) {
  #       background-color: rgba(red(@color), green(@color), blue(@color), @alpha);
  #       }" 
  #   )
  # ),
  # Main Panel features start 
  mainPanel(
    width = 12,
    fluidRow(
      column(6, highchartOutput(outputId = "cloud")),
      column(6, highchartOutput(outputId = "first_plot"))
      )
    )
  )





# Define Server ####
server <- function(input, output, session) {

  output$cloud <- renderHighchart(cloud)

}

# Create Shiny app ----
#options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

shinyApp(ui = ui, server = server)


