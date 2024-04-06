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
# source("src/market_spatial_analysis.R")

# load charts
source("src/load_charts.R")

# Define UI for app ####

ui = fluidPage(
  useShinyjs(),
  theme = my_theme,
  # Main Panel features start 
  mainPanel(
    width = 12,
    fluidRow(h1("Zip Code Demographics"), height = 50),
    fluidRow(
      column(4, 
        h4("Population"),
        h5("(hover on bubble)"),
        highchartOutput(outputId = "vt_map"), height = 600),
      column(4, 
        highchartOutput(outputId = "acs_age_plot", height = 300),
        highchartOutput(outputId = "income_boxplot", height = 300)),
      column(4, 
        highchartOutput(outputId = "acs_ethnicity_plot", height = 300),
        highchartOutput(outputId = "sex_composition", height = 300)
        )
      )
    )
  )


# Define Server ####
server <- function(input, output, session) {
  
  legendMouseOverFunction <- JS("function(event) {Shiny.onInputChange('legendmouseOver', this.name);}")
  
  values <- reactiveValues(default = "02143")
  
  observeEvent(input$legendmouseOver,{
    values$default <- input$legendmouseOver
  })
  
  bubble_map <- reactive({
    hcmap("countries/us/us-ma-all",
          nullColor = "#656565") |>
      hc_legend (enabled = FALSE)|>
      hc_add_series(
        data = zip_code_base,
        hcaes(group = zipcode),
        color = beercolors$light_blue,
        type = "mapbubble",
        minSize = "1%",
        maxSize = "8%",
        tooltip = list(pointFormat = "{point.major_city} <br> zip: {point.zipcode} <br> population: {point.z}")) |>
      hc_chart(borderColor = "#656565", borderWidth = 15) |>
      hc_plotOptions(
        series = list(events = list(mouseOver = legendMouseOverFunction)),
        credits = list(enabled = FALSE)
      ) |>
      hc_add_theme(hc_theme(chart = list(plotBackgroundColor = '#656565')))
  })
  
  output$vt_map <- renderHighchart({bubble_map()})
  
  selected_zip =  eventReactive(input$legendmouseOver, {
    input$legendmouseOver
  })
  
  selected_zips <- reactive({
    dynamic_data <<- zip_code_base |>
      mutate(
        dynamic_selection = selected_zip(),
        analysis_selection = if_else(zipcode == dynamic_selection, paste(dynamic_selection,major_city), "remaining aggregate"))
    return(dynamic_data)
    })
  
  
  acs_age_plot <- reactive({
    selected_zips() |>
      inner_join(
        state_sample,
        by = c("zipcode" = "zip_code")
        ) |>
      group_by(age_bracket, age_factor, analysis_selection) |>
      summarise(value = sum(estimate, na.rm = TRUE)) |>
      #mutate(age_bracket = fct_reorder(as.factor(age_bracket), desc)) |>
      ungroup() |>
      group_by(analysis_selection) |>
      mutate(
        total = sum(value),
        percent = (value/total)*100) |>
      ungroup() |>
      arrange(age_factor) |>
      hchart(
        type = "spline", 
        hcaes(x = age_bracket, y = percent, group = analysis_selection)) |>
      hc_legend(enabled = TRUE) |>
      hc_title(text = "Age") |>
      hc_yAxis(labels = list(format = "{value}%")) |>
      hc_colors(c(beercolors$light_blue, cool_winter_theme$off_white)) |>
      hc_add_theme(hc_theme_gsd())
    })
  
  output$acs_age_plot <- renderHighchart({acs_age_plot()})
  
  acs_ethnicity_plot <- reactive({
    selected_zips() |>
      inner_join(
        state_sample,
        by = c("zipcode" = "zip_code")
      ) |>
    group_by(concept, analysis_selection) |>
      summarise(value = sum(estimate, na.rm = TRUE)) |>
      #mutate(age_bracket = fct_reorder(as.factor(age_bracket), desc)) |>
      ungroup() |>
      group_by(analysis_selection) |>
      mutate(
        total = sum(value),
        percent = (value/total)*100) |>
      ungroup() |>
      arrange(desc(percent)) |>
      hchart(type = "bar", hcaes(x = concept, y = percent, group = analysis_selection)) |>
      hc_plotOptions(column = list(stacking = "normal")) |>
      hc_yAxis(labels = list(format = "{value}%")) |>
      hc_legend(enabled = TRUE) |>
      hc_title(text = "Group") |>
      hc_colors(c(beercolors$light_blue, cool_winter_theme$off_white)) |>
      hc_add_theme(hc_theme_gsd())
  })
  
  output$acs_ethnicity_plot <- renderHighchart({acs_ethnicity_plot()})
  
  acs_income_plot <- reactive({
    
    income_data <<- selected_zips() |>
      select(zipcode, analysis_selection) |>
      inner_join(
        state_sample,
        by = c("zipcode" = "zip_code")
      ) |>
      group_by(major_city, analysis_selection, state) |>
      summarise(median_household_income = as.numeric(first(median_household_income, na_rm = TRUE))) |>
      ungroup() |>
      filter(!is.na(median_household_income))
    
    income_boxplot <- highchart() |>
      hc_xAxis(type = "category") |>
      hc_chart(inverted = TRUE) |>
      hc_colors(c(green_state_data_theme$`off white`)) |>
      hc_add_series_list(data_to_boxplot(income_data, median_household_income, state)) |>
      hc_yAxis(
        title = list(text = "median income")) |>
      hc_add_series(
        data = income_data |>
          filter(analysis_selection == "remaining aggregate"),
        type = "scatter",
        hcaes(x = state, y = median_household_income, color = green_state_data_theme$medium_grey)
      ) |>
      hc_add_series(
        data = income_data |>
          filter(analysis_selection != "remaining aggregate"),
        type = "scatter",
        hcaes(x = state, y = median_household_income, size = 4, color = beercolors$light_blue)
      ) |>
      #hc_colors(c(beercolors$light_blue, green_state_data_theme$dark_grey)) |>
      hc_plotOptions(scatter = list(
        marker = list(
          radius = 5,
          symbol = "circle",
          lineWidth = 0.5
        )
      ))  |>
      hc_plotOptions(scatter = list(jitter = list(x = .2, y = 0))) |>
      hc_title(text = "Income") |>
      hc_add_theme(hc_theme_gsd()) |>
      hc_legend(enabled = FALSE)
    
    return(income_boxplot)
  })
  
  output$income_boxplot <- renderHighchart(acs_income_plot())
  
  sex_composition <- reactive({
    selected_zips() |>
      inner_join(
        state_sample,
        by = c("zipcode" = "zip_code")
      ) |>
      group_by(analysis_selection, sex) |>
      summarise(value = sum(estimate, na.rm = TRUE)) |>
      #mutate(age_bracket = fct_reorder(as.factor(age_bracket), desc)) |>
      ungroup() |>
      group_by(analysis_selection) |>
      mutate(
        total = sum(value),
        percent = (value/total)*100) |>
      ungroup() |>
      mutate(sort = if_else(analysis_selection == "remaining aggregate",2,1)) |>
      arrange(sort) |>
      hchart(type = "column", hcaes(x = analysis_selection, y = percent, group = sex)) |>
      hc_chart(inverted = TRUE) %>%
      hc_plotOptions(column = list(stacking = "percent")) |>
      hc_title(text = "Sex") |>
      hc_yAxis(
        labels = list(format = "{value}%"),
        max = 100) |>
      hc_legend(enabled = TRUE) |>
      hc_colors(c(cool_winter_theme$off_white, cool_winter_theme$mid_gray)) |>
      hc_add_theme(hc_theme_gsd())
  })
  
  output$sex_composition <- renderHighchart(sex_composition())

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

