# zip census testing 

# Plot of race/ethnicity by county in Illinois for 2010
library(tidycensus)
library(tidyverse)
library(highcharter)
# 
census_api_key(Sys.getenv("census_key"))
# 
year <- 2021

# ACS zipcode  ####
acs_vars <-load_variables(
  dataset = "acs5",
  year = year, cache = FALSE) |>
  mutate(id = name)

# tract by Age and Sex
acs_vars_1 <- acs_vars |>
  head(279) |>
  pull(id)

# this works for all states # this takes ~ 4 mins
acs_pop_data <- get_acs(geography = "zcta", variables = acs_vars_1 , year = year)

acs_demographic_data <- acs_pop_data |>
  inner_join(
    acs_vars,
    by = c("variable" = "name")) |>
  as.tibble() |>
  select(GEOID,NAME,label,estimate, concept) |>
  mutate(
    label = str_replace_all(paste(label), "!",""), #add concept to concat
    label = str_remove(label, "EstimateTotal:"),
    label = if_else(label == "", "Total Population", label),
    concept = str_remove(concept, "SEX BY AGE ")
  ) |>
  filter(label != "Male:" & label != "Female:" & label != "Total Population",
concept != "(WHITE ALONE)") |>
  separate_wider_delim(
    cols = label,
    delim = ":",
    names = c("sex", "age_bracket")
  ) |>
  mutate(
    age_factor = case_when(
      age_bracket == "Under 5 years" ~ 1,
      age_bracket == "5 to 9 years" ~ 2,
      age_bracket == "10 to 14 years" ~ 3,
      age_bracket == "15 to 17 years" ~ 4,
      age_bracket == "18 and 19 years" ~ 5,
      age_bracket == "20 to 24 years" ~ 6,
      age_bracket == "25 to 29 years" ~ 7,
      age_bracket == "30 to 34 years" ~ 8,
      age_bracket == "35 to 44 years" ~ 9,
      age_bracket == "45 to 54 years" ~ 10,
      age_bracket == "55 to 64 years" ~ 11,
      age_bracket == "65 to 74 years" ~ 12,
      age_bracket == "75 to 84 years" ~ 13,
      age_bracket == "85 years and over" ~ 14
    ),
    concept = case_when(
      concept == "(AMERICAN INDIAN AND ALASKA NATIVE ALONE)" ~ "American Indian and Alaska Native",
      concept == "(ASIAN ALONE)" ~ "Asian",
      concept == "(BLACK OR AFRICAN AMERICAN ALONE)" ~ "Afrian American or Black",
      concept == "(HISPANIC OR LATINO)" ~ "Hispanic or Latino",
      concept == "(NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)" ~ "Native Hawaiian or Pacific Island",
      concept == "(SOME OTHER RACE ALONE)" ~ "Other Race",
      concept == "(TWO OR MORE RACES)" ~ "More than one race",
      concept == "(WHITE ALONE, NOT HISPANIC OR LATINO)" ~ "White"
    ),
    zip_code = str_remove(NAME, "ZCTA5 ")
  )

set.seed(121)

selected_zips <- zip_code_db |>
  filter(state == "MA") |>
  mutate(lon = lng) |>
  sample_n(70) |>
  mutate(home = if_else(zipcode == "02143", "home", "other"))

state_sample <- acs_demographic_data |>
  inner_join(selected_zips, by = c("zip_code" = "zipcode"))



saveRDS(state_sample, "state_sample.rds")

# explore to match total population
vt_overall <- acs_demographic_data |>
  inner_join(
    zip_code_db |> filter(state == "VT"), 
    by = c("zip_code" = "zipcode")) |>
  filter(concept != "(WHITE ALONE)") |>
  group_by(concept) |>
  summarise(pop = sum(estimate)) |>
  ungroup() |>
  mutate(percent = pop / sum(pop))


acs_age_plot_data <- state_sample |>
  group_by(age_bracket, age_factor, home) |>
  summarise(value = sum(estimate, na.rm = TRUE)) |>
  #mutate(age_bracket = fct_reorder(as.factor(age_bracket), desc)) |>
  ungroup() |>
  group_by(home) |>
  mutate(
    total = sum(value),
    percent = value/total) |>
  ungroup() |>
  arrange(age_factor)

acs_age_plot <- acs_age_plot_data |>
  hchart(type = "spline", hcaes(x = age_bracket, y = percent, group = home)) |>
  hc_legend (enabled = TRUE) |>
  hc_colors(c(green_state_date_theme$`Bright green`, green_state_date_theme$dark_grey)) 
acs_age_plot

acs_race_plot_data <- state_sample |>
  group_by(concept, home) |>
  summarise(value = sum(estimate, na.rm = TRUE)) |>
  #mutate(age_bracket = fct_reorder(as.factor(age_bracket), desc)) |>
  ungroup() |>
  group_by(home) |>
  mutate(
    total = sum(value),
    percent = value/total) |>
  ungroup()

acs_race_plot <- acs_race_plot_data |>
  arrange(desc(percent)) |>
  hchart(type = "bar", hcaes(x = concept, y = percent, group = home)) |>
  hc_plotOptions(column = list(stacking = "normal")) |>
  hc_legend (enabled = TRUE) |>
  hc_colors(c(green_state_date_theme$`Bright green`, green_state_date_theme$dark_grey)) 
acs_race_plot

acs_income_plot_data <- state_sample |>
  group_by(NAME, home, state) |>
  summarise(median_household_income = as.numeric(first(median_household_income, na_rm = TRUE))) |>
  ungroup() |>
  filter(!is.na(median_household_income))

box_data <- data_to_boxplot(acs_income_plot_data,)

income_boxplot <- highchart() |>
  hc_xAxis(type = "category") |>
  hc_chart(inverted = TRUE) |>
  hc_colors(c(green_state_date_theme$`off white`)) |>
  hc_add_series_list(data_to_boxplot(acs_income_plot_data, median_household_income, state)) |>
  hc_title(text = "Median Income by Zipcode") |>
  hc_yAxis(title = list(text = "median income")) |>
  hc_add_series(
    data = acs_income_plot_data |>
      filter(home == "other"),
    type = "scatter",
    hcaes(x = state, y = median_household_income, color = green_state_date_theme$medium_grey)
  ) |>
  hc_add_series(
    data = acs_income_plot_data |>
      filter(home != "other"),
    type = "scatter",
    hcaes(x = state, y = median_household_income, size = 4, color = green_state_date_theme$`Bright green`)
  ) |>
  #hc_colors(c(green_state_date_theme$`Bright green`, green_state_date_theme$dark_grey)) |>
  hc_plotOptions(scatter = list(
    marker = list(
      radius = 5,
      symbol = "circle",
      lineWidth = 0.5
    )
  ))  |>
  hc_plotOptions(scatter = list(jitter = list(x = .2, y = 0))) |>
  hc_legend(enabled = FALSE)


# Pull Income by Race (not working yet) ####
# This was dumb its in zipcodeDB
# variables for selection start around row 2124
acs_income_vars <- acs_vars |>
  filter(concept == "MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS) BY AGE OF HOUSEHOLDER") |>
  pull(name)

acs_income_1 <- get_acs(
  geography = "zcta",
  variables = acs_income_vars,
  year = year,show_call = TRUE
)

acs_income_data <- acs_income_1 |>
  inner_join(
    acs_vars, 
    by = c("variable" = "name")) |>
  as.tibble() |>
  select(GEOID,NAME,label,estimate, concept) |>
  mutate(
    label = str_replace_all(paste(label), "!",""), #add concept to concat
    label = str_remove(label, "EstimateTotal:"),
    label = if_else(label == "", "Total Population", label),
    concept = str_remove(concept, "SEX BY AGE ")
  ) |>
  filter(!str_detect(label,"Householder")) |>
  mutate(zip_code = str_remove(NAME, "ZCTA5 ")) |>
  select(zip_code, median_income = estimate)


sex_composition <- dynamic_data |>
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
  hc_chart(inverted=T) %>%
  hc_plotOptions(column = list(stacking = "percent")) |>
  hc_yAxis(
    labels = list(format = "{value}%"),
    max = 100) |>
  hc_legend(enabled = TRUE) |>
  hc_colors(c("#F57E73", cool_winter_theme$mid_gray)) |>
  hc_add_theme(hc_theme_gsd())
sex_composition
