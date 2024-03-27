

# Plot of race/ethnicity by county in Illinois for 2010
library(tidycensus)
library(tidyverse)
library(highcharter)

census_api_key(Sys.getenv("census_key"))

year <- 2022

# ACS zipcode  ####
acs_vars <-load_variables(
  dataset = "acs5",
  year = 2022, cache = FALSE)

# tract by Age and Sex
acs_vars_1 <- acs_vars |>
  head(279) |>
  pull(name)

# this works for vermont
acs_demographic_1 <- get_acs(
  variables = acs_vars_1,
  geography = "county subdivision",
  state = "VT",
  year = 2022,
  output = "tidy",
  geometry = TRUE
  )

acs_demographic_data <- acs_demographic_1 |>
  inner_join(
    acs_vars, 
    by = c("variable" = "name")) |>
  as.tibble() |>
  select(GEOID,NAME,label,estimate, concept) |>
  mutate(
    label = str_replace_all(paste(label), "!",""), #add concept to concat
    label = str_remove(label, "EstimateTotal:"),
    label = if_else(label == "", "Total Population", label),
    concept = str_remove(concept, "Sex by Age ")
    # concept = str_remove("(", concept),
    # concept = str_remove(")", concept)
    ) |>
  filter(label != "Male:" & label != "Female:" & label != "Total Population" ) |>
  separate_wider_delim(cols = label, delim = ":", names = c("sex", "age_bracket")) |>
  mutate(age_factor = case_when(
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
  ))
  
# wide_acs_demographic_data <- acs_demographic_data |>
#   pivot_wider(id_cols = c(NAME, GEOID), names_from = label, values_from = estimate)



acs_age_gender_plot <- acs_demographic_data |>
  group_by(age_bracket, age_factor, NAME) |>
  summarise(value = sum(estimate, na.rm = TRUE)) |>
  #mutate(age_bracket = fct_reorder(as.factor(age_bracket), desc)) |>
  ungroup() |>
  group_by(NAME) |>
  mutate(
    total = sum(value),
    percent = value/total) |>
  ungroup() |>
  arrange(age_factor)

out <- acs_age_gender_plot |>
  hchart(type = "spline", hcaes(x = age_bracket, y = percent, group = NAME)) |>
  hc_legend (enabled = FALSE)
  #hc_colors(c(green_state_date_theme$`Bright green`, green_state_date_theme$dark_grey)) 
out
  #hc_plotOptions(credits = list(enabled = FALSE)) |>

  
# Pull Income by Race (not working yet) #### 
acs_income_vars <- acs_vars |>
  filter(label == "Estimate!!Aggregate household income in the past 12 months (in 2022 inflation-adjusted dollars)") |>
  pull(name)

acs_incomec_1 <- get_acs(
  variables = acs_income_vars,
  geography = "county",
  state = "VT",
  year = 2022,
  output = "tidy",
  geometry = TRUE
)

acs_income_data <- acs_incomec_1 |>
  inner_join(
    acs_vars, 
    by = c("variable" = "name")) |>
  as.tibble() |>
  select(GEOID,NAME,label,estimate, concept) |>
  mutate(
    label = str_replace_all(paste(label,concept), "!",""),
    label = str_remove(label, "EstimateTotal:"),
    label = if_else(label == "", "Total Population", label),
    label = str_remove(label, "Sex by Age ")
  ) |>
  select(-concept)
 
