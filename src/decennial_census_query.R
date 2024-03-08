# decennial_census 

#tidycensus DP1 ####

vars <-load_variables(
  dataset = "dp",
  year = 2020, cache = FALSE) |>
  head(20) |>
  pull(name)

# this works for vermont
demographic <- get_decennial(
  variables = vars,
  geography = "county subdivision",
  state = "VT",
  year = 2020,
  output = "tidy",
  geometry = TRUE
)


