# Load charts

#cloud <- readRDS("cloud.rds")
state_sample <- readRDS("state_sample.rds")

# Start the VT sample one with highcharts (save the image when done)
set.seed(121)
selected_zips <- zip_code_db |> 
  filter(state == "MA") |>
  mutate(lon = lng) |>
  sample_n(30) |>
  mutate(home = if_else(zipcode == "05477", cool_winter_theme$off_white, green_state_data_theme$background_grey))

zip_code_base <- selected_zips |>
  mutate(z = population) |>
  select(lat,lon, zipcode, z, major_city, home) 
