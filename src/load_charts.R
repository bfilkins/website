# Load charts

cloud <- readRDS("cloud.rds")
vt_sample <- readRDS("vt_sample.rds")

# Start the VT sample one with highcharts (save the image when done)
set.seed(121)
selected_zips <- zip_code_db |> 
  filter(state == "VT") |>
  mutate(lon = lng) |>
  sample_n(70) |>
  mutate(home = if_else(zipcode == "05477", cool_winter_theme$off_white, green_state_date_theme$`Bright green`))

zip_code_base <- selected_zips |>
  mutate(z = population) |>
  select(lat,lon, zipcode, z, major_city, home) 