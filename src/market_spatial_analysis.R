# Market analysis map 

# Start the VT sample one with highcharts (save the image when done)
set.seed(121)
selected_zips <- zip_code_db |> 
  filter(state == "VT") |>
  mutate(lon = lng) |>
  sample_n(70) |>
  mutate(home = if_else(zipcode == "05477", cool_winter_theme$off_white, green_state_date_theme$`Bright green`))
  

vt_map <-hcmap(
  "countries/us/us-vt-all",
  nullColor = "#656565") |>
  hc_legend (enabled = FALSE)|>
  #hc_title(text = "Current and Potential Markets") |>
  hc_add_series(
    data = selected_zips |>
      mutate(z = population) |>
      select(lat,lon, zipcode, z, major_city, home),
    hcaes(color = home),
    type = "mapbubble",
    name = "zipcode",
    minSize = "1%",
    maxSize = "8%",
    tooltip = list(pointFormat = "{point.major_city} <br> zip: {point.zipcode} <br> population: {point.z}")
  ) |>
  hc_colors(c(green_state_date_theme$`Bright green`, green_state_date_theme$dark_grey)) |>
  hc_chart(borderColor = "#656565", borderWidth = 15) |>
  hc_plotOptions(credits = list(enabled = FALSE)) |>
  hc_add_theme(hc_theme(chart = list(plotBackgroundColor = '#656565')))# |>
  #hc_mapNavigation(enabled = TRUE)
vt_map
saveRDS(vt_map, "vt_map.rds")
