
current_locations <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1uBsuCs_v-VfJ9QVliHs8cV80qyFU4ZmRCuRp6hN_gYo/edit#gid=0")|>
  mutate(location_type = "current_location")


# geocode current locations addresses
register_google(key = Sys.getenv("google_key"))

current_geo <- current_locations |>
  mutate(geo = map(paste(street,city,state,zip), geocode)) |>
  unnest_wider(geo)

# arrow::write_parquet(resorts_geo, "resorts_geo.parquet")

# Format data and Calc the distances/ join w/ census
tictoc::tic()
current_zipcodes <- current_geo |>
  select(branch_name,zip, location_type, address_lon = lon, address_lat = lat) |>
  inner_join(zip_code_db |> select(zipcode), by = c("zip" = "zipcode")) |>
  mutate(
    zipper = map(zip,reverse_zipcode)
  ) %>%
  unnest(zipper)
tictoc::toc()

current_distances_cart <- current_zipcodes |>
  mutate(join = "x") |>
  inner_join(
    current_zipcodes |>
      mutate(join = "x"),
    by = "join"
  )

calc_current_distances <- current_distances_cart |>
  rowwise() |>
  mutate(distance = as.data.frame(distm(c(address_lon.x, address_lat.x), c(address_lon.y, address_lat.y), fun = distHaversine)/ 1609)[,1]) |>
  ungroup() |>
  select(zip.x, zip.y, distance)

# What is the max(min_distance) for each location relative to the others
min_distances <- calc_current_distances |>
  filter(distance != 0) |>
  group_by(zip.x) |>
  mutate(min_distance = min(distance)) |>
  ungroup() |>
  filter(distance == min_distance)


# create geom data for map ####
state_lookup <- tibble(state.abb = datasets::state.abb, state.name = str_to_lower(datasets::state.name)) |>
  inner_join(current_zipcodes, by = c("state.abb" = "state")) |>
  group_by(state.name) |>
  summarise() |>
  ungroup()


# these are dependent on state selection, not 100% sure where that should occur
county <- map_data("county") |>
  inner_join(state_lookup |> select(state.name), by = c("region" = "state.name"))

states <- map_data("state", interior = TRUE) |>
  inner_join(state_lookup |> select(state.name), by = c("region" = "state.name"))



# Create geospatial visual
plot <- ggplot() + 
  geom_polygon(data = states, aes(x=long, y=lat, group=group, text = region), fill = "white", color = "grey", size = .4) +
  geom_polygon(data = county, aes(x=long, y=lat, group=group), size = .3, fill = "white", color = "grey") +
  geom_point(data = current_zipcodes, aes(group = location_type, size = population, x = lng, y = lat, text = branch_name),shape = 1, color = "#C32A3A") +
  #geom_point(data = prospect_zipcodes, aes(group = location_type, size = 2, x = lng, y = lat),shape = 1, color = "#C32A3A") +
  scale_size(range = c(1,20), name="index") +
  coord_fixed(1.3) +
  theme_map()



plotly::ggplotly(plot, tooltip = c("city", "size","text"), hovertemplate = paste('</b><extra></extra>'))

# define eligible zip codes through cartesian product of all ma zips
ma_zips <- zip_code_db |>
  filter(state == "MA")

eligible_zips <- current_zipcodes |>
  mutate(join = "x") |>
  inner_join(
    ma_zips |>
      mutate(join = "x"),
    by = "join"
  )|>
  filter(zipcode.x != zipcode.y) |>
  rowwise() |>
  mutate(distance = as.data.frame(distm(c(address_lon, address_lat), c(lng.y, lat.y), fun = distHaversine)/ 1609)[,1]) |>
  ungroup() |>
  select(branch_name, zipcode.y, distance) |>
  group_by(zipcode.y) |>
  mutate(min_to_exisiting_branch = min(distance)) |>
  ungroup() |>
  filter(min_to_exisiting_branch <= max(min_distances$distance)+1) |>
  group_by(zipcode.y) |>
  summarise() |>
  select(zip = zipcode.y) |>
  ungroup() |>
  inner_join(zip_code_db, by = c("zip" = "zipcode")) |>
  mutate(
    location_type = "prospect",
    lat = as.numeric(lat),
    lon = as.numeric(lng))


# Create geospatial visual
current_and_prospect <- ggplot() + 
  geom_polygon(data = states, aes(x=long, y=lat, group=group, text = region), fill = "white", color = "grey", size = .4) +
  geom_polygon(data = county, aes(x=long, y=lat, group=group), size = .3, fill = "white", color = "grey") +
  #geom_point(data = current_zipcodes, aes(group = location_type, size = population, x = lng, y = lat, text = branch_name),shape = 1, color = "#C32A3A") +
  geom_point(data = eligible_zips |> filter(zip == "01821"), aes(group = location_type, size = population, x = lng, y = lat),shape = 1, color = "grey") +
  geom_point(data = current_zipcodes, aes(group = location_type, size = population, x = address_lon, y = address_lat, text = branch_name), color = "#C32A3A") +
  scale_size(range = c(1,20), name="index") +
  coord_fixed(1.3) +
  theme_map()


plotly::ggplotly(current_and_prospect)#, tooltip = c("city", "size","text","lng", "lat"), hovertemplate = paste('</b><extra></extra>'))


hcmap(
  "countries/us/us-ma-all",
  zoomBy = list(howMuch = 3)
) |>
  hc_legend (
    enabled = FALSE
  ) |>
  hc_title(text = "Current and Potential Markets") |>
  hc_add_series(
    data = eligible_zips |>
      mutate(
        z = population) |>
      select(lat,lon, zip, z),
    type = "mapbubble",
    name = "zipcode",
    minSize = "1%",
    maxSize = "8%",
    tooltip = list(pointFormat = "zip: {point.zip} <br> population: {point.z}"),
    color = "grey"
  ) |>
  hc_add_series(
    data = current_zipcodes |>
      mutate(
        lat = address_lat,
        lon = address_lon,
        name = zipcode,
        z = population) |>
      select(lat,lon, name, z),
    type = "mapbubble",
    name = "zipcode",
    minSize = "1%",
    maxSize = "9%",
    tooltip = list(pointFormat = "zip: {point.zip} <br> population: {point.z}"),
    color = "red",
  ) |>
  hc_mapNavigation(enabled = TRUE) 

