gsd_cloud <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1JCJOc8ThPhf4au3PAPPVcI5F4ITRj5EnRkSNTz9GElQ/edit#gid=0")

easeOutBounce  <- JS("function (pos) {
    if ((pos) < (1 / 2.75)) {
      return (7.5625 * pos * pos);
    }
    if (pos < (2 / 2.75)) {
      return (7.5625 * (pos -= (1.5 / 2.75)) * pos + 0.75);
    }
    if (pos < (2.5 / 2.75)) {
      return (7.5625 * (pos -= (2.25 / 2.75)) * pos + 0.9375);
    }
    return (7.5625 * (pos -= (2.625 / 2.75)) * pos + 0.984375);
    }")

cloud <-gsd_cloud |>
  hchart(
    "wordcloud", 
    hcaes(name = word, weight = log(weight)),
    tooltip = list(pointFormat = "{point.}"),
    style = list(fontFamily = "Quicksand"),
    animation = list(
      duration = 4000#,
      #easing = easeOutBounce
    )
  ) |>
  hc_colors(c(green_state_date_theme$`Bright green`, green_state_date_theme$dark_grey, green_state_date_theme$medium_grey)) |>
  hc_add_theme(hc_theme(chart = list(backgroundColor = '#656565')))

saveRDS(cloud, "cloud.RDS")
# Add bounce effect?
