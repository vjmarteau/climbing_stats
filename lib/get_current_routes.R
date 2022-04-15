# 

get_current_routes <- function(sheets_data, metadata) {
  
dat <- sheets_data |>
  mutate_at(vars(-c("date", "time_start", "time_end", "route_number", "Comments")), factor) |>
  mutate(date = dmy(date)) |>
  arrange(route_number)
  
# Join pdf data with sheets data
meta <- metadata |>
  unite(id, c("Linie", "Schwierigkeitsgrad"), remove = FALSE)

sheets <- dat |>
  unite(id, c("route_number", "grade"), remove = FALSE)

# Remove routes that are not there any more according to pdf date
latest_dat <- left_join(sheets, meta, by = "id") |>
  select(-txt) |>
  arrange(Linie) |>
  relocate(date, .after = date_route_set) |>
  mutate(status = date_route_set < date) |>
  filter(status == T) |>
  select(route_number, grade, colour, date_route_set, date, person, `feeling1-5`, topped_out, Sektor, Routensetzer) |>
  unite(id, c("route_number", "grade", "colour"), remove = FALSE)

  return(latest_dat)
}
