# Function to compute to do list of current Ki Routes and write to googlesheets with respective name of person

update_todo_routes <- function(sheets, metadata, person, URL) {
  
# Remove routes that are not there any more according to "date_set"
latest <- left_join(sheets, metadata, by = "id") |>
  mutate(status = date_route_set < date) |>
  filter(status == T) |>
  select(route_number, grade, colour, date_route_set, date, person, feeling, topped_out, Sektor, Routensetzer) |>
  unite(id, c("route_number", "grade", "colour"), remove = FALSE)
  
red_point <- latest |>
  filter(person == person) |>
  filter(topped_out == "yes") |>
  unite(id, c("route_number", "grade"), remove = FALSE)
  
attempted <- latest |>
  filter(person == person) |>
  filter(topped_out == "no") |>
  unite(id, c("route_number", "grade"), remove = FALSE) |>
  select(id, date) |>
  group_by(id) |>
  filter(date == max(date))
  
# Exlude too easy/hard routes
missing <- meta |>
  filter(!meta$id %in% red_point$id) |>
  filter(!Schwierigkeitsgrad %in% c("3a","4a", "4b", "4c", "5a", "5a+", "5b", "5b+", "7c+", "8a", "8a+", "8b", "8b+", "8c", "8c+", "9a", "9a+")) |>
  select(id, Linie, Schwierigkeitsgrad, Sektor, Routensetzer) |>
  arrange(Schwierigkeitsgrad, Linie)
  
# Add column with date of last_attempt
to_do <- left_join(missing, attempted, by = "id") |>
  select(Linie, Schwierigkeitsgrad, date, Sektor, Routensetzer) |>
  rename("last_attempt" = "date")
  
# Write to_do routes to google sheets 
sheet_write(data = to_do, ss = URL,
            sheet = paste0(person, "_to_do"))
}