# Function to compute to do list of current Ki Routes and write to google-sheets with respective name of climber

update_todo_routes <- function(metadata, sheets, climber, URL) {

# Exlude too easy/hard routes and drop factor levels
metadata <- metadata |>
  filter(!Schwierigkeitsgrad %in% c("3a","4a", "4b", "4c", "5a", "5a+", "5b", "5b+", "7c+", "8a", "8a+", "8b", "8b+", "8c", "8c+", "9a", "9a+")) |>
  mutate(Schwierigkeitsgrad = fct_drop(Schwierigkeitsgrad))

# Remove routes that are not there any more according to date_set (Gebaut_am)
df <- left_join(metadata, sheets, by = "id") |>
  mutate(attempted = ifelse(is.na(topped_out), "not_attempted", "attempted")) |>
  mutate(latest = Gebaut_am <= date) |>
  mutate(latest = replace_na(latest, TRUE))

person <- df |>
  filter(latest == T, person == climber | attempted == "not_attempted")

curr_red_point <- person |>
  filter(topped_out == "yes")

# Add column with date of last_attempt + number of attempts
attempted <- person |>
  filter(topped_out == "no" | attempted == "attempted") |>
  filter(!id %in% curr_red_point$id) |>
  group_by(id) |>
  mutate(attempts = n_distinct(date)) |>
  filter(date == max(date)) |>
  distinct() |>
  ungroup()

not_attempted <- person |>
  filter(attempted == "not_attempted")

tmp_meta <- metadata |>
  filter(!metadata$id %in% person$id)

to_do <- bind_rows(attempted, not_attempted, tmp_meta) |>
  distinct(id, .keep_all = TRUE) |>
  select(Linie, Schwierigkeitsgrad, Farbe, date, attempts, Sektor, Routensetzer, feeling, Comments) |>
  rename("last_attempt" = "date", "Versuche" = "attempts") |>
  arrange(Linie, Schwierigkeitsgrad)

# Write to_do routes to google sheets
sheet_write(data = to_do, ss = URL,
            sheet = paste0(climber, "_to_do"))
}