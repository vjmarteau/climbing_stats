#!/usr/bin/env Rscript
'
Usage:
  Webscrape_KI_Routendatenbank.R --resDir=<resDir> [options]

Optional arguments:
  --resDir=<resDir>             Output directory [default: ./]
' -> doc

# load required packages
library(docopt)
arguments <- docopt(doc, version = "0.1")
print(arguments)

library(conflicted)
library(tidyverse)
library(RSelenium)
library(rvest)

conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")

# Load parameters
resDir <- arguments$resDir


remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4444L,
                                 browserName = "firefox")
# Connect to the server
remDr$open()

# Navigate to url of interest
remDr$navigate("https://www.kletterzentrum-innsbruck.at/index.php?id=235&no_cache=1")

# Define worker function
scrape_table <- function(html){
  html <- read_html(html) 
  
df_table <- html |>
  html_table(fill = T) |>
  pluck(3)

df_col <- bind_cols(Ki_ID = html |> html_elements("tr") |> html_attr("id"),
                    Farbe = html |> html_elements("tr") |>
                      html_attr("class") |>
                      str_remove("[[:space:]].*") |>
                      str_remove("pCColor")) |>
  drop_na()

bind_cols(df_table, df_col)
}

# Iterate over table content, applying the function each time and clicking the "Next" button
df <- tibble()

for(i in 1:11) {
  df_sub <- scrape_table(remDr$getPageSource()[[1]])
  remDr$findElement('xpath', '//*[@id="climbingRoutes_next"]')$sendKeysToElement(list(key = "enter"))
  Sys.sleep(1)
  df <- bind_rows(df, df_sub)
}

# Tidy table
df <- df |>
  mutate(Farbe = recode(Farbe, Black = "schwarz", Blue = "blau", Brown = "braun", Green = "grün", Orange = "orange", Pink = "pink",
                        Purple = "lila", Red = "rot", White = "weiß", Yellow = "gelb",
                        .default = NA_character_)) |>
  mutate(across(where(is.character), str_replace_all, pattern = " ", replacement = "_")) |>
  mutate(Routensetzer = map_chr(Routensetzer, ~str_remove(.x, pattern = "/"))) |>
  mutate(Routensetzer = map_chr(Routensetzer, ~str_replace(.x, pattern = "__", replacement = "_"))) |>
  relocate(Ki_ID) |>
  relocate(Farbe, .after = Schwierigkeitsgrad) |>
  rename("Gebaut_am" = "Gebaut am") |>
  mutate_at(vars(Schwierigkeitsgrad), list(factor)) |>
  arrange(desc(Gebaut_am), Linie, Schwierigkeitsgrad)

write_csv(df, file = file.path(resDir, paste0(lubridate::today(), "-KI_Routendatenbank.csv") ))