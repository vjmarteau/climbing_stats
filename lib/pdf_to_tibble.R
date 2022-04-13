# Takes the KI-Routendatenbank pdf from https://www.kletterzentrum-innsbruck.at/index.php?id=235&no_cache=1 and converts it to a tibble
# Requires packages "tidyverse" and "pdftools"

pdf_to_tibble <- function(path_to_file) {
  
# Read pdf lines and remove white space
metadat <- pdf_text(path_to_file) |>
  readr::read_lines() |>
  str_squish() |>
  str_subset(".+")
  
# Remove lines with less than 4 characters, extract colnames in first line
text <- metadat[str_count(metadat, " ") > 4][-1]
colnames <- metadat[1] |> stringr::str_split(" ") |> as_vector()
colnames <- c(colnames[1:2], Reduce(paste, colnames[3:4]), colnames[5:6])
  
# Check number of words
words <- text |>
  stringr::str_split(" ") |>
  map(length) |>
  unlist()
  
# Split character vector at spaces and put into columns
## Approach from https://localcoder.org/using-strsplit-and-subset-in-dplyr-and-mutate
metadat <- text |>
  tibble() |>
  filter(words > 6) |>
  mutate(tmp_chunks = stringr::str_split(text, fixed(" "),  n = 6)) |>
  mutate(!!colnames[1] := map_chr(tmp_chunks, 1),
          !!colnames[2] := map_chr(tmp_chunks, 2),
          !!colnames[3] := map_chr(tmp_chunks, 3),
          !!colnames[4] := map_chr(tmp_chunks, 4),
          !!colnames[5] := map_chr(tmp_chunks, 5),
          !!colnames[6] := map_chr(tmp_chunks, 6)) |>
  select(-tmp_chunks) |>
  unite(Sektor, c("Sektor", "Routensetzer")) |>
  rename(!!colnames[5] := "NA" ) |> 
  add_row(text |>
            tibble() |>
            filter(words < 7) |>
            mutate(tmp_chunks = stringr::str_split(text, fixed(" "),  n = 5)) |>
            mutate(!!colnames[1] := map_chr(tmp_chunks, 1),
                    !!colnames[2] := map_chr(tmp_chunks, 2),
                    !!colnames[3] := map_chr(tmp_chunks, 3),
                    !!colnames[4] := map_chr(tmp_chunks, 4),
                    !!colnames[5] := map_chr(tmp_chunks, 5)) |>
            select(-tmp_chunks))
  
return(metadat)
}