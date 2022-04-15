# Takes the KI-Routendatenbank pdf from https://www.kletterzentrum-innsbruck.at/index.php?id=235&no_cache=1 and converts it to a tibble
# Requires packages "tidyverse" and "pdftools"

pdf_to_tibble <- function(path_to_pdfs) {
  
  # Read pdf lines and remove lines with less than 4 characters
  filenames <- list.files(path_to_pdfs, recursive = TRUE, full.names = TRUE, pattern = '.pdf$')
  
  metadat <- map_df(filenames, ~ tibble(txt = pdf_text(.x) |>
                                          readr::read_lines() |>
                                          str_squish() |>
                                          str_subset(".+")))
  
  # Extract colnames in first line and all th rest
  colnames <- metadat |>
    slice_head() |> stringr::str_split(" ") |> unlist()
  colnames <- c(colnames[1:2], Reduce(paste, colnames[3:4]), colnames[5:6])
  
  metadat <- metadat |>
    filter(str_count(metadat$txt, " ") > 4) |>
    distinct() |>
    slice(-1)
  
  # Check number of words
  words <- metadat |>
    pull(txt) |>
    stringr::str_split(" ") |>
    map(length) |>
    unlist()
  
  # Split character vector at spaces and put into columns
  ## Approach from https://localcoder.org/using-strsplit-and-subset-in-dplyr-and-mutate
  
  textA <- metadat |> filter(words > 6) |> pull(txt)
  textB <- metadat |> filter(words < 7) |> pull(txt)
  
  meta <- metadat |>
    filter(words > 6) |>
    mutate(tmp_chunks = stringr::str_split(textA, fixed(" "),  n = 6)) |>
    mutate(!!colnames[1] := map_chr(tmp_chunks, 1),
           !!colnames[2] := map_chr(tmp_chunks, 2),
           !!colnames[3] := map_chr(tmp_chunks, 3),
           !!colnames[4] := map_chr(tmp_chunks, 4),
           !!colnames[5] := map_chr(tmp_chunks, 5),
           !!colnames[6] := map_chr(tmp_chunks, 6)) |>
    select(-tmp_chunks) |>
    unite(Sektor, c("Sektor", "Routensetzer")) |>
    rename(!!colnames[5] := "NA" ) |>
    add_row(metadat |>
              tibble() |>
              filter(words < 7) |>
              mutate(tmp_chunks = stringr::str_split(textB, fixed(" "),  n = 5)) |>
              mutate(!!colnames[1] := map_chr(tmp_chunks, 1),
                     !!colnames[2] := map_chr(tmp_chunks, 2),
                     !!colnames[3] := map_chr(tmp_chunks, 3),
                     !!colnames[4] := map_chr(tmp_chunks, 4),
                     !!colnames[5] := map_chr(tmp_chunks, 5)) |>
              select(-tmp_chunks))
  
  meta <- meta |>
    mutate_at(4, dmy) |>
    rename(date_route_set = `Gebaut am`) |>
    mutate(Linie = as.integer(Linie)) |>
    arrange(Linie) |>
    relocate(1, .after = last_col())
  
  return(meta)
}
