library(dplyr)
library(rvest)
library(stringr)

html <- read_html("https://www.bosshunting.com.au/entertainment/movies/best-movies-imdb/")

nomes <- html |> html_elements("ol.wp-block-list") |> html_elements("li") |> html_text2()

anos <- unlist(str_extract_all(string = nomes, pattern = "\\(\\d+\\)"))

anos <- unlist(str_extract_all(string = anos, pattern = "\\d+"))
anos <- as.numeric(anos)

diretor <- unlist(str_extract_all(string = nomes, pattern = "\\dir.+")

