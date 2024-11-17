library(httr2)
library(httr)
library(dplyr)
library(jsonlite)

url_base <- "https://blaze1.space/api/roulette_games/history?startDate=2024-08-14T16:43:15.806Z&endDate=2024-09-13T16:43:15.807Z&page=1"

resultados <- c()

for(j in 1:50){
  
  url <- paste0(url_base, j)
  dados <- content(GET(url_base), "text")
  dados <- fromJSON(dados)
  resultados <- c(resultados, dados$records$color)
  
}

resultados
prop.table(table(resultados))

GET(url)
content(GET(url))

dados <- content(GET(url), "text")

dados <- fromJSON(dados)

table(dados$records$color)

library(rvest)

url <- "https://www.letras.mus.br/belchior/medo-de-aviao-ii/"

html <- read_html(url)

html |> html_elements("h1") |> html_text2()

letra <- html |> html_elements("div.lyric") |> html_elements("p") |> html_text2() |> paste(collapse = " ")


library(tidytext)
library(ggplot2)

letra <- data.frame(letra)

letra |> unnest_tokens(output = word, input = letra) |> count(word, sort = TRUE) |> head(n = 10) |> ggplot(aes(y = word, x = n)) + geom_col()
