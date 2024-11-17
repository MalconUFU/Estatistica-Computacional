library(rvest) #raspagem
library(dplyr) #manipulação dos dados
library(tidytext) #quebrar um texto em palavras (manipulação de texto)
library(tidyr)#organizar um conjunto bagunçado
library(stopwords)
library(ggplot2)

url <- "https://www.bbc.com/portuguese/articles/c3dv8yy3d8jo"

html <- read_html(url)

#titulo <- html |> html_elements("h1") |> html_text2()
#noticia <- html |> html_elements("p.bbc-hhl7in") |> html_text2()
#noticia <- paste(noticia, collapse = " ")
#artigos <- data.frame(titulo, noticia)

texto <- html |> html_elements("p.bbc-hhl7in") |> html_text2()

paste(texto, collapse = " ")

conjunto <- data.frame(texto)

?unnest_tokens

conjunto |> unnest_tokens(output = word, input = texto) |> count(word, sort = TRUE) |> top_n(10)

stopwords_br <- data.frame(word = stopwords("pt"))

conjunto |> unnest_tokens(output = word, input = texto) |> anti_join(stopwords_br) |> count(word, sort = TRUE) |> top_n(10) |> mutate(word = reorder(word, n)) |> ggplot(aes(y = word, x = n)) + geom_col(fill = "orange") + theme_classic()

library(janeaustenr)

livro <- prideprejudice

livro <- data.frame(texto = livro)
stopwords_en <- data.frame(word = stopwords("en"))

livro |> unnest_tokens(word, texto) |> anti_join(stopwords_en) |> count(word, sort = TRUE) |> top_n(10) |> mutate(word = reorder(word, n)) |> ggplot(aes(y = word, x = n)) + geom_col(fill = "orange") + theme_classic()

sentimentos <- get_sentiments("bing")

library(stringr)

capitulos <- str_detect(livro$texto, "^Chapter \\d+")

capitulos <- cumsum(capitulos)

library(tidyr)

livro |> mutate(capitulo = capitulos) |> unnest_tokens(word, texto) |> inner_join(sentimentos) |> count(capitulo, sentiment) |> spread(sentiment, n, fill = 0) |> mutate(total = positive - negative) |> ggplot(aes(x = capitulo, y = total)) + geom_col(fill = "orange") + theme_classic()

#livro$texto[1:10]
