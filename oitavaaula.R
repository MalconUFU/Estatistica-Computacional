library(rvest) #RASPAGEM DE PÁGINAS ESTÁTICA
library(dplyr) #MANIPULAÇÃO DE DADOS
library(ggplot2)
library(stringr) #MANIPULAÇÃO DE STRING
library(geobr)

#Lista de unidades federativas do Brasil por alfabetização

url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetiza%C3%A7%C3%A3o"

html <- read_html(url)
html

#html_elements(html, "h1")
#html_text2(html_elements(html, "h1"))

#reescrevendo

html |>
  html_elements("h1") |>
  html_text2()

tabelas <- html |>
             html_elements("table") |>
             html_table()

alfabetizacao <- tabelas[[3]]

alfabetizacao <- alfabetizacao[,c(2,3)]
names(alfabetizacao) <- c("estado", "taxa")
names(alfabetizacao)

#"\\d" -> dígito qualquer
#str_replace_all(string = "Pedro145", pattern = "\\d", replacement = "")

parte1 <- str_replace_all(alfabetizacao$taxa, pattern = ",", replacement = ".")

parte2 <- str_replace_all(string = parte1, pattern = "%", replacement = "")

parte_final <- as.numeric(parte2)

parte_final <- parte_final/100

alfabetizacao$taxa <- parte_final

minas <- read_state(code_state = "MG")

ggplot(data = minas) + geom_sf(fill = "darkorange") + theme_void()

municipiomg <- read_municipality(code_muni = "MG")

ggplot(data = municipiomg) + geom_sf(fill = "white") + theme_void()

estados <- read_state()
estados$name_state
order(estados$name_state)

estados <- estados[order(estados$name_state),]

estados

alfabetizacao <- alfabetizacao[order(alfabetizacao$estado),]

alfabetizacao

estados$taxa <- alfabetizacao$taxa

ggplot(data = estados, aes(fill = taxa)) + geom_sf() + scale_fill_gradient(high = "#e6550d",
                                                                           low = "#fee6ce",)

