library(rvest) #RASPAGEM DE PÁGINAS ESTÁTICA
library(dplyr)
library(ggplot2)

url2 <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_taxa_de_homic%C3%ADdios"

html2 <- read_html(url)

tabelas2 <- html2 |> 
             html_elements("table") |>
              html_table()

homicidios <- tabelas2[[1]]
