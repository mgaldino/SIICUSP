library(tidyverse)
library(dplyr)
library(stringr)
df_resumos <- readRDS("banco_resumos.rds")

df_resumos <- df_resumos %>%
  mutate(comeca_usp = grepl("^USP", texto))

df_resumos <- df_resumos %>%
  mutate(texto_certo = ifelse(comeca_usp, texto, paste0(dplyr::lag(texto), " ", texto)))



df_resumos <- df_resumos %>%
  mutate(
    numero_inscricao = str_extract(texto_certo, "Inscrição No\\.?:\\s*\\d+"),
    autor = str_extract(texto_certo, "(?<=Resumo do Trabalho em português:\\s)[^\\n]+"),
    titulo = str_extract(texto_certo, "(?<=Resumo do Trabalho em português:)[^\\n]+"),
    orientador = str_extract(texto_certo, "Prof\\. Dr\\..*?(?=Universidade)"),
    resumo = str_extract(texto_certo, "(?<=Objetivos\\s).*")
  )

df_resumos_final <- df_resumos %>%
  filter(!is.na(autor)) %>%
  dplyr::select(df_id, texto_certo, numero_inscricao, autor, titulo, orientador, resumo) %>%
  mutate(num_char = nchar(resumo)) %>%
  group_by(numero_inscricao) %>%
  slice_max(order_by = num_char, with_ties = FALSE) %>%
  ungroup() 
View(df_resumos_final)
