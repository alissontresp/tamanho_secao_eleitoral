# -------------------------------------------------------------------------
# modelo_calibracao.R
# descricao: determina o tamanho da secao eleitoral com base no tempo de
#            votacao e no tempo de atraso da secao eleitoral, usando
#            modelo de calibracao e arvore de decisao
# data da ultima modificacao: 22.09.2025
# autores: alisson e olympio
# -------------------------------------------------------------------------

# le a base de dados
secoes <- readr::read_rds(
  file = here::here("data", "secoes16092025.rds")) |>
  janitor::clean_names()

# modelo calibracao -------------------------------------------------------

# realiza estudo por local considerando 325
locais <- secoes |>
  dplyr::mutate(id = paste(nom_localidade, num_zona, num_local, sep = "_")) |>
  dplyr::group_by(num_zona, num_local, id) |>
  dplyr::summarise(
    nom_local= dplyr::first(nom_local),
    secoes = dplyr::n(),
    aptos = sum(qtd_aptos, na.rm = TRUE),
    aptos_secao = mean(qtd_aptos, na.rm = TRUE),
    qtd_secoes_abaixo_325 = sum(qtd_aptos <= 325, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    sec_necessarias = ceiling(aptos/325),
    saldo = sec_necessarias - secoes
  )

# calcula saldo necessario por ZE considerando 325
resumo_ze <- locais |>
  dplyr::group_by(num_zona) |>
  dplyr::summarise(
    locais = dplyr::n(),
    secoes = sum(secoes, na.rm = TRUE),
    aptos = sum(aptos, na.rm = TRUE),
    sec_necessarias = sum(sec_necessarias, na.rm = TRUE),
    saldo = sum(saldo, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(saldo)) |>
  dplyr::mutate(
    perc_saldo = saldo/secoes
  )

# modelo curva roc -------------------------------------------------------

# realiza estudo por local considerando 341
locais_roc <- secoes |>
  dplyr::mutate(id = paste(nom_localidade, num_zona, num_local, sep = "_")) |>
  dplyr::group_by(num_zona, num_local, id) |>
  dplyr::summarise(
    nom_local= dplyr::first(nom_local),
    secoes = dplyr::n(),
    aptos = sum(qtd_aptos, na.rm = TRUE),
    aptos_secao = mean(qtd_aptos, na.rm = TRUE),
    qtd_secoes_abaixo_325 = sum(qtd_aptos <= 325, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    sec_necessarias = ceiling(aptos/341),
    saldo = sec_necessarias - secoes
  )

# calcula saldo necessario por ZE considerando 341
resumo_ze_roc <- locais_roc |>
  dplyr::group_by(num_zona) |>
  dplyr::summarise(
    locais = dplyr::n(),
    secoes = sum(secoes, na.rm = TRUE),
    aptos = sum(aptos, na.rm = TRUE),
    sec_necessarias = sum(sec_necessarias, na.rm = TRUE),
    saldo = sum(saldo, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(saldo)) |>
  dplyr::mutate(
    perc_saldo = saldo/secoes
  )
