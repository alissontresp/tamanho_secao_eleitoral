
# -------------------------------------------------------------------------
# comparacao_2018_2022.R
# descricao: compara algumas medidas descritivas dos tempos de votacao
#            e eleitorado apto nos pleitos de 2018 e 2022
# autor: alisson e olympio
# data da ultima modificacao: 14.10.2025
# -------------------------------------------------------------------------

# cria vetor com os caminhos dos arquivos
files <- c("base_2018_01.csv", "base_2022_01.csv") |>
  purrr::map(
    \(x) paste0("data/", x) |>
      here::here()
  ) |>
  unlist()

# leitura dos dados para os anos de 2018 e 2022
dados <- vroom::vroom(
  file = files,
  locale = vroom::locale(encoding = "latin1", decimal_mark = ","),
  delim = ";"
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    atraso = ifelse(tempo_atraso >= lubridate::hms("01:00:00"), 1, 0)
  ) |>
  dplyr::filter(tipo == "bio", subs_urna == 0)

# le o shape de zonas eleitorias
zonas <- readr::read_rds(file = here::here("data", "shape_zona.rds")) |>
  janitor::clean_names()

# calcula medidas descritivas considerando os anos 2018 e 2022
dados |>
  dplyr::group_by(ano_eleicao, tipo) |>
  dplyr::summarise(
    media = mean(atendimento_total_tmae_seg, na.rm = TRUE),
    mediana = mean(atendimento_total_tmae_seg, na.rm = TRUE),
    mininmo = min(atendimento_total_tmae_seg, na.rm = TRUE),
    maximo = max(atendimento_total_tmae_seg, na.rm = TRUE)
  )

# calcula
dados |>
  dplyr::group_by(ano_eleicao, tipo) |>
  dplyr::summarise(
    media = mean(qt_aptos, na.rm = TRUE),
    mediana = mean(qt_aptos, na.rm = TRUE),
    mininmo = min(qt_aptos, na.rm = TRUE),
    maximo = max(qt_aptos, na.rm = TRUE)
  )

# constrio grafico de dispersao entre tempo e tamanho da secao
dados |>
  ggplot2::ggplot(mapping = ggplot2::aes(
    x = qt_aptos,
    y = atendimento_total_tmae_seg
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~ano_eleicao) +
  ggplot2::theme_classic()

# cria mapa apresentando a distribuicao do tempo de atraso de acordo com as zonas
dados_mapa <- dados |>
  dplyr::group_by(nr_zona, ano_eleicao) |>
  dplyr::summarise(
    minimo_atraso = min(tempo_atraso),
    media_atraso = mean(tempo_atraso),
    maximo_atraso = max(tempo_atraso),
    minimo_secao = min(qt_aptos),
    media_secao = mean(qt_aptos),
    maximo_secao = max(qt_aptos)
  ) |>
  dplyr::right_join(zonas, by = "nr_zona") |>
  sf::st_as_sf()

tmap::tm_shape(dados_mapa) +
  tmap::tm_polygons(
    fill = "media_atraso",
    fill.scale = tmap::tm_scale_continuous(values = "viridis"),
    fill.legend = tmap::tm_legend(title = "Tempo de atraso")
  ) +
  tmap::tm_facets(by = "ano_eleicao")

tmap::tm_shape(dados_mapa) +
  tmap::tm_polygons(
    fill = "media_secao",
    fill.scale = tmap::tm_scale_continuous(values = "viridis"),
    fill.legend = tmap::tm_legend(title = "Tamanho da seção")
  )



dados_teste <- dados |>
  dplyr::group_by(nr_zona, ano_eleicao) |>
  dplyr::summarise(
    minimo_atraso = min(tempo_atraso),
    media_atraso = mean(tempo_atraso),
    maximo_atraso = max(tempo_atraso),
    minimo_secao = min(qt_aptos),
    media_secao = mean(qt_aptos),
    maximo_secao = max(qt_aptos)
  ) |>
  tidyr::pivot_wider(names_from = ano_eleicao, values_from = dplyr::starts_with("m")
  ) |>
  dplyr::right_join(zonas, by = "nr_zona") |>
  sf::st_as_sf()


tmap::tm_shape(dados_teste) +
  tmap::tm_polygons(
    fill = c("maximo_atraso_2018", "maximo_atraso_2022"),
    fill.scale = tmap::tm_scale_continuous(values = "viridis"),
    fill.legend = tmap::tm_legend(title = "Tempo de atraso")
  )
