# -------------------------------------------------------------------------
# modelo_calibracao.R
# descricao: determina o tamanho da secao eleitoral com base no tempo de
#            votacao e no tempo de atraso da secao eleitoral, usando
#            modelo de calibracao e arvore de decisao
# data da ultima modificacao: 22.09.2025
# autores: alisson e olympio
# -------------------------------------------------------------------------

# leitura dos dados  ------------------------------------------------------

# leitura da base de dados
dados <- readr::read_csv2(
  file = here::here("data", "base_2018_01.csv"),
  locale = readr::locale(encoding = "latin1")
) |>
  janitor::clean_names() |>
  dplyr::filter(tipo == "bio", subs_urna == 0) |>
  dplyr::mutate(atraso =
                  ifelse(tempo_atraso >= lubridate::hms("01:00:00"), 1, 0),
                tempo_total = tempo_atraso + 9)

# estatistica descritiva --------------------------------------------------

# grafico de dispersao por grupo
grafico_dispersao <- dados |>
  dplyr::group_by(tipo) |>
  ggplot2::ggplot(ggplot2::aes(x = qt_aptos,
                               y = atendimento_total_tmae_seg,
                               colour = tipo)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = TRUE) +
  ggplot2::theme_classic()

# grafico interativo
plotly::ggplotly(grafico_dispersao)

# modelo de calibracao  ---------------------------------------------------

# ajuste o modelo de regressao linear
modelo_lm <- lm(atendimento_total_tmae_seg ~ qt_aptos, data = dados)

# estimativas dos parametros do modelo
resultados <- summary(modelo_lm)

# qualidade do ajuste do modelo
plot(modelo_lm)

# histograma dos residuos
ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x = residuals(modelo_lm)))

# boxplot dos residuos
ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(y = residuals(modelo_lm)))

# extrai o valor do alpha
alpha <- coef(modelo_lm)[[1]]

# extrai o valor do beta
beta <- coef(modelo_lm)[[2]]

# calcula o parametro de calibracao
x0 <- (81 - alpha) / beta

# calcula a variancia estimada do parametro de calibracao
xbar <- mean(dados$qt_aptos)
n <- nrow(dados)
nsxx <- (n - 1) * var(dados$qt_aptos)
sigma2 <- resultados$sigma ^ 2
varx0 <- (sigma2 / beta ^ 2) * (1 + 1 / n + ((xbar - x0) ^ 2) / nsxx)

# cria funcao para estimar o intervalo de confianca via bootstrap
boot_func <- function(data, indices) {
  modelo_lm <- lm(atendimento_total_tmae_seg ~ qt_aptos, data = data[indices, ])
  x0 <- (81 - coef(modelo_lm)[[1]]) / coef(modelo_lm)[[2]]
  return(x0)
}

# cria amostras bootstrap
boot_results <- boot::boot(data = dados, statistic = boot_func, R = 200)

# estima os intervalos de confianca bootstrap
boot::boot.ci(boot_results)

# curva ROC ---------------------------------------------------------------

# ajusta o modelo de regressao logistica
modelo_logistico <- glm(atraso ~ QT_APTOS, data = dados, family = "binomial")

# inclui termos uteis no banco de dados
broom::augment(modelo_logistico, dados) |>
  View()

# constroi a curva roc para avaliar o modelo
ggplot2::ggplot(dados_teste, ggplot2::aes(d = atraso, m = .hat)) +
  plotROC::geom_roc(labelround = 4)

# arvore de decisao -------------------------------------------------------

# estima a arvore de decisao
modelo_arvore <- rpart::rpart(
  tempo_atraso ~ qt_aptos,
  data = dados,
  control = rpart::rpart.control(maxdepth = 1)
)

# apresenta a estrutura da arvore
plot(modelo_arvore)

# calcula a area abaixo da curva ROC
roc <- pROC::roc(dados$atraso, predict(modelo_arvore))

# constroi o grafico da curva ROC
plot(roc)

# constroi o grafico da arvore de forma mais amigavel
rpart.plot::rpart.plot(modelo_arvore)

