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
                  ifelse(tempo_atraso >= lubridate::hms("01:00:00"), 1, 0)
                )

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

# calcula o parametro de calibracao (81s)
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
boot_results <- boot::boot(data = dados, statistic = boot_func, R = 1000)

# estima os intervalos de confianca bootstrap
icbooot <- boot::boot.ci(boot_results)

# curva ROC ---------------------------------------------------------------

#criação da curva com o pacote pROC

curva_roc <- dados |>
  pROC::roc(atraso, qt_aptos)




#Ponto de corte que maximiza a sensibilidade e especificidade
ponto_corte_best <- pROC::coords(curva_roc, "best",
                                 ret = c("threshold",
                                         "specificity",
                                         "sensitivity"))


#Gráfico
pROC::ggroc(curva_roc) +
  ggplot2::labs(
    title = "Curva ROC",
    x = "Especificidade",
    y = "Sensibilidade"
  ) +
  ggplot2::geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
  ggplot2::geom_point(data = ponto_corte_best,
                      ggplot2::aes(
                        x = specificity,
                        y = sensitivity
                      ),
                      color = "red",
                      size = 3
  ) +
  ggplot2::geom_label( data = ponto_corte_best,
                      ggplot2::aes(
                        x = specificity,
                        y = sensitivity,
                        label = paste0("Corte: ", ceiling(threshold))
                      ),
                      nudge_x = 0.1,
                      nudge_y = -0.1,
                      color = "red"
  ) +
  ggplot2::theme_minimal()

#Intervalo de confiança do ponto de corte "best"
ic_ponto_corte <- pROC::ci.coords(curva_roc, "best")

# arvore de decisao -------------------------------------------------------

# estima a arvore de decisao baseado no tempo de atraso considerando
modelo_arvore <- rpart::rpart(
  tempo_atraso ~ qt_aptos,
  data = dados,
  control = rpart::rpart.control(maxdepth = 1)
)


# constroi o grafico da arvore de forma mais amigavel
rpart.plot::rpart.plot(modelo_arvore)

# apresenta a estrutura da arvore
plot(modelo_arvore)

# calcula a area abaixo da curva ROC
roc <- pROC::roc(dados$atraso, predict(modelo_arvore))

# constroi o grafico da curva ROC
plot(roc)

# calcula a area abaixo da curva ROC
auc <- pROC::auc(roc)
