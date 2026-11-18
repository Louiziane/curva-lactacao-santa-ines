# Modelagem da curva de lactação de ovelhas Santa Inês
# 70ª RBras (2026)
# Código disponibilizado sem os dados originais (uso sujeito a autorização).

library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(qpcR)
library(nlstools)

# O arquivo Dados_sem.xlsx NÃO acompanha este repositório.
# Para executar, obtenha acesso autorizado e coloque-o em sua pasta de trabalho.
arquivo <- 'Dados_sem.xlsx'

dados <- read_excel(arquivo) %>%
  rename(Prod = `Leite_S (kg)`)

# Médias semanais por grupo genético
dados_media <- dados %>%
  group_by(Raca, Semana) %>%
  summarise(media_prod = mean(Prod, na.rm = TRUE),
            n_animais = sum(!is.na(Prod)), .groups = 'drop')

dados_raca1 <- dados_media %>%
  filter(Raca == 1) %>%
  arrange(Semana)

# O código original dividia por 1000 para obter kg.
x <- dados_raca1$Semana
y <- dados_raca1$media_prod / 1000

# Ajustes por mínimos quadrados não lineares
mod_wood <- nls(y ~ a * x^b * exp(-c * x),
                start = list(a = 0.200, b = 0.5, c = 0.1))
mod_brody <- nls(y ~ a * exp(-b * x) - a * exp(-c * x),
                 start = list(a = 0.800, b = 0.1, c = 0.5))
mod_nelder <- nls(y ~ x / (a + b * x + c * x^2),
                  start = list(a = 0.3, b = 0.1, c = 0.02))
mod_cappio <- nls(y ~ a * x^(b * exp(-c * x)),
                  start = list(a = 0.200, b = 0.5, c = 0.1))

modelos <- list(Wood = mod_wood, Brody = mod_brody,
                Nelder = mod_nelder, `Cappio-Borlino` = mod_cappio)

# Diagnósticos: reproduzem o procedimento do script de análise original.
# BP e DW são aplicados ao modelo auxiliar linear formado pelo gradiente do nls.
diagnosticos <- function(modelo) {
  res <- residuals(modelo)
  gradiente <- attr(modelo$m$fitted(), 'gradient')
  auxiliar <- lm(y ~ -1 + gradiente)
  list(
    shapiro = shapiro.test(res),
    breusch_pagan = bptest(auxiliar),
    durbin_watson = dwtest(auxiliar),
    R2 = Rsq(modelo),
    AIC = AIC(modelo)
  )
}
resultados <- lapply(modelos, diagnosticos)
print(lapply(modelos, summary))
print(resultados)

# Intervalos de confiança conforme a análise original
print(confint2(mod_wood))
print(confint2(mod_brody))

# Características da curva de Wood calculadas com parâmetros estimados
param <- coef(mod_wood)
a <- unname(param['a']); b <- unname(param['b']); c <- unname(param['c'])
if (b > 0 && c > 0) {
  semana_pico <- b / c
  producao_pico <- a * (b / c)^b * exp(-b)
  persistencia <- c^(-(b + 1))
  print(c(semana_pico = semana_pico,
          producao_pico = producao_pico,
          persistencia = persistencia))
} else {
  warning('Não há pico interno positivo de Wood com estes parâmetros.')
}

# Gráficos (observações semanais médias e curvas ajustadas)
observados <- data.frame(Semana = x, Producao = y)
x_pred <- seq(min(x), max(x), length.out = 300)
curvas <- data.frame(
  Semana = rep(x_pred, 2),
  Producao = c(predict(mod_wood, newdata = data.frame(x = x_pred)),
               predict(mod_brody, newdata = data.frame(x = x_pred))),
  Modelo = factor(rep(c('Wood', 'Brody'), each = length(x_pred)),
                  levels = c('Wood', 'Brody'))
)

grafico_comparativo <- ggplot() +
  geom_point(data = observados, aes(Semana, Producao),
             color = 'grey45', size = 1.2, alpha = 0.6) +
  geom_line(data = curvas, aes(Semana, Producao, color = Modelo), linewidth = 1.1) +
  scale_color_manual(values = c(Wood = '#D55E00', Brody = '#0072B2')) +
  scale_x_continuous(breaks = seq(1, 17, by = 2)) +
  labs(x = 'Semanas em lactação', y = 'Produção de leite (kg)', color = 'Modelos') +
  theme_classic(base_size = 14) +
  theme(legend.position = 'bottom', legend.title = element_blank())

grafico_wood <- ggplot(observados, aes(Semana, Producao)) +
  geom_point(color = 'gray40', size = 1.8, alpha = 0.8) +
  geom_line(data = subset(curvas, Modelo == 'Wood'),
            aes(Semana, Producao), inherit.aes = FALSE,
            color = 'red', linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1, 17, by = 2)) +
  labs(x = 'Semanas em lactação', y = 'Produção de leite (kg)') +
  theme_classic(base_size = 15)

print(grafico_comparativo)
print(grafico_wood)
ggsave('Wood_Brody.pdf', grafico_comparativo, width = 18, height = 12, units = 'cm')
ggsave('Wood.pdf', grafico_wood, width = 18, height = 12, units = 'cm')
