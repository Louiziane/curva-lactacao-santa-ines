# Modelagem da curva de lactação de ovelhas Santa Inês
# 70ª RBras (2026)
#
# Código das análises estatísticas apresentadas no pôster.
# Dados originais não disponibilizados.


### CARREGAR PACOTES
library(readxl)
library(dplyr)
library(ggplot2)

### CARREGAR A PLANILHA
dados <- read_excel("Dados_sem.xlsx")

### RENOMEAR A COLUNA DE PRODUÇÃO
# Os valores originais estão em gramas (g)
dados <- dados %>%
  rename(Prod = `Leite_S (g)`)

### CALCULAR A MÉDIA SEMANAL
dados_media <- dados %>%
  group_by(Raca, Semana) %>%
  summarise(
    media_prod = mean(Prod, na.rm = TRUE),
    n_animais = sum(!is.na(Prod)),
    .groups = "drop"
  )

### SELECIONAR A RAÇA SANTA INÊS
dados_raca1 <- dados_media %>%
  filter(Raca == 1) %>%
  arrange(Semana)

### DEFINIR AS VARIÁVEIS
# Semanas em lactação
x <- dados_raca1$Semana

# Produção média semanal convertida de g para kg
y <- dados_raca1$media_prod / 1000


###############################################################
################### AJUSTE MODELO WOOD ######################

mod_wod <- nls(y ~ a * x^b * exp(-c * x), start = list(a = 0.200, b = 0.5, c = 0.1))

summary(mod_wod)  
wd_aj <- fitted(mod_wod)
plot(x, y, ylim=c(0.1,0.5),  main= "Modelo Wood", xlab="semanas em lactação", ylab="produção de leite (kg)", las=1)
lines(x, wd_aj, col="red") # Curva Modelo


## Análise Resíduos ##
shapiro.test(residuals(mod_wod)) # normalidade

MG_wd <- attr(mod_wod$m$fitted(),"gradient")
mlg_wd <- lm(y~-1+MG_wd) # modelo auxiliar

library(lmtest)
bptest(mlg_wd) # Homocedasticidade

library(car)
dwtest(mlg_wd) # Independência

### Qualidade do ajuste ####
library(qpcR)
Rsq(mod_wod)  #Coeficiente de determinação
AIC(mod_wod) #Critério de Informação de AKAIKE

### Intervalo de confiança ###
library(nlstools)
confint2(mod_wod)


############ PICO E PERSISTÊNCIA ################
# Parâmetros estimados pelo modelo Wood
parametros <- coef(mod_wod)

a_w <- unname(parametros["a"])
b_w <- unname(parametros["b"])
c_w <- unname(parametros["c"])

# Semana do pico
semana_pico <- b_w / c_w

# Produção no pico (kg)
producao_pico <- a_w *
  semana_pico^b_w *
  exp(-c_w * semana_pico)

# Persistência da lactação
persistencia <- c_w^(-(b_w + 1))

# Resultados
cat("Semana do pico:", semana_pico, "\n")
cat("Produção no pico (kg):", producao_pico, "\n")
cat("Persistência:", persistencia, "\n")


##############################################################
########## MODELO BRODY e ANÁLISE RESÍDUOS ##########

plot(x,y, xlab = "Semanas em lactação",ylab = "Produção de leite (kg)")

br <- nls(y~a*exp(-b*x)-a*exp(-c*x),start=c(a = 0.800, b = 0.1, c = 0.5))
summary(br) 
lines(x, (fitted(br)), col= "blue") 

## Análise Resíduos ##
shapiro.test(residuals(br)) # normalidade

MG_br <- attr(br$m$fitted(),"gradient")
mlg_br <- lm(y~-1+MG_br) # modelo auxiliar

bptest(mlg_br) # Homocedasticidade

dwtest(mlg_br) # Independência

### Qualidade do ajuste ####
Rsq(br)  #Coeficiente de determinação
AIC(br) #Critério de Informação de AKAIKE

### Intervalo de confiança ###
confint2(br)

#################################################################
################ MODELO NELDER e ANÁLISE RESÍDUOS ######

plot(x,y, xlab = "Semanas em lactação",ylab = "Produção de leite (kg)")

n1 <- nls(y~x/(a+b*x+c*x^2),start=c(a = 0.3, b = 0.1, c = 0.02))
summary(n1) 
lines(x, (fitted(n1)), col= "blue") 

###### Análise Resíduos ######
shapiro.test(residuals(n1)) #normalidade

MG=attr(n1$m$fitted(),"gradient")
mlg=lm(y~-1+MG)
library(lmtest)
bptest(mlg) #Homocedasticidade

library(car)
dwtest(mlg) #Independência

### Qualidade do ajuste ####
Rsq(n1)  #Coeficiente de determinação
AIC(n1) #Critério de Informação de AKAIKE

##############################################################
######### MODELO Cappio-Borlino e ANÁLISE RESÍDUOS ######

plot(x,y, xlab = "Semanas em lactação",ylab = "Produção de leite (kg)")

cap1 <- nls(y~a*x^(b*exp(-c*x)),start=c(a=0.200,b=0.5,c=0.1))
summary(cap1) 
lines(x, (fitted(cap1)), col= "blue") 

###### Análise Resíduos ######
shapiro.test(residuals(cap1)) #normalidade

MG=attr(cap1$m$fitted(),"gradient")
mlg=lm(y~-1+MG)
bptest(mlg) #Homocedasticidade

dwtest(mlg) #Independência

### Qualidade do ajuste ####
Rsq(cap1)  #Coeficiente de determinação
AIC(cap1) #Critério de Informação de AKAIKE



################## GRAFICO WOOD 
# Dados observados
observados <- data.frame(Semana = x,Producao = y)

# Sequência para curvas suaves
x_pred <- seq(min(x), max(x), length.out = 300)

# Fonte
windowsFonts(Montserrat = windowsFont("Montserrat"))

# Predições do modelo Wood
dados_pred <- data.frame(
  Semana = x_pred,
  Producao = predict(
    mod_wod,
    newdata = data.frame(x = x_pred)
  )
)

grafico_wood <- ggplot() +
  
  # Produções médias semanais observadas
  geom_point(
    data = observados,
    aes(x = Semana, y = Producao),
    color = "gray45",
    size = 3.5,
    alpha = 0.85
  ) +
  
  # Curva ajustada pelo modelo Wood
  geom_line(
    data = dados_pred,
    aes(x = Semana, y = Producao),
    color = "#073A70",
    linewidth = 2.2
  ) +
  
  # Eixo X
  scale_x_continuous(
    breaks = seq(1, 17, by = 2),
    limits = c(1, 17),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  # Eixo Y
  scale_y_continuous(
    breaks = seq(0.1, 0.5, by = 0.1),
    labels = function(z) format(
      z, decimal.mark = ",", nsmall = 1
    ),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  
  # Identificação dos eixos
  labs(
    x = "Semanas em lactação",
    y = "Produção de leite (kg)"
  ) +
  
  # Formatação para pôster A0
  
  theme_classic(
    base_size = 18,
    base_family = "Montserrat"
  ) +
  theme(
    axis.title = element_text(
      size = 19,
      color = "black"
    ),
    axis.text = element_text(
      size = 16,
      color = "black"
    ),
    axis.line = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    plot.margin = margin(5, 5, 5, 5, unit = "pt")
  )



# Visualizar
print(grafico_wood)

# Exportar em PDF vetorial
ggsave(
  filename = "Wood.pdf",
  plot = grafico_wood,
  width = 18,
  height = 12,
  units = "cm",
  device = cairo_pdf,
  bg = "white"
)ht = 12, units = 'cm')
