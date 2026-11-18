########### CURVA LACTAÇÃO OVELHAS SANTA INÊS #################

## Dados Silas
### Rbras 2026

### CARREGAR PACOTES
library(readxl)
library(dplyr)
library(ggplot2)

### CARREGAR A PLANILHA
setwd("~/ESTATISTICA/Dados/Dados ovelhas Silas")
dados <- read_excel("Dados_sem.xlsx")


### RENOMEAR A COLUNA DE PRODUÇÃO
dados <- dados %>%
  rename(
    Prod = `Leite_S (mg)`
  )

# Conferir
head(dados)


### CALCULAR A MÉDIA SEMANAL 
dados_media <- dados %>%
  group_by(Raca, Semana) %>%
  summarise(
    media_prod = mean(Prod, na.rm = TRUE),
    n_animais = n(),
    .groups = "drop"
  )


## Selecionando Raça 1 - Santa Inês 
dados_raca1 <- subset(dados_media, Raca == 1)

x <- c(1:17)
y <- dados_raca1$media_prod/1000 ### transformar kg


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

## funcão acf e pacf
acf(residuals(mod_wod)) # Gráfico função autocorrelação
pacf(residuals(mod_wod)) # Gráfico função autocorrelação parcial


### Qualidade do ajuste ####
library(qpcR)
Rsq(mod_wod)  #Coeficiente de determinação
AIC(mod_wod) #Critério de Informação de AKAIKE

### Intervalo de confiança ###
library(nlstools)
confint2(mod_wod)

########### Pico - persistendia - produção total
coef(mod_wod)

a_w <- 0.3537789
b_w <- 0.6892169
c_w <- 0.1751898 

## Data pico (d)
d_w <- b_w/c_w; d_w 

## Pico
p_w <- a_w*((b_w/c_w)^b_w)*exp(-b_w); p_w

# Persistência
pe_w <- -(b_w+1)*log(c_w); pe_w # ln(s)
s_w <- c_w^-(b_w+1); s_w # s


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

## funcão acf e pacf
acf(residuals(br)) # Gráfico função autocorrelação
pacf(residuals(br)) # Gráfico função autocorrelação parcial

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

# funcão acf
acf(residuals(n1)) # Gráfico correlação
pacf(residuals(n1))

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

# funcão acf
acf(residuals(cap1)) # Gráfico correlação
pacf(residuals(cap1))


### Qualidade do ajuste ####
Rsq(cap1)  #Coeficiente de determinação
AIC(cap1) #Critério de Informação de AKAIKE



################## GRAFICO WOOD e BRODY #############

# Dados observados
observados <- data.frame(Semana = x,Producao = y)

# Sequência para curvas suaves
x_pred <- seq(min(x), max(x), length.out = 300)

# Predições dos modelos Wood e Brody
curvas <- data.frame(
  Semana = rep(x_pred, 2),
  
  Producao = c(
    predict(mod_wod, newdata = data.frame(x = x_pred)),
    predict(br, newdata = data.frame(x = x_pred))
  ),
  
  Modelo = factor(
    rep(c("Wood", "Brody"), each = length(x_pred)),
    levels = c("Wood", "Brody")
  )
)

# Gráfico
grafico <- ggplot() +
  
  # Dados observados
  geom_point(
    data = observados,
    aes(x = Semana, y = Producao),
    color = "grey45",
    size = 1.2,
    alpha = 0.6
  ) +
  
  # Curvas ajustadas
  geom_line(
    data = curvas,
    aes(x = Semana, y = Producao, color = Modelo),
    linewidth = 1.1
  ) +
  
  # Cores
  scale_color_manual(
    values = c(
      "Wood" = "#D55E00",
      "Brody" = "#0072B2"
    )
  ) +
  
  # Eixos
  scale_x_continuous(
    breaks = seq(1, 17, by = 2)
  ) +
  
  labs(
    x = "Semanas em lactação",
    y = "Produção de leite (kg)",
    color = "Modelos"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

# Visualizar
grafico

ggsave(
  filename = "Wood_Brody.pdf",
  plot = grafico,
  width = 18,
  height = 12,
  units = "cm",
  device = cairo_pdf
)


#################### GRAFICO WOOD ########################

# Predições do modelo Wood
dados_pred <- data.frame(Semana = x_pred,
  Producao = predict(mod_wod,
    newdata = data.frame(x = x_pred))
)

# Gráfico
grafico_wood <- ggplot() +
  geom_point(
    data = observados,
    aes(x = Semana, y = Producao),
    color = "gray40",
    size = 1.8,
    alpha = 0.8
  ) +
  geom_line(
    data = dados_pred,
    aes(x = Semana, y = Producao),
    color = "red",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = seq(1, 17, by = 2)
  ) +
  labs(
    x = "Semanas em lactação",
    y = "Produção de leite (kg)"
  ) +
  theme_classic(base_size = 15) 
  
  

print(grafico_wood)

ggsave(
  filename = "Wood.pdf",
  plot = grafico_wood,
  width = 18,
  height = 12,
  units = "cm",
  device = cairo_pdf
)
