# Modelagem da curva de lactação de ovelhas Santa Inês

**70ª Reunião Anual da Região Brasileira da Sociedade Internacional de Biometria (RBras) — 2026**

Este repositório disponibiliza o código R utilizado nas análises estatísticas apresentadas no pôster *Modelagem da curva de lactação de ovelhas Santa Inês*.

## Dados e metodologia

Foram utilizados dados de produção de leite de 17 ovelhas Santa Inês, provenientes do estudo de Greca (2013). As análises consideraram 17 médias semanais de produção de leite, expressas em quilogramas (kg).

Foram ajustados quatro modelos de regressão não linear pelo método dos mínimos quadrados, utilizando a função `nls()` do software R:

- Brody (1924);
- Nelder (1966);
- Wood (1967);
- Cappio-Borlino (1995).

A qualidade dos ajustes foi avaliada pelo coeficiente de determinação (R²) e pelo Critério de Informação de Akaike (AIC).

Foram realizados diagnósticos de resíduos utilizando os testes de Shapiro-Wilk (SW), Breusch-Pagan (BP) e Durbin-Watson (DW). Para BP e DW, foram utilizados modelos lineares auxiliares baseados nos gradientes dos ajustes não lineares; a adequação desse procedimento como diagnóstico dos modelos originais requer avaliação específica.

## Resultados dos ajustes

| Modelo | R² | AIC | Erro-padrão residual |
|---|---:|---:|---:|
| Wood | 0,9478 | -68,68 | 0,02796 |
| Brody | 0,9375 | -63,93 | 0,03215 |
| Nelder | 0,8264 | -51,52 | 0,04631 |
| Cappio-Borlino | 0,7597 | -41,86 | 0,06152 |

O modelo de Wood apresentou o maior R² e o menor AIC entre os modelos avaliados.

## Diagnósticos dos resíduos

Valores-p obtidos com os procedimentos implementados no script:

| Modelo | Shapiro-Wilk | Breusch-Pagan | Durbin-Watson |
|---|---:|---:|---:|
| Wood | 0,6089 | 0,6207 | 0,1543 |
| Brody | 0,6818 | 0,1651 | 0,0712 |
| Nelder | 0,7461 | 0,3652 | 0,0028 |
| Cappio-Borlino | 0,1888 | 0,0609 | 0,0002 |

## Estimativas do modelo de Wood

| Parâmetro | Estimativa | Erro-padrão | IC 95% |
|---|---:|---:|---|
| a | 0,35378 | 0,02310 | [0,30423; 0,40332] |
| b | 0,68922 | 0,08554 | [0,50575; 0,87268] |
| c | 0,17519 | 0,01538 | [0,14221; 0,20817] |

O modelo de Wood é representado por:

**y(t) = a · t^b · exp(-c · t) + ε**

em que *t* representa o tempo em semanas de lactação.

### Características da curva estimada

| Característica | Estimativa |
|---|---:|
| Semana do pico | 3,9341 |
| Produção no pico | 0,4564 kg |
| Índice de persistência (s) | 18,9617 |
| Logaritmo do índice de persistência (ln s) | 2,9424 |

A produção máxima estimada ocorreu aproximadamente na quarta semana de lactação.

## Disponibilidade dos dados

Os dados originais não são disponibilizados neste repositório por dependerem de autorização dos responsáveis.

O script R é fornecido para documentar os procedimentos estatísticos realizados. Sua execução requer acesso autorizado à planilha original.

## Referência

BRODY, S.; TURNER, C. W.; RAGSDALE, A. C. The relation between the initial rise and the subsequent decline of milk secretion following parturition. The Journal of General Physiology, v. 6, n. 5, p. 541–545, 1924. 

CAPPIO-BORLINO, A.; PULINA, G.; ROSSI, G. A non-linear modification of Wood's equation fitted to lactation curves of Sardinian dairy ewes. Small Ruminant Research, v. 18, n. 1, p. 75–79, 1995. 

GRECA, S. P. *Produção e composição do leite ovino de diferentes grupos genéticos*. 2013. 53 f. Dissertação (Mestrado) – Universidade Federal de Lavras, Lavras, 2013.

NELDER, J. A. Inverse polynomials, a useful group of multi-factor response functions. Biometrics, v. 22, n. 1, p. 128–141, 1966. 

WOOD, P. D. P. Algebraic model of the lactation curve in cattle. Nature, v. 216, p. 164–165, 1967.

WOOD, P. D. P. Factors affecting persistency of lactation in cattle. Nature, v. 218, p. 894, 1968. 


