# Instalamos librerías.
install.packages("foreast")
library(mFilter)
library(writexl)
library(readxl)
library(ggplot2)
library(tidyverse)
library(usethis)
library(datos)
library(dplyr)
library(tidyr)
library(xtable)
library(beepr)
library(gridExtra)
library(xts)
library(zoo)
library(tseries)
library(forecast)
options(scipen=999)
Sys.setlocale("LC_TIME", "C")

# Traemos la serie de IPC y le damos formato a la fecha.
## ACLARAR: debido a la intervención del INDEC entre feb 07 y dic 15, se utilizó el IPC de San Luis para el período feb 07 a jun 12 y IPC CABA (utilizando los ponderadores de IPC nacional -citar metodología 2016-)de jul 12 a dic 2015.

serieipc <- read.csv2("C:/Users/joaco/Downloads/ipc_04_24.csv", sep = ";", dec = ",")

serieipc$mes <- as.Date(paste0("01-", serieipc$mes), format = "%d-%b-%y")

### Gráfico Punto 1)

grafico_serie_tend <- ggplot(data = serieipc, aes(x = mes, y = var)) +
  geom_line(linewidth = 0.75) +
  theme_minimal() +
  labs(x = "", y = "Porcentaje") +
  ggtitle("Variación Mensual IPC Nacional")

### Pruebas de raíz unitaria:

# Test de Dickey-Fuller

adf.test(serieipc$var, k=0)          # Sin lags adicionales

adf.test(serieipc$var)               # Con selección automática de lags

# Test de Phillips-Perron

pp.test(serieipc$var)

# Rechazamos la hipótesis nula en ambos test, por lo que concluimos que no existe raíz unitaria 
# trabajando con la primer diferencia (en nuestro caso, tasa de variación).
# En consecuencia, trabajamos con un orden de integración 1.

### Tendencia

# Generamos una variable t para el análisis de regresión (tendencia lineal)

serieipc$t <- 1:nrow(serieipc)

# Generamos una variable t2 para el análisis de regresión (tendencia cuadrática)

serieipc <- serieipc %>% mutate(t2 = t^2)

# Generamos una variable t3 para el análisis de regresión (tendencia cúbica)

serieipc <- serieipc %>% mutate(t3 = t^3)

# Correr una regresión lineal de var en las t.

lm(var ~ t + t2 + t3, data = serieipc) %>%
  summary()

# Los coeficientes de la regresión son altamente significativos, por lo que puede que exista tendencia.

# Guardamos los valores predichos de la regresión en un nuevo dataset llamado 'tendencias'

tendencias <- serieipc %>%
  mutate(tendencia1 = predict(lm(var ~ t, data = serieipc))) %>% 
  mutate(tendencia2 = predict(lm(var ~ t2, data = serieipc))) %>% 
  mutate(tendencia3 = predict(lm(var ~ t3, data = serieipc)))

# Gráfico de tendencias polinómicas:

grafico_serie_tend <- ggplot(data = tendencias, aes(x = mes, y = var)) +
  geom_line(linewidth = 0.75) +
  theme_minimal() +
  labs(x = "", y = "Porcentaje") +
  ggtitle("Variación Mensual IPC Nacional") +
  geom_line(aes(y = tendencia1), color = "#22A884FF", linewidth = 1) +
  geom_line(aes(y = tendencia2), color = "#7AD151FF", linewidth = 1) +
  geom_line(aes(y = tendencia3), color = "#FDE725FF", linewidth = 1)

# Gráficamente, vemos también que las tendencias polinómicas predichas guardan una relación con la serie, lo que refuerza nuestra hipótesis.

# Luego, aplicamos el filtro de Hodrick-Prescott:

filtro_hp <- hpfilter(serieipc$var, freq = 14400)

grafico_serie_hp <- ggplot(data = tendencias, aes(x = mes, y = var)) +
  geom_line(color = "black", linewidth = 0.75) +  
  geom_line(aes(y = filtro_hp$trend), color = "steelblue", linewidth = 0.75) +  # Tendencia extraída del filtro HP
  geom_line(aes(y = filtro_hp$cycle), color = "darkred", linewidth = 0.75) +    # Ciclo filtro HP
  theme_minimal()

# Creamos una variable en nuestro df con la serie desestacionalizada vía filtro de HP y otra con la desestacionalización polinómica:

serieipc <- serieipc %>% mutate(serieipc_pol = residuals(lm(var ~ t + t2 + t3, data = serieipc)))

serieipc <- serieipc %>% mutate(serieipc_hp = filtro_hp$trend)

serieipc <- serieipc %>% mutate(serieipc_sin_tend = var - serieipc_hp)

# Grafico la serie original y las series sin tendencia (polinómica y HP):

grafico_serie_tendencias <- ggplot(data = serieipc, aes(x = mes, y = var)) +
  geom_line(linewidth = 0.75) +
  theme_minimal() +
  labs(x = "", y = "Porcentaje") +
  ggtitle("Variación Mensual IPC Nacional") +
  geom_line(aes(y = serieipc_sin_tend), color = "darkred", linewidth = 0.75) +  # Serie sin tendencia vía filtro HP (rojo)
  geom_line(aes(y = serieipc_pol), color = "steelblue", linewidth = 0.75)       # Serie sin tendencia polinómica (azul)

### Estacionalidad

# Con nuestra serie sin tendencia procedemos a analizar la estacionalidad. 

# Creamos una variable dummy para el mes.

serieipc <- serieipc %>%
  mutate(mes_de = month(mes)) %>% 
  mutate(mes_de = as.factor(mes_de)) 

# Primero verificamos si hay patrones estacionales a través de un boxplot:

boxplot_est <- ggplot(serieipc, aes(x = mes_de, y = serieipc_sin_tend)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Dispersión mensual IPC sin tendencia",
       x = "Mes",  
       y = "Porcentaje") + 
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(color = "gray20"),  
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
    )

# Estudiamos estacionalidad con una regresión tomando los meses como factor:

lm(serieipc_sin_tend ~ factor(mes_de), data = serieipc) %>% 
  summary()

# En el análisis se puede observar una leve estacionalidad en algunos meses por lo que nos quedamos con los residuos de la regresión, es decir,
# la parte de la serie que no está explicada por los meses.

serieipc <- serieipc %>%
  mutate(serieipc_sin_tend_est = residuals(lm(serieipc_sin_tend ~ factor(mes_de), data = serieipc)))

# Ahora graficamos la serie de IPC sin tendencia y estacionalidad, comparando con la original y la serie únicamente sin tendencia.

ggplot(serieipc, aes(x = mes)) +
  geom_line(aes(y = serieipc_sin_tend), color = "darkred", linewidth = 0.75) +          # Serie sin tend (rojo)
  geom_line(aes(y = serieipc_sin_tend_est), color = "steelblue", linewidth = 0.75) +    # Serie sin tend ni est (azul)
  labs(title = "Serie IPC sin tendencia y estacionalidad",
       x = "",
       y = "Porcentaje") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(color = "gray20"), 
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )


### Elección del modelo óptimo

# Creamos un correlograma para encontrar nuestro modelo arima óptimo:

acf(serieipc$serieipc_sin_tend_est)

pacf(serieipc$serieipc_sin_tend_est)

# Selección del mejor modelo ARMA(p,q) mediante AIC y BIC

results <- NULL

for(p in 0:3) {
  for(q in 0:3) {
    estim <- arima(serieipc$serieipc_sin_tend_est, c(p,0,q)) 
    results <- rbind(results, c(p, q, AIC(estim), BIC(estim)))
  }
}

colnames(results) <- c("p", "q", "AIC", "BIC")

# Convertimos los resultados a data frame:

results_df <- as.data.frame(results)

# Selección del mejor modelo ARIMA según AIC y BIC

mejor_AIC <- results_df[order(results_df$AIC), ][1:3, ]  # Tres mejores modelos según AIC

mejor_BIC <- results_df[order(results_df$BIC), ][1:3, ]  # Tres mejores modelos según BIC

# Análisis de los modelos seleccionados

print(mejor_AIC)

print(mejor_BIC)

# En base  a lo anterior vemos que los dos mejores modelos para explicar la inflación de acuerdo al criterio son:
# Criterio AIC: ARMA(3,2)
# Criterio BIC: ARMA (1,1)

# A continuación, estudiamos los p-valores de los coeficientes de estas dos regresiones para elegir entre ellas.

# Definimos una función para calcular p-valores de los coeficientes de un modelo ARIMA

calcular_pvalores <- function(modelo) {
  coeficientes <- modelo$coef   # Extrae los coeficientes del modelo ARIMA
  errores_estandar <- sqrt(diag(modelo$var.coef)) # Calcula el error estándar de cada coeficiente
  z_valores <- coeficientes / errores_estandar # Calcula el valor z para cada coeficiente
  p_valores <- 2 * (1 - pnorm(abs(z_valores)))  # Calcula el p-valor de dos colas para cada coeficiente
  data.frame(Coefficient = coeficientes, StdError = errores_estandar, Z = z_valores, P_Value = p_valores) # Crea un data frame con los resultados
}

# Definimos los modelos ARIMA.

modelo_arima_101 <- arima(serieipc$serieipc_sin_tend_est, order = c(1, 0, 1)) # Ajusta un modelo ARIMA(0,0,2)
modelo_arima_302 <- arima(serieipc$serieipc_sin_tend_est, order = c(3, 0, 2)) # Ajusta un modelo ARIMA(0,0,3)

# Calcula p-valores para cada modelo.

pvalores_arima_101 <- calcular_pvalores(modelo_arima_101) 
pvalores_arima_302 <- calcular_pvalores(modelo_arima_302)

# Exhibimos los resultados:

print("Modelo ARIMA(1,0,1)")
print(pvalores_arima_101)

print("Modelo ARIMA(3,0,2)")
print(pvalores_arima_302)

# Habiendo analizado los resultados, concluimos que el mejor modelo es el ARMA(1,1).
# Este modelo es el que presenta mayor eficiencia bajo el criterio BIC, que penaliza la cantidad de variables -rezagos- elegidos.
# En ese sentido, observamos que lo que se pierde en eficiencia se gana en simpleza:
# Mientras que los dos coeficientes del ARMA(1,1) son altamente significativos, el ARMA(3,2) tiene sólo 2 coeficientes significativos 
# sobre los 5 evaluados (con un nivel de confianza de 0,01).


### Predicción 2024 ene/sep (con datos hasta 2023)

serieipc23 <- serieipc %>%
  filter(mes <= as.Date("2023-12-31"))

arima(serieipc23$serieipc_sin_tend_est, order = c(1, 0, 1))

modelo_arima_101 <- arima(serieipc23$serieipc_sin_tend_est, order = c(1, 0, 1))

# Generamos un pronóstico para los siguientes 12 periodos, con un intervalo de confianza al 95%.

forecast_sin_tend_est <- forecast(modelo_arima_101, h = 9, level = 95)

predic_df <- data.frame(
  date = seq(from = 1, by = 1, length.out = 9),
  forecast_ipc_sin_tend_est = forecast_sin_tend_est$mean,  # Predicciones del modelo.
  lower = forecast_sin_tend_est$lower[, 1],       # Límite inferior del intervalo de confianza (95%).
  upper = forecast_sin_tend_est$upper[, 1]) %>%   # Límite superior del intervalo de confianza (95%).
  mutate(forecast_ipc_sin_tend_est = as.numeric(forecast_ipc_sin_tend_est)) %>%
  mutate(lower = as.numeric(lower)) %>% 
  mutate(upper = as.numeric(upper))
  

print(predic_df)

# Ahora incorporamos tendencia y estacionalidad calculadas previamente para 2024 a la predicción y a los intervalos de confianza.

# Aislamos la tendencia seleccionando sólo la tendencia de HP.

trend_24 <- serieipc %>% 
  filter(mes > as.Date("2023-12-31") & mes < as.Date("2024-09-30")) %>% 
  select(serieipc_hp) %>% 
  rename(tend = serieipc_hp)

# Aislamos el efecto estacionalidad mediante la resta entre la serie sin tend ni est y la serie sin tend (a - b)

mes <- data.frame(mes = serieipc$mes)

a <- data.frame(a = serieipc$serieipc_sin_tend_est)

b <- data.frame(b = serieipc$serieipc_sin_tend)

est_24 <- cbind(a,b,mes) %>% mutate(est = a - b) %>% filter(mes > as.Date("2023-12-31") & mes < as.Date("2024-09-30"))

# Nos quedamos sólo con las columnas estacionalidad y mes, habiendo filtrado previamente el año 2024.

est_24 <- est_24 %>% select(est, mes)

forecast <- cbind(predic_df, trend_24, est_24)

forecast_2024 <- forecast %>% mutate(var = forecast_ipc_sin_tend_est + tend + est) %>% 
                              mutate(forecast_lower = lower + tend + est) %>% 
                              mutate(forecast_upper = upper + tend + est) %>% 
                              select(mes, var, forecast_lower, forecast_upper)

### Visualización del pronóstico

# Agrego los valores de pronóstico 2024 a la serie hasta 2023.

serie_forecast_24 <- serieipc23 %>%
  bind_rows(forecast_2024) %>% 
  select(1,2,12,13)

serie_forecast_24$t <- 1:nrow(serie_forecast_24)
  

# Grafico la serie con una linea vertical punteada señalando donde empieza la predicción.

ggplot(serie_forecast_24, aes(x = mes)) +
  geom_line(aes(y = var), linewidth = 0.75) +
  geom_vline(xintercept = as.Date("2024-01-01"), linetype = "dashed", color = "darkred", linewidth = 0.75) +
  labs(title = "IPC con predicción 2024", 
       x = "", 
       y = "Porcentaje") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )

# Ahora hacemos zoom a la predicción, incorporando intervalos de confianza:

serie_forecast_24 %>%
  filter(mes >= as.Date("2023-01-01")) %>%
  ggplot(aes(x = mes)) +
  geom_line(aes(y = var), size = 0.75) +
  geom_ribbon(aes(ymin = forecast_lower, ymax = forecast_upper, color = "IC 95%"), fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = as.Date("2024-01-01"), linetype = "dashed", color = "darkred", size = 0.75) +
  labs(title = "Predicción IPC 2024", 
       x = "", 
       y = "Porcentaje") +
  scale_y_continuous(limits = c(0, 27.5), breaks = seq(0,25, by = 5)) + # Cambio límites en Y + eje de 5 en 5 e/ 0 y 25
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )




#Ejercicio 4
# Con la misma metodología utilizada anteriormente hacemos una predicción para NOV24 y DIC24

arima(serieipc$serieipc_sin_tend_est, order = c(1, 0, 1))

modelo_arima_101F <- arima(serieipc$serieipc_sin_tend_est, order = c(1, 0, 1))

# Generamos un pronóstico para la inflación sin tendencia ni estacionalidad de los 2 periodos restantes del 2024, con un intervalo de confianza al 95% .

forecast_sin_tend_estF <- forecast(modelo_arima_101F, h = 2, level = 95)

predic_dfF <- data.frame(
  date = seq(from = 1, by = 1, length.out = 2),
  forecast_ipc_sin_tend_estF = forecast_sin_tend_estF$mean,  # Predicciones del modelo.
  lower = forecast_sin_tend_estF$lower[, 1],       # Límite inferior del intervalo de confianza (95%).
  upper = forecast_sin_tend_estF$upper[, 1]) %>%   # Límite superior del intervalo de confianza (95%).
  mutate(forecast_ipc_sin_tend_estF = as.numeric(forecast_ipc_sin_tend_estF)) %>%
  mutate(lower = as.numeric(lower)) %>% 
  mutate(upper = as.numeric(upper))


print(predic_dfF)

# Ahora debemos incorporar la tendencia y la estacionalidad 

#Consideramos que la estacionalidad se mantiene igual (usamos la del nov y dic 2023 respectivamente), Sin embargo, debemos estimar la tendencia de noviembre y diciembre antes de incorporarla.

#Para esto utilizaremos el modelo de tendencias polin+omico obtenido en el primer ejercicio

pron_ten<- lm(var ~ t + t2 + t3, data = serieipc) 

summary(pron_ten)

#Creamos  un data frame con los meses 251 (noviembre 2024) y 252 (noviembre 2025). También creamos las potencias de la variable que utilizamos en la predicción.

nov_dic_24 <- data.frame(t = c(251, 252))
nov_dic_24$t2 <- nov_dic_24$t^2
nov_dic_24$t3 <- nov_dic_24$t^3

# Realizamos la predicción de la tendencia
ten_nov_dic_24 <- predict(pron_ten, newdata = nov_dic_24, interval = "confidence")

# Armamos un dataframe con la estacionalidad de noviembre y dicimebre 2023
est_novdic<- cbind(a,b,mes) %>% mutate(est = a - b) %>% filter(mes > as.Date("2023-10-31") & mes < as.Date("2023-12-31"))

#Finalmente, hacemos la predicción de la inflación para los dos períodos sumando todos los componentes.

datospron <- cbind(predic_dfF,ten_nov_dic_24 , est_novdic)
datospron <- datospron %>%
  mutate(
    var = forecast_ipc_sin_tend_estF + fit + est,  # Sumar inflación, tendencia y estacionalidad
    lower_final = lower + lwr + est,                           # Límite inferior
    upper_final = upper + upr + est                            # Límite superior
  ) %>%
  select(mes, var, lower_final, upper_final)
datospron$mes <- sub("2023", "2024", datospron$mes)
datospron$mes <- as.Date(datospron$mes)




### Visualización del pronóstico

# Agrego los valores de pronóstico 2024 a la serie hasta 2023.

serie2024 <- serieipc %>%
  bind_rows(datospron) %>% 
  select(1,2,12,13)%>% 
  filter(mes > as.Date("2023-12-31"))

serie2024$t <- 1:nrow(serie2024)


# Grafico la serie con una linea vertical punteada señalando donde empieza la predicción e incorporando intervalos de confianza.


serie2024 %>%
  ggplot(aes(x = mes)) +
  geom_line(aes(y = var), size = 0.75) +
  geom_ribbon(aes(ymin = lower_final, ymax = upper_final, color = "IC 95%"), fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = as.Date("2024-11-01"), linetype = "dashed", color = "darkred", size = 0.75) +
  labs(title = "Predicción IPC 2024", 
       x = "", 
       y = "Porcentaje") +
  scale_y_continuous(limits = c(0, 27.5), breaks = seq(0,25, by = 5)) + # Cambio límites en Y + eje de 5 en 5 e/ 0 y 25
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )


# Calculamos y graficamos la inflación anual

ipc2024 <- serieipc %>%
  bind_rows(datospron) %>% 
  select(1,2,12,13)%>% 
  filter(mes > as.Date("2013-12-31"))


# Creamos una columna con el año
ipc2024$anio <- format(ipc2024$mes, "%Y")
ipc2024$anio <- as.numeric(format(ipc2024$mes, "%Y"))

# Calculamos la inflación anual 
inflacion_anual <- ipc2024 %>%
  group_by(anio) %>%
  summarise(inflacion_anual = prod(1 + var / 100) - 1) %>% 
  mutate(inflacion_anual = inflacion_anual * 100)         
print(inflacion_anual)


# Calculamos la inflación anual 
inflacion_anual <- ipc2024 %>%
  group_by(anio) %>%
  summarise(inflacion_anual = prod(1 + var / 100) - 1) %>% 
  mutate(inflacion_anual = inflacion_anual * 100)         
print(inflacion_anual)


#Finalmente graficamos
inflacion_anual <- inflacion_anual %>%
  mutate(anio = anio ) %>%    
  filter(!is.na(inflacion_anual))  %>%
  mutate(color = ifelse(anio == 2024, "2024", "otros"))

ggplot(inflacion_anual, aes(x = anio, y = inflacion_anual, fill = color)) +
  geom_bar(stat = "identity")  +
  labs(title = "Evolución inflación anual (2024 proyectado)", 
       x = "", 
       y = "Porcentaje") +
  scale_x_continuous(breaks = seq(min(inflacion_anual$anio), max(inflacion_anual$anio), by = 1)) +
  scale_fill_manual(values = c("2024" = "darkred", "otros" = "steelblue")) + 
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


#Evidentemente, el pronóstico de la inflación no es buena y resulta incluso peor a la obtenida en el anterior ejercicio
#La causa del empeoramiento en el pronóstico del IPC está en pronóstico de la tendencia. Al pronosticarla con un modelo polinómico (de tercer grado), la tendencia no ajusta tanto cómo debería.
#Consecuentemente, esta predicción no logra captar con precisión el componente inercial de la inflación
