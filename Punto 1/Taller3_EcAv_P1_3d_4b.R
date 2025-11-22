# ARCHIVO: Taller3_EcAv_3d_4b
# AUTORES: Marlon Angulo Ramos // Carlos Agamez
# FECHA: 21/11/2025
# DESCRIPCIÓN: Primer ejercicio, TWFE DNP

# 0.1 Configuración inicial=================================

# Instalar y cargar paquetes
install.packages("pacman")
library(pacman)
p_load(bacondecomp, haven, ggplot2, dplyr, fixest)

# Cargar datos

url <- "https://raw.githubusercontent.com/9marlon9/Taller-3-EcAv/master/Punto%201/Empresas_Sim.dta"
download.file(url, "Empresas_Sim.dta", mode = "wb")
# Abrir la base
datos <- read_dta("Empresas_Sim.dta")

# 3.d Descomposici´on Goodman–Bacon=================================

# Realizar descomposición de Bacon 
bacon_results <- bacon(y_2 ~ treat,
                       data = data,
                       id_var = "id", 
                       time_var = "year",
                       quietly = FALSE)

# Ver resultados detallados
print(bacon_results)

# 4.b Leads and lags =================================

# Crear variable de tiempo relativo
data$time_rel <- data$year - data$treat_date

# Crear dummies para cada k
data$D_k_m8 <- as.numeric(data$time_rel == -8)
data$D_k_m7 <- as.numeric(data$time_rel == -7)
data$D_k_m6 <- as.numeric(data$time_rel == -6)
data$D_k_m5 <- as.numeric(data$time_rel == -5)
data$D_k_m4 <- as.numeric(data$time_rel == -4)
data$D_k_m3 <- as.numeric(data$time_rel == -3)
data$D_k_m2 <- as.numeric(data$time_rel == -2)
# Omitimos k = -1 (referencia)
data$D_k_0 <- as.numeric(data$time_rel == 0)
data$D_k_1 <- as.numeric(data$time_rel == 1)
data$D_k_2 <- as.numeric(data$time_rel == 2)
data$D_k_3 <- as.numeric(data$time_rel == 3)
data$D_k_4 <- as.numeric(data$time_rel == 4)
data$D_k_5 <- as.numeric(data$time_rel == 5)

model <- feols(y_2 ~ D_k_m8 + D_k_m7 + D_k_m6 + D_k_m5 + D_k_m4 + 
                 D_k_m3 + D_k_m2 + D_k_0 + D_k_1 + D_k_2 + D_k_3 + 
                 D_k_4 + D_k_5 | id + year, 
               data = data, 
               cluster = ~id)

# Ver resultados
summary(model)

# Extraer coeficientes manualmente
results <- data.frame(
  k = c(-8, -7, -6, -5, -4, -3, -2, 0, 1, 2, 3, 4, 5),
  delta_hat = c(coef(model)["D_k_m8"], coef(model)["D_k_m7"], 
                coef(model)["D_k_m6"], coef(model)["D_k_m5"],
                coef(model)["D_k_m4"], coef(model)["D_k_m3"],
                coef(model)["D_k_m2"], coef(model)["D_k_0"],
                coef(model)["D_k_1"], coef(model)["D_k_2"],
                coef(model)["D_k_3"], coef(model)["D_k_4"],
                coef(model)["D_k_5"]),
  se = c(se(model)["D_k_m8"], se(model)["D_k_m7"],
         se(model)["D_k_m6"], se(model)["D_k_m5"],
         se(model)["D_k_m4"], se(model)["D_k_m3"],
         se(model)["D_k_m2"], se(model)["D_k_0"],
         se(model)["D_k_1"], se(model)["D_k_2"],
         se(model)["D_k_3"], se(model)["D_k_4"],
         se(model)["D_k_5"])
)

# Calcular intervalos de confianza
results$ci_lower <- results$delta_hat - 1.96 * results$se
results$ci_upper <- results$delta_hat + 1.96 * results$se

# Calcular gamma_k(w) teórico
C <- 0.1504
results$gamma_k <- ifelse(results$k >= 0, (results$k + 1) * C, 0)

# Gráfico
ggplot(results, aes(x = k)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.2, fill = "blue") +
  geom_point(aes(y = delta_hat), color = "red", size = 2) +
  geom_line(aes(y = gamma_k), color = "darkgreen", linetype = "dashed", size = 1) +
  labs(x = "Periodos relativos al tratamiento (k)",
       y = "Coeficiente",
       title = "Event Study: Leads and Lags",
       subtitle = "Modelo 2 - Efectos dinámicos") +
  scale_x_continuous(breaks = -8:5) +
  theme_minimal() +
  annotate("text", x = -4, y = max(results$ci_upper), 
           label = "Referencia omitida: k = -1", size = 3)


# Extraer resultados detallados
results <- data.frame(
  k = c(-8, -7, -6, -5, -4, -3, -2, 0, 1, 2, 3, 4, 5),
  delta_hat = coef(model),
  se = se(model),
  p_value = summary(model)$coeftable[,4]
)

# Calcular significancia estadística
results$significant <- ifelse(results$p_value < 0.05, "Sí", "No")
results$ci_lower <- results$delta_hat - 1.96 * results$se
results$ci_upper <- results$delta_hat + 1.96 * results$se

# Calcular gamma_k(w) teórico
C <- 0.1504
results$gamma_k <- ifelse(results$k >= 0, (results$k + 1) * C, 0)

# Mostrar tabla completa
print(results)

# Análisis de períodos previos
pre_treatment <- results[results$k < 0, ]
print(pre_treatment)

# Análisis de períodos posteriores  
post_treatment <- results[results$k >= 0, ]
print(post_treatment)

# Verificar si cero está en los intervalos de confianza para k < 0
for(i in 1:nrow(pre_treatment)) {
  row <- pre_treatment[i,]
  zero_in_ci <- row$ci_lower <= 0 & 0 <= row$ci_upper
  cat(paste0("k = ", row$k, ": Cero en IC 95% = ", zero_in_ci, 
             " (IC: [", round(row$ci_lower, 4), ", ", round(row$ci_upper, 4), "])\n"))
}


