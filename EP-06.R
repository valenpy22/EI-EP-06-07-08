# En este momento, los investigadores buscan determinar si existen diferencias 
# en el tiempo que tardan los usuarios en formular consultas para problemas con 
# diferente nivel de dificultad en el área de psicología.

library("tidyverse")
library("ggpubr")
library("ez")
library("nlme")
library("emmeans")
library("PASWR2")
library("dplyr")

# Se importan los datos
data <- read.csv("EP05 Datos.csv")

# Formulación de hipótesis
# Ho: No existen diferencias en el tiempo que tardan los usuarios en formular
#     consultas para problemas con diferente nivel de dificultad en
#     el área de psicología.
# Ha: Sí existen diferencias en el tiempo que tardan los usuarios en formular
#     consultas para problemas con diferente nivel de dificultad en
#     el área de psicología.

# Se filtra solo las pruebas de psicología
filtered_by_psychology <- data[data$area == "Psicología", ]

print(filtered_by_psychology)
low <- filtered_by_psychology[filtered_by_psychology$dificultad == "Baja", ]$tiempo
medium <- filtered_by_psychology[filtered_by_psychology$dificultad == "Media", ]$tiempo
high <- filtered_by_psychology[filtered_by_psychology$dificultad == "Alta", ]$tiempo

usuario <- factor(1:600)

# Se hace el dataframe donde se colocan los tiempos ya extraídos de cada dificultad
datos <- data.frame(usuario, low, medium, high)

datos <- datos %>% pivot_longer(c("low", "medium", "high"),
                                names_to = "dificultad",
                                values_to = "tiempo")

datos[["dificultad"]] <- factor(datos[["dificultad"]])

print(datos)

# Condiciones
# 1. La escala con que se mide la variable dependiente tiene las propiedades
#    de una escala de intervalos iguales.
# 2. Las mediciones son independientes al interior de cada grupo, ya que cada
#    prueba es distinta en cada nivel.
# 3. Se puede suponer que las poblaciones siguen una distribución normal.
# 4. La matriz de varianzas-covarianzas es esférica.

# Según los gráficos, se puede ver que existen pocos valores atípicos por lo que
# se elige usar un nivel de significancia del 0.05.
eda(low)
eda(medium)
eda(high)

# Prueba omnibus
# ANOVA using aov procedure.
cat("Using ANOVA with aov\n\n")
prueba <- aov(tiempo ~ dificultad + Error(usuario/dificultad), data = datos)
print(summary(prueba))

# p = 2e-16 < alfa, por lo que se rechaza la hipótesis nula en
# favor de la hipótesis alternativa. Es decir, hay al menos una
# diferencia de tiempos entre las dificultades de la misma área.

# ANOVA using ezANOVA procedure.
cat("\nUsing ANOVA with ezANOVA\n\n")
prueba2 <- ezANOVA(data = datos, dv = tiempo, within = dificultad, wid = usuario, return_aov = TRUE)
print(summary(prueba2$aov))

cat("\nFor more information:\n")
cat("Result of Mauchly's test of sphericity:\n")
print(prueba2$`Mauchly's Test for Sphericity`)

# Se tiene que p vale 0.4153, y al ser un valor alto se cumple la condición de esfericidad.

alfa <- 0.05

# Prueba post hoc
# Post-hoc análisis usando la corrección Bonferroni
bonferroni <- pairwise.t.test(datos[["tiempo"]], datos[["dificultad"]],
                              p.adj = "bonferroni", paired = TRUE)

cat("Bonferroni Correction\n")
print(bonferroni)

# Se tiene que "high vs low" es menor a 2e-16, lo que significa que
# hay una diferencia significativa entre estos dos grupos.
# Además, entre "medium" y "low" se sugiere que hay una diferencia significativa
# pero menos fuerte que la anterior.
# Finalmente, se tiene que las pruebas de dificultad alta y media tienen
# también una diferencia significativa.

# Por lo tanto, se puede concluir con un 95% de confianza (debido a que p < alfa) 
# que sí existen diferencias significativas entre las pruebas de psicología entre 
# las pruebas de dificultad baja y alta, y entre las de dificultad
# media y baja.