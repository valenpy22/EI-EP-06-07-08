# ¿Existe una diferencia significativa en la frecuencia 
# de enfermedades crónicas entre los individuos que 
# pertenecen a un pueblo originario y aquellos que no, 
# en la población de Chile?

library(ggpubr)
library(boot) # Para bootstrapping
library(dplyr) # Para manipulación de datos
library(multcomp) # Para comparaciones post-hoc

dir <- getwd()

setwd(dir)

# Se leen los datos
datos <- read.csv2("Desktop/EP-Grupo-8/EP08 Datos CASEN 2017.csv", fileEncoding = "Latin1")

head(datos)
# Se hace la selección de una muestra aleatoria
set.seed(123)
n <- sample(101:149, 1)
muestra <- datos[sample(nrow(datos), n), ]

# Se define el número de simulaciones
num_simulaciones <- 99
diferencias <- numeric(num_simulaciones)

for(i in 1:num_simulaciones){
  muestra_simulada <- muestra[sample(nrow(muestra), n, replace = TRUE), ]
  frecuencia_originarios <- mean((muestra_simulada$r3 == "No pertenece a ningún pueblo indígena" | muestra_simulada$r3 == "No sabe/no responde") & ((muestra_simulada$s28 != "No ha estado en tratamiento por ninguna condicion de salud a") | (muestra_simulada$s28 == "No sabe/No recuerda")), na.rm = TRUE)
  frecuencia_no_originarios <- mean((muestra_simulada$r3 != "No pertenece a ningún pueblo indígena" | muestra_simulada$r3 == "No sabe/no responde") & ((muestra_simulada$s28 != "No ha estado en tratamiento por ninguna condicion de salud a") | (muestra_simulada$s28 == "No sabe/No recuerda")), na.rm = TRUE)
  diferencias[i] <- frecuencia_originarios - frecuencia_no_originarios
}

# Análisis de los resultados
media_diferencias <- mean(diferencias)
ic_inf <- quantile(diferencias, 0.025, na.rm = TRUE)
ic_sup <- quantile(diferencias, 0.975, na.rm = TRUE)

cat("La diferencia media estimada es:", media_diferencias, "\n")
cat("El intervalo de confianza al 95% para la diferencia es: [", ic_inf, ",", ic_sup, "]\n")

# Determinar si la diferencia es estadísticamente significativa
if(ic_inf > 0 | ic_sup < 0) {
  cat("La diferencia es estadísticamente significativa.\n")
} else {
  cat("La diferencia no es estadísticamente significativa.\n")
}


# ¿Difieren significativamente los promedios de de horas de trabajo entre las 
# personas según su nivel de educación (Sin educación, M. Hum. completa y 
# profesional completo)?

# Establecer la semilla para la reproducibilidad
set.seed(100) # Reemplaza una_nueva_semilla con un número entero de tu elección

# Leer los datos desde el CSV
datos2 <- read.csv2("Desktop/EP-Grupo-8/EP08 Datos CASEN 2017.csv", fileEncoding = "Latin1")

# Asegurarse de que 'educ' y 'o10' están en el formato correcto
datos2$educ <- as.factor(datos2$educ)
datos2$o10 <- as.numeric(as.character(datos2$o10))

# Seleccionar una muestra aleatoria de hogares
n2 <- sample(201:299, 1) # Un número aleatorio entre 200 y 300
muestra2 <- sample_n(datos2, n2)

# Realizar bootstrapping
# Definir la estadística de interés
media_trabajo <- function(data, indices) {
  d <- data[indices, ] # Permite el muestreo con reemplazo
  medias <- tapply(d$o10, d$educ, mean, na.rm = TRUE)
  return(medias)
}

# Aplicar el método de bootstrapping
resultados_bootstrap <- boot(data = muestra2, statistic = media_trabajo, R = 99)
print(resultados_bootstrap)

Análisis post-hoc con bootstrapping
# Utilizaremos la función `glht` de `multcomp` para múltiples comparaciones
modelo <- lm(o10 ~ educ, data = muestra2)
comparaciones <- glht(modelo, linfct = mcp(educ = "Tukey"))
# summary(comparaciones)

# Podemos también realizar bootstrapping para las comparaciones post-hoc
comparacion_posthoc <- function(data, indices) {
  d <- data[indices, ]
  modelo <- lm(o10 ~ educ, data = d)
  comp <- glht(modelo, linfct = mcp(educ = "Tukey"))
  cld <- summary(comp)$test$coefficients
  return(cld)
}

# Aplicar bootstrapping a las comparaciones post-hoc
resultados_posthoc_bootstrap <- boot(data = muestra2, statistic = comparacion_posthoc, R = 99)
