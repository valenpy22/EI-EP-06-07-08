# ¿Existe una diferencia significativa en la frecuencia 
# de enfermedades crónicas entre los individuos que 
# pertenecen a un pueblo originario y aquellos que no, 
# en la población de Chile?

library(ggpubr)

# Se leen los datos
datos <- read.csv2("Desktop/EP-Grupo-8/EP08 Datos CASEN 2017.csv")

head(datos)
# Se hace la selección de una muestra aleatoria
set.seed(123)
n <- sample(101:149, 1)
muestra <- datos[sample(nrow(datos), n), ]

# Se define el número de simulaciones
num_simulaciones <- 2999
diferencias <- numeric(num_simulaciones)

for(i in 1:num_simulaciones){
  muestra_simulada <- muestra[sample(nrow(muestra), n, replace = TRUE), ]
  frecuencia_originarios <- mean((muestra_simulada$r3 == "No pertenece a ningún pueblo indígena" || muestra_simulada$r.3 == "No sabe/no responde") & ((muestra_simulada$s28 != "No ha estado en tratamiento por ninguna condicion de salud a") || (muestra_simulada != "No sabe/No recuerda")))
  frecuencia_no_originarios <- mean((muestra_simulada$r3 != "No pertenece a ningún pueblo indígena" || muestra_simulada$r.3 == "No sabe/no responde") & ((muestra_simulada$s28 != "No ha estado en tratamiento por ninguna condicion de salud a") || (muestra_simulada != "No sabe/No recuerda")))
  diferencias[i] <- frecuencia_originarios - frecuencia_no_originarios
}

# Análisis de los resultados
media_diferencias <- mean(diferencias)
ic_inf <- quantile(diferencias, 0.025)
ic_sup <- quantile(diferencias, 0.975)

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
