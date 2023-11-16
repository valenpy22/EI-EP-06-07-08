# Se fija la carpeta de trabajo
dir <- getwd()
setwd(dir)

# Se cargan las librerías necesarias
library(ggpubr)
library(boot) # Para bootstrapping
library(dplyr) # Para manipulación de datos
library(multcomp) # Para comparaciones post-hoc
library(ez)

# Se cargan los datos
datos <- read.csv2("Desktop/EI-EP-06-07-08/EP08 Datos CASEN 2017.csv", fileEncoding = "Latin1", colClasses = c(r3 = "factor", s28 = "factor"))

##############################################################################
# PREGUNTA 1                                                                 #
##############################################################################
# ¿Existe una diferencia significativa en la frecuencia 
# de enfermedades crónicas entre los individuos que 
# pertenecen a un pueblo originario y aquellos que no, 
# en la población de Chile?
##############################################################################

# Pregunta a desarrollar:
cat("1. ¿Es igual la frecuencia de enfermedades crónicas entre los individuos que
    pertenecen a un pueblo originario y aquellos que no en Chile?\n\n")

# Se verifica que los datos se imprimen bien
# head(datos)

# Se ve el nombre de las variables de la tabla
# names(datos)

datos$r3 <- as.factor(datos$r3)
datos$s28 <- as.factor(datos$s28)

# Se verifican los niveles de los datos
# levels(datos$r3)
# levels(datos$s28)

# Verificar los valores únicos en las columnas r3 y s28
# unique(datos$r3)
# unique(datos$s28)

# Se escogen 125 personas
datos1 <- sample_n(datos, 125)
datos1 <- datos1 %>% 
  dplyr::select(r3, s28) %>%
  mutate(pueblo_originario = ifelse(r3 == "No pertenece a ningún pueblo indígena" | r3 == "No sabe/no responde", "No", "Si"),
         enfermedad_cronica = ifelse(s28 == "No ha estado en tratamiento por ninguna condicion de salud a" | s28 == "No sabe/No recuerda", "No", "Si"))

# Análisis exploratorio
table(datos1$pueblo_originario, datos1$enfermedad_cronica)

# Formulación de hipótesis
# Ho: No hay diferencia en la frecuencia de enfermedades crónicas entre los grupos
# Ha: Hay una diferencia en la frecuencia de enfermedades crónicas entre los grupos

# Se calcula la estadística de la prueba observada (chi-cuadrado)
obs_chi <- chisq.test(table(datos1$pueblo_originario, datos1$enfermedad_cronica))$statistic
obs_chi

# Simulación de Monte Carlo
set.seed(123)
n_simulaciones <- 9999
simulaciones <- replicate(n_simulaciones, {
  datos_simulados <- datos1 %>%
    mutate(pueblo_originario = sample(pueblo_originario, replace = TRUE),
           enfermedad_cronica = sample(enfermedad_cronica, replace = TRUE))
  chisq.test(table(datos_simulados$pueblo_originario, datos_simulados$enfermedad_cronica))$statistic
})

# Se calcula el valor de p
p_valor <- mean(simulaciones >= obs_chi)

# Resultados
cat("El valor p del análisis Monte Carlo es: ", p_valor, "\n")

cat("Como el valor de p es 0.5963, se falla en rechazar la hipótesis nula en favor de la
    alternativa, lo que quiere decir que no se puede descartar de que no exista una diferencia
    en la frecuencia de enfermedades crónicas entre las personas de pueblos originarios y
    las personas que no pertenecen a pueblos originarios.")

##############################################################################
# PREGUNTA 2                                                                 #
##############################################################################
# ¿Difieren significativamente los promedios de de horas de trabajo entre las 
# personas según su nivel de educación (Sin educación, M. Hum. completa y 
# profesional completo)?
##############################################################################

# Pregunta a desarrollar:
cat("2. ¿Son significativamente distintos los promedios de horas de trabajo entre
    las personas según su nivel de educación (Sin educación formal, M. Hum. Completa
    y Profesional Completo)?")

datos$educ <- as.factor(datos$educ)
datos$o10 <- as.numeric(as.character(datos$o10))

# Se verifican los niveles de educación
levels(datos$educ)

# Se filtran los datos según los niveles de educación
set.seed(1)
datos2 <- sample_n(datos, 250)

datos2 <- datos2 %>%
  filter(educ %in% c("Sin Educ. Formal", "M. Hum. Completa", "Profesional Completo")) %>%
  dplyr::select(o10, educ) %>%
  filter(!is.na(o10))

datos2

# Función para calcular la media de las horas de trabajo
media_horas <- function(data, indices){
  d <- data[indices, ]
  return(mean(d$o10))
}

# Se aplica boostrapping
resultados_bootstrap <- lapply(unique(datos2$educ), function(nivel) {
  data_nivel <- filter(datos2, educ == nivel)
  boot(data_nivel, media_horas, R = 999)
})

nombres <- unique(datos2$educ)
names(resultados_bootstrap) <- nombres

resultados_bootstrap

# Formulación de hipótesis
# H0: Las medias de las horas de trabajo son iguales para todos
# los niveles de educación. Es decir, no hay diferencias significativas
# entre los grupos.
# HA: Al menos una de las medias de las horas de trabajo es diferente a
# las otras. Es decir, hay al menos una diferencia significativa
# entre los grupos.

tukey_resultados <- TukeyHSD(aov(o10 ~ educ, data = datos2))

print(tukey_resultados)

# Conclusión
# Se tiene que entre las personas que no tienen educación formal y las que tienen
# enseñanza media completa, tienen una diferencia significativa.
# Entre las personas que no tienen educación formal y las que tienen profesional completo
# tienen una diferencia un poco más significativas que la anterior mencionada.
# Además, entre las personas que tienen la enseñanza media completa y las que tienen
# profesión, no hay diferencias significativas.

# Finalmente se tiene que se rechaza la hipótesis nula en favor de la hipótesis alternativa.
# Es decir, sí existen diferencias significativas.
