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
# ¿Existe una diferencia significativa en la frecuencia 
# de enfermedades crónicas entre los individuos que 
# pertenecen a un pueblo originario y aquellos que no, 
# en la población de Chile?
##############################################################################

# Pregunta a desarrollar:
cat("1. ¿Es igual la frecuencia de enfermedades crónicas entre los individuos que
    pertenecen a un pueblo originario y aquellos que no en Chile?\n\n")

# Se verifica que los datos se imprimen bien
head(datos)

# Se ve el nombre de las variables de la tabla
# names(datos)

datos$r3 <- as.factor(datos$r3)
datos$s28 <- as.factor(datos$s28)

# Se verifican los niveles de los datos
levels(datos$r3)
levels(datos$s28)

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

#Análisis post-hoc con bootstrapping
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
