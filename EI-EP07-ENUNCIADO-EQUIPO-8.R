# Se importan las librerías
library("tidyverse")
library("ggpubr")
library("ez")
library("nlme")
library("emmeans")
library("PASWR2")
library("dplyr")

# III. La memorista sospecha que hay diferencias significativas 
# en el tiempo de ejecución entre las versiones del algoritmo
# cuando las instancias de prueba tienen 50 o más nodos. 
# ¿Los datos respaldan la intuición de la memorista?
# Para responder, filtren los datos para tener las instancias con 
# 50 o más nodos. 
# Seleccionen las columnas con los tiempos de 
# ejecución registrados (en formato ancho). Usando como semilla 
# el valor 43, obtengan muestras aleatorias independientes de 13, 

##################################################################
# 14 y 13 tiempos registrados por las versiones A, B y C, 
# respectivamente. Realicen un análisis estadístico pertinente 
# (enunciar hipótesis, revisar condiciones, seleccionar prueba) 
# para responder la pregunta planteada, utilizando pruebas no 
# paramétricas de ser necesario. 

# 1. Recoger los datos
datos <- read.csv("Desktop/EP-Grupo-8/EP07 Datos.csv")
head(datos)

# 2. Se filtran los datos que tengan 50 o más nodos
datos_filtrados <- datos[datos$n.nodos >= 50, ]
head(datos_filtrados)

summary(datos_filtrados)

# 3. Seleccionar los tiempos de ejecución registrados
tiempos_A <- datos_filtrados$tiempo.A
tiempos_B <- datos_filtrados$tiempo.B
tiempos_C <- datos_filtrados$tiempo.C

# 4. Usar semilla 43
muestra_A <- sample(tiempos_A, 13)
muestra_B <- sample(tiempos_B, 14)
muestra_C <- sample(tiempos_C, 13)

# 5. Plantear las hipótesis
# Ho: No hay diferencia en la media de los tiempos de ejecución entre
#     los tiempos A, B y C.
#     mu_A = mu_B = mu_C
# Ha: Hay al menos una media de tiempo de ejecución que es distinta entre
#     los tiempos A, B y C.
#     

# 6. Revisar condiciones para emplear ANOVA
# - Las poblaciones son independientes.
# - No todas las muestras tienen una distribución a la normal,
#   por lo que no se puede emplear ANOVA. Se demuestra empleando
#   el shapiro.test.

shapiro.test(muestra_A)
shapiro.test(muestra_B)
shapiro.test(muestra_C)

eda(muestra_A)
eda(muestra_B)
eda(muestra_C)

# En el caso de la muestra C, se tiene un valor de p mayor a 0.10,
# por lo que no es una distribución cercana a la normal con un 95%
# de confianza.

# A continuación, verifican las condiciones para realizar
# la prueba de Kruskal-Wallis.

# 7. Revisar condiciones para emplear la prueba de Kruskal-Wallis
# - La variable independiente debe tener a lo menos dos niveles.
#   En este caso tiene 3 niveles.
# - La escala de la variable dependiente debe ser a lo menos ordinal.
#   Sí se cumple porque se puede ordenar según los tiempos de ejecición
# - Las observaciones son independientes entre sí.
#   También se cumple.

# Se construye la matriz de datos
Tiempo <- c(muestra_A, muestra_B, muestra_C)

Algoritmo <- c(rep("A", length(muestra_A)),
               rep("B", length(muestra_B)),
               rep("C", length(muestra_C)))

Algoritmo <- factor(Algoritmo)

datas <- data.frame(Tiempo, Algoritmo)

# 8. Se establece el nivel de significancia
alfa <- 0.01

# 9. Se hace la prueba de Kruskal-Wallis.
prueba <- kruskal.test(Tiempo ~ Algoritmo, data = datas)
print(prueba)

# 10. Realizar la conclusión en base al valor de p
if(prueba$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(datas$Tiempo,
                                   datas$Algoritmo,
                                   p.adjust.method = "holm",
                                   paired = FALSE)
  print("Se encontraron diferencias significativas, y estos son los resultados: ")
  print(post_hoc)
  
}

# Por lo tanto, si p es menor que alfa se tienen diferencias significativas.

# IV. La memorista también sospecha que, al comparar las mismas instancias 
# con iguales características, las mejores soluciones encontradas por las 
# diferentes versiones del algoritmo tienen rendimientos distintos. 
# ¿Estará en lo cierto?
# Para responder, filtren los datos para tener las instancias con 50 o más 
# nodos y seleccionen las columnas con los mejores rendimientos registrados. 
# Usando como semilla el valor 16, obtengan una muestra aleatoria de 21 instancias. 
###################################################################################

# Realicen un análisis estadístico pertinente 
# (enunciar hipótesis, revisar condiciones, seleccionar prueba) para responder la 
# pregunta planteada, utilizando pruebas no paramétricas de ser necesario.

# 1. Recoger los datos
datos2 <- read.csv("Desktop/EP-Grupo-8/EP07 Datos.csv")
head(datos2)

# 2. Se filtran los datos que tengan 50 o más nodos
datos_filtrados2 <- datos2[datos2$n.nodos >= 50, ]
head(datos_filtrados2)

# 3. Seleccionar los tiempos de ejecución registrados
tiempos_mejor_A <- datos_filtrados2$mejor.A
tiempos_mejor_B <- datos_filtrados2$mejor.B
tiempos_mejor_C <- datos_filtrados2$mejor.C

# 4. Usar semilla 16
muestra_mejor_A <- sample(tiempos_mejor_A, 8)
muestra_mejor_B <- sample(tiempos_mejor_B, 7)
muestra_mejor_C <- sample(tiempos_mejor_C, 6)

# 5. Plantear las hipótesis
# Ho: No hay diferencia en la media de los tiempos de ejecución entre
#     los tiempos A, B y C.
#     mu_A = mu_B = mu_C
# Ha: Hay al menos una media de tiempo de ejecución que es distinta entre
#     los tiempos A, B y C.
#     

# 6. Revisar condiciones para emplear ANOVA
# - Las poblaciones son independientes.
# - No todas las muestras tienen una distribución a la normal,
#   por lo que no se puede emplear ANOVA. Se demuestra empleando
#   el shapiro.test.

shapiro.test(muestra_mejor_A)
shapiro.test(muestra_mejor_B)
shapiro.test(muestra_mejor_C)

eda(muestra_mejor_A)
eda(muestra_mejor_B)
eda(muestra_mejor_C)

# En el caso de la muestra C, se tiene un valor de p mayor a 0.10,
# por lo que no es una distribución cercana a la normal con un 95%
# de confianza.

# A continuación, verifican las condiciones para realizar
# la prueba de Kruskal-Wallis.

# 7. Revisar condiciones para emplear la prueba de Kruskal-Wallis
# - La variable independiente debe tener a lo menos dos niveles.
#   En este caso tiene 3 niveles.
# - La escala de la variable dependiente debe ser a lo menos ordinal.
#   Sí se cumple porque se puede ordenar según los tiempos de ejecición
# - Las observaciones son independientes entre sí.
#   También se cumple.

# Se construye la matriz de datos
Tiempo2 <- c(muestra_mejor_A, muestra_mejor_B, muestra_mejor_C)

Algoritmo2 <- c(rep("A", length(muestra_mejor_A)),
               rep("B", length(muestra_mejor_B)),
               rep("C", length(muestra_mejor_C)))

Algoritmo2 <- factor(Algoritmo2)

datas2 <- data.frame(Tiempo2, Algoritmo2)

# 8. Se establece el nivel de significancia
alfa <- 0.01

# 9. Se hace la prueba de Kruskal-Wallis.
prueba2 <- kruskal.test(Tiempo2 ~ Algoritmo2, data = datas2)
print(prueba2)

# 10. Realizar la conclusión en base al valor de p
if(prueba2$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(datas2$Tiempo2,
                                   datas2$Algoritmo2,
                                   p.adjust.method = "holm",
                                   paired = FALSE)
  print("Se encontraron diferencias significativas, y estos son los resultados: ")
  print(post_hoc)
}

# Por lo tanto, si p es menor que alfa se tienen diferencias significativas.