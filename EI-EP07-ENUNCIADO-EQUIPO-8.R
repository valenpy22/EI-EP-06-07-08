# Se importan las librerías
library("tidyverse")
library("ggpubr")
library("ez")
library("nlme")
library("emmeans")
library("PASWR2")
library("dplyr")
library("car")

# Pregunta N°1
# Observando los datos, la memorista sospecha que hay diferencias significativas 
# en el tiempo de ejecución entre las versiones B y C del
# algoritmo cuando las instancias tienen 75 o más nodos. ¿Los datos respaldan 
# la intuición de la memorista?

# Para responder, filtren los datos para tener las instancias con 75 o más 
# nodos y seleccionen las columnas de los tiempos de ejecución de 
# las versiones B y C en formato ancho. Usando como semilla el valor 23, 
# obtenga muestras aleatorias independientes de 23 tiempos registrados por 
# la versión B y 20 tiempos registrados por la versión C del algoritmo. 
# Realicen un análisis estadístico pertinente (enunciar hipótesis, revisar 
# condiciones, seleccionar prueba) para responder la pregunta planteada, 
# utilizando pruebas no paramétricas de ser necesario.

# 1-Filtrar el conjunto de datos para seleccionar solo las instancias con 75 o más nodos.
# 2-Seleccionar las columnas de los tiempos de ejecución de las versiones B y C.
# 3-Obtener muestras aleatorias independientes con los tamaños indicados.
# 4-Realizar un análisis estadístico para determinar si hay diferencias 
#   significativas entre los tiempos de ejecución de las versiones B y C.

# Formulación de la Hipótesis 
# H0: No hay diferencias significativas entre los tiempos de ejecución de 
#     las versiones B y C del algoritmo.
# Ha: Hay diferencias significativas entre los tiempos de ejecución de las versiones B y C del algoritmo.

# Cargar los datos
datos <- read.csv("EP07 Datos.csv", stringsAsFactors = TRUE)

# 1.Se filtran los datos para incluir solo las instancias con 75 o más nodos

# Filtrar las instancias con 75 o más nodos y seleccionar tiempos de ejecución de B y C
filtered_data <- datos %>% dplyr::filter(n.nodos >= 75) %>% dplyr::select(tiempo.B, tiempo.C)


# Dado que no sabemos si los datos siguen una distribución normal, utilizaremos pruebas no paramétricas. 
# Pero antes de seleccionar una prueba específica, vamos a evaluar la normalidad de las muestras usando 
# la prueba de Shapiro-Wilk. Si las muestras no son normales, recurriremos a pruebas no paramétricas.

# Obtener las muestras aleatorias

set.seed(23)
sample_B <- sample_n(filtered_data, 23)$tiempo.B
sample_C <- sample_n(filtered_data, 20)$tiempo.C

# Prueba de Shapiro-Wilk para normalidad
shapiro_test_B <- shapiro.test(sample_B)
shapiro_test_B
shapiro_test_C <- shapiro.test(sample_C)
shapiro_test_C

# Los resultados de la prueba de Shapiro-Wilk son los siguientes:
# Para la muestra B: p-valor = 0.02942
# Para la muestra C: p-valor = 0.0483

# Dado que ambos p-valores son menores que 0.05, rechazamos la hipótesis nula de la prueba de Shapiro-Wilk.
# Esto indica que si hay evidencia suficiente para afirmar que las muestras no siguen una distribución normal.

# Dado que ambas muestras no parecen ser normales y provienen de poblaciones independientes, no podemos considerar
# la prueba t de Student para muestras independientes. Sin embargo, para asegurarnos de que es la prueba 
# adecuada, debemos verificar la homogeneidad de varianzas utilizando la prueba de Levene.

# Realizar la prueba de Levene
grouping_vector <- factor(c(rep(1, length(sample_B)), rep(2, length(sample_C))))
levene_test <- leveneTest(y = c(sample_B, sample_C), group = grouping_vector)

# Mostrar los resultados
print(levene_test)
# Si las varianzas no son homogéneas, podemos considerar pruebas no paramétricas como la prueba de suma 
# de rangos de Wilcoxon. Veamos si las varianzas son homogéneas usando la prueba de Levene.

# El resultado de la prueba de Levene es: p-valor = 0.0175
# Dado que el p-valor es menor que 0.05, rechazamos la hipótesis nula de la prueba 
# de Levene, lo que indica que las varianzas no son homogéneas.

# Dado que las varianzas no son homogéneas y  las muestras no parecen normales, 
# para ser conservadores y teniendo en cuenta las recomendaciones de las pruebas 
# no paramétricas,
# utilizaremos la prueba de suma de rangos de Wilcoxon para muestras independientes 
# (también conocida como prueba de Mann-Whitney U) 
# para comparar las dos muestras.

# Realizaremos la prueba de Mann-Whitney U entre las muestras B y C.
mannwhitney_test <- wilcox.test(sample_B, sample_C, paired = FALSE, exact = FALSE, correct = FALSE, alternative = "two.sided")

# Mostrar los resultados
print(mannwhitney_test)

# Los resultados de la prueba de Mann-Whitney U son los siguientes:

# Valor de p: 0.0004146

# Dado que el p-valor es significativamente menor que 0.05, rechazamos la hipótesis nula. 
# Esto significa que hay evidencia suficiente para afirmar que hay diferencias significativas entre los tiempos
# de ejecución de las versiones B y C del algoritmo para instancias con 75 o más nodos.
# Por lo tanto, los datos respaldan la intuición de la memorista.

#2. La memorista también sospecha que, al comparar las mismas instancias de iguales 
# características, las mejores soluciones encontradas por las versiones B y C 
# tienen rendimientos distintos. ¿Estará en lo cierto?
# Para responder, filtren los datos para tener las instancias con 75 o más nodos 
# y seleccionen las columnas con el mejor rendimiento de las versiones B y C en 
# formato ancho. Usando como semilla el valor 73, obtengan una muestra aleatoria de 
# 19 instancias. Realicen un análisis estadístico pertinente (enunciar hipótesis, 
# revisar condiciones, seleccionar prueba) para responder la pregunta planteada, 
# utilizando pruebas no paramétricas de ser necesario.

# Se importan los datos
data <- read.csv("EP07 Datos.csv")

# Se filtran los datos
data_filtered <- data[data$n.nodos >= 75, ]
data_filtered <- data_filtered[, c("mejor.B", "mejor.C")]
print(data_filtered)

# Se setea la semilla para sacar la muestra
set.seed(73)
sample <- data_filtered[sample(nrow(data_filtered), 19), ]
print(sample)

# Se plantea la hipótesis:

# H0: Las medianas de los mejores rendimientos de las versiones B y C son iguales.
# Ha: Las medianas de los mejores rendimientos de las versiones B y C son diferentes.

# Se analizan las condiciones para utilizar ANOVA:

# Los datos son independientes entre sí.

# Usamos el test de Shapiro-Wilk para verificar la normalidad de los datos
shapiro_test_B <- shapiro.test(sample$mejor.B)
shapiro_test_C <- shapiro.test(sample$mejor.C)

shapiro_test_B$p.value
shapiro_test_C$p.value

# Dado que los valores p de ambas muestras son menor a una nivel de significancía del 0.05, 
# se puede rechazar la hipótesis nula en favor de la alternativa, es decir, no 
# presentan una distribución normal por lo que se debe usar una prueba no paramétrica.
# Se analizan las condiciones para utilzar la prueba de Wilcoxon Mann Whitney
# Las observaciones de ambas muestras son independientes, puesto que son algoritmos 
# diferentes y el tiempo que demora cada uno solo depende de su implementación.
# La escala de medición es a lo menos ordinal, se cumple pues existe la posibilidad 
# de comparar los tiempos y discriminar la relación que poseen, por ejemplo, si uno es menor que otro. 
# Se realiza la prueba de Wilcoxon
result <- wilcox.test(sample$mejor.B, sample$mejor.C, paired = TRUE, conf.level = 0.05)

print(result)

# Al resultar un valor p de 0.2413, es decir, mayor que nuestro alfa = 0.05, se 
# concluye que no se tiene información suficiente para rechazar la hipótesis nula 
# en favor de la alternativa, en simples palabras no se puede concluir que haya 
# una diferencia significativa entre los rendimientos de las mejores soluciones 
# de la version B y C.
# Como se realiza una comparación entre dos muestras conocidas no se hace necesaria una prueba post-hoc.

# III. La memorista sospecha que hay diferencias significativas 
# en el tiempo de ejecución entre las versiones del algoritmo
# cuando las instancias de prueba tienen 50 o más nodos. 
# ¿Los datos respaldan la intuición de la memorista?
# Para responder, filtren los datos para tener las instancias con 
# 50 o más nodos. 
# Seleccionen las columnas con los tiempos de 
# ejecución registrados (en formato ancho). Usando como semilla 
# el valor 43, obtengan muestras aleatorias independientes de 13, 
# 14 y 13 tiempos registrados por las versiones A, B y C, 
# respectivamente. Realicen un análisis estadístico pertinente 
# (enunciar hipótesis, revisar condiciones, seleccionar prueba) 
# para responder la pregunta planteada, utilizando pruebas no 
# paramétricas de ser necesario. 

# 1. Recoger los datos
datos <- read.csv("EP07 Datos.csv")
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
set.seed(43)

indices_A <- sample(seq_len(length(tiempos_A)), 13)
muestra_A <- tiempos_A[indices_A]
tiempos_A <- tiempos_A[-indices_A]

indices_B <- sample(seq_len(length(tiempos_B)), 14)
muestra_B <- tiempos_B[indices_B]
tiempos_B <- tiempos_B[-indices_B]

indices_C <- sample(seq_len(length(tiempos_C)), 13)
muestra_C <- tiempos_C[indices_C]
tiempos_C <- tiempos_C[-indices_C]

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

# Realicen un análisis estadístico pertinente 
# (enunciar hipótesis, revisar condiciones, seleccionar prueba) para responder la 
# pregunta planteada, utilizando pruebas no paramétricas de ser necesario.

# 1. Recoger los datos
datos2 <- read.csv("EP07 Datos.csv")
head(datos2)

# 2. Se filtran los datos que tengan 50 o más nodos
datos_filtrados2 <- datos2[datos2$n.nodos >= 50, ]
head(datos_filtrados2)

set.seed(16)
muestra_2 <- datos_filtrados2[sample(nrow(datos_filtrados), 21), ]

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
                                   paired = TRUE)
  print("Se encontraron diferencias significativas, y estos son los resultados: ")
  print(post_hoc)
}

# Por lo tanto, si p es menor que alfa se tienen diferencias significativas.