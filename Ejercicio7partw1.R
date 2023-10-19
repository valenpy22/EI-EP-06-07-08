# Pregunt N°1
# Observando los datos, la memorista sospecha que hay diferencias significativas en el tiempo de ejecución entre las versiones B y C del
# algoritmo cuando las instancias tienen 75 o más nodos. ¿Los datos respaldan la intuición de la memorista?

# Para responder, filtren los datos para tener las instancias con 75 o más nodos y seleccionen las columnas de los tiempos de ejecución de 
# las versiones B y C en formato ancho. Usando como semilla el valor 23, obtenga muestras aleatorias independientes de 23 tiempos registrados por 
# la versión B y 20 tiempos registrados por la versión C del algoritmo. Realicen un análisis estadístico pertinente (enunciar hipótesis, revisar 
# condiciones, seleccionar prueba) para responder la pregunta planteada, utilizando pruebas no paramétricas de ser necesario.


# 1-Filtrar el conjunto de datos para seleccionar solo las instancias con 75 o más nodos.
# 2-Seleccionar las columnas de los tiempos de ejecución de las versiones B y C.
# 3-Obtener muestras aleatorias independientes con los tamaños indicados.
# 4-Realizar un análisis estadístico para determinar si hay diferencias significativas entre los tiempos de ejecución de las versiones B y C.

# Formulación de la Hipótesis 

# H0 : No hay diferencias significativas entre los tiempos de ejecución de las versiones B y C del algoritmo.
# Ha :Hay diferencias significativas entre los tiempos de ejecución de las versiones B y C del algoritmo.


library(tidyverse)
library(car)


# Cargar los datos
datos <- read.csv("~/Escritorio/EP07 Datos.csv", stringsAsFactors = TRUE)

# 1 . Se filtran los datos para incluir solo las instancias con 75 o más nodos

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
#Esto indica que si hay evidencia suficiente para afirmar que las muestras no siguen una distribución normal.

# Dado que ambas muestras no parecen ser normales y provienen de poblaciones independientes,no podemos considerar
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
# Dado que el p-valor es menor que 0.05, rechazamos la hipótesis nula de la prueba de Levene, lo que indica que las varianzas no son homogéneas.

# Dado que las varianzas no son homogéneas y  las muestras no parecen normales, para ser conservadores y teniendo en cuenta las recomendaciones de las pruebas no paramétricas,
# utilizaremos la prueba de suma de rangos de Wilcoxon para muestras independientes (también conocida como prueba de Mann-Whitney U) 
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





