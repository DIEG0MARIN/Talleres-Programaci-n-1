####################
#                  #
# Copiar todo esto #
#                  #
####################


#FUNCIONES CON VECTORES:

# length(), sum(),mean()

# video 05 - vectores

# Objetivo: estudiar qué es una vector en R.
# --------------------------------------------
# En este ejercicio vamos a:
# 1. Crear vectores en un script
# 2. Realizar opraciones aritméticas con vectores
# 3. Seleccionar elementos en un vector


#####################################
# práctica 1: creando vectores en R #
#####################################

# crear vector carácter con nombre de las películas

v_1 <- c("Que paso ayer", "Jhon wick", "La teoria del todo", "El hombre que conocía el infinito")


# crear vector numérico con puntuación de las películas

v_2 <- c(9.5,9.7,8.8,9)


# crear vector lógico sobre si la película es posterior a 2015

v_3 <- c(TRUE, FALSE, TRUE, FALSE)


####################################################
# práctica 2: operaciones aritméticas con vectores #
####################################################

# sumar 2 a la puntuación
v_2 +2
# dividir la puntuación entre 2

V
# crea la puntuación de Diego

puntuacion_diego <- c(9.2,9.8,9, 8.8)


# calcular diferencia entre puntuaciones


puntuacion_diego - v_2 

# calcular la longitud del vector

length(puntuacion_diego)


# calcular el promedio del vector puntuacion

cat("Este es el promedio de la puntuación de diego:", mean(puntuacion_diego))


###################################################
# práctica 3: selección de elementos de un vector #
###################################################

## selección basada en posición
# seleccionar la tercera película


v_1[3]

# seleccionar la primera y la última película

v_1[c(1,length(v_1))] # SE IMPRIME LA PRIMERA Y LA ULTIMA PELICULA , SIN NECESIDAD DE CONTAR LA POSICION DE LA ULTIMA PELICULA
v_1[c(1,4)] # IGUAL SE IMPRIME LA PRIMERA Y ULTIMA , PERO AQUI SE DEBE CONTAR CUAL ES EL NUMERO DE LA ULTIMA POSICIÓN


## selección basada en condición lógica
# crear condición lógica
cat("Estas peliculas se encuentran dentro de la puntuación promedio del cine:" ,puntuacion_promedio <- v_2 > 9)
nombre 

# mostrar puntuaciones altas
v_2[puntuacion_promedio]

# mostrar nombres de películas con puntuaciones Altas


cat("Estas peliculas hacen parte del listado de peliculas con puntuación alta:" ,v_1[puntuacion_promedio])



























