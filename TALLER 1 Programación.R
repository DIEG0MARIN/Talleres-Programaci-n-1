# TALLER PROGRAMACIÓN 1 - DIEGO FERNANDO MARIN

pg <- PlantGrowth #En esta linea se carga el dataset en una variable llamada 'pg'
str(pg) # esta función nos determina la estructura de la variable pg, para el caso del dataframe que cargamos
head(pg) # se aprecian las primeras 6 filas 
names(pg) # nombre de las columnas
is.factor((pg$group)) # Esta linea de codigo determina si el valor group es factor , se asocia con un booleano
levels(pg$group) # A continuación se muestran los niveles y su orden


#---------------------------------------------2.FILTRO SIMPLE----------------------------------------------

pg_pesadas <- pg[pg$weight >= 5.0, ]  # Se crea una nueva variable donde se almacena un nuevo dataframe , bajo ciertas condiciones
nrow(pg_pesadas) # se listan el número de filas que cumplen la condición
head(pg_pesadas, 5) # Se muestran las primeras 5 filas, con el parametro adicional en el head


#--------------------------------------------3. Filtros Compuestos----------------------------------------

pg_no_ctrl <- pg[pg$group != "ctrl" & pg$weight >= 5.0,] # Se crea una nueva variable que almacena la condición en donde el grupo sea diferente de ctrl y el tamaño mayor o igual a 5

pg_no_ctrl # Se realiza una pequeña previsualización de los datos en consola


pg_ctrl_o_w <- pg[pg$group == "ctrl" | pg$weight <4.5, ]  # Esta variable almacena los valores = ctrl o weight sea menor a 4.5
pg_ctrl_o_w # Previsualización por consola de la condición mencionada anteriormente


#---------------------------------- 4.Pertenencia ---------------------------------------------------------

pg_trt <- pg[pg$group %in% c("trt1", "trt2"),] # Se trae unicamente a trt1 y trt2
table(pg_trt$group) # con esta función se grafica la tabla

#----------------------------------5.Rango ----------------------------------------------------------------

pg_rango <- pg[pg$weight >= 4.5 & pg$weight <= 5.5 , ] # Se filtra en el rango indicado 
summary(pg_rango$weight) # Se muestra un resumen de la variable pg_rango en el rango indicado
range(pg_rango$weight) # Se visualiza el mínimo y máximo observados en el rango



#----------------------------------6. Ordenar--------------------------------------------------------------

pg_orden_we <- pg[order(pg$weight),] # esta función ordena de forma ascendente
pd_orden_g <- pg[order(pg$group, -pg$weight),] # se ordena por group  y weight , por cada grupo de forma descendente


#------------------------------------7. NA BASICOS---------------------------------------------------------


pg_na <- pg # se realiza una copia del archivo original paa no alterar datos
id <- sample(seq_len(nrow(pg_na)),2) # se eligen 2 índices aleatorios
pg_na$weight[id] <- NA # Se introduce NA
colSums(is.na(pg_na)) #Cuenta cada NA por columna
mean(pg_na$weight) # Media con NA
mean(pg_na$weight, na.rm = TRUE) #Media ignorando NA

#--------------------------------8. Top -k-------------------------------------------------------------------



pg$id <- seq_len(nrow(pg))  #Se crea un identificador simple por fila
top5_ids <- order(pg$weight, decreasing = TRUE)[1:5] # indices de las 5 mayores observaciones
top5 <- pg[top5_ids, c("id", "group", "weight")] # Se selecciona id, group y weight de dichas filas
top5 # A continuación se verá el top5



#------------------------------9. Resumen por grupo Manual----------------------------------------------------


niveles <- levels(pg$group)  # Niveles del factor (ctrl, trt1, trt2)
n_vec <- mean_vec <- sd_vec <- pct_ge5_vec <- numeric(length(niveles)) # Reasigna vectores para resultados

for (i in seq_along(niveles)) {                          # Itera por cada nivel de group sin usar agregación avanzada.
  g <- niveles[i]                                        # Nivel actual.
  mask <- pg$group == g                               # Máscara booleana para filas del grupo.
  w <- pg$weight[mask]                                # Pesos del grupo.
  n_vec[i] <- sum(mask)                               # Conteo de casos en el grupo.
  mean_vec[i] <- mean(w)                              # Media de weight en el grupo.
  sd_vec[i] <- sd(w)                                  # Desviación estándar en el grupo.
  pct_ge5_vec[i] <- mean(w >= 5.0) * 100              # % con weight >= 5.0 (mean(TRUE) = proporción).
}
resumen_grupos <- data.frame(                         # Construye tabla de resumen manual.
  group = niveles,
  n = n_vec,
  mean = round(mean_vec, 3),
  sd = round(sd_vec, 3),
  pct_ge5 = round(pct_ge5_vec, 1)
)
resumen_grupos         # Se muestra el resumen final por grupo





#-----------------------------------------------10. Lista ---------------------------------------------------------------


ctrl_df <- pg[pg$group == "ctrl", ]                   # Sub-data.frame solo del grupo ctrl.
resumen <- c(mean = mean(pg$weight), sd = sd(pg$weight)) # Resumen global de weight.
meta <- list(autor = "(tu nombre)",                   # Metadatos de autoría (ajusta con tu nombre real).
             fecha = as.character(Sys.Date()),        # Fecha actual como texto.
             version = "1.0")                         # Versión del script.
L <- list(datos_ctrl = ctrl_df,                       # Lista final con 3 componentes (datos, resumen y meta).
          resumen = resumen,
          meta = meta)
L$datos_ctrl[1:3, ]                                   # Acceso a primeras 3 filas de datos_ctrl.
L$resumen                                             # Acceso al vector resumen.
L$meta$autor                                          # Acceso a un campo especifico




# ============================================================
# BLOQUE 2 — Gráficos 
# ============================================================



#------------------------------- 1. Scatter simple ------------------------------------------------------------------------------




plot(iris$Sepal.Length, iris$Sepal.Width,       # Dispersión: x = Sepal.Length, y = Sepal.Width
     main = "Iris: Sepal.Length vs Sepal.Width",# se asigna el título
     xlab = "Sepal.Length",                     # Nombre del eje x
     ylab = "Sepal.Width")                      # Nombre del eje y


#--------------------------------2. Histograma ---------------------------------------------------------------------------------

hist(iris$Petal.Length,                         # Variable: Petal.Length
     breaks = 10,                               # Como mínimo 10 intervalos
     main = "Iris: Histograma de Petal.Length",# Título
     xlab = "Petal.Length")                     # Etiqueta del eje x


#----------------------------------------- 3. Boxplot por especie ----------------------------------------------------------------


boxplot(Petal.Width ~ Species, data = iris,     # Boxplot: Petal.Width agrupado por especies
        main = "Iris: Petal.Width por especie", # Título
        xlab = "Species", ylab = "Petal.Width") # Etiquetas de ejes


#--------------------------------------------4. (Scatter mtcars) ------------------------------------------------------------------


plot(mtcars$hp, mtcars$mpg,                     # Dispersión: x = hp (potencia), y = mpg (rendimiento)
     pch = 16,                                  # Símbolo de punto (relleno)
     cex = 1.1,                                 # Tamaño del punto
     main = "mtcars: hp vs mpg",                # Título
     xlab = "hp", ylab = "mpg")                 # Etiquetas de ejes
abline(h = mean(mtcars$mpg), lty = 2)           # Línea horizontal en la media de mpg (línea punteada)


#------------------------------------------5. Panel múultiple ----------------------------------------------------------------------


old_par <- par(no.readonly = TRUE)              # Se guarda configuración gráfica actual
par(mfrow = c(2, 2))                            # 2 filas x 2 columnas de gráficos

hist(mtcars$mpg,                                # (i) Histograma de mpg
     main = "Hist mpg",
     xlab = "mpg")

boxplot(mpg ~ cyl, data = mtcars,               # (ii) Boxplot de mpg por número de cilindros
        main = "mpg por cyl",
        xlab = "cyl", ylab = "mpg")

plot(mtcars$hp, mtcars$mpg,                     # (iii) Dispersión hp vs mpg
     main = "hp vs mpg",
     xlab = "hp", ylab = "mpg")

hist(mtcars$hp,                                 # (iv) Histograma de hp
     main = "Hist hp",
     xlab = "hp")

par(old_par)                                    # Se restaura configuración gráfica previa



#------------------------------------------6.  Curvas con loop --------------------------------------------------------------------


x <- seq(-5, 5, length.out = 400)               # Vector de x en [-5,5]
a_vals <- c(0.5, 1, 2, 3)                       # Conjunto de parámetros a
plot(x, a_vals[1] * x^2, type = "l",            # a continuación se dibuja la primera parábola como línea
     main = "Parábolas y = a x^2",
     xlab = "x", ylab = "y")
for (k in 2:length(a_vals)) {                   # para el caso se  agregan las curvas restantes
  lines(x, a_vals[k] * x^2)
}
legend("topleft",                                # Leyenda en esquina superior izquierda
       legend = paste("a =", a_vals),            # Etiquetas de cada curva
       lty = 1,                                  # Mismo tipo de línea
       bty = "n")                                # Sin marco en la leyenda



### =======================================================================================================================
### BLOQUE 3 — Condicionales, bucles y funciones 

#===========================================================================================================================

#------------------------------------------------------- 1. if / else -----------------------------------------------------------------



es_par_impar <- function(n) {                 # Definir una función que clasifica un ENTERO como "par" o "impar".
  if (!is.numeric(n) || n != as.integer(n))   # Validación: debe ser numérico y entero (sin decimales).
    stop("n debe ser entero.")                # Si no cumple, detiene con mensaje claro.
  if (n %% 2 == 0) "par" 
  else "impar"         # Usar el módulo: residuo 0 => par; otro => impar.
}
clasifica_signo <- function(x) {              # función que clasifica un REAL por su signo.
  if (x < 0) "negativo" else if (x == 0) "cero" else "positivo"  # Devuelve etiqueta según comparación.
}
# Pruebas
es_par_impar(2) # Esperado: "par"
es_par_impar(7)    # Esperado: "impar"
clasifica_signo(-2)  # Esperado: "negativo"



#---------------------------------------2. for básico --------------------------------------------------------------------------------------



cuadrados <- integer(10)                      # Preasigna vector entero de longitud 10 
for (i in 1:10) {                             # Recorre los valores i = 1,2,...,10.
  cuadrados[i] <- i^2                         # Guarda en la posición i el valor i^2.
  cat(i, "^2 = ", cuadrados[i], "\n", sep = "")  # Muestra progreso
}
cuadrados       # Se muestra el vector final de cuadrados





# ---------------------------------------------3. while -------------------------------------------------------------------------------------


m <- 0                                        # Inicializa contador m.
suma <- 0                                     # Inicializa acumulador de la suma parcial.
while (suma < 200) {                          # Repite hasta que se alcance/ supere 200.
  m <- m + 1                                  # Aumenta m en 1.
  suma <- suma + m                            # Agrega m a la suma acumulada (1+2+...+m).
}
m                                            # Devuelve m mínimo que cumple la condición.
suma                                         # Devuelve la suma lograda (>= 200).


#-----------------------------------------------4. (Función: área por Herón --------------------------------------------------------------------

heron_area <- function(a, b, c) {             # Calcula área usando s = (a+b+c)/2 y √(s(s-a)(s-b)(s-c)).
  if (any(!is.numeric(c(a,b,c))) || any(c(a,b,c) <= 0))      # Valida que los lados sean numéricos y positivos.
    stop("Lados deben ser numéricos y positivos.")           # Error si hay lados inválidos.
  if (a + b <= c || a + c <= b || b + c <= a)                # Verifica desigualdad triangular
    stop("No cumple desigualdad triangular.")                # Error si no forma triángulo.
  s <- (a + b + c) / 2                                       # Semiperímetro s.
  area <- sqrt(s * (s - a) * (s - b) * (s - c))              # Área por Herón.
  return(area)                                               # Retorna (numérico).
}
heron_area(3, 4, 5)                              # Prueba: triángulo 3-4-5 → área esperada = 6.




# ----------------------------------------------- 5. Función: derivada & tangente ---------------------------------------------------------------


recta_tangente <- function(f, x0, h) {         # Devuelve pendiente y coeficientes (m,b) de y = m x + b en x0.
  if (!is.function(f))                         # Validación: f debe ser función (callable).
    stop("f debe ser una función.")
  if (!is.numeric(x0) || !is.numeric(h) || h <= 0)   # x0 numérico y paso h positivo.
    stop("x0 numérico, h > 0.")
  f_x0 <- f(x0)                                # Valor de la función en x0 
  m <- (f(x0 + h) - f(x0 - h)) / (2 * h)       # Aproximación de f'(x0) por diferencias centradas.
  b <- f_x0 - m * x0                           # Intersección con eje y para que la recta pase por (x0, f(x0)).
  list(pendiente = m, ecuacion = c(m = m, b = b))   # Retorna pendiente y parámetros (m, b).
}
recta_tangente(function(x) x^2, x0 = 2, h = 1e-5)  # Prueba: f(x)=x^2 → derivada real en 2 es 4.
recta_tangente(sin, x0 = pi/4, h = 1e-5)           # Prueba: f(x)=sin x → derivada real en π/4 es cos(π/4).


#------------------------------------------------6. Scope conceptual -------------------------------------------------------------------------------


# Las variables creadas dentro de una función se alojan en un entorno local Y no modifican las variables globales con el mismo nombre.























































































