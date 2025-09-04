# Cargar la librería 'palmerpenguins'
# Instalamos la librería si no está instalada
if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
  install.packages("palmerpenguins")  # Instala el paquete desde CRAN
}

# Cargamos la librería
library(palmerpenguins)

# 1. Filtrar todos los pingüinos de especie Adelie
A <- penguins[penguins$species == "Adelie", ]
head(A)

# 2. Filtrar los pingüinos con masa corporal > 4500 g (body_mass_g)
B <- penguins[penguins$body_mass_g > 4500, ]
head(B)

# 3. Filtrar los pingüinos de isla Dream con largo de pico > 45 mm (bill_length_mm)
C <- penguins[penguins$island == "Dream" & penguins$bill_length_mm > 45, ]
head(C)

# 4. De (A), devolver solo species, island, body_mass_g
A_min <- A[, c("species", "island", "body_mass_g")]
head(A_min)

# 5. Crear una vista con species, sex, flipper_length_mm para quienes no son de la isla Torgersen
D <- penguins[penguins$island != "Torgersen", c("species", "sex", "flipper_length_mm")]
head(D)

# 6. Seleccionar pingüinos de especies Adelie o Gentoo usando %in%
E <- penguins[penguins$species %in% c("Adelie", "Gentoo"), ]
head(E)

# 7. Excluir pingüinos con sex en {"male"} (solo hembras y NAs)
F <- penguins[penguins$sex != "male" | is.na(penguins$sex), ]
head(F)

# 8. Tomar una muestra aleatoria de n = 20 filas de penguins sin reemplazo
G <- penguins[sample.int(nrow(penguins), size = 20, replace = FALSE), ]
nrow(G)

# 9. Tomar una muestra de n = 60 con reemplazo
H <- penguins[sample.int(nrow(penguins), size = 60, replace = TRUE), ]
nrow(H)

# 10. Tomar una muestra de 10 pingüinos únicamente de especie Gentoo
idx_gentoo <- which(penguins$species == "Gentoo")
I <- penguins[sample(idx_gentoo, size = 10, replace = FALSE), ]
nrow(I)

# 11. Tomar 5 pingüinos por especie (máximo posible si falta)
split_especie <- split(penguins, penguins$species)

# Tomamos 5 pingüinos de cada especie (o el máximo disponible)
lista_muestras <- lapply(split_especie, function(df) {
  df <- df[!is.na(df$species), ]           # Aseguramos que no haya NA en la columna 'species'
  n <- min(5, nrow(df))                    # Tomamos el mínimo entre 5 y el número de filas
  if (n == 0) return(df[0, ])              # Si no hay filas, devolvemos un data frame vacío
  df[sample.int(nrow(df), n), ]            # Tomamos una muestra aleatoria
})

# Unimos las muestras de cada especie en un solo data frame
J <- do.call(rbind, lista_muestras)

# Mostramos las especies presentes en la muestra
table(J$species)

# Parte 2 — Manejo de bases de datos

# 12. Parte penguins en dos mitades p1 y p2 por filas
n <- nrow(penguins)
mitad <- floor(n / 2)
p1 <- penguins[1:mitad, ]
p2 <- penguins[(mitad + 1):n, ]
nrow(p1)
nrow(p2)

# 13. Recomponer con rbind(p1, p2) y verificar dimensiones
recompuesto <- rbind(p1, p2)
identical(dim(recompuesto), dim(penguins))     # TRUE si coincide
all.equal(recompuesto, penguins)               # compara contenido

# 14. ¿Qué ocurre si las columnas no están en el mismo orden?
ej_A <- penguins[, c("id","species","island","bill_length_mm")]
ej_B <- penguins[, c("bill_length_mm","island","species","id")]
dim(rbind(ej_A, ej_B))   # funciona porque los nombres están

# 15. Crea p3 y p4 con orden distinto y explica
p3 <- penguins[, c("id","species","island","bill_length_mm")]
p4 <- penguins[, c("bill_length_mm","island","species","id")]
# p3: columnas en orden (id, species, island, bill_length_mm)
# p4: mismas columnas pero orden distinto

# 16. rbind(p3, p4) — ¿por qué funciona?
ok <- rbind(p3, p4)  # Empareja por **nombres** de columnas, no por posición
head(ok)

# 17. Crear vector lógico ok_peso = body_mass_g >= 4500
ok_peso <- penguins$body_mass_g >= 4500
table(ok_peso, useNA = "ifany")

# 18. Une ok_peso a un subconjunto mini <- penguins[, c("id","species","body_mass_g")] con cbind
mini <- penguins[, c("id","species","body_mass_g")]
mini_ok <- cbind(mini, ok_peso)
head(mini_ok)

# 19. Explica el riesgo de usar cbind() cuando el orden de filas no coincide entre objetos
# cbind no reordena ni alinea por claves; solo **pega por posición**. Si los objetos están barajados, los valores se desalinean.

# 20. Baraja las filas de mini (por ejemplo, con sample()) y muestra por qué cbind(mini_barajado, ok_peso) es incorrecto si no reordenas ok_peso.
mini_barajado <- mini[sample.int(nrow(mini)), ]
malo <- cbind(mini_barajado, ok_peso)  # ok_peso sigue en orden original → MAL emparejado
head(malo, 10)
# Solución correcta: reordenar ok_peso con el mismo orden de mini_barajado
ok_reordenado <- ok_peso[order(match(penguins$id, mini_barajado$id))]
bien <- cbind(mini_barajado, ok_peso = ok_reordenado)

# 21. Hacer un inner join de penguins con tabla_especie por species
tabla_especie <- data.frame(
  species = c("Adelie", "Chinstrap", "Gentoo"),
  continente = c("Antártida", "Antártida", "Antártida"),
  stringsAsFactors = FALSE
)

inner_21 <- merge(penguins, tabla_especie, by = "species", all = FALSE)
dim(inner_21)

# 22. Repetir como left join (todas las filas de penguins) y como full join
left_22  <- merge(penguins, tabla_especie, by = "species", all.x = TRUE)
full_22  <- merge(penguins, tabla_especie, by = "species", all = TRUE)
dim(left_22)
dim(full_22)

# 23. Une penguins con tabla_medallas por id (inner y left)
set.seed(123)
ids_con_medalla <- sample(penguins$id, size = 200)
tabla_medallas <- data.frame(
  id = rep(ids_con_medalla, each = sample(1:2, 1)),   # 1 o 2 medallas por id
  medalla = sample(c("Oro","Plata","Bronce"), size = length(ids_con_medalla), replace = TRUE)
)

inner_23 <- merge(penguins, tabla_medallas, by = "id", all = FALSE)
left_23  <- merge(penguins, tabla_medallas, by = "id", all.x = TRUE)
dim(inner_23)
dim(left_23)

# 24. Observa el aumento de filas por duplicación (cartesiano cuando hay varias medallas por el mismo id)
# Explicación (2–3 líneas): Cuando la clave (id) aparece varias veces en tabla_medallas, cada coincidencia genera una fila distinta en el merge.

# Parte 3 — Familia apply

# 25. Usar apply sobre las columnas numéricas de penguins para calcular: mínimo, máximo, media, desviación estándar
num_cols <- sapply(penguins, is.numeric)
nums <- penguins[, num_cols]

res_25 <- rbind(
  min  = apply(nums, 2, min, na.rm = TRUE),
  max  = apply(nums, 2, max, na.rm = TRUE),
  mean = apply(nums, 2, mean, na.rm = TRUE),
  sd   = apply(nums, 2, sd,   na.rm = TRUE)
)
round(res_25, 3)

# 26. Estandarizar (z = (x - media)/desv) las columnas numéricas de penguins usando lapply
z_list <- lapply(penguins, function(x) {
  if (is.numeric(x)) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    (x - m) / s
  } else x
})
Z <- as.data.frame(z_list)
str(Z)

# 27. Usar sapply para obtener el número de valores faltantes (NA) por columna en penguins
na_por_col <- sapply(penguins, function(x) sum(is.na(x)))
na_por_col

# 28. Calcular, por especie, la media de body_mass_g y la mediana de flipper_length_mm usando: tapply, by
media_peso_tapply <- tapply(penguins$body_mass_g, penguins$species, mean, na.rm = TRUE)
mediana_aleta_tapply <- tapply(penguins$flipper_length_mm, penguins$species, median, na.rm = TRUE)

media_peso_by <- by(penguins$body_mass_g, penguins$species, function(v) mean(v, na.rm = TRUE))
mediana_aleta_by <- by(penguins$flipper_length_mm, penguins$species, function(v) median(v, na.rm = TRUE))

media_peso_tapply
mediana_aleta_tapply

# 29. Simula el lanzamiento de un dado justo (valores 1–6) 100 veces. Calcula la proporción de 6 obtenidos.
lanzos <- sample(1:6, size = 100, replace = TRUE)
prop_6 <- mean(lanzos == 6)
prop_6

# 30. Usar replicate para repetir el experimento anterior 1000 veces y guarda los resultados en un vector.
set.seed(123)
props_6 <- replicate(1000, {
  mean(sample(1:6, size = 100, replace = TRUE) == 6)
})
length(props_6)
head(props_6)

# 31. Grafica un histograma de las proporciones obtenidas en el ejercicio anterior. ¿Qué distribución reconoces?
hist(props_6, main = "Proporción de 6 en 100 lanzamientos (1000 repeticiones)",
     xlab = "Proporción de 6")
# Se reconoce una distribución aproximadamente Normal

# 32. Simula 1000 medias muestrales de tamaño 30 de una variable normal estándar usando replicate.
set.seed(123)
medias_norm <- replicate(1000, mean(rnorm(30, mean = 0, sd = 1)))
length(medias_norm)
head(medias_norm)

# 33. Calcula la media y desviación estándar de esas medias muestrales. Compáralas con los valores teóricos esperados.
emp_mean <- mean(medias_norm)
emp_sd   <- sd(medias_norm)

teo_mean <- 0
teo_sd   <- 1 / sqrt(30)

c(emp_mean = emp_mean, emp_sd = emp_sd,
  teo_mean = teo_mean, teo_sd = teo_sd)

3
