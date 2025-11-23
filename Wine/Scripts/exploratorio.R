# --- LIBRERÍAS (Asegurarse de que tidyverse esté cargado) ---
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')}
if(!require('janitor')) {
  install.packages('janitor')
  library('janitor')
}

# --- Procesamiento Vino Tinto ---
wine_raw <- read.csv("data/raw/winequality-red.csv", sep = ";")
str(wine_raw)

# Creamos una copia para nuestras transformaciones.
wine_processed <- wine_raw

#En las variables remplazamos los puntos por guiones bajos
names(wine_processed) <- str_replace_all(names(wine_processed), "\\.", "_")
head(wine_processed, 3)


#Ahora usamos la variable quality para separar en tres grupos
# basic: vinos con una calificacion baja
# good: vinos con una calificacion normal
# premium: vinos de buena calidad

wine_processed <- wine_processed %>%
  mutate(
    quality_class = case_when(
      quality <= 5 ~ "Basic",
      quality == 6 ~ "Good",
      quality >= 7 ~ "Premium"
    )
  ) %>%
  
  #ahora convetimos la nueva variable quality_class
  # a un factor ordenado
  mutate(
    quality_class = factor(quality_class, 
                           levels = c("Basic", "Good", "Premium"), 
                           ordered = TRUE)
  )

#verificamos si todo funciono
print("--- Estructura final de 'wine_processed' ---")
str(wine_processed)

print("\n--- Conteo de las clases ---")
print(table(wine_processed$quality_class))


#como todo funciono, lo gaurdare
ruta_salida_rds_tinto <- "data/processed/wine_red_processed.rds" #guardo como rds
saveRDS(wine_processed, file = ruta_salida_rds_tinto)
print(paste("¡Éxito! Dataframe guardado como RDS en:", ruta_salida_rds_tinto))

#tambien lo guardo como CSV
ruta_salida_csv_tinto <- "data/processed/wine_red_processed.csv"
write_csv(wine_processed, file = ruta_salida_csv_tinto)
print(paste("¡Éxito! Dataframe guardado como CSV en:", ruta_salida_csv_tinto))


### ESO FUE EL VINO TINTO###


# --- Procesamiento Vino Blanco ---
print("\n--- Procesando Vino Blanco (White) ---")

wine_white_raw <- read.csv("data/raw/winequality-white.csv", sep = ";")
print("--- Estructura de 'wine_white_raw' (Original Blanco) ---")
str(wine_white_raw)

wine_white_processed <- wine_white_raw %>%
  
# Limpiar nombres: Reemplaza 'fixed.acidity' por 'fixed_acidity'
  set_names(str_replace_all(names(.), "\\.", "_")) %>%
  
  mutate(
    quality_class = case_when(
      quality <= 5 ~ "Basic",
      quality == 6 ~ "Good",
      quality >= 7 ~ "Premium"
    )
  ) %>%
  mutate(
    quality_class = factor(quality_class, 
                           levels = c("Basic", "Good", "Premium"), 
                           ordered = TRUE)
  )

print("--- Estructura final de 'wine_white_processed' ---")
str(wine_white_processed)

print("\n--- Conteo de las clases (Vino Blanco) ---")
print(table(wine_white_processed$quality_class))


# Guardar datos procesados (Blanco)
print("--- Guardando el dataframe de Vino Blanco ---")
dir.create("data/processed/", showWarnings = FALSE) # Asegurarse de que la carpeta exista

ruta_salida_rds_blanco <- "data/processed/wine_white_processed.rds"
saveRDS(wine_white_processed, file = ruta_salida_rds_blanco)
print(paste("¡Éxito! Dataframe blanco guardado como RDS en:", ruta_salida_rds_blanco))

ruta_salida_csv_blanco <- "data/processed/wine_white_processed.csv"
write_csv(wine_white_processed, file = ruta_salida_csv_blanco)
print(paste("¡Éxito! Dataframe blanco guardado como CSV en:", ruta_salida_csv_blanco))

#eso fue el vino blanco


# VERIFICACIÓN 

print("\n--- EDA: Frecuencia de Clases (Vino Tinto) ---")
wine_processed %>%
  tabyl(quality_class)

print("\n--- EDA: Frecuencia de Clases (Vino Blanco) ---")
wine_white_processed %>%
  tabyl(quality_class)

##############################
print("--- Resumen Estadístico de 'quality' (Vino Tinto) ---")
print(summary(wine_processed$quality))

print("\n--- Resumen Estadístico de 'quality' (Vino Blanco) ---")
print(summary(wine_white_processed$quality))

#los valores de quality van del 3 al 8 para el tinto y del 3 al 9 para el blanco
#con un promedio de:
# 5.636 para el tinto
# 5.878 para el blanco



print("--- Frecuencia Absoluta y Relativa (Vino Tinto) ---")
wine_raw %>%
  tabyl(quality) %>%
  adorn_pct_formatting(digits = 1)


# --- 2. FRECUENCIA (VINO BLANCO) ---
print("--- Frecuencia Absoluta y Relativa (Vino Blanco) ---")

wine_white_raw %>%
  tabyl(quality) %>%
  adorn_pct_formatting(digits = 1)


