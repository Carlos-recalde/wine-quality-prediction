

if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')}
if(!require('janitor')) {
  install.packages('janitor')
  library('janitor')
}

wine_red <- readRDS("data/processed/wine_red_processed.rds")
wine_white <- readRDS("data/processed/wine_white_processed.rds")

#  Añadir la variable 'wine_type', si es tinto "red" o blanco "white"

wine_red <- wine_red %>% 
  mutate(wine_type = "Red")

wine_white <- wine_white %>% 
  mutate(wine_type = "White")

#lo unificamos
wine_unified <- bind_rows(wine_red, wine_white)


#Convertir 'wine_type' a Factor 
wine_unified <- wine_unified %>%
  mutate(wine_type = factor(wine_type))


# --- Verificación Final ---
print("--- Estructura del Dataframe Unificado ---")
str(wine_unified)

print("\n--- Conteo por Tipo y Calidad (Tabla Cruzada) ---")
wine_unified %>%
  tabyl(wine_type, quality_class)



ruta_salida_csv_wine_unified <- "data/processed/wine_unified_processed.csv"
write_csv(wine_unified, file = ruta_salida_csv_wine_unified)

print("--- Frecuencia Absoluta y Relativa (Tipo de Vino) ---")
wine_unified %>%
  tabyl(wine_type) %>%
  adorn_pct_formatting(digits = 1)
#Evidencia el desequilibrio que existe entre los tipos de vinos




###Grafico



# --- LIBRERÍAS (Asegurarse de que tidyverse y scales estén cargados) ---
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}
# 'scales' es necesario para formatear las etiquetas de porcentaje
if(!require('scales')) { 
  install.packages('scales')
  library('scales')
}

# (Asumimos que 'wine_unified' ya existe en tu entorno)
# wine_unified <- readRDS("data/processed/wine_unified_processed.rds")

# --- PASO 1: Crear la carpeta de gráficos (si no existe) ---
dir.create("reports/figures/", showWarnings = FALSE, recursive = TRUE) 

# --- GRÁFICO 3: Gráfico de Barras del Desequilibrio de Tipos ---
# Precondición: 'wine_unified' existe.

grafico_desequilibrio_tipo <- ggplot(wine_unified, aes(x = wine_type, fill = wine_type)) +
  
  # 1. Dibuja las barras (el conteo en 'y' es automático)
  geom_bar(show.legend = FALSE) + # 'show.legend = FALSE' porque el eje X ya lo explica
  
  # 2. Añade las etiquetas de texto (Conteo y Porcentaje)
  geom_text(
    stat = 'count', 
    # 'after_stat(count)' es el conteo (ej. 1599)
    # 'scales::percent(...)' calcula el porcentaje
    aes(label = paste(after_stat(count), "\n", 
                      scales::percent(after_stat(count / sum(count)), accuracy = 0.1))),
    vjust = -0.5 # Ajuste vertical para ponerlo encima de la barra
  ) +
  
  # 3. Títulos y etiquetas
  labs(
    title = " Desequilibrio de Clases por Tipo de Vino",
    subtitle = "Dataset Unificado (N=6,497)",
    x = "Tipo de Vino",
    y = "Frecuencia Absoluta (Conteo)"
  ) +
  
  # 4. Ajustar los límites del eje Y para que el texto quepa
  ylim(0, 5500) + 
  
  # 5. Tema limpio
  theme_minimal()

# --- Guardar el gráfico en un archivo ---
ggsave("reports/figures/desequilibrio_tipo_vino.png", plot = grafico_desequilibrio_tipo, width = 7, height = 5)

print("¡Gráfico 3 (Desequilibrio) guardado en 'reports/figures/desequilibrio_tipo_vino.png'!")