
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}


if (!exists("wine_unified")) {
  wine_unified <- readRDS("data/processed/wine_unified_processed.rds")
}


print("--- Conteo y Porcentaje de Outliers (Método 1.5*IQR) ---")

# 1. Contamos los outliers para cada variable, agrupados por tipo
outlier_counts <- wine_unified %>%
  group_by(wine_type) %>%
  summarise(
    # Aplicamos la función 'boxplot.stats' a cada columna numérica
    across(where(is.numeric), ~ length(boxplot.stats(.)$out)),
    .groups = "drop"
  ) %>%
  # Convertimos a formato largo para poder unir y comparar
  pivot_longer(
    cols = -wine_type, 
    names_to = "variable", 
    values_to = "outlier_count"
  )

# Obtenemos el conteo total (N) para cada tipo
total_counts <- wine_unified %>%
  group_by(wine_type) %>%
  summarise(N = n()) # Red=1599, White=4898

#Unimos las dos tablas para calcular el porcentaje
outlier_analysis <- outlier_counts %>%
  left_join(total_counts, by = "wine_type") %>%
  mutate(
    # el porcentaje
    outlier_percent = round((outlier_count / N) * 100, 1)
  ) %>%
  #ordenamos para la presentación
  select(wine_type, variable, outlier_count, N, outlier_percent) %>%
  arrange(desc(outlier_percent)) # Ordenar por el peor

# el análisis final
print(outlier_analysis, n = 30)





###########################################################
# --- LIBRERÍAS ---
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}
if(!require('corrplot')) {
  install.packages('corrplot')
  library('corrplot')
}

if (!exists("wine_unified")) {
  wine_unified <- readRDS("data/processed/wine_unified_processed.rds")
}

######################################3

# 1. Preparar los datos:
# Para la correlación, solo necesitamos las variables numéricas.
# Excluimos nuestras variables 'factor' (quality_class y wine_type).
df_numeric <- wine_unified %>%
  select(where(is.numeric))

# 2. Calcular la Matriz de Correlación
# Usamos el método "pearson"
M <- cor(df_numeric, method = "pearson")

# 3. Crear la carpeta de gráficos (si no existe) ---
dir.create("reports/figures/", showWarnings = FALSE, recursive = TRUE) 

# 4. Generar y Guardar el Gráfico (Corrplot)
png(filename = "reports/figures/matriz_correlacion.png", width = 10, height = 8, units = "in", res = 300)

corrplot(
  M,
  method = "color",       # Usa color para representar el valor
  type = "upper",         # Muestra solo la diagonal superior (es simétrica)
  order = "hclust",       # Agrupa variables que tienen correlación similar
  addCoef.col = "black",  # Añade el número (coeficiente) a cada celda
  number.cex = 0.7,       # Tamaño del número
  tl.col = "black",       # Color del texto de las etiquetas
  tl.srt = 45,            # Rota las etiquetas 45 grados para que quepan
  diag = FALSE            # No muestra la diagonal (siempre es 1)
)

dev.off() # Cierra el archivo PNG

print("¡Gráfico 5 (Matriz de Correlación) guardado en 'reports/figures/matriz_correlacion.png'!")