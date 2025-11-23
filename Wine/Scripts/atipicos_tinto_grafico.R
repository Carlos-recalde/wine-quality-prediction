# --- LIBRERÍAS ---
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}
if (!exists("wine_unified")) {
  wine_unified <- readRDS("data/processed/wine_unified_processed.rds")
}

# --- PASO 7: INVESTIGACIÓN VISUAL DE OUTLIERS ---

# 1. Crear el dataframe de investigación con las "banderas" (flags)
#    (Nos enfocamos solo en el peor: residual_sugar en Tintos)
wine_investigation <- wine_unified %>%
  # Agrupamos por 'wine_type' para calcular los límites por separado
  group_by(wine_type) %>%
  mutate(
    # --- Calcular límites para residual_sugar ---
    Q1_sugar = quantile(residual_sugar, 0.25),
    Q3_sugar = quantile(residual_sugar, 0.75),
    IQR_sugar = Q3_sugar - Q1_sugar,
    upper_limit_sugar = Q3_sugar + (1.5 * IQR_sugar),
    
    # --- Crear la Bandera (Flag) ---
    # Marcamos solo los outliers de azúcar en vinos tintos
    flag_sugar_outlier = ifelse(
      wine_type == "Red" & residual_sugar > upper_limit_sugar, 
      "Outlier (9.7%)", 
      "Normal"
    )
  ) %>%
  ungroup() # Desagrupamos para poder graficar

print("--- Dataframe 'wine_investigation' con Banderas (Flags) creado ---")


# --- 2. Crear la carpeta de gráficos (si no existe) ---
dir.create("reports/figures/", showWarnings = FALSE, recursive = TRUE) 

# --- ¿Dónde viven los Outliers de Azúcar? ---
# Graficamos solo los Vinos Tintos
plot_investigacion <- wine_investigation %>%
  filter(wine_type == "Red") %>%
  
  # Graficamos Alcohol (X) vs. Acidez Volátil (Y)
  ggplot(aes(x = alcohol, y = volatile_acidity, color = flag_sugar_outlier)) +
  
  # Usamos geom_jitter() en lugar de geom_point() para evitar 
  # que los puntos se superpongan (es un scatterplot con "ruido")
  geom_jitter(alpha = 0.6) + # alpha = 0.6 para ver superposiciones
  scale_color_manual(values = c("Normal" = "grey", "Outlier (9.7%)" = "red")) +
  
  labs(
    title = "Investigación de Outliers (Vino Tinto)",
    subtitle = "Vinos con Azúcar Residual Atípica (en rojo) vs. Vinos Normales (en gris)",
    x = "Nivel de Alcohol (%)",
    y = "Acidez Volátil (g/dm^3)",
    color = "Grupo de Azúcar"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") # Poner la leyenda abajo

# --- Guardar el gráfico ---
ggsave("reports/figures/investigacion_outliers_azucar.png", plot = plot_investigacion, width = 9, height = 6)

print("¡Gráfico 6 (Investigación Outliers) guardado en 'reports/figures/'!")