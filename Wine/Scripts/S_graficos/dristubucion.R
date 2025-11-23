# --- LIBRERÍAS (Asegurarse de que tidyverse esté cargado) ---
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

# (Asumimos que 'wine_unified' ya existe en tu entorno)
# wine_unified <- readRDS("data/processed/wine_unified_processed.rds")


# --- PASO 1: Crear la carpeta de destino ---
# Usamos 'recursive = TRUE' para que cree 'reports' y 'figures' si no existen.
dir.create("reports/figures/", showWarnings = FALSE, recursive = TRUE) 

# --- GRÁFICO 1: Histograma de la Calidad Original ---
# (El código de ggplot es el mismo de antes)
histograma_calidad <- ggplot(wine_unified, aes(x = quality)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  facet_wrap(~ wine_type, scales = "free") +
  labs(
    title = " Distribución de Calificaciones de Calidad Original",
    subtitle = "Comparación entre Vinos Tintos (N=1599) y Blancos (N=4898)",
    x = "Calificación de Calidad (3-9)",
    y = "Frecuencia (Cantidad de Vinos)"
  ) +
  theme_minimal()

# --- Guardar el Gráfico 1 en la nueva ruta ---
ggsave("reports/figures/histograma_calidad_original.png", plot = histograma_calidad, width = 8, height = 5)

print("¡Gráfico 1 (Histograma) guardado en 'reports/figures/'!")


# --- GRÁFICO 2: Gráfico de Barras de Clases (Desequilibrio) ---
# (El código de ggplot es el mismo de antes)
barras_clases <- ggplot(wine_unified, aes(x = quality_class, fill = quality_class)) +
  geom_bar() +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = -0.5) +
  facet_wrap(~ wine_type, scales = "free_y") +
  labs(
    title = " Distribución de Clases de Calidad Creadas",
    subtitle = "Comparación del Desequilibrio de Clases entre Vinos Tintos y Blancos",
    x = "Clase de Calidad (Basic, Good, Premium)",
    y = "Cantidad de Observaciones (Frecuencia)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

# --- Guardar el Gráfico 2 en la nueva ruta ---
ggsave("reports/figures/barras_distribucion_clases.png", plot = barras_clases, width = 8, height = 5)

print("¡Gráfico 2 (Barras) guardado en 'reports/figures/'!")