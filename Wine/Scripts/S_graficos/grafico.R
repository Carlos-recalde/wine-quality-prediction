# --- 1. CONFIGURACIÓN ---
if(!require('tidyverse')) { install.packages('tidyverse'); library('tidyverse') }

# Cargar datos (si no están en el entorno)
if (!exists("wine_unified")) {
  wine_unified <- readRDS("data/processed/wine_unified_processed.rds")
}

# Crear directorio si no existe
dir.create("reports/figures/", showWarnings = FALSE, recursive = TRUE)

# --- 2. GENERAR EL GRÁFICO FALTANTE ---

print("Generando gráfico de outliers de ácido cítrico (Blancos)...")

# Filtramos datos de vino blanco
w_white <- wine_unified %>% filter(wine_type == "White")

# Calculamos los límites de outliers para colorearlos
Q3_c <- quantile(w_white$citric_acid, 0.75)
IQR_c <- IQR(w_white$citric_acid)
limit_c <- Q3_c + 1.5 * IQR_c

# Creamos el gráfico
p6 <- w_white %>%
  mutate(flag = ifelse(citric_acid > limit_c, "Outlier", "Normal")) %>%
  ggplot(aes(x = pH, y = citric_acid, color = flag)) +
  geom_jitter(alpha = 0.6) +
  scale_color_manual(values = c("Normal" = "grey", "Outlier" = "orange")) +
  labs(
    title = "Investigación Outliers: Ácido Cítrico (Blanco)", 
    color = "Estado",
    x = "pH",
    y = "Ácido Cítrico (g/dm^3)"
  ) +
  theme_minimal()

# Guardamos la imagen con el nombre EXACTO que busca LaTeX
ggsave("reports/figures/investigacion_outliers_citric_white.png", plot = p6, width = 8, height = 5)

print("¡Listo! Imagen 'investigacion_outliers_citric_white.png' creada.")