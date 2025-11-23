
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}


# wine_unified <- readRDS("data/processed/wine_unified_processed.rds")



print("--- Resumen Estadístico Comparativo (Tinto vs. Blanco) ---")

summary_stats <- wine_unified %>%
  group_by(wine_type) %>%
  summarise(
    across(where(is.numeric), 
           list(
             mean = mean, 
             median = median, 
             sd = sd, 
             min = min, 
             max = max
           ),
           .names = "{.col}___{.fn}") # (ej. "alcohol___mean")
  )

# Reordenar la tabla (pivotar)
summary_table <- summary_stats %>%
  pivot_longer(
    cols = -wine_type, 
    names_to = "variable_stat", 
    values_to = "value"
  ) %>%

  separate(variable_stat, into = c("variable", "statistic"), sep = "___") %>%
  pivot_wider(
    names_from = statistic, 
    values_from = value
  )

# Imprimir la tabla de resumen final
print(summary_table, n = 30)





##############################################
summary_stats <- wine_unified %>%
  group_by(wine_type) %>%
  summarise(
    across(where(is.numeric), 
           list(
             mean = mean, 
             median = median, 
             sd = sd, 
             min = min, 
             max = max
           ),
           .names = "{.col}___{.fn}")
  )

# Re-pivotamos la tabla
summary_table <- summary_stats %>%
  pivot_longer(
    cols = -wine_type, 
    names_to = "variable_stat", 
    values_to = "value"
  ) %>%
  separate(variable_stat, into = c("variable", "statistic"), sep = "___") %>%
  pivot_wider(
    names_from = statistic, 
    values_from = value
  )


cv_analysis <- summary_table %>%
  mutate(
    # CV (%) = (Desv. Estándar / Media) * 100
    # Usamos abs(mean) por si la media fuera 0 
    cv_percent = round((sd / abs(mean)) * 100, 1) 
  ) %>%
  # Seleccionar solo las columnas que nos importan para este análisis
  select(wine_type, variable, mean, sd, cv_percent) %>%
  # Ordenar de mayor a menor variabilidad
  arrange(desc(cv_percent))


print(cv_analysis, n = 30) # n=30 para mostrar todas las filas




####### GRAFICO ##############
# --- LIBRERÍAS ---
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

# --- Cargar el Dataframe Unificado ---
# (Asegurarse de que 'wine_unified' esté en el entorno)
if (!exists("wine_unified")) {
  wine_unified <- readRDS("data/processed/wine_unified_processed.rds")
}

# --- Crear la carpeta de gráficos (si no existe) ---
dir.create("reports/figures/", showWarnings = FALSE, recursive = TRUE) 

# --- GRÁFICO 4.A: Boxplot de Alcohol ---
# Análisis: Fuerte predictor positivo.
# --------------------------------------------------
boxplot_alcohol <- ggplot(wine_unified, aes(x = quality_class, y = alcohol, fill = quality_class)) +
  geom_boxplot() +
  facet_wrap(~ wine_type) + # Separa "Red" y "White"
  labs(
    title = "Figura 4a: Relación entre Alcohol y Calidad del Vino",
    x = "Clase de Calidad (Basic, Good, Premium)",
    y = "Porcentaje de Alcohol"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar Gráfico 4.A
ggsave("reports/figures/boxplot_alcohol_vs_calidad.png", plot = boxplot_alcohol, width = 8, height = 5)
print("¡Gráfico 4.A (Alcohol) guardado en 'reports/figures/'!")


# --- GRÁFICO 4.B: Boxplot de Azúcar Residual ---
# Análisis: La variable más inestable (CV alto).
# --------------------------------------------------
boxplot_sugar <- ggplot(wine_unified, aes(x = quality_class, y = residual_sugar, fill = quality_class)) +
  geom_boxplot() +
  facet_wrap(~ wine_type, scales = "free_y") + # 'scales = "free_y"' es vital aquí
  labs(
    title = "Figura 4b: Relación entre Azúcar Residual y Calidad del Vino",
    x = "Clase de Calidad (Basic, Good, Premium)",
    y = "Azúcar Residual (g/dm^3)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar Gráfico 4.B
ggsave("reports/figures/boxplot_sugar_vs_calidad.png", plot = boxplot_sugar, width = 8, height = 5)
print("¡Gráfico 4.B (Azúcar) guardado en 'reports/figures/'!")


# --- GRÁFICO 4.C: Boxplot de Acidez Volátil ---
# Análisis: Fuerte predictor negativo, muy diferente entre tipos.
# --------------------------------------------------
boxplot_acidity <- ggplot(wine_unified, aes(x = quality_class, y = volatile_acidity, fill = quality_class)) +
  geom_boxplot() +
  facet_wrap(~ wine_type) +
  labs(
    title = "Figura 4c: Relación entre Acidez Volátil y Calidad del Vino",
    x = "Clase de Calidad (Basic, Good, Premium)",
    y = "Acidez Volátil (g/dm^3)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar Gráfico 4.C
ggsave("reports/figures/boxplot_acidity_vs_calidad.png", plot = boxplot_acidity, width = 8, height = 5)
print("¡Gráfico 4.C (Acidez Volátil) guardado en 'reports/figures/'!")








# --- LIBRERÍAS ---
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

# (Cargar dataset final si no está cargado)
if (!exists("wine_features")) {
  # Intentamos cargar el procesado final, si no existe, cargamos el unificado y transformamos
  if (file.exists("data/processed/wine_features_final.rds")) {
    wine_features <- readRDS("data/processed/wine_features_final.rds")
  } else {
    wine_unified <- readRDS("data/processed/wine_unified_processed.rds")
    vars_to_transform <- c("residual_sugar", "chlorides", "citric_acid", "free_sulfur_dioxide", "total_sulfur_dioxide", "sulphates", "volatile_acidity", "fixed_acidity")
    wine_features <- wine_unified %>% mutate(across(all_of(vars_to_transform), log1p, .names = "{.col}_log"))
  }
}

# --- Crear carpeta de figuras ---
dir.create("reports/figures/", showWarnings = FALSE, recursive = TRUE)


# --- GRÁFICO 4.B (FINAL): Azúcar Residual Transformada ---
# Solo mostramos la versión logarítmica, limpia y escalada.

plot_sugar_final <- ggplot(wine_features, aes(x = quality_class, y = residual_sugar_log, fill = quality_class)) +
  
  # Boxplot
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1) +
  
  # Separar Tinto y Blanco con escalas independientes (La mejora de escala clave)
  facet_wrap(~ wine_type, scales = "free_y") +
  
  # Títulos y Etiquetas
  labs(
    title = "Figura 4b: Distribución de Azúcar Residual (Transformada)",
    subtitle = "La escala logarítmica revela la distribución real sin la distorsión de outliers",
    x = "Clase de Calidad",
    y = "Log(Azúcar Residual)" 
  ) +
  
  # Estilo Profesional
  theme_minimal() +
  theme(legend.position = "none")

# Guardar
ggsave("reports/figures/boxplot_sugar_log_final.png", plot = plot_sugar_final, width = 9, height = 6)
print("¡Gráfico Final de Azúcar guardado!")


# --- GRÁFICO 4.C (FINAL): Acidez Volátil Transformada ---
# Solo versión logarítmica

plot_acidity_final <- ggplot(wine_features, aes(x = quality_class, y = volatile_acidity_log, fill = quality_class)) +
  
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1) +
  
  # Escalas independientes para ver mejor las diferencias en cada tipo
  facet_wrap(~ wine_type, scales = "free_y") +
  
  labs(
    title = "Figura 4c: Distribución de Acidez Volátil (Transformada)",
    subtitle = "Relación negativa clara: Menor acidez volátil indica mayor calidad",
    x = "Clase de Calidad",
    y = "Log(Acidez Volátil)"
  ) +
  
  theme_minimal() +
  theme(legend.position = "none")

# Guardar
ggsave("reports/figures/boxplot_acidity_log_final.png", plot = plot_acidity_final, width = 9, height = 6)
print("¡Gráfico Final de Acidez guardado!")