if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

if (!exists("wine_unified")) {
  wine_unified <- readRDS("data/processed/wine_unified_processed.rds")
}

#  si mintras más alcohol mejor la calidad
# por qué hay basicos con mucho alcohol? que variables le afecta

print("--- 1. Calculando la frontera de 'Alto Alcohol' (Q3) para cada tipo ---")

thresholds <- wine_unified %>%
  group_by(wine_type) %>%
  summarise(
    Q3_alcohol = quantile(alcohol, 0.75),
    .groups = 'drop'
  )

print(thresholds)

# --- 2. Comparación Dinámica ---
# Unimos el umbral al dataset y filtramos
# Cada vino se compara contra SU propio umbral 

comparison_custom <- wine_unified %>%
  left_join(thresholds, by = "wine_type") %>%
  
  # FILTRO PERSONALIZADO: Alcohol mayor al Q3 de SU tipo
  filter(alcohol > Q3_alcohol) %>%
  
  # Agrupamos para comparar
  group_by(wine_type, quality_class) %>%
  summarise(
    count = n(),
    avg_alcohol = mean(alcohol),
    
    # Sospechosos 
    avg_volatile_acidity = mean(volatile_acidity), # Vinagre
    avg_chlorides = mean(chlorides),               # Sal
    avg_residual_sugar = mean(residual_sugar),     # ¿Azúcar desequilibrada?
    avg_sulphates = mean(sulphates),               # Cuerpo
    avg_citric_acid = mean(citric_acid),           # Frescura
    avg_density = mean(density),                   # Cuerpo
    .groups = 'drop'
  ) %>%
  # Solo nos interesan los casos extremos para la comparación
  filter(quality_class %in% c("Basic", "Premium"))

print("\n--- Resultados: ¿Por qué fallan los vinos de alto alcohol? (Personalizado) ---")
print(comparison_custom)

# Imprimir todas las columnas de la comparación personalizada
print(comparison_custom, width = Inf)





# Objetivo: Calcular cuánto más (o menos) tienen los vinos 'Basic' respecto a los 'Premium'
difference_table <- comparison_custom %>%
  # 1. Seleccionamos solo las columnas de interés (tipo y promedios)
  select(wine_type, quality_class, starts_with("avg_")) %>%
  
  # 2. "Apilamos" las variables para poder procesarlas todas juntas
  pivot_longer(
    cols = starts_with("avg_"), 
    names_to = "variable", 
    values_to = "value"
  ) %>%
  
  # 3. Pivotamos a lo ancho para tener 'Basic' y 'Premium' como columnas comparables
  pivot_wider(
    names_from = quality_class, 
    values_from = value
  ) %>%
  
  # 4. Calculamos la diferencia porcentual: ((Basic - Premium) / Premium) * 100
  mutate(
    diferencia_porcentual = round(((Basic - Premium) / Premium) * 100, 1)
  ) %>%
  
  # 5. Ordenamos por tipo y magnitud de la diferencia (para ver los "culpables" arriba)
  arrange(wine_type, desc(abs(diferencia_porcentual)))

# --- Imprimir Resultados ---
print("\n--- Análisis de Brechas: Diferencia % de 'Basic' vs 'Premium' (con mismo alcohol) ---")
print("Nota: Un valor POSITIVO significa que el vino 'Basic' tiene MÁS de eso (ej. más defecto).")
print("Nota: Un valor NEGATIVO significa que el vino 'Basic' tiene MENOS (ej. le falta cuerpo).")
print(difference_table, n = Inf)