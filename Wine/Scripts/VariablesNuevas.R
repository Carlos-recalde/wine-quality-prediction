# --- LIBRERÍAS ---
if(!require('tidyverse')) { install.packages('tidyverse'); library('tidyverse') }

# --- 1. CARGAR DATASET UNIFICADO ---
if (file.exists("data/processed/wine_unified_processed.rds")) {
  wine_unified <- readRDS("data/processed/wine_unified_processed.rds")
  print("Dataset 'wine_unified' cargado correctamente.")
} else {
  stop("Error: No se encuentra 'wine_unified_processed.rds'.")
}

# --- 2. CREACIÓN DE VARIABLES AVANZADAS (GOLDEN FEATURES) ---
print("--- Creando nuevas variables (Ratios + Índice Global) ---")

wine_enhanced <- wine_unified %>%
  mutate(
    # Variable 1: Ratio Potencia vs. Defecto
    ratio_potencia_defecto = alcohol / (volatile_acidity + 0.01),
    
    # Variable 2: Ratio de Eficiencia del Azufre
    ratio_azufre_eficiente = free_sulfur_dioxide / (total_sulfur_dioxide + 0.01),
    
    # Variable 3: Índice de Estructura
    indice_estructura = alcohol * sulphates,
    
    # Variable 4: ÍNDICE DE CALIDAD GLOBAL 
    # Combina lo bueno (Alcohol * Sulfatos) dividido por lo malo (Acidez)
    indice_calidad_global = (alcohol * sulphates) / (volatile_acidity + 0.01),
    
    # Variable 5: Tipo de vino numérico (por si acaso)
    is_white = ifelse(wine_type == "White", 1, 0)
  )

# --- 3. VERIFICACIÓN ---
print("--- Nuevas variables creadas. Vista preliminar: ---")
wine_enhanced %>% 
  select(wine_type, quality_class, indice_calidad_global, ratio_potencia_defecto) %>%
  head() %>%
  print()

# --- 4. GUARDAR EL DATASET MEJORADO ---
saveRDS(wine_enhanced, "data/processed/wine_unified_enhanced.rds")
print("Dataset mejorado 'wine_unified_enhanced.rds' (con Índice Global) guardado.")