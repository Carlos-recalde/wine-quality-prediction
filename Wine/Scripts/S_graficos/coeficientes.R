
if(!require('tidyverse')) { install.packages('tidyverse'); library('tidyverse') }
if(!require('corrplot')) { install.packages('corrplot'); library('corrplot') }
if (!exists("wine_unified")) {
  wine_unified <- readRDS("data/processed/wine_unified_processed.rds")
}




# --- CORRELACIONES SEPARADAS ---
print("--- Generando Matrices de Correlación Separadas (Tinto vs. Blanco) ---")
# queiro ver las relaciones de lso tintos y de los blancos por separado


# Función auxiliar para graficar y guardar
crear_corrplot <- function(data, tipo, filename) {
  
  # 1. Seleccionar solo numéricas y filtrar por tipo
  df_cor <- data %>%
    filter(wine_type == tipo) %>%
    select(where(is.numeric))
  
  # 2. Calcular correlación
  M <- cor(df_cor, method = "pearson")
  
  # 3. Guardar imagen
  png(filename = filename, width = 800, height = 800, res = 100)
  
  corrplot(M, 
           method = "color", 
           type = "upper", 
           order = "hclust", 
           addCoef.col = "black", 
           number.cex = 0.7, 
           tl.col = "black", 
           diag = FALSE,
           title = paste("Matriz de Correlación - Vino", tipo),
           mar = c(0,0,2,0)) # Ajustar márgenes para el título
  
  dev.off()
  print(paste("Guardado:", filename))
}

# Generar para Tinto (Red)
crear_corrplot(wine_unified, "Red", "reports/figures/matriz_correlacion_red.png")

# Generar para Blanco (White)
crear_corrplot(wine_unified, "White", "reports/figures/matriz_correlacion_white.png")