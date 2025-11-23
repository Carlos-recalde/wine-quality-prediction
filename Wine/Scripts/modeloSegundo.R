# ==============================================================================
# ETAPA 5-B: ENTRENAMIENTO DEL MODELO V2 (OPTIMIZADO)
# ==============================================================================

# --- 1. LIBRERÍAS ---
if(!require('tidyverse')) { install.packages('tidyverse'); library('tidyverse') }
if(!require('caret')) { install.packages('caret'); library('caret') }
if(!require('randomForest')) { install.packages('randomForest'); library('randomForest') }
if(!require('doParallel')) { install.packages('doParallel'); library('doParallel') }

# --- 2. CARGAR DATASET MEJORADO (EL QUE YA CREASTE) ---
if (file.exists("data/processed/wine_unified_enhanced.rds")) {
  wine_enhanced <- readRDS("data/processed/wine_unified_enhanced.rds")
  print("Dataset con variables nuevas cargado correctamente.")
} else {
  stop("Error: No se encuentra 'wine_unified_enhanced.rds'. Ejecuta tu script de creación de variables primero.")
}

# --- 3. APLICAR TRANSFORMACIONES LOGARÍTMICAS ---
# (Esto falta aplicarlo a este dataset específico)
print("--- Aplicando Logs y Preparando Dataset Maestro ---")

wine_master <- wine_enhanced %>%
  mutate(
    residual_sugar_log = log1p(residual_sugar),
    chlorides_log = log1p(chlorides),
    citric_acid_log = log1p(citric_acid),
    volatile_acidity_log = log1p(volatile_acidity),
    sulphates_log = log1p(sulphates),
    fixed_acidity_log = log1p(fixed_acidity),
    free_sulfur_dioxide_log = log1p(free_sulfur_dioxide),
    total_sulfur_dioxide_log = log1p(total_sulfur_dioxide)
  ) %>%
  # SELECCIÓN FINAL: Nos quedamos con lo mejor de todo
  select(
    quality_class,      # Target
    wine_type,          # Estructural
    alcohol, density, pH, # Físicas estables (sin log)
    ends_with("_log"),  # Las transformadas (logs)
    starts_with("ratio_"), # Tus nuevas variables
    indice_estructura   # Tu nueva variable
  )

print("Dataset V2 listo para entrenar.")

# --- 4. DIVISIÓN TRAIN/TEST ---
set.seed(123)
index <- createDataPartition(wine_master$quality_class, p = 0.8, list = FALSE)
train_set_v2 <- wine_master[index, ]
test_set_v2  <- wine_master[-index, ]

# --- 5. ENTRENAMIENTO (Mismo proceso que antes) ---
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

fitControl <- trainControl(
  method = "cv", number = 10, sampling = "up", 
  classProbs = TRUE, allowParallel = TRUE, verboseIter = TRUE
)

print("--- Entrenando Random Forest V2... ---")
set.seed(123)
model_rf_v2 <- train(
  quality_class ~ ., data = train_set_v2, method = "rf",
  trControl = fitControl, ntree = 500, importance = TRUE
)

stopCluster(cl)
registerDoSEQ()

# --- 6. EVALUACIÓN ---
# Guardar y evaluar
saveRDS(model_rf_v2, "data/models/model_rf_v2_enhanced.rds")

pred_v2 <- predict(model_rf_v2, test_set_v2)
print(">>> MATRIZ DE CONFUSIÓN (MODELO V2) <<<")
print(confusionMatrix(pred_v2, test_set_v2$quality_class))

# Ver Importancia (¿Sirvieron tus variables?)
plot(varImp(model_rf_v2), top = 15, main = "Importancia de Variables (V2)")








##################################
# --- LIBRERÍAS ---
if(!require('tidyverse')) { install.packages('tidyverse'); library('tidyverse') }
if(!require('caret')) { install.packages('caret'); library('caret') }

# (Asegúrate de tener 'test_set_v2' y 'model_rf_v2' en memoria)
# Si no, cárgalos con:
# model_rf_v2 <- readRDS("data/models/model_rf_v2_enhanced.rds")
# (Y tendrías que regenerar el test_set_v2 con el script de split)

print("--- EVALUACIÓN COMPARATIVA: TINTO VS BLANCO ---")

# 1. Añadir predicciones al set de prueba original
# Esto nos permite filtrar por 'wine_type' después
test_results_v2 <- test_set_v2 %>%
  mutate(
    Prediccion = predict(model_rf_v2, test_set_v2)
  )

# 2. Evaluación para VINO TINTO
print(">>> RESULTADOS: SOLO VINO TINTO <<<")
results_red <- test_results_v2 %>% filter(wine_type == "Red")
cm_red <- confusionMatrix(results_red$Prediccion, results_red$quality_class)
print(cm_red)

# 3. Evaluación para VINO BLANCO
print(">>> RESULTADOS: SOLO VINO BLANCO <<<")
results_white <- test_results_v2 %>% filter(wine_type == "White")
cm_white <- confusionMatrix(results_white$Prediccion, results_white$quality_class)
print(cm_white)

# 4. Comparación Directa de Accuracy
acc_red <- cm_red$overall['Accuracy']
acc_white <- cm_white$overall['Accuracy']

print(paste("Exactitud (Tinto):", round(acc_red * 100, 2), "%"))
print(paste("Exactitud (Blanco):", round(acc_white * 100, 2), "%"))