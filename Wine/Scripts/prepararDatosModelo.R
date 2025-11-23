# --- LIBRERÍAS ---
if(!require('tidyverse')) { install.packages('tidyverse'); library('tidyverse') }
if(!require('caret')) { install.packages('caret'); library('caret') } # Para el split profesional

# --- 1. CARGAR DATASET FINAL ---
if (!exists("wine_features")) {
  wine_features <- readRDS("data/processed/wine_features_final.rds")
}

# --- 2. SELECCIÓN FINAL DE VARIABLES ---
# Nos quedamos solo con lo que el modelo va a usar.
# - VARIABLES QUE SÍ: wine_type, alcohol, density, pH, sulphates_log, etc.
# - VARIABLES QUE NO: quality (usamos quality_class), y las originales sin log.

datos_modelo <- wine_features %>%
  select(
    # 1. La Variable Objetivo
    quality_class,
    
    # 2. El Predictor Estructural
    wine_type,
    
    # 3. Las Variables Físicas (Que no transformamos porque eran estables)
    alcohol, 
    density, 
    pH,
    
    # 4. Las Variables Transformadas (Las versiones _log)
    residual_sugar_log,
    chlorides_log,
    citric_acid_log,
    free_sulfur_dioxide_log,
    total_sulfur_dioxide_log,
    sulphates_log,
    volatile_acidity_log,
    fixed_acidity_log
  )

print("--- Estructura de los Datos para el Modelo ---")
glimpse(datos_modelo)

# --- 3. DIVISIÓN ESTRATIFICADA (TRAIN / TEST) ---
set.seed(123) # Semilla para que sea reproducible

# Creamos una variable combinada para estratificar por AMBAS cosas:
# Queremos asegurar que en el Test haya proporciones iguales de "Tinto-Premium", "Blanco-Basic", etc.
datos_modelo$estrato <- paste(datos_modelo$wine_type, datos_modelo$quality_class)

# Índice de partición (80% Train, 20% Test)
index <- createDataPartition(datos_modelo$estrato, p = 0.8, list = FALSE)

train_set <- datos_modelo[index, ] %>% select(-estrato) # Quitamos la var auxiliar
test_set  <- datos_modelo[-index, ] %>% select(-estrato)

print(paste("Datos de Entrenamiento:", nrow(train_set)))
print(paste("Datos de Prueba:", nrow(test_set)))

# Verificación rápida de proporciones
print("--- Proporción en Entrenamiento ---")
prop.table(table(train_set$quality_class))
print("--- Proporción en Prueba (Debería ser casi idéntica) ---")
prop.table(table(test_set$quality_class))



















###########################################################################



# ENTRENAMINETO
# --- 5. CONFIGURACIÓN DEL ENTRENAMIENTO ---
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE
)

# Crear carpeta para guardar modelos
dir.create("data/models/", showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# MODELO A: RANDOM FOREST (RF)
# ==============================================================================

# Verificamos si el modelo ya existe guardado
if (file.exists("data/models/model_rf.rds")) {
  print("Cargando modelo Random Forest existente...")
  model_rf <- readRDS("data/models/model_rf.rds")
} else {
  print("--- Entrenando Random Forest (puede tardar un momento)... ---")
  set.seed(123)
  model_rf <- train(
    quality_class ~ ., 
    data = train_set,
    method = "rf",
    trControl = fitControl,
    ntree = 100,
    importance = TRUE
  )
  # Guardamos el modelo recién entrenado
  saveRDS(model_rf, "data/models/model_rf.rds")
  print("Modelo Random Forest guardado en 'data/models/model_rf.rds'")
}

print(model_rf)

# ==============================================================================
# MODELO B: SUPPORT VECTOR MACHINE (SVM Radial)
# ==============================================================================

if (file.exists("data/models/model_svm.rds")) {
  print("Cargando modelo SVM existente...")
  model_svm <- readRDS("data/models/model_svm.rds")
} else {
  print("--- Entrenando SVM (Kernel Radial)... ---")
  set.seed(123)
  model_svm <- train(
    quality_class ~ ., 
    data = train_set,
    method = "svmRadial",
    trControl = fitControl,
    preProcess = c("center", "scale") 
  )
  saveRDS(model_svm, "data/models/model_svm.rds")
  print("Modelo SVM guardado en 'data/models/model_svm.rds'")
}

print(model_svm)




############################################
#varios nucleos



# --- LIBRERÍAS DE PARALELIZACIÓN ---
if(!require('doParallel')) {
  install.packages('doParallel')
  library('doParallel')
}

# 1. Configurar Procesamiento en Paralelo
# Detectamos cuántos núcleos tiene tu PC y usamos todos menos 1 (para que no se congele la PC)
cores <- detectCores() - 1
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

print(paste("--- Iniciando entrenamiento SVM usando", cores, "núcleos ---"))

# 2. Configuración con 'verboseIter' (Para ver el progreso)
fitControl_verbose <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  verboseIter = TRUE,  # ¡ESTO ES CLAVE! Te mostrará texto mientras entrena
  allowParallel = TRUE # Permitir uso de múltiples núcleos
)

# 3. ENTRENAMIENTO SVM (Optimizado)
if (file.exists("data/models/model_svm.rds")) {
  print("Cargando modelo SVM existente...")
  model_svm <- readRDS("data/models/model_svm.rds")
} else {
  print("--- Entrenando SVM (Kernel Radial)... Por favor espera... ---")
  
  set.seed(123)
  model_svm <- train(
    quality_class ~ ., 
    data = train_set,
    method = "svmRadial",
    trControl = fitControl_verbose, # Usamos el control con verbose
    preProcess = c("center", "scale") 
  )
  
  # Guardamos el modelo
  saveRDS(model_svm, "data/models/model_svm.rds")
  print("Modelo SVM guardado exitosamente.")
}

# 4. Detener el cluster (Importante para liberar memoria)
stopCluster(cl)
registerDoSEQ() # Volver a modo secuencial

# 5. VER RESULTADO
print(model_svm)








###############################################
# RF re hacer
# --- LIBRERÍAS NECESARIAS ---
if(!require('caret')) { install.packages('caret'); library('caret') }
if(!require('randomForest')) { install.packages('randomForest'); library('randomForest') }
# Librería para usar múltiples núcleos
if(!require('doParallel')) { install.packages('doParallel'); library('doParallel') }

# --- 0. CONFIGURAR PARALELIZACIÓN (EL TURBO) ---
# Detectar núcleos reales de tu PC y dejar 1 libre para el sistema operativo
cores <- detectCores() - 1 
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

print(paste("--- Iniciando entrenamiento usando", cores, "núcleos en paralelo ---"))


# --- 1. ESTRATEGIA DE CONTROL Y BALANCEO ---
fitControl_tuned <- trainControl(
  method = "cv",
  number = 10,             # 10-Fold Cross Validation
  search = "grid",         # Búsqueda en rejilla
  classProbs = TRUE,       # Calcular probabilidades
  sampling = "up",         # Balanceo por Upsampling
  verboseIter = TRUE,      # Mostrar progreso
  allowParallel = TRUE     # <--- Habilitar explícitamente el paralelo
)

# --- 2. GRID DE HIPERPARÁMETROS ---
grid_rf <- expand.grid(
  mtry = c(2, 3, 4, 6, 8) 
)

# --- 3. ENTRENAMIENTO AVANZADO ---
print("--- Entrenando Random Forest Optimizado (Tuning + Balanceo)... ---")
set.seed(123)

# Cronometramos el tiempo para que veas la mejora
tiempo_inicio <- Sys.time()

model_rf_tuned <- train(
  quality_class ~ ., 
  data = train_set,
  method = "rf",
  metric = "Kappa",        # Optimizamos Kappa
  trControl = fitControl_tuned,
  tuneGrid = grid_rf,      
  ntree = 500,             # 500 árboles
  importance = TRUE
)

tiempo_fin <- Sys.time()
print(paste("Tiempo de entrenamiento:", round(difftime(tiempo_fin, tiempo_inicio, units = "mins"), 2), "minutos"))

# --- 4. IMPORTANTE: CERRAR EL CLUSTER ---
# Si no haces esto, los núcleos se quedan "ocupados" y tu PC puede seguir lenta
stopCluster(cl)
registerDoSEQ() # Volver a modo secuencial


# --- 5. VER RESULTADOS DEL TUNING ---
print("--- Mejor configuración encontrada: ---")
print(model_rf_tuned$bestTune)
print(model_rf_tuned)

# --- 6. EVALUACIÓN FINAL ---
print("--- Evaluando en Test Set ---")
pred_rf_tuned <- predict(model_rf_tuned, test_set)

# Matriz de Confusión
conf_mat_tuned <- confusionMatrix(pred_rf_tuned, test_set$quality_class)
print(conf_mat_tuned)

# Comparación de métricas finales
print(paste("Accuracy Nuevo:", round(conf_mat_tuned$overall['Accuracy'], 4)))
print(paste("Kappa Nuevo:", round(conf_mat_tuned$overall['Kappa'], 4)))



########################

# --- ANALISIS DE ERRORES POR TIPO ---

# 1. Unimos las predicciones con los datos de prueba originales
# (Para saber qué vino era tinto y cuál blanco)
test_results <- test_set %>%
  mutate(
    Prediccion = pred_rf_tuned,
    Acierto = quality_class == Prediccion
  )

# 2. Matriz de Confusión para VINOS TINTOS
print(">>> RESULTADOS: SOLO VINOS TINTOS <<<")
red_results <- test_results %>% filter(wine_type == "Red")
conf_mat_red <- confusionMatrix(red_results$Prediccion, red_results$quality_class)
print(conf_mat_red)

# 3. Matriz de Confusión para VINOS BLANCOS
print(">>> RESULTADOS: SOLO VINOS BLANCOS <<<")
white_results <- test_results %>% filter(wine_type == "White")
conf_mat_white <- confusionMatrix(white_results$Prediccion, white_results$quality_class)
print(conf_mat_white)