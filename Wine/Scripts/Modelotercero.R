# ==============================================================================
# ETAPA 5-C: ENTRENAMIENTO DEL MODELO V3 (VARIABLE MAESTRA) + AUDITORÍA
# ==============================================================================

# --- 1. LIBRERÍAS ---
if(!require('tidyverse')) { install.packages('tidyverse'); library('tidyverse') }
if(!require('caret')) { install.packages('caret'); library('caret') }
if(!require('randomForest')) { install.packages('randomForest'); library('randomForest') }
if(!require('doParallel')) { install.packages('doParallel'); library('doParallel') }
if(!require('scales')) { install.packages('scales'); library('scales') }

# --- 2. CARGAR DATASET YA MEJORADO ---
if (file.exists("data/processed/wine_unified_enhanced.rds")) {
  wine_enhanced <- readRDS("data/processed/wine_unified_enhanced.rds")
  print("Dataset 'wine_unified_enhanced.rds' cargado. Variables listas.")
} else {
  stop("Error: No se encuentra el archivo. Ejecuta el script de creación de variables primero.")
}

# --- 3. PREPARACIÓN FINAL (Transformación Log) ---
print("--- Aplicando Logs y Seleccionando Variables para V3 ---")

wine_master_v3 <- wine_enhanced %>%
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
  select(
    quality_class,          # Objetivo
    wine_type,              # Estructural (Vital para la auditoría posterior)
    indice_calidad_global,  # Variable Maestra
    ratio_potencia_defecto, # Otra variable fuerte
    alcohol, density, pH,   # Físicas estables
    ends_with("_log")       # Transformadas
  )

# --- 4. DIVISIÓN TRAIN/TEST ---
set.seed(123)
index <- createDataPartition(wine_master_v3$quality_class, p = 0.8, list = FALSE)
train_v3 <- wine_master_v3[index, ]
test_v3  <- wine_master_v3[-index, ]

# --- 5. ENTRENAMIENTO TURBO (Random Forest) ---
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

fitControl <- trainControl(
  method = "cv", number = 10, sampling = "up", 
  classProbs = TRUE, allowParallel = TRUE, verboseIter = TRUE
)

print("--- Entrenando Modelo V3 (Variable Maestra + Logs)... ---")
set.seed(123)
model_rf_v3 <- train(
  quality_class ~ ., 
  data = train_v3, 
  method = "rf",
  trControl = fitControl, 
  ntree = 500, 
  importance = TRUE
)

stopCluster(cl)
registerDoSEQ()

# Guardar modelo
dir.create("data/models/", showWarnings = FALSE)
saveRDS(model_rf_v3, "data/models/model_rf_v3_master.rds")


# ==============================================================================
# PASO 6: AUDITORÍA COMPARATIVA (TINTO VS. BLANCO)
# ==============================================================================
print("--- INICIANDO AUDITORÍA COMPARATIVA ---")

# 1. Generar predicciones y unir con datos originales (para saber el tipo)
test_results <- test_v3 %>%
  mutate(
    Prediccion = predict(model_rf_v3, test_v3)
  )

# 2. Función para extraer métricas clave por tipo
get_metrics <- function(data, tipo) {
  cm <- confusionMatrix(data$Prediccion, data$quality_class)
  acc <- cm$overall['Accuracy']
  sens_basic <- cm$byClass['Class: Basic', 'Sensitivity']
  sens_good <- cm$byClass['Class: Good', 'Sensitivity']
  sens_premium <- cm$byClass['Class: Premium', 'Sensitivity']
  
  return(data.frame(
    Tipo = tipo,
    Metric = c("Accuracy Global", "Sensibilidad Basic", "Sensibilidad Good", "Sensibilidad Premium"),
    Value = c(acc, sens_basic, sens_good, sens_premium)
  ))
}

# 3. Calcular métricas para cada grupo
metrics_red <- get_metrics(test_results %>% filter(wine_type == "Red"), "Tinto")
metrics_white <- get_metrics(test_results %>% filter(wine_type == "White"), "Blanco")

# 4. Unir y Mostrar Tabla
comparison_results <- bind_rows(metrics_red, metrics_white)
print("--- Tabla Comparativa de Rendimiento ---")
print(comparison_results)

# 5. VISUALIZACIÓN DE LA AUDITORÍA
dir.create("reports/figures/", showWarnings = FALSE, recursive = TRUE)

plot_audit <- ggplot(comparison_results, aes(x = Metric, y = Value, fill = Tipo)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = scales::percent(Value, accuracy = 0.1)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) + # 1.1 para espacio arriba
  scale_fill_manual(values = c("Tinto" = "#800020", "Blanco" = "#F0E68C")) + 
  labs(
    title = "Auditoría de Modelo V3: Tinto vs. Blanco",
    subtitle = "Comparación de Exactitud y Sensibilidad por Clase",
    x = "", y = "Puntuación"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("reports/figures/auditoria_tinto_vs_blanco_v3.png", plot = plot_audit, width = 9, height = 6)
print("¡Gráfico de auditoría guardado en 'reports/figures/auditoria_tinto_vs_blanco_v3.png'!")

# --- 7. IMPORTANCIA DE VARIABLES ---
print("--- Ranking de Importancia ---")
imp <- varImp(model_rf_v3)
print(head(imp$importance, 10))
png("reports/figures/importancia_variables_v3.png", width = 800, height = 600, res = 100)
plot(imp, top = 15, main = "Importancia: ¿Ganó el Índice Global?")




















######################################
# ==============================================================================
# AUDITORÍA FINAL: RENDIMIENTO POR TIPO DE VINO
# ==============================================================================

print("--- Generando Matrices de Confusión Separadas ---")

# 1. Añadir las predicciones al dataset de prueba original
#    (Esto nos permite filtrar después por 'wine_type')
test_audit <- test_v3 %>%
  mutate(
    Prediccion = predict(model_rf_v3, test_v3)
  )

# --- MATRIZ 1: Vinos Tintos (Red) ---
print(">>> MATRIZ DE CONFUSIÓN: VINOS TINTOS <<<")
red_data <- test_audit %>% filter(wine_type == "Red")
conf_mat_red <- confusionMatrix(red_data$Prediccion, red_data$quality_class)
print(conf_mat_red)

# --- MATRIZ 2: Vinos Blancos (White) ---
print(">>> MATRIZ DE CONFUSIÓN: VINOS BLANCOS <<<")
white_data <- test_audit %>% filter(wine_type == "White")
conf_mat_white <- confusionMatrix(white_data$Prediccion, white_data$quality_class)
print(conf_mat_white)

# --- RESUMEN COMPARATIVO RÁPIDO ---
acc_red <- round(conf_mat_red$overall['Accuracy'] * 100, 2)
acc_white <- round(conf_mat_white$overall['Accuracy'] * 100, 2)

print(paste0("Exactitud Tintos: ", acc_red, "%"))
print(paste0("Exactitud Blancos: ", acc_white, "%"))
