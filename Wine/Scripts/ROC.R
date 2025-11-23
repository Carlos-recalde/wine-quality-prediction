if(!require('pROC')) { install.packages('pROC'); library('pROC') }
if(!require('tidyverse')) { install.packages('tidyverse'); library('tidyverse') }
if (!exists("model_rf_v3")) { model_rf_v3 <- readRDS("data/models/model_rf_v3_master.rds") }
}



print("--- Calculando probabilidades de predicción... ---")
pred_prob <- predict(model_rf_v3, test_v3, type = "prob")

# --- 3. CALCULAR CURVAS ROC (Estrategia One-vs-All) ---
# Calculamos una curva para cada clase contra el resto

# ROC para Clase 'Basic'
roc_basic <- roc(response = (test_v3$quality_class == "Basic"), predictor = pred_prob$Basic)

# ROC para Clase 'Good'
roc_good <- roc(response = (test_v3$quality_class == "Good"), predictor = pred_prob$Good)

# ROC para Clase 'Premium'
roc_premium <- roc(response = (test_v3$quality_class == "Premium"), predictor = pred_prob$Premium)

# --- 4. MOSTRAR ÁREA BAJO LA CURVA (AUC) ---
# El AUC va de 0.5 (azar) a 1.0 (perfecto). >0.8 es excelente.
print(">>> RESULTADOS AUC (Área Bajo la Curva) <<<")
print(paste("AUC Basic:  ", round(auc(roc_basic), 4)))
print(paste("AUC Good:   ", round(auc(roc_good), 4)))
print(paste("AUC Premium:", round(auc(roc_premium), 4)))

# --- 5. GENERAR Y GUARDAR EL GRÁFICO ---
print("--- Generando gráfico comparativo ROC... ---")
dir.create("reports/figures/", showWarnings = FALSE, recursive = TRUE)

# Usamos ggroc para un gráfico elegante estilo ggplot
g_roc <- ggroc(list("Basic" = roc_basic, "Good" = roc_good, "Premium" = roc_premium), legacy.axes = TRUE) +
  
  # Estilo profesional
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed") + # Línea del azar
  scale_color_manual(values = c("Basic" = "#E69F00", "Good" = "#56B4E9", "Premium" = "#009E73")) +
  labs(
    title = "Curvas ROC Multiclase (One-vs-All)",
    subtitle = paste("Comparación de rendimiento por clase.",
                     "\nAUC Premium:", round(auc(roc_premium), 2), 
                     "| AUC Basic:", round(auc(roc_basic), 2)),
    x = "1 - Especificidad (Tasa de Falsos Positivos)",
    y = "Sensibilidad (Tasa de Verdaderos Positivos)",
    color = "Clase Objetivo"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Guardar
ggsave("reports/figures/curvas_roc_v3.png", plot = g_roc, width = 8, height = 6)
print("¡Gráfico ROC guardado en 'reports/figures/curvas_roc_v3.png'!")