# ================================================================
# MODELO RANDOM FOREST SEMANAL (PRECIO + CLIMA) - PAPA SUPER CHOLA
# ================================================================

# Paquetes necesarios.
paquetes <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "lubridate",
  "zoo",
  "Metrics",
  "randomForest",
  "imputeTS",
  "ggplot2",
  "scales",
  "writexl",
  "rstudioapi"
)

# Instalar únicamente los paquetes faltantes.
faltantes <- paquetes[!vapply(
  paquetes,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)]

if (length(faltantes) > 0L) {
  install.packages(faltantes, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(zoo)
  library(Metrics)
  library(randomForest)
  library(imputeTS)
  library(ggplot2)
  library(scales)
  library(writexl)
})

options(stringsAsFactors = FALSE)


# =========================================================
# 1) Carga de datos históricos
# =========================================================

# Leer el Excel ubicado junto al archivo .R.
full_precio <- read_excel(
  "Papa_03-01-2022_al_30-06-2026.xlsx"
)

full_climaH <- read_excel(
  "Clima_03-01-2022_al_30-06-2026.xlsx"
)

# =========================================================
# 2) Limpieza de los datos históricos del precio
# =========================================================

# Conservar fecha y precio.
full_precio <- full_precio[, c(
  "Fecha Investigación",
  "Precio/Presentación (USD)"
)]

colnames(full_precio) <- c("Fecha", "Precio_Quintal")

# Convertir formatos, consolidar fechas repetidas y ordenar.
precio <- full_precio %>%
  dplyr::mutate(
    Fecha = as.Date(Fecha),
    Precio_Quintal = as.numeric(Precio_Quintal)
  ) %>%
  dplyr::filter(
    !is.na(Fecha),
    !is.na(Precio_Quintal)
  ) %>%
  dplyr::group_by(Fecha) %>%
  dplyr::summarise(
    Precio_Quintal = mean(Precio_Quintal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Fecha)

# Verificar la base definitiva.
stopifnot(
  nrow(precio) == 628L,
  min(precio$Fecha) == as.Date("2022-01-12"),
  max(precio$Fecha) == as.Date("2026-06-17"),
  !anyNA(precio$Precio_Quintal)
)

# ================================================================
# 3) Limpieza de los datos climáticos
# ================================================================

# Conservar las tres variables climáticas empleadas en el Rmd.
full_climaH <- full_climaH[, c(
  "date",
  "tavg",
  "prcp",
  "wspd"
)]

colnames(full_climaH) <- c(
  "Fecha",
  "Tpromedio",
  "Precipitacion",
  "Viento"
)

# Convertir formatos y consolidar fechas repetidas.
#
# Se conserva el rango original completo del clima, tal como ocurre en el
# Rmd. Esto es importante para Random Forest porque los datos de los días
# anteriores al primer precio completan la primera semana climática y entran
# en los acumulados de 12 y 24 semanas. El ajuste al rango semanal del precio
# se realiza solo después de la agregación.
clima <- full_climaH %>%
  dplyr::mutate(
    Fecha = as.Date(Fecha),
    Tpromedio = as.numeric(Tpromedio),
    Precipitacion = as.numeric(Precipitacion),
    Viento = as.numeric(Viento)
  ) %>%
  dplyr::filter(!is.na(Fecha)) %>%
  dplyr::group_by(Fecha) %>%
  dplyr::summarise(
    Tpromedio = mean(Tpromedio, na.rm = TRUE),
    Precipitacion = sum(Precipitacion, na.rm = TRUE),
    Viento = mean(Viento, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    Fecha = seq.Date(
      min(precio$Fecha),
      max(precio$Fecha),
      by = "day"
    )
  ) %>%
  dplyr::arrange(Fecha)

# Registrar los faltantes antes de aplicar la imputación del Rmd.
faltantes_clima_antes <- data.frame(
  Variable = c("Tpromedio", "Precipitacion", "Viento"),
  Faltantes_Antes = c(
    sum(is.na(clima$Tpromedio)),
    sum(is.na(clima$Precipitacion)),
    sum(is.na(clima$Viento))
  )
)

# Imputación causal mediante filtro de Kalman, igual que en los
# scripts con clima ya validados.
clima$Tpromedio <- imputeTS::na_kalman(
  clima$Tpromedio,
  smooth = FALSE
)

clima$Precipitacion <- imputeTS::na_kalman(
  clima$Precipitacion,
  smooth = FALSE
)

clima$Viento <- imputeTS::na_kalman(
  clima$Viento,
  smooth = FALSE
)

stopifnot(
  min(clima$Fecha) <= min(precio$Fecha),
  max(clima$Fecha) >= max(precio$Fecha),
  !anyNA(clima[, c(
    "Tpromedio",
    "Precipitacion",
    "Viento"
  )])
)

# ================================================================
# 4) Agregación semanal
# ================================================================

# Promediar únicamente los precios observados.
precio_semanal <- precio %>%
  dplyr::mutate(
    Fecha_Semana = lubridate::floor_date(
      Fecha,
      unit = "week",
      week_start = 1
    )
  ) %>%
  dplyr::group_by(Fecha_Semana) %>%
  dplyr::summarise(
    Precio_Quintal = mean(Precio_Quintal, na.rm = TRUE),
    N_Observaciones = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::rename(Fecha = Fecha_Semana) %>%
  tidyr::complete(
    Fecha = seq.Date(min(Fecha), max(Fecha), by = "week"),
    fill = list(N_Observaciones = 0L)
  ) %>%
  dplyr::mutate(
    Semana_Observada = N_Observaciones > 0L
  ) %>%
  dplyr::arrange(Fecha)

fechas_faltantes <- precio_semanal %>%
  dplyr::filter(!Semana_Observada) %>%
  dplyr::pull(Fecha)

# Comprobar la estructura semanal.
stopifnot(
  nrow(precio_semanal) == 232L,
  sum(precio_semanal$Semana_Observada) == 230L,
  identical(
    fechas_faltantes,
    as.Date(c("2022-06-20", "2026-06-08"))
  )
)

# ================================================================
# 5) División temporal e imputación causal
# ================================================================

# Reservar las primeras 24 semanas, igual que en el Rmd.
ventana_maxima <- 24L
n_semanas_modelables <- nrow(precio_semanal) - ventana_maxima
n_train_modelable <- floor(0.8 * n_semanas_modelables)
indice_corte <- ventana_maxima + n_train_modelable

fecha_corte <- precio_semanal$Fecha[indice_corte]
fecha_inicio_test <- precio_semanal$Fecha[indice_corte + 1L]

stopifnot(
  fecha_corte == as.Date("2025-08-25"),
  fecha_inicio_test == as.Date("2025-09-01")
)

# Dividir antes de imputar para evitar fuga de información.
precio_train_raw <- precio_semanal %>%
  dplyr::filter(Fecha <= fecha_corte)

precio_test_raw <- precio_semanal %>%
  dplyr::filter(Fecha >= fecha_inicio_test)

# Localizar el faltante de entrenamiento.
semana_faltante_train <- precio_train_raw %>%
  dplyr::filter(!Semana_Observada)

stopifnot(nrow(semana_faltante_train) == 1L)

fecha_faltante_train <- semana_faltante_train$Fecha[[1L]]

# Usar solo semanas anteriores para imputar.
serie_previa_train <- precio_train_raw %>%
  dplyr::filter(
    Fecha < fecha_faltante_train,
    Semana_Observada
  ) %>%
  dplyr::arrange(Fecha) %>%
  dplyr::pull(Precio_Quintal)

modelo_imputacion_train <- stats::StructTS(
  stats::ts(serie_previa_train, frequency = 1),
  type = "level"
)

pronostico_imputacion_train <- stats::predict(
  modelo_imputacion_train,
  n.ahead = 1
)

valor_imputado_train <- as.numeric(
  pronostico_imputacion_train$pred[1]
)

error_estandar_train <- as.numeric(
  pronostico_imputacion_train$se[1]
)

# Completar solo el faltante de entrenamiento.
precio_train <- precio_train_raw %>%
  dplyr::mutate(
    Precio_Imputado = !Semana_Observada,
    Precio_Quintal = dplyr::if_else(
      Precio_Imputado,
      valor_imputado_train,
      Precio_Quintal
    ),
    Conjunto = "Entrenamiento"
  )

# Reconstruir la serie completa.
precio_modelado <- dplyr::bind_rows(
  precio_train,
  precio_test_raw %>%
    dplyr::mutate(
      Precio_Imputado = FALSE,
      Conjunto = "Prueba"
    )
) %>%
  dplyr::arrange(Fecha) %>%
  dplyr::mutate(
    Precio_Observado = dplyr::if_else(
      Semana_Observada,
      Precio_Quintal,
      NA_real_
    )
  ) %>%
  dplyr::select(
    Fecha,
    Precio_Quintal,
    Precio_Observado,
    N_Observaciones,
    Semana_Observada,
    Precio_Imputado,
    Conjunto
  )

# Localizar el faltante de prueba.
semana_faltante_test <- precio_modelado %>%
  dplyr::filter(
    Conjunto == "Prueba",
    !Semana_Observada
  )

stopifnot(nrow(semana_faltante_test) == 1L)

fecha_faltante_test <- semana_faltante_test$Fecha[[1L]]

# Calcular el valor de apoyo con datos anteriores.
datos_previos_test <- precio_modelado %>%
  dplyr::filter(Fecha < fecha_faltante_test) %>%
  dplyr::arrange(Fecha)

serie_previa_test <- datos_previos_test$Precio_Quintal

stopifnot(!anyNA(serie_previa_test))

modelo_apoyo_test <- stats::StructTS(
  stats::ts(serie_previa_test, frequency = 1),
  type = "level"
)

pronostico_apoyo_test <- stats::predict(
  modelo_apoyo_test,
  n.ahead = 1
)

valor_apoyo_test <- as.numeric(
  pronostico_apoyo_test$pred[1]
)

error_estandar_test <- as.numeric(
  pronostico_apoyo_test$se[1]
)

# Marcar el apoyo y las semanas evaluables.
precio_modelado <- precio_modelado %>%
  dplyr::mutate(
    Precio_Apoyo =
      Conjunto == "Prueba" & !Semana_Observada,
    Precio_Para_Rezagos = dplyr::if_else(
      Precio_Apoyo,
      valor_apoyo_test,
      Precio_Quintal
    ),
    Evaluar_Test =
      Conjunto == "Prueba" & Semana_Observada
  )

# ================================================================
# 6) Ingeniería de variables y base de Random Forest con clima
# ================================================================

# Agregar primero todo el rango climático a frecuencia semanal. La
# temperatura y el viento se promedian; la precipitación se acumula mediante
# suma. Solo después se seleccionan las semanas existentes en la serie de
# precio, reproduciendo el efecto del left_join utilizado en el Rmd.
clima_semanal_completo <- clima %>%
  dplyr::mutate(
    Fecha_Semana = lubridate::floor_date(
      Fecha,
      unit = "week",
      week_start = 1
    )
  ) %>%
  dplyr::group_by(Fecha_Semana) %>%
  dplyr::summarise(
    Tpromedio = mean(Tpromedio, na.rm = TRUE),
    Precipitacion = sum(Precipitacion, na.rm = TRUE),
    Viento = mean(Viento, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::rename(Fecha = Fecha_Semana) %>%
  dplyr::arrange(Fecha)

clima_semanal <- clima_semanal_completo %>%
  dplyr::filter(
    Fecha >= min(precio_semanal$Fecha),
    Fecha <= max(precio_semanal$Fecha)
  ) %>%
  dplyr::arrange(Fecha)

stopifnot(
  nrow(clima_semanal) == 232L,
  min(clima_semanal$Fecha) == min(precio_semanal$Fecha),
  max(clima_semanal$Fecha) == max(precio_semanal$Fecha),
  !anyNA(clima_semanal[, c(
    "Tpromedio",
    "Precipitacion",
    "Viento"
  )])
)

# Función única para reconstruir las variables históricas de precio
# y clima empleadas por Random Forest.
# Se usa tanto en la evaluación como en el pronóstico futuro.
crear_features_rf_clima <- function(data_hist) {
  data_hist %>%
    dplyr::arrange(Fecha) %>%
    dplyr::mutate(
      # Variables históricas del precio.
      lag1 = dplyr::lag(Precio_Para_Rezagos, 1L),
      lag2 = dplyr::lag(Precio_Para_Rezagos, 2L),
      lag3 = dplyr::lag(Precio_Para_Rezagos, 3L),
      lag4 = dplyr::lag(Precio_Para_Rezagos, 4L),
      lag8 = dplyr::lag(Precio_Para_Rezagos, 8L),
      lag12 = dplyr::lag(Precio_Para_Rezagos, 12L),
      lag16 = dplyr::lag(Precio_Para_Rezagos, 16L),
      lag24 = dplyr::lag(Precio_Para_Rezagos, 24L),
      diff1 = lag1 - lag2,
      diff4 = lag1 - dplyr::lag(Precio_Para_Rezagos, 5L),
      roll_mean_4 = dplyr::lag(
        zoo::rollmean(
          Precio_Para_Rezagos,
          k = 4,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      roll_sd_4 = dplyr::lag(
        zoo::rollapply(
          Precio_Para_Rezagos,
          width = 4,
          FUN = stats::sd,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      roll_min_4 = dplyr::lag(
        zoo::rollapply(
          Precio_Para_Rezagos,
          width = 4,
          FUN = min,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      roll_max_4 = dplyr::lag(
        zoo::rollapply(
          Precio_Para_Rezagos,
          width = 4,
          FUN = max,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      roll_mean_8 = dplyr::lag(
        zoo::rollmean(
          Precio_Para_Rezagos,
          k = 8,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      roll_sd_8 = dplyr::lag(
        zoo::rollapply(
          Precio_Para_Rezagos,
          width = 8,
          FUN = stats::sd,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      
      # Variables climáticas del bloque Random Forest del Rmd.
      # Todas se construyen con información previa a la semana objetivo.
      temp_lag1 = dplyr::lag(Tpromedio, 1L),
      precip_lag1 = dplyr::lag(Precipitacion, 1L),
      viento_lag1 = dplyr::lag(Viento, 1L),
      precip_cum12 = dplyr::lag(
        zoo::rollsum(
          Precipitacion,
          k = 12,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      precip_cum24 = dplyr::lag(
        zoo::rollsum(
          Precipitacion,
          k = 24,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      viento_roll12 = dplyr::lag(
        zoo::rollmean(
          Viento,
          k = 12,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      viento_roll24 = dplyr::lag(
        zoo::rollmean(
          Viento,
          k = 24,
          fill = NA_real_,
          align = "right"
        ),
        1L
      ),
      
      # Variables de calendario.
      month = lubridate::month(Fecha),
      yday = lubridate::yday(Fecha)
    )
}

variables_rf_precio <- c(
  "lag1", "lag2", "lag3", "lag4",
  "lag8", "lag12", "lag16", "lag24",
  "diff1", "diff4",
  "roll_mean_4", "roll_sd_4",
  "roll_min_4", "roll_max_4",
  "roll_mean_8", "roll_sd_8",
  "month", "yday"
)

variables_rf_clima <- c(
  variables_rf_precio,
  "temp_lag1",
  "precip_lag1",
  "viento_lag1",
  "precip_cum12",
  "precip_cum24",
  "viento_roll12",
  "viento_roll24"
)

base_rf_clima <- precio_modelado %>%
  dplyr::left_join(clima_semanal, by = "Fecha") %>%
  dplyr::mutate(
    Precio_Objetivo = Precio_Observado
  ) %>%
  crear_features_rf_clima()

# Conservar las 208 semanas modelables.
filas_modelables <- seq.int(
  ventana_maxima + 1L,
  nrow(base_rf_clima)
)

full_data_rf_clima <- base_rf_clima %>%
  dplyr::slice(filas_modelables)

train_data <- full_data_rf_clima %>%
  dplyr::filter(Conjunto == "Entrenamiento") %>%
  dplyr::arrange(Fecha)

test_data <- full_data_rf_clima %>%
  dplyr::filter(Conjunto == "Prueba") %>%
  dplyr::arrange(Fecha)

# Controles del protocolo y de las variables explicativas.
stopifnot(
  length(variables_rf_precio) == 18L,
  length(variables_rf_clima) == 25L,
  nrow(full_data_rf_clima) == 208L,
  nrow(train_data) == 166L,
  nrow(test_data) == 42L,
  sum(test_data$Evaluar_Test) == 41L,
  sum(is.na(test_data$Precio_Objetivo)) == 1L,
  !anyNA(train_data[, variables_rf_clima]),
  !anyNA(test_data[, variables_rf_clima]),
  max(train_data$Fecha) < min(test_data$Fecha)
)

# ================================================================
# 7) Funciones de métricas
# ================================================================

calc_mape <- function(real, pred) {
  mean(
    abs((real - pred) / real),
    na.rm = TRUE
  ) * 100
}

calc_smape <- function(real, pred) {
  mean(
    200 * abs(real - pred) /
      (abs(real) + abs(pred)),
    na.rm = TRUE
  )
}

calc_r2 <- function(real, pred) {
  casos_validos <- is.finite(real) & is.finite(pred)
  real_valido <- real[casos_validos]
  pred_valido <- pred[casos_validos]
  
  if (length(real_valido) < 2L) {
    return(NA_real_)
  }
  
  sst <- sum((real_valido - mean(real_valido))^2)
  
  if (sst == 0) {
    return(NA_real_)
  }
  
  sse <- sum((real_valido - pred_valido)^2)
  1 - sse / sst
}

calc_metrics <- function(real, pred, nombre_modelo) {
  casos_validos <- is.finite(real) & is.finite(pred)
  real_valido <- real[casos_validos]
  pred_valido <- pred[casos_validos]
  
  data.frame(
    Modelo = nombre_modelo,
    MAE = Metrics::mae(real_valido, pred_valido),
    RMSE = Metrics::rmse(real_valido, pred_valido),
    MAPE = calc_mape(real_valido, pred_valido),
    sMAPE = calc_smape(real_valido, pred_valido),
    R2 = calc_r2(real_valido, pred_valido),
    N_Evaluadas = length(real_valido)
  )
}

# ================================================================
# 8) Grid Search temporal de Random Forest
# ================================================================

# Configuración literal del Rmd:
# - 300 árboles
# - mtry igual a un tercio o la mitad de las variables
# - nodesize igual a 1 o 5
# - tres bloques temporales de validación
ntree_rf <- 300L
n_folds_rf <- 3L

grid_rf <- expand.grid(
  mtry = c(
    floor(length(variables_rf_clima) / 3),
    floor(length(variables_rf_clima) / 2)
  ),
  nodesize = c(1L, 5L),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

tune_rf_rolling <- function(train_df, features, grid, ntree) {
  n_train <- nrow(train_df)
  fold_size <- floor(n_train / 4)
  resultados <- vector("list", nrow(grid))
  
  best_smape <- Inf
  best_index <- NA_integer_
  
  for (k in seq_len(nrow(grid))) {
    params <- grid[k, , drop = FALSE]
    resultados_folds <- vector("list", n_folds_rf)
    
    for (fold in seq_len(n_folds_rf)) {
      idx_subtrain <- seq_len(fold * fold_size)
      idx_valid <- seq.int(
        fold * fold_size + 1L,
        (fold + 1L) * fold_size
      )
      
      if (max(idx_valid) > n_train) {
        next
      }
      
      subtrain <- train_df[idx_subtrain, , drop = FALSE]
      valid <- train_df[idx_valid, , drop = FALSE]
      
      df_train_rf <- stats::na.omit(
        subtrain[, c("Precio_Objetivo", features)]
      )
      
      if (nrow(df_train_rf) < 10L) {
        next
      }
      
      formula_rf <- stats::as.formula(
        paste(
          "Precio_Objetivo ~",
          paste(features, collapse = " + ")
        )
      )
      
      set.seed(123)
      
      fit <- randomForest::randomForest(
        formula_rf,
        data = df_train_rf,
        ntree = ntree,
        mtry = as.integer(params$mtry),
        nodesize = as.integer(params$nodesize)
      )
      
      preds_valid <- as.numeric(
        stats::predict(fit, newdata = valid)
      )
      
      resultados_folds[[fold]] <- data.frame(
        Configuracion = k,
        Fold = fold,
        N_Subtrain = nrow(df_train_rf),
        N_Validacion = nrow(valid),
        mtry = as.integer(params$mtry),
        nodesize = as.integer(params$nodesize),
        MAE = Metrics::mae(valid$Precio_Objetivo, preds_valid),
        RMSE = Metrics::rmse(valid$Precio_Objetivo, preds_valid),
        MAPE = calc_mape(valid$Precio_Objetivo, preds_valid),
        sMAPE = calc_smape(valid$Precio_Objetivo, preds_valid),
        R2 = calc_r2(valid$Precio_Objetivo, preds_valid)
      )
    }
    
    detalle_k <- dplyr::bind_rows(resultados_folds)
    
    if (nrow(detalle_k) == 0L) {
      next
    }
    
    resultados[[k]] <- detalle_k %>%
      dplyr::summarise(
        Configuracion = k,
        mtry = dplyr::first(mtry),
        nodesize = dplyr::first(nodesize),
        N_Folds = dplyr::n(),
        MAE = mean(MAE, na.rm = TRUE),
        RMSE = mean(RMSE, na.rm = TRUE),
        MAPE = mean(MAPE, na.rm = TRUE),
        sMAPE = mean(sMAPE, na.rm = TRUE),
        R2 = mean(R2, na.rm = TRUE)
      )
    
    smape_k <- resultados[[k]]$sMAPE[[1L]]
    
    # El Rmd selecciona la primera configuración con menor sMAPE.
    if (is.finite(smape_k) && smape_k < best_smape) {
      best_smape <- smape_k
      best_index <- k
    }
  }
  
  resultados_grid <- dplyr::bind_rows(resultados)
  
  if (is.na(best_index)) {
    best_params <- data.frame(
      mtry = floor(length(features) / 3),
      nodesize = 5L
    )
  } else {
    best_params <- grid[best_index, c("mtry", "nodesize"), drop = FALSE]
  }
  
  resultados_grid <- resultados_grid %>%
    dplyr::mutate(
      Seleccionado =
        mtry == best_params$mtry[[1L]] &
        nodesize == best_params$nodesize[[1L]]
    ) %>%
    dplyr::arrange(sMAPE, RMSE, MAE)
  
  list(
    best_params = best_params,
    resultados_grid = resultados_grid
  )
}

resultado_tuning_rf <- tune_rf_rolling(
  train_df = train_data,
  features = variables_rf_clima,
  grid = grid_rf,
  ntree = ntree_rf
)

best_rf_clima <- resultado_tuning_rf$best_params
resultados_grid_rf <- resultado_tuning_rf$resultados_grid

hiperparametros_rf <- data.frame(
  Hiperparametro = c(
    "Número de árboles",
    "Variables candidatas por división",
    "Tamaño mínimo de nodo terminal",
    "Criterio de selección",
    "Bloques de validación temporal"
  ),
  Parametro_R = c(
    "ntree",
    "mtry",
    "nodesize",
    "sMAPE",
    "n_folds"
  ),
  Valor = c(
    as.character(ntree_rf),
    as.character(best_rf_clima$mtry[[1L]]),
    as.character(best_rf_clima$nodesize[[1L]]),
    "Mínimo sMAPE promedio",
    as.character(n_folds_rf)
  )
)

# ================================================================
# 9) Rolling forecast Random Forest de un paso
# ================================================================

rolling_forecast_rf <- function(
    train_df,
    test_df,
    features,
    best_params,
    ntree
) {
  history_df <- train_df %>%
    dplyr::filter(!is.na(Precio_Objetivo)) %>%
    dplyr::arrange(Fecha)
  
  test_df <- test_df %>%
    dplyr::arrange(Fecha)
  
  preds <- rep(NA_real_, nrow(test_df))
  diagnostico <- vector("list", nrow(test_df))
  
  formula_rf <- stats::as.formula(
    paste(
      "Precio_Objetivo ~",
      paste(features, collapse = " + ")
    )
  )
  
  for (i in seq_len(nrow(test_df))) {
    df_train_rf <- stats::na.omit(
      history_df[, c("Precio_Objetivo", features)]
    )
    
    set.seed(123 + i)
    
    fit <- randomForest::randomForest(
      formula_rf,
      data = df_train_rf,
      ntree = ntree,
      mtry = as.integer(best_params$mtry[[1L]]),
      nodesize = as.integer(best_params$nodesize[[1L]])
    )
    
    preds[i] <- as.numeric(
      stats::predict(
        fit,
        newdata = test_df[i, , drop = FALSE]
      )
    )[[1L]]
    
    diagnostico[[i]] <- data.frame(
      Fecha = test_df$Fecha[i],
      Iteracion = i,
      N_Entrenamiento = nrow(df_train_rf),
      OOB_MSE_Final = as.numeric(fit$mse[ntree]),
      mtry = as.integer(best_params$mtry[[1L]]),
      nodesize = as.integer(best_params$nodesize[[1L]]),
      ntree = ntree
    )
    
    # La observación real se incorpora para la siguiente iteración.
    history_df <- dplyr::bind_rows(
      history_df,
      test_df[i, ]
    )
  }
  
  list(
    predicciones = preds,
    diagnostico = dplyr::bind_rows(diagnostico)
  )
}

resultado_rolling_rf <- rolling_forecast_rf(
  train_df = train_data,
  test_df = test_data,
  features = variables_rf_clima,
  best_params = best_rf_clima,
  ntree = ntree_rf
)

pred_rf_clima <- resultado_rolling_rf$predicciones
diagnostico_rolling_rf <- resultado_rolling_rf$diagnostico

stopifnot(
  length(pred_rf_clima) == 42L,
  all(is.finite(pred_rf_clima))
)

# ================================================================
# 10) Predicciones y métricas de prueba
# ================================================================

y_real_test <- test_data$Precio_Objetivo

metricas_rf <- calc_metrics(
  real = y_real_test,
  pred = pred_rf_clima,
  nombre_modelo = "Random Forest semanal (Precio + clima)"
)

predicciones_test <- test_data %>%
  dplyr::transmute(
    Fecha,
    Conjunto,
    Semana_Observada,
    Evaluar_Test,
    Precio_Real = Precio_Objetivo,
    Precio_Predicho = pred_rf_clima,
    temp_lag1,
    precip_lag1,
    viento_lag1,
    precip_cum12,
    precip_cum24,
    viento_roll12,
    viento_roll24,
    Error = dplyr::if_else(
      Evaluar_Test,
      Precio_Real - Precio_Predicho,
      NA_real_
    ),
    Error_Absoluto = abs(Error),
    Error_Cuadratico = Error^2,
    Error_Porcentual = dplyr::if_else(
      Evaluar_Test,
      abs(Error / Precio_Real) * 100,
      NA_real_
    )
  )

# Valores publicados en la Tabla 12 del PDF más reciente.
metricas_documento <- data.frame(
  Metrica = c("MAE", "RMSE", "MAPE", "sMAPE", "R2"),
  Valor_Documento = c(1.69, 2.34, 8.36, 8.29, 0.17)
)

# Comparar al mismo nivel de redondeo del documento.
verificacion_documento <- metricas_rf %>%
  dplyr::select(MAE, RMSE, MAPE, sMAPE, R2) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "Metrica",
    values_to = "Valor_Calculado"
  ) %>%
  dplyr::left_join(metricas_documento, by = "Metrica") %>%
  dplyr::mutate(
    Valor_Calculado_2d = round(Valor_Calculado, 2),
    Coincide_Documento =
      Valor_Calculado_2d == Valor_Documento
  ) %>%
  dplyr::select(
    Metrica,
    Valor_Calculado,
    Valor_Calculado_2d,
    Valor_Documento,
    Coincide_Documento
  )

if (!all(verificacion_documento$Coincide_Documento)) {
  warning(
    paste0(
      "Las métricas calculadas no coinciden completamente con la Tabla 12 ",
      "al redondear a dos decimales. Revise la base y las versiones de paquetes."
    )
  )
}

# ================================================================
# 11) Modelo final, importancia y pronóstico futuro
# ================================================================

datos_modelo_final_rf <- full_data_rf_clima %>%
  dplyr::filter(!is.na(Precio_Objetivo)) %>%
  dplyr::arrange(Fecha)

stopifnot(
  nrow(datos_modelo_final_rf) == 207L,
  !anyNA(datos_modelo_final_rf[, variables_rf_clima]),
  !anyNA(datos_modelo_final_rf$Precio_Objetivo)
)

formula_rf <- stats::as.formula(
  paste(
    "Precio_Objetivo ~",
    paste(variables_rf_clima, collapse = " + ")
  )
)

set.seed(123)

fit_rf_final <- randomForest::randomForest(
  formula_rf,
  data = datos_modelo_final_rf[
    ,
    c("Precio_Objetivo", variables_rf_clima)
  ],
  ntree = ntree_rf,
  mtry = as.integer(best_rf_clima$mtry[[1L]]),
  nodesize = as.integer(best_rf_clima$nodesize[[1L]]),
  importance = TRUE
)

# Importancia por incremento del MSE y pureza de nodos.
importancia_mse <- randomForest::importance(
  fit_rf_final,
  type = 1,
  scale = TRUE
)

importancia_pureza <- randomForest::importance(
  fit_rf_final,
  type = 2,
  scale = FALSE
)

importancia_variables <- data.frame(
  Variable = rownames(importancia_mse),
  IncMSE = as.numeric(importancia_mse[, 1L]),
  IncNodePurity = as.numeric(importancia_pureza[, 1L]),
  stringsAsFactors = FALSE
) %>%
  dplyr::arrange(dplyr::desc(IncMSE)) %>%
  dplyr::mutate(Posicion = dplyr::row_number()) %>%
  dplyr::select(
    Posicion,
    Variable,
    IncMSE,
    IncNodePurity
  )

# Horizonte recursivo de 12 semanas.
horizonte_futuro <- 12L

fechas_futuras <- seq.Date(
  from = max(precio_modelado$Fecha) + 7L,
  by = "week",
  length.out = horizonte_futuro
)

# Preparar el clima histórico semanal y un respaldo promedio por
# semana del año. Este es el mismo criterio utilizado en los otros
# scripts con clima del proyecto.
clima_historico_forecast <- clima_semanal %>%
  dplyr::arrange(Fecha) %>%
  dplyr::mutate(
    Semana_Anio = lubridate::isoweek(Fecha),
    Anio = lubridate::year(Fecha)
  )

clima_promedio_semana <- clima_historico_forecast %>%
  dplyr::group_by(Semana_Anio) %>%
  dplyr::summarise(
    Tpromedio_Futuro = mean(Tpromedio, na.rm = TRUE),
    Precipitacion_Futura = mean(Precipitacion, na.rm = TRUE),
    Viento_Futuro = mean(Viento, na.rm = TRUE),
    .groups = "drop"
  )

obtener_clima_futuro <- function(fecha_futura) {
  semana_futura <- lubridate::isoweek(fecha_futura)
  anio_referencia <- lubridate::year(fecha_futura) - 1L
  
  clima_referencia <- clima_historico_forecast %>%
    dplyr::filter(
      Semana_Anio == semana_futura,
      Anio == anio_referencia
    ) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::transmute(
      Tpromedio_Futuro = Tpromedio,
      Precipitacion_Futura = Precipitacion,
      Viento_Futuro = Viento
    )
  
  referencia_valida <-
    nrow(clima_referencia) == 1L &&
    all(is.finite(unlist(
      clima_referencia,
      use.names = FALSE
    )))
  
  if (referencia_valida) {
    return(clima_referencia %>%
             dplyr::mutate(
               Fuente_Clima = "Misma semana del año anterior"
             ))
  }
  
  clima_respaldo <- clima_promedio_semana %>%
    dplyr::filter(Semana_Anio == semana_futura) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::select(
      Tpromedio_Futuro,
      Precipitacion_Futura,
      Viento_Futuro
    )
  
  respaldo_valido <-
    nrow(clima_respaldo) == 1L &&
    all(is.finite(unlist(
      clima_respaldo,
      use.names = FALSE
    )))
  
  if (!respaldo_valido) {
    stop(
      paste(
        "No fue posible estimar el clima para",
        as.character(fecha_futura)
      )
    )
  }
  
  clima_respaldo %>%
    dplyr::mutate(
      Fuente_Clima = "Promedio histórico de la semana"
    )
}

clima_futuro <- dplyr::bind_rows(lapply(
  seq_along(fechas_futuras),
  function(i) {
    fecha_i <- fechas_futuras[[i]]
    
    obtener_clima_futuro(fecha_i) %>%
      dplyr::mutate(
        Fecha = fecha_i,
        Semana_Anio = lubridate::isoweek(fecha_i),
        .before = 1
      )
  }
))

stopifnot(
  nrow(clima_futuro) == horizonte_futuro,
  !anyNA(clima_futuro[, c(
    "Tpromedio_Futuro",
    "Precipitacion_Futura",
    "Viento_Futuro"
  )])
)

# Unir clima histórico y estimado para alimentar de forma recursiva
# los rezagos y las ventanas climáticas del modelo.
clima_extendido <- dplyr::bind_rows(
  clima_semanal %>%
    dplyr::transmute(
      Fecha,
      Tpromedio,
      Precipitacion,
      Viento,
      Fuente_Clima = "Histórico observado"
    ),
  clima_futuro %>%
    dplyr::transmute(
      Fecha,
      Tpromedio = Tpromedio_Futuro,
      Precipitacion = Precipitacion_Futura,
      Viento = Viento_Futuro,
      Fuente_Clima
    )
) %>%
  dplyr::arrange(Fecha)

hist_forecast_rf <- precio_modelado %>%
  dplyr::arrange(Fecha) %>%
  dplyr::transmute(
    Fecha,
    Precio_Para_Rezagos
  ) %>%
  dplyr::left_join(
    clima_semanal,
    by = "Fecha"
  ) %>%
  dplyr::arrange(Fecha)

stopifnot(
  nrow(hist_forecast_rf) == 232L,
  !anyNA(hist_forecast_rf[, c(
    "Precio_Para_Rezagos",
    "Tpromedio",
    "Precipitacion",
    "Viento"
  )]),
  max(hist_forecast_rf$Fecha) == as.Date("2026-06-15")
)

# Comprobar que la reconstrucción reproduce la última fila del modelo.
X_comprobacion_rf <- crear_features_rf_clima(
  hist_forecast_rf
) %>%
  dplyr::slice_tail(n = 1L) %>%
  dplyr::select(dplyr::all_of(variables_rf_clima))

X_referencia_rf <- full_data_rf_clima %>%
  dplyr::slice_tail(n = 1L) %>%
  dplyr::select(dplyr::all_of(variables_rf_clima))

stopifnot(
  isTRUE(all.equal(
    as.numeric(unlist(
      X_comprobacion_rf[1L, ],
      use.names = FALSE
    )),
    as.numeric(unlist(
      X_referencia_rf[1L, ],
      use.names = FALSE
    )),
    tolerance = 1e-10,
    check.attributes = FALSE
  ))
)

resultados_futuros_lista <- vector(
  mode = "list",
  length = horizonte_futuro
)

for (i in seq_len(horizonte_futuro)) {
  next_date <- max(hist_forecast_rf$Fecha) + 7L
  
  clima_i <- clima_extendido %>%
    dplyr::filter(Fecha == next_date) %>%
    dplyr::slice_head(n = 1L)
  
  stopifnot(
    nrow(clima_i) == 1L,
    all(is.finite(unlist(
      clima_i[, c(
        "Tpromedio",
        "Precipitacion",
        "Viento"
      )],
      use.names = FALSE
    )))
  )
  
  # Incorporar temporalmente la nueva semana con su clima estimado,
  # pero aún sin precio.
  hist_forecast_rf <- dplyr::bind_rows(
    hist_forecast_rf,
    data.frame(
      Fecha = as.Date(next_date),
      Precio_Para_Rezagos = NA_real_,
      Tpromedio = clima_i$Tpromedio[[1L]],
      Precipitacion = clima_i$Precipitacion[[1L]],
      Viento = clima_i$Viento[[1L]]
    )
  )
  
  X_new <- crear_features_rf_clima(
    hist_forecast_rf
  ) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::select(dplyr::all_of(variables_rf_clima))
  
  stopifnot(
    nrow(X_new) == 1L,
    ncol(X_new) == length(variables_rf_clima),
    !anyNA(X_new),
    all(is.finite(as.matrix(X_new)))
  )
  
  pred_i <- as.numeric(
    stats::predict(
      fit_rf_final,
      newdata = X_new
    )
  )[[1L]]
  
  stopifnot(
    length(pred_i) == 1L,
    is.finite(pred_i)
  )
  
  # La predicción alimenta los rezagos de la siguiente semana.
  hist_forecast_rf$Precio_Para_Rezagos[
    nrow(hist_forecast_rf)
  ] <- pred_i
  
  resultados_futuros_lista[[i]] <- data.frame(
    Semana_Pronosticada = i,
    Fecha = as.Date(next_date),
    Precio_Pronosticado = pred_i,
    Tpromedio_Estimada = clima_i$Tpromedio[[1L]],
    Precipitacion_Estimada = clima_i$Precipitacion[[1L]],
    Viento_Estimado = clima_i$Viento[[1L]],
    Fuente_Clima = clima_i$Fuente_Clima[[1L]],
    temp_lag1 = X_new$temp_lag1[[1L]],
    precip_lag1 = X_new$precip_lag1[[1L]],
    viento_lag1 = X_new$viento_lag1[[1L]],
    precip_cum12 = X_new$precip_cum12[[1L]],
    precip_cum24 = X_new$precip_cum24[[1L]],
    viento_roll12 = X_new$viento_roll12[[1L]],
    viento_roll24 = X_new$viento_roll24[[1L]]
  )
}

pronostico_futuro <- dplyr::bind_rows(
  resultados_futuros_lista
) %>%
  dplyr::arrange(Fecha)

fechas_futuras_esperadas <- seq.Date(
  from = as.Date("2026-06-22"),
  by = "week",
  length.out = horizonte_futuro
)

stopifnot(
  nrow(pronostico_futuro) == 12L,
  all(pronostico_futuro$Fecha == fechas_futuras_esperadas),
  min(pronostico_futuro$Fecha) == as.Date("2026-06-22"),
  max(pronostico_futuro$Fecha) == as.Date("2026-09-07"),
  all(is.finite(pronostico_futuro$Precio_Pronosticado))
)

# ================================================================
# 12) Gráficos
# ================================================================

# 12.1. Precio observado frente a predicción en prueba.
g_test <- ggplot2::ggplot(
  predicciones_test,
  ggplot2::aes(x = Fecha)
) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = Precio_Real,
      color = "Precio observado"
    ),
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      y = Precio_Real,
      color = "Precio observado"
    ),
    size = 1,
    na.rm = TRUE
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = Precio_Predicho,
      color = "Predicción RF con clima"
    ),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      y = Precio_Predicho,
      color = "Predicción RF con clima"
    ),
    size = 1
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Precio observado" = "black",
      "Predicción RF con clima" = "#0072B2"
    )
  ) +
  ggplot2::scale_x_date(
    date_breaks = "3 months",
    date_labels = "%m-%Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.05, 0.05))
  ) +
  ggplot2::labs(
    x = "Fecha",
    y = "Precio del quintal (USD)",
    color = NULL
  ) +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(
      size = 20,
      face = "bold"
    ),
    axis.text.x = ggplot2::element_text(
      size = 14,
      angle = 45,
      hjust = 1,
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = 15,
      colour = "black"
    ),
    legend.position = "top",
    legend.text = ggplot2::element_text(size = 14),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black",
      linewidth = 0.8
    )
  )

# 12.2. Importancia de variables del modelo final.
importancia_grafico <- importancia_variables %>%
  dplyr::slice_min(
    order_by = Posicion,
    n = 12L,
    with_ties = FALSE
  ) %>%
  dplyr::arrange(IncMSE)

g_importancia <- ggplot2::ggplot(
  importancia_grafico,
  ggplot2::aes(
    x = stats::reorder(Variable, IncMSE),
    y = IncMSE
  )
) +
  ggplot2::geom_col(
    fill = "#0072B2",
    width = 0.72
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::number(
        IncMSE,
        accuracy = 0.01,
        decimal.mark = ","
      )
    ),
    hjust = -0.10,
    size = 4.2,
    fontface = "bold"
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.05, 0.20))
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Incremento porcentual del MSE"
  ) +
  ggplot2::theme_bw(base_size = 18) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(
      face = "bold",
      size = 17
    ),
    axis.text.x = ggplot2::element_text(
      size = 13,
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = 14,
      colour = "black",
      face = "bold"
    ),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black",
      linewidth = 0.8
    ),
    plot.margin = ggplot2::margin(8, 32, 8, 8)
  )

# 12.3. Histórico y pronóstico futuro.
historico_grafico <- precio_modelado %>%
  dplyr::arrange(Fecha) %>%
  dplyr::transmute(
    Fecha,
    Precio = Precio_Observado,
    Serie = "Histórico"
  )

ultimo_observado <- historico_grafico %>%
  dplyr::filter(!is.na(Precio)) %>%
  dplyr::slice_max(
    order_by = Fecha,
    n = 1L,
    with_ties = FALSE
  )

ultima_fecha_observada <- ultimo_observado$Fecha[[1L]]

pronostico_grafico <- pronostico_futuro %>%
  dplyr::transmute(
    Fecha,
    Precio = Precio_Pronosticado,
    Serie = "Pronóstico"
  ) %>%
  dplyr::bind_rows(
    ultimo_observado %>%
      dplyr::mutate(Serie = "Pronóstico"),
    .
  ) %>%
  dplyr::arrange(Fecha)

serie_completa_grafico <- dplyr::bind_rows(
  historico_grafico,
  pronostico_grafico
)

g_historico_futuro <- ggplot2::ggplot(
  serie_completa_grafico,
  ggplot2::aes(
    x = Fecha,
    y = Precio,
    color = Serie
  )
) +
  ggplot2::geom_line(
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  ggplot2::geom_vline(
    xintercept = ultima_fecha_observada,
    linetype = "dashed",
    linewidth = 0.6,
    color = "black"
  ) +
  ggplot2::annotate(
    "text",
    x = ultima_fecha_observada - 35,
    y = max(serie_completa_grafico$Precio, na.rm = TRUE) - 1,
    label = "Inicio del pronóstico",
    hjust = 1,
    size = 5
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Histórico" = "#F8766D",
      "Pronóstico" = "#00BFC4"
    )
  ) +
  ggplot2::scale_x_date(
    date_breaks = "6 months",
    date_labels = "%m-%Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.04, 0.08))
  ) +
  ggplot2::labs(
    x = "Fecha",
    y = "Precio del quintal (USD)",
    color = NULL
  ) +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(
      size = 20,
      face = "bold"
    ),
    axis.text.x = ggplot2::element_text(
      size = 14,
      angle = 45,
      hjust = 1,
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = 15,
      colour = "black"
    ),
    legend.position = "top",
    legend.text = ggplot2::element_text(size = 18),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black",
      linewidth = 0.8
    )
  )

# 12.4. Solo las 12 semanas futuras.
g_pronostico_futuro <- ggplot2::ggplot(
  pronostico_futuro,
  ggplot2::aes(
    x = Fecha,
    y = Precio_Pronosticado
  )
) +
  ggplot2::geom_line(
    colour = "#0072B2",
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    colour = "#0072B2",
    size = 2
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::number(
        Precio_Pronosticado,
        accuracy = 0.01,
        decimal.mark = ","
      )
    ),
    vjust = -1,
    size = 4.5,
    colour = "black"
  ) +
  ggplot2::scale_x_date(
    breaks = pronostico_futuro$Fecha,
    date_labels = "%d-%m-%Y",
    expand = ggplot2::expansion(mult = c(0.03, 0.03))
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.08, 0.18))
  ) +
  ggplot2::labs(
    x = "Semana pronosticada",
    y = "Precio del quintal (USD)"
  ) +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(
      size = 20,
      face = "bold"
    ),
    axis.text.x = ggplot2::element_text(
      size = 12,
      angle = 45,
      hjust = 1,
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = 15,
      colour = "black"
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black",
      linewidth = 0.8
    )
  )

print(g_test)
print(g_importancia)
print(g_historico_futuro)
print(g_pronostico_futuro)

# ================================================================
# 13) Controles y exportación
# ================================================================

variables_exportar <- data.frame(
  Orden = seq_along(variables_rf_clima),
  Variable = variables_rf_clima,
  stringsAsFactors = FALSE
)

controles_datos <- data.frame(
  Indicador = c(
    "Registros diarios de precio",
    "Registros diarios de clima",
    "Semanas totales de precio",
    "Semanas climáticas",
    "Semanas observadas",
    "Semanas modelables",
    "Semanas de entrenamiento RF",
    "Semanas de prueba",
    "Semanas evaluadas",
    "Número de variables explicativas",
    "Número de árboles",
    "mtry seleccionado",
    "nodesize seleccionado",
    "Fecha final de entrenamiento",
    "Fecha inicial de prueba",
    "Semana imputada en entrenamiento",
    "Valor imputado en entrenamiento",
    "Error estándar de imputación",
    "Semana no observada en prueba",
    "Valor de apoyo en prueba",
    "Error estándar del apoyo",
    "Primera semana futura",
    "Última semana futura"
  ),
  Valor = c(
    as.character(nrow(precio)),
    as.character(nrow(clima)),
    as.character(nrow(precio_semanal)),
    as.character(nrow(clima_semanal)),
    as.character(sum(precio_semanal$Semana_Observada)),
    as.character(nrow(full_data_rf_clima)),
    as.character(nrow(train_data)),
    as.character(nrow(test_data)),
    as.character(sum(test_data$Evaluar_Test)),
    as.character(length(variables_rf_clima)),
    as.character(ntree_rf),
    as.character(best_rf_clima$mtry[[1L]]),
    as.character(best_rf_clima$nodesize[[1L]]),
    format(fecha_corte, "%Y-%m-%d"),
    format(fecha_inicio_test, "%Y-%m-%d"),
    format(fecha_faltante_train, "%Y-%m-%d"),
    format(valor_imputado_train, digits = 12),
    format(error_estandar_train, digits = 12),
    format(fecha_faltante_test, "%Y-%m-%d"),
    format(valor_apoyo_test, digits = 12),
    format(error_estandar_test, digits = 12),
    format(min(pronostico_futuro$Fecha), "%Y-%m-%d"),
    format(max(pronostico_futuro$Fecha), "%Y-%m-%d")
  )
)

# Registrar versiones para reproducibilidad.
versiones_paquetes <- data.frame(
  Componente = c("R", paquetes),
  Version = c(
    R.version.string,
    vapply(
      paquetes,
      function(paquete) {
        as.character(utils::packageVersion(paquete))
      },
      FUN.VALUE = character(1)
    )
  )
)

# Exportar un Excel exclusivo de Random Forest con clima.
writexl::write_xlsx(
  list(
    Metricas_Test = metricas_rf,
    Predicciones_Test = predicciones_test,
    Pronostico_Futuro = pronostico_futuro,
    Clima_Futuro = clima_futuro,
    Verificacion_Documento = verificacion_documento,
    Hiperparametros = hiperparametros_rf,
    Grid_Search = resultados_grid_rf,
    Diagnostico_Rolling = diagnostico_rolling_rf,
    Importancia_Variables = importancia_variables,
    Variables_Modelo = variables_exportar,
    Faltantes_Clima = faltantes_clima_antes,
    Controles_Datos = controles_datos,
    Versiones = versiones_paquetes,
    Base_Modelo_RF_Clima = full_data_rf_clima,
    Base_Semanal_Precio = precio_modelado,
    Base_Semanal_Clima = clima_semanal
  ),
  path = "Resultados_RF_Clima_Semanal.xlsx"
)

# Guardar observado frente a predicho.
ggplot2::ggsave(
  filename = "rf_clima_s_test.pdf",
  plot = g_test,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar la importancia de variables.
ggplot2::ggsave(
  filename = "Grafico_rf_clima_s_importancia.pdf",
  plot = g_importancia,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar el histórico y el pronóstico futuro.
ggplot2::ggsave(
  filename = "Grafico_rf_clima_s_hist_y_pred.pdf",
  plot = g_historico_futuro,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar únicamente las 12 semanas futuras.
ggplot2::ggsave(
  filename = "Grafico_rf_clima_s_solo_pred.pdf",
  plot = g_pronostico_futuro,
  width = 7,
  height = 4.8,
  units = "in"
)

# Mostrar resultados principales.
print(metricas_rf)
print(verificacion_documento)
print(hiperparametros_rf)
print(pronostico_futuro)

cat(
  "\nResultados guardados en: ",
  normalizePath(getwd(), winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)
