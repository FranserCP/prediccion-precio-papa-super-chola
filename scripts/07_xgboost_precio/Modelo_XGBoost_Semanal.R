# ================================================================
# MODELO XGBOOST SEMANAL (SOLO PRECIO) - PAPA SUPER CHOLA
# ================================================================

# Paquetes necesarios.
paquetes <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "lubridate",
  "zoo",
  "Metrics",
  "xgboost",
  "ggplot2",
  "scales",
  "writexl",
  "rstudioapi"
)

# Instalar unicamente los paquetes faltantes.
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
  library(xgboost)
  library(ggplot2)
  library(scales)
  library(writexl)
})

options(stringsAsFactors = FALSE)
set.seed(123)


# ================================================================
# 1) Carga de datos historicos del precio
# ================================================================

archivo_precio <- "Papa_03-01-2022_al_30-06-2026.xlsx"

if (!file.exists(archivo_precio)) {
  stop(
    paste0(
      "No se encontro el archivo '",
      archivo_precio,
      "' en la carpeta del script: ",
      carpeta_script
    )
  )
}

full_precio <- readxl::read_excel(archivo_precio)

# ================================================================
# 2) Limpieza de los datos historicos del precio
# ================================================================

# Conservar las mismas columnas utilizadas en el codigo completo.
columnas_requeridas <- c(
  "Fecha Investigación",
  "Precio/Presentación (USD)"
)

if (!all(columnas_requeridas %in% names(full_precio))) {
  stop(
    paste0(
      "El Excel debe contener las columnas: ",
      paste(columnas_requeridas, collapse = ", "),
      "."
    )
  )
}

full_precio <- full_precio[, columnas_requeridas]
colnames(full_precio) <- c("Fecha", "Precio_Quintal")
full_precio <- full_precio[order(full_precio$Fecha), ]

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

# Controles de la base utilizada en el documento.
stopifnot(
  nrow(precio) == 628L,
  min(precio$Fecha) == as.Date("2022-01-12"),
  max(precio$Fecha) == as.Date("2026-06-17"),
  !anyNA(precio$Precio_Quintal)
)

# ================================================================
# 3) Agregacion semanal
# ================================================================

# Promediar exclusivamente los precios realmente observados.
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

stopifnot(
  nrow(precio_semanal) == 232L,
  sum(precio_semanal$Semana_Observada) == 230L,
  identical(
    fechas_faltantes,
    as.Date(c("2022-06-20", "2026-06-08"))
  )
)

# ================================================================
# 4) Division temporal e imputacion causal
# ================================================================

# Maxima ventana historica utilizada por el modelo.
ventana_maxima <- 24L
n_semanas_modelables <- nrow(precio_semanal) - ventana_maxima
n_train_modelable <- floor(0.8 * n_semanas_modelables)
indice_corte <- ventana_maxima + n_train_modelable

fecha_corte <- precio_semanal$Fecha[indice_corte]
fecha_inicio_test <- precio_semanal$Fecha[indice_corte + 1L]

stopifnot(
  n_semanas_modelables == 208L,
  n_train_modelable == 166L,
  fecha_corte == as.Date("2025-08-25"),
  fecha_inicio_test == as.Date("2025-09-01")
)

# Dividir antes de imputar para evitar fuga de informacion.
precio_train_raw <- precio_semanal %>%
  dplyr::filter(Fecha <= fecha_corte)

precio_test_raw <- precio_semanal %>%
  dplyr::filter(Fecha >= fecha_inicio_test)

# Imputacion causal de la semana faltante del entrenamiento.
semana_faltante_train <- precio_train_raw %>%
  dplyr::filter(!Semana_Observada)

stopifnot(nrow(semana_faltante_train) == 1L)

fecha_faltante_train <- semana_faltante_train$Fecha[[1L]]

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

# Reconstruir la serie semanal completa.
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

# Valor causal de apoyo para la semana faltante de prueba.
semana_faltante_test <- precio_modelado %>%
  dplyr::filter(
    Conjunto == "Prueba",
    !Semana_Observada
  )

stopifnot(nrow(semana_faltante_test) == 1L)

fecha_faltante_test <- semana_faltante_test$Fecha[[1L]]

datos_previos_test <- precio_modelado %>%
  dplyr::filter(Fecha < fecha_faltante_test) %>%
  dplyr::arrange(Fecha)

serie_previa_test <- datos_previos_test$Precio_Quintal
ultima_fecha_usada_test <- max(datos_previos_test$Fecha)

stopifnot(
  !anyNA(serie_previa_test),
  ultima_fecha_usada_test < fecha_faltante_test
)

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
# 5) Ingenieria de variables de precio
# ================================================================

# La misma funcion se utiliza en evaluacion y pronostico futuro.
crear_features_xgb_precio <- function(data_hist) {
  data_hist %>%
    dplyr::arrange(Fecha) %>%
    dplyr::mutate(
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
      month = lubridate::month(Fecha),
      yday = lubridate::yday(Fecha)
    )
}

# Variables literales del XGBoost sin clima del codigo completo.
variables_modelo_precio <- c(
  "lag1", "lag2", "lag3", "lag4",
  "lag8", "lag12", "lag16", "lag24",
  "diff1", "diff4",
  "roll_mean_4", "roll_sd_4",
  "roll_min_4", "roll_max_4",
  "roll_mean_8", "roll_sd_8",
  "month", "yday"
)

precio_features <- precio_modelado %>%
  dplyr::mutate(
    Precio_Objetivo = Precio_Observado
  ) %>%
  crear_features_xgb_precio()

filas_modelables <- seq.int(
  ventana_maxima + 1L,
  nrow(precio_features)
)

full_data_xgb <- precio_features %>%
  dplyr::slice(filas_modelables)

train_data <- full_data_xgb %>%
  dplyr::filter(Conjunto == "Entrenamiento") %>%
  dplyr::arrange(Fecha)

test_data <- full_data_xgb %>%
  dplyr::filter(Conjunto == "Prueba") %>%
  dplyr::arrange(Fecha)

stopifnot(
  length(variables_modelo_precio) == 18L,
  nrow(full_data_xgb) == 208L,
  nrow(train_data) == 166L,
  nrow(test_data) == 42L,
  sum(test_data$Evaluar_Test) == 41L,
  sum(is.na(test_data$Precio_Objetivo)) == 1L,
  !anyNA(train_data[, variables_modelo_precio]),
  !anyNA(test_data[, variables_modelo_precio]),
  max(train_data$Fecha) < min(test_data$Fecha)
)

# ================================================================
# 6) Funciones de metricas
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

# ================================================================
# 7) Preparacion de matrices y Grid Search de XGBoost
# ================================================================

preparar_xgb_precio <- function(df) {
  X <- df %>%
    dplyr::select(
      dplyr::all_of(variables_modelo_precio)
    ) %>%
    as.matrix()
  
  storage.mode(X) <- "double"
  
  y <- df$Precio_Objetivo
  
  list(X = X, y = y)
}

# Conservar solo semanas con objetivo observado para el ajuste.
train_validacion <- train_data %>%
  dplyr::filter(!is.na(Precio_Objetivo)) %>%
  dplyr::arrange(Fecha)

# Cuadricula literal del codigo completo: 432 combinaciones.
grid <- expand.grid(
  nrounds = c(100, 200, 300),
  eta = c(0.03, 0.05, 0.10),
  max_depth = c(3, 4, 5),
  min_child_weight = c(1, 3),
  subsample = c(0.8, 1.0),
  colsample_bytree = c(0.8, 1.0),
  gamma = c(0, 0.1),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Validacion de origen rodante con tres bloques temporales.
n_folds <- 3L
n_train <- nrow(train_validacion)
fold_size <- floor(n_train / (n_folds + 1L))

stopifnot(
  nrow(grid) == 432L,
  n_train == 166L,
  fold_size == 41L
)

resultados_grid_lista_precio <- vector(
  mode = "list",
  length = nrow(grid)
)

for (i in seq_len(nrow(grid))) {
  params_i <- grid[i, , drop = FALSE]
  
  rmse_folds <- numeric(n_folds)
  smape_folds <- numeric(n_folds)
  mae_folds <- numeric(n_folds)
  mape_folds <- numeric(n_folds)
  r2_folds <- numeric(n_folds)
  
  for (f in seq_len(n_folds)) {
    train_idx <- seq_len(f * fold_size)
    valid_idx <- seq.int(
      f * fold_size + 1L,
      (f + 1L) * fold_size
    )
    
    subtrain_data <- train_validacion[train_idx, , drop = FALSE]
    valid_data <- train_validacion[valid_idx, , drop = FALSE]
    
    prep_subtrain_p <- preparar_xgb_precio(subtrain_data)
    prep_valid_p <- preparar_xgb_precio(valid_data)
    
    stopifnot(
      !anyNA(prep_subtrain_p$X),
      !anyNA(prep_subtrain_p$y),
      !anyNA(prep_valid_p$X),
      !anyNA(prep_valid_p$y)
    )
    
    dsubtrain_p <- xgboost::xgb.DMatrix(
      data = prep_subtrain_p$X,
      label = prep_subtrain_p$y
    )
    
    dvalid_p <- xgboost::xgb.DMatrix(
      data = prep_valid_p$X,
      label = prep_valid_p$y
    )
    
    set.seed(123 + i)
    
    fit_xgb_valid_p <- xgboost::xgb.train(
      params = list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        eta = params_i$eta,
        max_depth = params_i$max_depth,
        min_child_weight = params_i$min_child_weight,
        subsample = params_i$subsample,
        colsample_bytree = params_i$colsample_bytree,
        gamma = params_i$gamma
      ),
      data = dsubtrain_p,
      nrounds = params_i$nrounds,
      verbose = 0
    )
    
    pred_valid_p_i <- predict(
      fit_xgb_valid_p,
      dvalid_p
    )
    
    rmse_folds[f] <- Metrics::rmse(
      prep_valid_p$y,
      pred_valid_p_i
    )
    
    smape_folds[f] <- calc_smape(
      prep_valid_p$y,
      pred_valid_p_i
    )
    
    mae_folds[f] <- Metrics::mae(
      prep_valid_p$y,
      pred_valid_p_i
    )
    
    # Se conserva la funcion Metrics::mape del codigo completo.
    mape_folds[f] <- Metrics::mape(
      prep_valid_p$y,
      pred_valid_p_i
    )
    
    r2_folds[f] <- calc_r2(
      prep_valid_p$y,
      pred_valid_p_i
    )
  }
  
  resultados_grid_lista_precio[[i]] <- data.frame(
    nrounds = params_i$nrounds,
    eta = params_i$eta,
    max_depth = params_i$max_depth,
    min_child_weight = params_i$min_child_weight,
    subsample = params_i$subsample,
    colsample_bytree = params_i$colsample_bytree,
    gamma = params_i$gamma,
    RMSE = mean(rmse_folds),
    sMAPE = mean(smape_folds),
    MAE = mean(mae_folds),
    MAPE = mean(mape_folds),
    R2 = mean(r2_folds)
  )
  
  if (i %% 25L == 0L || i == nrow(grid)) {
    cat(
      "Grid Search XGBoost sin clima: ",
      i,
      " de ",
      nrow(grid),
      " combinaciones\n",
      sep = ""
    )
  }
}

resultados_grid_precio <- dplyr::bind_rows(
  resultados_grid_lista_precio
)

metricas_validacion_precio <- resultados_grid_precio %>%
  dplyr::arrange(sMAPE, RMSE, MAE)

best_precio <- metricas_validacion_precio %>%
  dplyr::slice_head(n = 1L)

best_params_precio <- list(
  nrounds = best_precio$nrounds,
  eta = best_precio$eta,
  max_depth = best_precio$max_depth,
  min_child_weight = best_precio$min_child_weight,
  subsample = best_precio$subsample,
  colsample_bytree = best_precio$colsample_bytree,
  gamma = best_precio$gamma
)

# ================================================================
# 8) Rolling forecast XGBoost de un paso
# ================================================================

rolling_forecast_one_step_xgb_precio <- function(
    train_df,
    test_df,
    best_params
) {
  history_df <- train_df %>%
    dplyr::filter(!is.na(Precio_Objetivo)) %>%
    dplyr::arrange(Fecha)
  
  test_df <- test_df %>%
    dplyr::arrange(Fecha)
  
  preds <- rep(NA_real_, nrow(test_df))
  
  for (i in seq_len(nrow(test_df))) {
    history_model <- history_df %>%
      dplyr::filter(!is.na(Precio_Objetivo))
    
    datos_train_i <- preparar_xgb_precio(
      history_model
    )
    
    test_i <- test_df %>%
      dplyr::slice(i)
    
    datos_test_i <- preparar_xgb_precio(
      test_i
    )
    
    stopifnot(
      !anyNA(datos_train_i$X),
      !anyNA(datos_train_i$y),
      !anyNA(datos_test_i$X)
    )
    
    dtrain_i <- xgboost::xgb.DMatrix(
      data = datos_train_i$X,
      label = datos_train_i$y
    )
    
    dtest_i <- xgboost::xgb.DMatrix(
      data = datos_test_i$X
    )
    
    set.seed(123 + i)
    
    fit_i <- xgboost::xgb.train(
      params = list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        eta = best_params$eta,
        max_depth = best_params$max_depth,
        min_child_weight = best_params$min_child_weight,
        subsample = best_params$subsample,
        colsample_bytree = best_params$colsample_bytree,
        gamma = best_params$gamma
      ),
      data = dtrain_i,
      nrounds = best_params$nrounds,
      verbose = 0
    )
    
    preds[i] <- predict(fit_i, dtest_i)
    
    history_df <- dplyr::bind_rows(
      history_df,
      test_i
    )
    
    if (i %% 10L == 0L || i == nrow(test_df)) {
      cat(
        "Rolling forecast XGBoost: ",
        i,
        " de ",
        nrow(test_df),
        " semanas\n",
        sep = ""
      )
    }
  }
  
  preds
}

train_final <- train_data %>%
  dplyr::arrange(Fecha)

test_final <- test_data %>%
  dplyr::arrange(Fecha)

pred_xgb_precio <- rolling_forecast_one_step_xgb_precio(
  train_df = train_final,
  test_df = test_final,
  best_params = best_params_precio
)

stopifnot(
  length(pred_xgb_precio) == 42L,
  all(is.finite(pred_xgb_precio))
)

# ================================================================
# 9) Predicciones y metricas de prueba
# ================================================================

resultados_xgb_precio <- test_final %>%
  dplyr::mutate(
    Prediccion_XGB_Precio = pred_xgb_precio,
    Residuo_XGB_Precio = dplyr::if_else(
      dplyr::coalesce(Evaluar_Test, FALSE) &
        !is.na(Precio_Objetivo),
      Precio_Objetivo - Prediccion_XGB_Precio,
      NA_real_
    )
  )

evaluacion_xgb_precio <- resultados_xgb_precio %>%
  dplyr::filter(
    dplyr::coalesce(Evaluar_Test, FALSE),
    is.finite(Precio_Objetivo),
    is.finite(Prediccion_XGB_Precio)
  )

real_test_precio <- evaluacion_xgb_precio$Precio_Objetivo
pred_test_precio <- evaluacion_xgb_precio$Prediccion_XGB_Precio

stopifnot(
  nrow(evaluacion_xgb_precio) == 41L,
  length(real_test_precio) == 41L,
  length(pred_test_precio) == 41L
)

metricas_finales_xgb_precio <- data.frame(
  Modelo = "XGBoost semanal (Solo Precio)",
  N = length(real_test_precio),
  MAE = Metrics::mae(real_test_precio, pred_test_precio),
  RMSE = Metrics::rmse(real_test_precio, pred_test_precio),
  MAPE = calc_mape(real_test_precio, pred_test_precio),
  sMAPE = calc_smape(real_test_precio, pred_test_precio),
  R2 = calc_r2(real_test_precio, pred_test_precio)
)

predicciones_test <- resultados_xgb_precio %>%
  dplyr::transmute(
    Fecha,
    Conjunto,
    Semana_Observada,
    Evaluar_Test,
    Precio_Real = Precio_Objetivo,
    Precio_Predicho = Prediccion_XGB_Precio,
    Residuo = Residuo_XGB_Precio,
    Error_Absoluto = abs(Residuo),
    Error_Cuadratico = Residuo^2,
    Error_Porcentual = dplyr::if_else(
      Evaluar_Test,
      abs(Residuo / Precio_Real) * 100,
      NA_real_
    )
  )

# Valores vigentes de la Tabla 12 del documento enviado.
metricas_documento <- data.frame(
  Metrica = c("MAE", "RMSE", "MAPE", "sMAPE", "R2"),
  Valor_Documento = c(1.85, 2.46, 9.21, 9.07, 0.08)
)

verificacion_documento <- metricas_finales_xgb_precio %>%
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
      "Las metricas calculadas no coinciden completamente con la ",
      "Tabla 12 al redondear a dos decimales. Revise la base y las ",
      "versiones de R y xgboost."
    )
  )
}

# ================================================================
# 10) Modelo final, importancia y pronostico futuro
# ================================================================

# Ajustar el modelo final con todas las semanas cuyo precio fue observado.
datos_modelo_final_xgb <- full_data_xgb %>%
  dplyr::filter(!is.na(Precio_Objetivo)) %>%
  dplyr::arrange(Fecha)

prep_full_xgb <- preparar_xgb_precio(
  datos_modelo_final_xgb
)

stopifnot(
  nrow(datos_modelo_final_xgb) == 207L,
  ncol(prep_full_xgb$X) == 18L,
  !anyNA(prep_full_xgb$X),
  !anyNA(prep_full_xgb$y)
)

dfull_xgb <- xgboost::xgb.DMatrix(
  data = prep_full_xgb$X,
  label = prep_full_xgb$y
)

set.seed(123)

fit_final_xgb <- xgboost::xgb.train(
  params = list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = best_params_precio$eta,
    max_depth = best_params_precio$max_depth,
    min_child_weight = best_params_precio$min_child_weight,
    subsample = best_params_precio$subsample,
    colsample_bytree = best_params_precio$colsample_bytree,
    gamma = best_params_precio$gamma
  ),
  data = dfull_xgb,
  nrounds = best_params_precio$nrounds,
  verbose = 0
)

# Importancia mediante Gain, Cover y Frequency.
importancia_variables <- xgboost::xgb.importance(
  feature_names = colnames(prep_full_xgb$X),
  model = fit_final_xgb
) %>%
  as.data.frame() %>%
  dplyr::mutate(Posicion = dplyr::row_number()) %>%
  dplyr::select(
    Posicion,
    Feature,
    Gain,
    Cover,
    Frequency
  )

# Horizonte recursivo de 12 semanas.
horizonte_futuro <- 12L

hist_forecast_xgb <- precio_modelado %>%
  dplyr::arrange(Fecha) %>%
  dplyr::transmute(
    Fecha,
    Precio_Para_Rezagos
  )

stopifnot(
  nrow(hist_forecast_xgb) == 232L,
  !anyNA(hist_forecast_xgb$Precio_Para_Rezagos),
  max(hist_forecast_xgb$Fecha) == as.Date("2026-06-15")
)

# Comprobar que la reconstruccion reproduce la ultima fila del modelo.
X_comprobacion_xgb <- crear_features_xgb_precio(
  hist_forecast_xgb
) %>%
  dplyr::slice_tail(n = 1L) %>%
  dplyr::select(dplyr::all_of(variables_modelo_precio))

X_referencia_xgb <- full_data_xgb %>%
  dplyr::slice_tail(n = 1L) %>%
  dplyr::select(dplyr::all_of(variables_modelo_precio))

stopifnot(
  isTRUE(all.equal(
    as.numeric(X_comprobacion_xgb[1L, ]),
    as.numeric(X_referencia_xgb[1L, ]),
    tolerance = 1e-10,
    check.attributes = FALSE
  ))
)

resultados_futuros_lista <- vector(
  mode = "list",
  length = horizonte_futuro
)

for (i in seq_len(horizonte_futuro)) {
  next_date <- max(hist_forecast_xgb$Fecha) + 7L
  
  # Incorporar temporalmente la nueva semana sin precio.
  hist_forecast_xgb <- dplyr::bind_rows(
    hist_forecast_xgb,
    data.frame(
      Fecha = as.Date(next_date),
      Precio_Para_Rezagos = NA_real_
    )
  )
  
  X_new <- crear_features_xgb_precio(
    hist_forecast_xgb
  ) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::select(dplyr::all_of(variables_modelo_precio)) %>%
    as.matrix()
  
  storage.mode(X_new) <- "double"
  
  stopifnot(
    nrow(X_new) == 1L,
    ncol(X_new) == length(variables_modelo_precio),
    !anyNA(X_new),
    all(is.finite(X_new))
  )
  
  dnew_xgb <- xgboost::xgb.DMatrix(data = X_new)
  
  pred_i <- as.numeric(
    predict(fit_final_xgb, dnew_xgb)
  )[[1L]]
  
  stopifnot(
    length(pred_i) == 1L,
    is.finite(pred_i)
  )
  
  # La prediccion alimenta los rezagos de la semana siguiente.
  hist_forecast_xgb$Precio_Para_Rezagos[
    nrow(hist_forecast_xgb)
  ] <- pred_i
  
  resultados_futuros_lista[[i]] <- data.frame(
    Semana_Pronosticada = i,
    Fecha = as.Date(next_date),
    Precio_Pronosticado = pred_i
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
# 11) Graficos
# ================================================================

# 11.1. Precio observado frente a prediccion en prueba.
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
      color = "Prediccion XGBoost"
    ),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      y = Precio_Predicho,
      color = "Prediccion XGBoost"
    ),
    size = 1
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Precio observado" = "black",
      "Prediccion XGBoost" = "#0072B2"
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

# 11.2. Importancia de variables del modelo final.
importancia_grafico <- importancia_variables %>%
  dplyr::slice_min(
    order_by = Posicion,
    n = 12L,
    with_ties = FALSE
  ) %>%
  dplyr::arrange(Gain)

g_importancia <- ggplot2::ggplot(
  importancia_grafico,
  ggplot2::aes(
    x = stats::reorder(Feature, Gain),
    y = Gain
  )
) +
  ggplot2::geom_col(
    fill = "#0072B2",
    width = 0.72
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::number(
        Gain,
        accuracy = 0.001,
        decimal.mark = ","
      )
    ),
    hjust = -0.10,
    size = 4.2,
    fontface = "bold"
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.05, 0.22))
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Ganancia relativa (Gain)"
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

# 11.3. Historico y pronostico futuro.
historico_grafico <- precio_modelado %>%
  dplyr::arrange(Fecha) %>%
  dplyr::transmute(
    Fecha,
    Precio = Precio_Observado,
    Serie = "Historico"
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
    Serie = "Pronostico"
  ) %>%
  dplyr::bind_rows(
    ultimo_observado %>%
      dplyr::mutate(Serie = "Pronostico"),
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
    label = "Inicio del pronostico",
    hjust = 1,
    size = 5
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Historico" = "#F8766D",
      "Pronostico" = "#00BFC4"
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

# 11.4. Solo las 12 semanas futuras.
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
# 12) Controles y exportacion
# ================================================================

variables_exportar <- data.frame(
  Orden = seq_along(variables_modelo_precio),
  Variable = variables_modelo_precio,
  stringsAsFactors = FALSE
)

hiperparametros_xgb <- data.frame(
  Parametro = c(
    "nrounds",
    "eta",
    "max_depth",
    "min_child_weight",
    "subsample",
    "colsample_bytree",
    "gamma"
  ),
  Valor = c(
    best_params_precio$nrounds,
    best_params_precio$eta,
    best_params_precio$max_depth,
    best_params_precio$min_child_weight,
    best_params_precio$subsample,
    best_params_precio$colsample_bytree,
    best_params_precio$gamma
  ),
  stringsAsFactors = FALSE
)

controles_datos <- data.frame(
  Indicador = c(
    "Registros diarios",
    "Semanas totales",
    "Semanas observadas",
    "Semanas modelables",
    "Semanas de entrenamiento",
    "Semanas de prueba",
    "Semanas evaluadas",
    "Variables explicativas",
    "Combinaciones del Grid Search",
    "Bloques de validacion temporal",
    "Tamano de cada bloque",
    "Fecha final de entrenamiento",
    "Fecha inicial de prueba",
    "Semana imputada en entrenamiento",
    "Valor imputado en entrenamiento",
    "Error estandar de imputacion",
    "Semana no observada en prueba",
    "Valor de apoyo en prueba",
    "Error estandar del apoyo",
    "Primera semana futura",
    "Ultima semana futura"
  ),
  Valor = c(
    as.character(nrow(precio)),
    as.character(nrow(precio_semanal)),
    as.character(sum(precio_semanal$Semana_Observada)),
    as.character(nrow(full_data_xgb)),
    as.character(nrow(train_data)),
    as.character(nrow(test_data)),
    as.character(nrow(evaluacion_xgb_precio)),
    as.character(length(variables_modelo_precio)),
    as.character(nrow(grid)),
    as.character(n_folds),
    as.character(fold_size),
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
  ),
  stringsAsFactors = FALSE
)

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
  ),
  stringsAsFactors = FALSE
)

# Exportar un Excel exclusivo de XGBoost sin clima.
writexl::write_xlsx(
  list(
    Metricas_Test = metricas_finales_xgb_precio,
    Predicciones_Test = predicciones_test,
    Pronostico_Futuro = pronostico_futuro,
    Verificacion_Documento = verificacion_documento,
    Hiperparametros = hiperparametros_xgb,
    Grid_Search = metricas_validacion_precio,
    Importancia_Variables = importancia_variables,
    Variables_Modelo = variables_exportar,
    Controles_Datos = controles_datos,
    Versiones = versiones_paquetes,
    Base_Modelo_XGB = full_data_xgb,
    Base_Semanal = precio_modelado
  ),
  path = "Resultados_XGBoost_Semanal.xlsx"
)

# Guardar observado frente a predicho.
ggplot2::ggsave(
  filename = "xgb_s_test.pdf",
  plot = g_test,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar la importancia de variables.
ggplot2::ggsave(
  filename = "Grafico_xgb_s_importancia.pdf",
  plot = g_importancia,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar el historico y el pronostico futuro.
ggplot2::ggsave(
  filename = "Grafico_xgb_s_hist_y_pred.pdf",
  plot = g_historico_futuro,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar unicamente las 12 semanas futuras.
ggplot2::ggsave(
  filename = "Grafico_xgb_s_solo_pred.pdf",
  plot = g_pronostico_futuro,
  width = 7,
  height = 4.8,
  units = "in"
)

cat(
  "\nResultados guardados en: ",
  normalizePath(getwd(), winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)

