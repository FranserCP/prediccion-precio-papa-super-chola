# ================================================================
# MODELO XGBOOST SEMANAL CON CLIMA - PAPA SUPER CHOLA
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
  "imputeTS",
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
  library(imputeTS)
})

options(stringsAsFactors = FALSE)
set.seed(123)


# ================================================================
# 1) Carga de datos historicos de precio y clima
# ================================================================

archivo_precio <- "Papa_03-01-2022_al_30-06-2026.xlsx"
archivo_clima <- "Clima_03-01-2022_al_30-06-2026.xlsx"

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

if (!file.exists(archivo_clima)) {
  stop(
    paste0(
      "No se encontro el archivo '",
      archivo_clima,
      "' en la carpeta del script: ",
      carpeta_script
    )
  )
}

full_precio <- readxl::read_excel(archivo_precio)
full_climaH <- readxl::read_excel(archivo_clima)

# ================================================================
# 2) Limpieza de los datos historicos de precio y clima
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

# Conservar las cuatro columnas climaticas utilizadas en el codigo completo.
columnas_clima_requeridas <- c("date", "tavg", "prcp", "wspd")

if (!all(columnas_clima_requeridas %in% names(full_climaH))) {
  stop(
    paste0(
      "El Excel climatico debe contener las columnas: ",
      paste(columnas_clima_requeridas, collapse = ", "),
      "."
    )
  )
}

full_climaH <- full_climaH[, columnas_clima_requeridas]
colnames(full_climaH) <- c(
  "Fecha",
  "Tpromedio",
  "Precipitacion",
  "Viento"
)

# Consolidar duplicados diarios: temperatura y viento mediante promedio,
# y precipitacion mediante suma, tal como se hizo en el documento.
clima <- full_climaH %>%
  dplyr::mutate(
    Fecha = as.Date(Fecha),
    Tpromedio = as.numeric(Tpromedio),
    Precipitacion = as.numeric(Precipitacion),
    Viento = as.numeric(Viento)
  ) %>%
  dplyr::filter(!is.na(Fecha)) %>%
  dplyr::arrange(Fecha) %>%
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

# Imputacion del clima mediante Filtro de Kalman sin suavizado retrospectivo.
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
  !anyNA(clima$Tpromedio),
  !anyNA(clima$Precipitacion),
  !anyNA(clima$Viento)
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

# Agregacion semanal del clima: promedio para variables intensivas y
# suma para la precipitacion acumulada.
clima_semanal <- clima %>%
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

stopifnot(
  !anyNA(clima_semanal$Tpromedio),
  !anyNA(clima_semanal$Precipitacion),
  !anyNA(clima_semanal$Viento)
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
# 5) Ingenieria de variables de precio y clima
# ================================================================

# La misma funcion se utiliza en evaluacion y pronostico futuro.
crear_features_xgb_clima <- function(data_hist) {
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
      # Clima rezagado una semana para evitar fuga de informacion.
      temp_lag1 = dplyr::lag(Tpromedio, 1L),
      precip_lag1 = dplyr::lag(Precipitacion, 1L),
      viento_lag1 = dplyr::lag(Viento, 1L),
      
      # Rezagos climaticos fenologicos.
      temp_lag12 = dplyr::lag(Tpromedio, 12L),
      temp_lag16 = dplyr::lag(Tpromedio, 16L),
      temp_lag20 = dplyr::lag(Tpromedio, 20L),
      temp_lag24 = dplyr::lag(Tpromedio, 24L),
      precip_lag12 = dplyr::lag(Precipitacion, 12L),
      precip_lag16 = dplyr::lag(Precipitacion, 16L),
      precip_lag20 = dplyr::lag(Precipitacion, 20L),
      precip_lag24 = dplyr::lag(Precipitacion, 24L),
      viento_lag12 = dplyr::lag(Viento, 12L),
      viento_lag16 = dplyr::lag(Viento, 16L),
      viento_lag20 = dplyr::lag(Viento, 20L),
      viento_lag24 = dplyr::lag(Viento, 24L),
      
      # Acumulados y promedios climaticos desplazados una semana.
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
      
      month = lubridate::month(Fecha),
      yday = lubridate::yday(Fecha)
    )
}

# Las 37 variables literales del XGBoost con clima del codigo completo.
variables_modelo_clima <- c(
  "lag1", "lag2", "lag3", "lag4",
  "lag8", "lag12", "lag16", "lag24",
  "diff1", "diff4",
  "roll_mean_4", "roll_sd_4",
  "roll_min_4", "roll_max_4",
  "roll_mean_8", "roll_sd_8",
  "temp_lag1", "precip_lag1", "viento_lag1",
  "temp_lag12", "temp_lag16", "temp_lag20", "temp_lag24",
  "precip_lag12", "precip_lag16", "precip_lag20", "precip_lag24",
  "viento_lag12", "viento_lag16", "viento_lag20", "viento_lag24",
  "precip_cum12", "precip_cum24",
  "viento_roll12", "viento_roll24",
  "month", "yday"
)

meses_es <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio",
  "julio", "agosto", "septiembre", "octubre", "noviembre",
  "diciembre"
)

etiqueta_semana_mes <- function(fecha_inicio_semana) {
  dia_mes <- lubridate::day(fecha_inicio_semana)
  numero_semana_mes <- ((dia_mes - 1L) %/% 7L) + 1L
  mes_texto <- meses_es[lubridate::month(fecha_inicio_semana)]
  paste("Semana", numero_semana_mes, "de", mes_texto)
}

# Integrar precio y clima antes de construir los rezagos.
full_data_xgb <- precio_modelado %>%
  dplyr::left_join(clima_semanal, by = "Fecha") %>%
  dplyr::arrange(Fecha) %>%
  dplyr::mutate(
    Precio_Objetivo = Precio_Observado
  ) %>%
  crear_features_xgb_clima()

filas_modelables <- seq.int(
  ventana_maxima + 1L,
  nrow(full_data_xgb)
)

full_data_xgb <- full_data_xgb %>%
  dplyr::slice(filas_modelables) %>%
  dplyr::mutate(
    Etiqueta = vapply(
      Fecha,
      etiqueta_semana_mes,
      FUN.VALUE = character(1)
    ),
    Anio = lubridate::year(Fecha)
  )

train_data <- full_data_xgb %>%
  dplyr::filter(Conjunto == "Entrenamiento") %>%
  dplyr::arrange(Fecha)

test_data <- full_data_xgb %>%
  dplyr::filter(Conjunto == "Prueba") %>%
  dplyr::arrange(Fecha)

stopifnot(
  length(variables_modelo_clima) == 37L,
  nrow(full_data_xgb) == 208L,
  nrow(train_data) == 166L,
  nrow(test_data) == 42L,
  sum(test_data$Evaluar_Test) == 41L,
  sum(is.na(test_data$Precio_Objetivo)) == 1L,
  !anyNA(train_data[, variables_modelo_clima]),
  !anyNA(test_data[, variables_modelo_clima]),
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

preparar_xgb <- function(df) {
  X <- df %>%
    dplyr::select(
      dplyr::all_of(variables_modelo_clima)
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

resultados_grid_lista_clima <- vector(
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
    
    prep_subtrain_p <- preparar_xgb(subtrain_data)
    prep_valid_p <- preparar_xgb(valid_data)
    
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
  
  resultados_grid_lista_clima[[i]] <- data.frame(
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
      "Grid Search XGBoost con clima: ",
      i,
      " de ",
      nrow(grid),
      " combinaciones\n",
      sep = ""
    )
  }
}

resultados_grid_clima <- dplyr::bind_rows(
  resultados_grid_lista_clima
)

metricas_validacion_clima <- resultados_grid_clima %>%
  dplyr::arrange(sMAPE, RMSE, MAE)

best_clima <- metricas_validacion_clima %>%
  dplyr::slice_head(n = 1L)

best_params_clima <- list(
  nrounds = best_clima$nrounds,
  eta = best_clima$eta,
  max_depth = best_clima$max_depth,
  min_child_weight = best_clima$min_child_weight,
  subsample = best_clima$subsample,
  colsample_bytree = best_clima$colsample_bytree,
  gamma = best_clima$gamma
)

# ================================================================
# 8) Rolling forecast XGBoost de un paso
# ================================================================

rolling_forecast_one_step_xgb <- function(
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
    
    datos_train_i <- preparar_xgb(
      history_model
    )
    
    test_i <- test_df %>%
      dplyr::slice(i)
    
    datos_test_i <- preparar_xgb(
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
        eta = as.numeric(best_params$eta[[1L]]),
        max_depth = as.integer(best_params$max_depth[[1L]]),
        min_child_weight = as.numeric(
          best_params$min_child_weight[[1L]]
        ),
        subsample = as.numeric(best_params$subsample[[1L]]),
        colsample_bytree = as.numeric(
          best_params$colsample_bytree[[1L]]
        ),
        gamma = as.numeric(best_params$gamma[[1L]])
      ),
      data = dtrain_i,
      nrounds = as.integer(best_params$nrounds[[1L]]),
      verbose = 0
    )
    
    preds[i] <- as.numeric(predict(fit_i, dtest_i))[[1L]]
    
    # Solo una semana con precio observado ingresa al historial de ajuste.
    if (!is.na(test_i$Precio_Objetivo[[1L]])) {
      history_df <- dplyr::bind_rows(
        history_df,
        test_i
      )
    }
    
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

pred_xgb_clima <- rolling_forecast_one_step_xgb(
  train_df = train_final,
  test_df = test_final,
  best_params = best_params_clima
)

stopifnot(
  length(pred_xgb_clima) == 42L,
  all(is.finite(pred_xgb_clima))
)

# ================================================================
# 9) Predicciones y metricas de prueba
# ================================================================

resultados_xgb_clima <- test_final %>%
  dplyr::mutate(
    Prediccion_XGB_Clima = pred_xgb_clima,
    Residuo_XGB_Clima = dplyr::if_else(
      dplyr::coalesce(Evaluar_Test, FALSE) &
        !is.na(Precio_Objetivo),
      Precio_Objetivo - Prediccion_XGB_Clima,
      NA_real_
    )
  )

evaluacion_xgb_clima <- resultados_xgb_clima %>%
  dplyr::filter(
    dplyr::coalesce(Evaluar_Test, FALSE),
    is.finite(Precio_Objetivo),
    is.finite(Prediccion_XGB_Clima)
  )

real_test_clima <- evaluacion_xgb_clima$Precio_Objetivo
pred_test_clima <- evaluacion_xgb_clima$Prediccion_XGB_Clima

stopifnot(
  nrow(evaluacion_xgb_clima) == 41L,
  length(real_test_clima) == 41L,
  length(pred_test_clima) == 41L
)

metricas_finales_xgb_clima <- data.frame(
  Modelo = "XGBoost semanal con clima",
  N = length(real_test_clima),
  MAE = Metrics::mae(real_test_clima, pred_test_clima),
  RMSE = Metrics::rmse(real_test_clima, pred_test_clima),
  MAPE = calc_mape(real_test_clima, pred_test_clima),
  sMAPE = calc_smape(real_test_clima, pred_test_clima),
  R2 = calc_r2(real_test_clima, pred_test_clima)
)

residuos_test_clima <- real_test_clima - pred_test_clima

resumen_residuos_xgb_clima <- data.frame(
  N = length(residuos_test_clima),
  Sesgo_Medio = mean(residuos_test_clima),
  Varianza_Residual_Muestral = stats::var(residuos_test_clima),
  Desviacion_Residual_Muestral = stats::sd(residuos_test_clima),
  MSE = mean(residuos_test_clima^2)
)

predicciones_test <- resultados_xgb_clima %>%
  dplyr::transmute(
    Fecha,
    Conjunto,
    Semana_Observada,
    Evaluar_Test,
    Precio_Real = Precio_Objetivo,
    Precio_Predicho = Prediccion_XGB_Clima,
    Residuo = Residuo_XGB_Clima,
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
  Valor_Documento = c(1.79, 2.38, 8.83, 8.76, 0.14)
)

verificacion_documento <- metricas_finales_xgb_clima %>%
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

prep_full_xgb <- preparar_xgb(
  datos_modelo_final_xgb
)

stopifnot(
  nrow(datos_modelo_final_xgb) == 207L,
  ncol(prep_full_xgb$X) == 37L,
  identical(
    colnames(prep_full_xgb$X),
    variables_modelo_clima
  ),
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
    eta = as.numeric(best_params_clima$eta[[1L]]),
    max_depth = as.integer(best_params_clima$max_depth[[1L]]),
    min_child_weight = as.numeric(
      best_params_clima$min_child_weight[[1L]]
    ),
    subsample = as.numeric(best_params_clima$subsample[[1L]]),
    colsample_bytree = as.numeric(
      best_params_clima$colsample_bytree[[1L]]
    ),
    gamma = as.numeric(best_params_clima$gamma[[1L]])
  ),
  data = dfull_xgb,
  nrounds = as.integer(best_params_clima$nrounds[[1L]]),
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

hist_forecast_xgb <- full_data_xgb %>%
  dplyr::arrange(Fecha) %>%
  dplyr::transmute(
    Fecha,
    Precio_Para_Rezagos,
    Tpromedio,
    Precipitacion,
    Viento
  )

stopifnot(
  nrow(hist_forecast_xgb) == 208L,
  !anyNA(hist_forecast_xgb$Precio_Para_Rezagos),
  !anyNA(hist_forecast_xgb[, c(
    "Tpromedio",
    "Precipitacion",
    "Viento"
  )]),
  max(hist_forecast_xgb$Fecha) == as.Date("2026-06-15")
)

# Comprobar que la reconstruccion reproduce la ultima fila del modelo.
X_comprobacion_xgb <- crear_features_xgb_clima(
  hist_forecast_xgb
) %>%
  dplyr::slice_tail(n = 1L) %>%
  dplyr::select(dplyr::all_of(variables_modelo_clima))

X_referencia_xgb <- full_data_xgb %>%
  dplyr::slice_tail(n = 1L) %>%
  dplyr::select(dplyr::all_of(variables_modelo_clima))

stopifnot(
  isTRUE(all.equal(
    as.numeric(X_comprobacion_xgb[1L, ]),
    as.numeric(X_referencia_xgb[1L, ]),
    tolerance = 1e-10,
    check.attributes = FALSE
  ))
)

# Clima historico para estimar las covariables de las 12 semanas futuras.
clima_historico_forecast_xgb <- full_data_xgb %>%
  dplyr::arrange(Fecha) %>%
  dplyr::transmute(
    Fecha,
    Semana_Anio = lubridate::isoweek(Fecha),
    Anio = lubridate::year(Fecha),
    Tpromedio,
    Precipitacion,
    Viento
  )

# Respaldo: promedio historico de cada semana ISO.
clima_promedio_semana_xgb <- clima_historico_forecast_xgb %>%
  dplyr::group_by(Semana_Anio) %>%
  dplyr::summarise(
    Tpromedio_Futuro = mean(Tpromedio, na.rm = TRUE),
    Precipitacion_Futura = mean(Precipitacion, na.rm = TRUE),
    Viento_Futuro = mean(Viento, na.rm = TRUE),
    .groups = "drop"
  )

obtener_clima_futuro_xgb <- function(fecha_futura) {
  semana_futura <- lubridate::isoweek(fecha_futura)
  anio_referencia <- lubridate::year(fecha_futura) - 1L
  
  # Primera opcion del codigo completo: misma semana del anio anterior.
  clima_referencia <- clima_historico_forecast_xgb %>%
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
    all(is.finite(unlist(clima_referencia, use.names = FALSE)))
  
  if (referencia_valida) {
    return(
      clima_referencia %>%
        dplyr::mutate(
          Fuente_Clima = "Misma semana del anio anterior"
        )
    )
  }
  
  # Segunda opcion: promedio historico de esa semana ISO.
  clima_respaldo <- clima_promedio_semana_xgb %>%
    dplyr::filter(Semana_Anio == semana_futura) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::select(
      Tpromedio_Futuro,
      Precipitacion_Futura,
      Viento_Futuro
    )
  
  respaldo_valido <-
    nrow(clima_respaldo) == 1L &&
    all(is.finite(unlist(clima_respaldo, use.names = FALSE)))
  
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
      Fuente_Clima = "Promedio historico de la semana"
    )
}

resultados_futuros_lista <- vector(
  mode = "list",
  length = horizonte_futuro
)

for (i in seq_len(horizonte_futuro)) {
  next_date <- max(hist_forecast_xgb$Fecha) + 7L
  
  clima_futuro_i <- obtener_clima_futuro_xgb(next_date)
  
  # Incorporar la semana con clima estimado y, temporalmente, sin precio.
  hist_forecast_xgb <- dplyr::bind_rows(
    hist_forecast_xgb,
    data.frame(
      Fecha = as.Date(next_date),
      Precio_Para_Rezagos = NA_real_,
      Tpromedio = clima_futuro_i$Tpromedio_Futuro[[1L]],
      Precipitacion = clima_futuro_i$Precipitacion_Futura[[1L]],
      Viento = clima_futuro_i$Viento_Futuro[[1L]]
    )
  )
  
  X_new <- crear_features_xgb_clima(
    hist_forecast_xgb
  ) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::select(dplyr::all_of(variables_modelo_clima)) %>%
    as.matrix()
  
  storage.mode(X_new) <- "double"
  
  stopifnot(
    nrow(X_new) == 1L,
    ncol(X_new) == length(variables_modelo_clima),
    identical(colnames(X_new), variables_modelo_clima),
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
    Anio = lubridate::year(next_date),
    Etiqueta = etiqueta_semana_mes(next_date),
    Fecha = as.Date(next_date),
    Precio_Pronosticado = pred_i,
    Tpromedio_Futuro = clima_futuro_i$Tpromedio_Futuro[[1L]],
    Precipitacion_Futura =
      clima_futuro_i$Precipitacion_Futura[[1L]],
    Viento_Futuro = clima_futuro_i$Viento_Futuro[[1L]],
    Fuente_Clima = clima_futuro_i$Fuente_Clima[[1L]],
    stringsAsFactors = FALSE
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
  all(is.finite(pronostico_futuro$Precio_Pronosticado)),
  all(is.finite(pronostico_futuro$Tpromedio_Futuro)),
  all(is.finite(pronostico_futuro$Precipitacion_Futura)),
  all(is.finite(pronostico_futuro$Viento_Futuro)),
  nrow(hist_forecast_xgb) == nrow(full_data_xgb) + horizonte_futuro,
  !anyNA(hist_forecast_xgb$Precio_Para_Rezagos)
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
historico_grafico <- full_data_xgb %>%
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
  Orden = seq_along(variables_modelo_clima),
  Variable = variables_modelo_clima,
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
    best_params_clima$nrounds,
    best_params_clima$eta,
    best_params_clima$max_depth,
    best_params_clima$min_child_weight,
    best_params_clima$subsample,
    best_params_clima$colsample_bytree,
    best_params_clima$gamma
  ),
  stringsAsFactors = FALSE
)

controles_datos <- data.frame(
  Indicador = c(
    "Registros diarios",
    "Registros climaticos diarios",
    "Semanas climaticas",
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
    as.character(nrow(clima)),
    as.character(nrow(clima_semanal)),
    as.character(nrow(precio_semanal)),
    as.character(sum(precio_semanal$Semana_Observada)),
    as.character(nrow(full_data_xgb)),
    as.character(nrow(train_data)),
    as.character(nrow(test_data)),
    as.character(nrow(evaluacion_xgb_clima)),
    as.character(length(variables_modelo_clima)),
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

# Exportar un Excel exclusivo de XGBoost con clima.
writexl::write_xlsx(
  list(
    Metricas_Test = metricas_finales_xgb_clima,
    Predicciones_Test = predicciones_test,
    Pronostico_Futuro = pronostico_futuro,
    Resumen_Residuos = resumen_residuos_xgb_clima,
    Verificacion_Documento = verificacion_documento,
    Hiperparametros = hiperparametros_xgb,
    Grid_Search = metricas_validacion_clima,
    Importancia_Variables = importancia_variables,
    Variables_Modelo = variables_exportar,
    Controles_Datos = controles_datos,
    Versiones = versiones_paquetes,
    Base_Modelo_XGB_Clima = full_data_xgb,
    Base_Precio_Semanal = precio_modelado,
    Base_Clima_Semanal = clima_semanal
  ),
  path = "Resultados_XGBoost_Clima_Semanal.xlsx"
)

# Guardar observado frente a predicho.
ggplot2::ggsave(
  filename = "xgb_clima_s_test.pdf",
  plot = g_test,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar la importancia de variables.
ggplot2::ggsave(
  filename = "Grafico_xgb_clima_s_importancia.pdf",
  plot = g_importancia,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar el historico y el pronostico futuro.
ggplot2::ggsave(
  filename = "Grafico_xgb_clima_s_hist_y_pred.pdf",
  plot = g_historico_futuro,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar unicamente las 12 semanas futuras.
ggplot2::ggsave(
  filename = "Grafico_xgb_clima_s_solo_pred.pdf",
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
