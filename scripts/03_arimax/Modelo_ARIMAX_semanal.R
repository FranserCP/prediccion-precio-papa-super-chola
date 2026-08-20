# ================================================================
# MODELO ARIMAX SEMANAL - PAPA SUPER CHOLA
# ================================================================

# Paquetes necesarios.
paquetes <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "lubridate",
  "forecast",
  "Metrics",
  "ggplot2",
  "scales",
  "writexl",
  "imputeTS",
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
  library(forecast)
  library(Metrics)
  library(ggplot2)
  library(scales)
  library(writexl)
  library(imputeTS)
})

options(stringsAsFactors = FALSE)

# ================================================================
# 1. Carga de datos históricos
# ================================================================

# Leer los Excel ubicados junto al archivo .R.
full_precio <- readxl::read_excel(
  "Papa_03-01-2022_al_30-06-2026.xlsx"
)

full_climaH <- readxl::read_excel(
  "Clima_03-01-2022_al_30-06-2026.xlsx"
)

# ================================================================
# 2. Limpieza de los datos del precio
# ================================================================

# Conservar fecha y precio.
full_precio <- full_precio[, c(
  "Fecha Investigación",
  "Precio/Presentación (USD)"
)]

colnames(full_precio) <- c("Fecha", "Precio_Quintal")

# Convertir formatos y consolidar fechas repetidas.
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
# 3. Limpieza de los datos climáticos
# ================================================================

# Conservar las tres variables climáticas.
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

# Convertir formatos y ordenar.
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
  # Alinear el clima con el intervalo disponible del precio.
  dplyr::filter(
    Fecha >= min(precio$Fecha),
    Fecha <= max(precio$Fecha)
  ) %>%
  tidyr::complete(
    Fecha = seq.Date(
      min(precio$Fecha),
      max(precio$Fecha),
      by = "day"
    )
  ) %>%
  dplyr::arrange(Fecha)

# Registrar faltantes antes de imputar.
faltantes_clima_antes <- data.frame(
  Variable = c("Tpromedio", "Precipitacion", "Viento"),
  Faltantes_Antes = c(
    sum(is.na(clima$Tpromedio)),
    sum(is.na(clima$Precipitacion)),
    sum(is.na(clima$Viento))
  )
)

# Imputar con el filtro de Kalman del Rmd.
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
  min(clima$Fecha) == min(precio$Fecha),
  max(clima$Fecha) == max(precio$Fecha),
  !anyNA(clima[, c(
    "Tpromedio",
    "Precipitacion",
    "Viento"
  )])
)

# ================================================================
# 4. Agregación semanal del precio
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
# 5. División temporal e imputación causal
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

# Dividir antes de imputar.
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
# 6. Base semanal utilizada por ARIMAX
# ================================================================

# Agregar el clima a frecuencia semanal.
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

# Crear los tres regresores climáticos del Rmd.
base_semanal_arimax <- precio_modelado %>%
  dplyr::left_join(clima_semanal, by = "Fecha") %>%
  dplyr::arrange(Fecha) %>%
  dplyr::mutate(
    Precio_Objetivo = Precio_Observado,
    temp_lag1 = dplyr::lag(Tpromedio, 1L),
    precip_lag1 = dplyr::lag(Precipitacion, 1L),
    viento_lag1 = dplyr::lag(Viento, 1L)
  )

variables_arimax <- c(
  "temp_lag1",
  "precip_lag1",
  "viento_lag1"
)

# Conservar las mismas 208 semanas modelables.
filas_modelables <- seq.int(
  ventana_maxima + 1L,
  nrow(base_semanal_arimax)
)

full_data_arimax <- base_semanal_arimax %>%
  dplyr::slice(filas_modelables)

train_data <- full_data_arimax %>%
  dplyr::filter(Conjunto == "Entrenamiento") %>%
  dplyr::arrange(Fecha)

test_data <- full_data_arimax %>%
  dplyr::filter(Conjunto == "Prueba") %>%
  dplyr::arrange(Fecha)

# Controles del protocolo.
stopifnot(
  nrow(clima_semanal) == 232L,
  nrow(full_data_arimax) == 208L,
  nrow(train_data) == 166L,
  nrow(test_data) == 42L,
  sum(test_data$Evaluar_Test) == 41L,
  sum(is.na(test_data$Precio_Objetivo)) == 1L,
  !anyNA(full_data_arimax[, variables_arimax])
)

# ================================================================
# 7. Funciones de métricas
# ================================================================

# Error porcentual absoluto medio.
calc_mape <- function(real, pred) {
  mean(
    abs((real - pred) / real),
    na.rm = TRUE
  ) * 100
}

# Error porcentual absoluto medio simétrico.
calc_smape <- function(real, pred) {
  mean(
    200 * abs(real - pred) /
      (abs(real) + abs(pred)),
    na.rm = TRUE
  )
}

# Coeficiente de determinación.
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

# Resumir las cinco métricas.
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
# 8. Rolling forecast ARIMAX de un paso
# ================================================================

# Reestimar ARIMAX en cada semana de prueba.
rolling_forecast_arimax <- function(
    train_df,
    test_df,
    xreg_cols
) {
  history_df <- train_df %>%
    dplyr::filter(!is.na(Precio_Objetivo)) %>%
    dplyr::arrange(Fecha)
  
  test_df <- test_df %>%
    dplyr::arrange(Fecha)
  
  preds <- rep(NA_real_, nrow(test_df))
  ordenes <- vector("list", nrow(test_df))
  coeficientes <- vector("list", nrow(test_df))
  
  for (i in seq_len(nrow(test_df))) {
    y_hist <- stats::ts(
      history_df$Precio_Objetivo,
      frequency = 52
    )
    
    xreg_hist <- as.matrix(
      history_df[, xreg_cols, drop = FALSE]
    )
    
    xreg_test <- as.matrix(
      test_df[i, xreg_cols, drop = FALSE]
    )
    
    storage.mode(xreg_hist) <- "double"
    storage.mode(xreg_test) <- "double"
    
    # Configuración literal del Rmd del tutor.
    fit <- forecast::auto.arima(
      y_hist,
      xreg = xreg_hist,
      seasonal = FALSE,
      stepwise = TRUE,
      approximation = FALSE
    )
    
    preds[i] <- as.numeric(
      forecast::forecast(
        fit,
        h = 1,
        xreg = xreg_test
      )$mean[1]
    )
    
    orden <- forecast::arimaorder(fit)
    
    ordenes[[i]] <- data.frame(
      Fecha = test_df$Fecha[i],
      Iteracion = i,
      p = unname(orden["p"]),
      d = unname(orden["d"]),
      q = unname(orden["q"]),
      AICc = as.numeric(fit$aicc)
    )
    
    coeficientes[[i]] <- data.frame(
      Fecha = test_df$Fecha[i],
      Iteracion = i,
      Termino = names(stats::coef(fit)),
      Estimacion = unname(stats::coef(fit))
    )
    
    # Incorporar la observación real al historial.
    history_df <- dplyr::bind_rows(
      history_df,
      test_df[i, ]
    )
  }
  
  list(
    predicciones = preds,
    ordenes = dplyr::bind_rows(ordenes),
    coeficientes = dplyr::bind_rows(coeficientes)
  )
}

resultado_rolling <- rolling_forecast_arimax(
  train_df = train_data,
  test_df = test_data,
  xreg_cols = variables_arimax
)

pred_arimax <- resultado_rolling$predicciones
ordenes_rolling <- resultado_rolling$ordenes
coeficientes_rolling <- resultado_rolling$coeficientes

# ================================================================
# 9. Predicciones y métricas
# ================================================================

y_real_test <- test_data$Precio_Objetivo

metricas_arimax <- calc_metrics(
  real = y_real_test,
  pred = pred_arimax,
  nombre_modelo = "ARIMAX"
)

# Preparar el detalle semanal.
predicciones_test <- test_data %>%
  dplyr::transmute(
    Fecha,
    Conjunto,
    Semana_Observada,
    Evaluar_Test,
    Precio_Real = Precio_Objetivo,
    Precio_Predicho = pred_arimax,
    temp_lag1,
    precip_lag1,
    viento_lag1,
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

# ================================================================
# 10. Gráfico de observados y predichos
# ================================================================

# Comparar los precios reales y las predicciones en prueba.
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
      color = "Predicción ARIMAX"
    ),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      y = Precio_Predicho,
      color = "Predicción ARIMAX"
    ),
    size = 1
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Precio observado" = "black",
      "Predicción ARIMAX" = "#0072B2"
    )
  ) +
  ggplot2::scale_x_date(
    date_breaks = "3 months",
    date_labels = "%m-%Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(
      floor(min(
        c(
          predicciones_test$Precio_Real,
          predicciones_test$Precio_Predicho
        ),
        na.rm = TRUE
      )),
      ceiling(max(
        c(
          predicciones_test$Precio_Real,
          predicciones_test$Precio_Predicho
        ),
        na.rm = TRUE
      )),
      by = 4
    ),
    expand = ggplot2::expansion(mult = c(0.05, 0.05))
  ) +
  ggplot2::labs(
    x = "Fecha",
    y = "Precio del quintal (USD)",
    color = NULL
  ) +
  ggplot2::theme_bw(base_size = 24) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(
      size = 28,
      face = "bold",
      margin = ggplot2::margin(t = 14)
    ),
    axis.title.y = ggplot2::element_text(
      size = 28,
      face = "bold",
      margin = ggplot2::margin(r = 14)
    ),
    axis.text.x = ggplot2::element_text(
      size = 19,
      angle = 45,
      hjust = 1,
      vjust = 1,
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = 19,
      colour = "black"
    ),
    legend.position = "top",
    legend.justification = "center",
    legend.text = ggplot2::element_text(size = 19),
    legend.key.width = grid::unit(1.5, "cm"),
    legend.key.height = grid::unit(0.7, "cm"),
    panel.grid.major = ggplot2::element_line(
      colour = "gray82",
      linewidth = 0.55
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black",
      linewidth = 1
    ),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  ) +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      override.aes = list(
        linewidth = 1.5,
        size = 3
      )
    )
  )

print(g_test)

# Valores publicados en la Tabla 12 del PDF.
metricas_documento <- data.frame(
  Metrica = c("MAE", "RMSE", "MAPE", "sMAPE", "R2"),
  Valor_Documento = c(1.56, 2.09, 7.75, 7.81, 0.33)
)

# Comparar al mismo nivel de redondeo del documento.
verificacion_documento <- metricas_arimax %>%
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
      "al redondear a dos decimales. Revise las bases y las versiones de paquetes."
    )
  )
}

# ================================================================
# 11. Pronóstico futuro de 12 semanas
# ================================================================

horizonte_futuro <- 12L

fechas_futuras <- seq.Date(
  from = max(precio_modelado$Fecha) + 7,
  by = "week",
  length.out = horizonte_futuro
)

# Preparar el clima histórico semanal.
clima_historico_forecast <- clima_semanal %>%
  dplyr::arrange(Fecha) %>%
  dplyr::mutate(
    Semana_Anio = lubridate::isoweek(Fecha),
    Anio = lubridate::year(Fecha)
  )

# Calcular el promedio por semana del año.
clima_promedio_semana <- clima_historico_forecast %>%
  dplyr::group_by(Semana_Anio) %>%
  dplyr::summarise(
    Tpromedio_Futuro = mean(Tpromedio, na.rm = TRUE),
    Precipitacion_Futura = mean(Precipitacion, na.rm = TRUE),
    Viento_Futuro = mean(Viento, na.rm = TRUE),
    .groups = "drop"
  )

# Estimar el clima con el criterio usado en el Rmd.
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

# Estimar el clima de las 12 semanas futuras.
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

# Unir el clima histórico y el clima estimado.
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

# Construir los regresores rezagados del horizonte futuro.
xreg_futuro_df <- data.frame(
  Fecha = fechas_futuras,
  Fecha_Clima_Rezagada = fechas_futuras - 7
) %>%
  dplyr::left_join(
    clima_extendido %>%
      dplyr::transmute(
        Fecha_Clima_Rezagada = Fecha,
        temp_lag1 = Tpromedio,
        precip_lag1 = Precipitacion,
        viento_lag1 = Viento,
        Fuente_Clima_Rezagada = Fuente_Clima
      ),
    by = "Fecha_Clima_Rezagada"
  )

stopifnot(
  nrow(xreg_futuro_df) == horizonte_futuro,
  !anyNA(xreg_futuro_df[, variables_arimax])
)

# Preparar todo el historial disponible para el modelo final.
base_final_arimax <- precio_modelado %>%
  dplyr::transmute(
    Fecha,
    Precio_Modelo = Precio_Para_Rezagos
  ) %>%
  dplyr::left_join(clima_semanal, by = "Fecha") %>%
  dplyr::arrange(Fecha) %>%
  dplyr::mutate(
    temp_lag1 = dplyr::lag(Tpromedio, 1L),
    precip_lag1 = dplyr::lag(Precipitacion, 1L),
    viento_lag1 = dplyr::lag(Viento, 1L)
  ) %>%
  dplyr::filter(
    is.finite(Precio_Modelo),
    dplyr::if_all(
      dplyr::all_of(variables_arimax),
      is.finite
    )
  )

stopifnot(
  nrow(base_final_arimax) == 231L,
  max(base_final_arimax$Fecha) == as.Date("2026-06-15")
)

y_final_arimax <- stats::ts(
  base_final_arimax$Precio_Modelo,
  frequency = 52
)

xreg_final_arimax <- as.matrix(
  base_final_arimax[, variables_arimax, drop = FALSE]
)

xreg_futuro <- as.matrix(
  xreg_futuro_df[, variables_arimax, drop = FALSE]
)

storage.mode(xreg_final_arimax) <- "double"
storage.mode(xreg_futuro) <- "double"

# Reajustar ARIMAX con toda la información disponible.
fit_arimax_final <- forecast::auto.arima(
  y_final_arimax,
  xreg = xreg_final_arimax,
  seasonal = FALSE,
  stepwise = TRUE,
  approximation = FALSE
)

# Generar 12 semanas con intervalos del 80 % y 95 %.
fc_arimax_futuro <- forecast::forecast(
  fit_arimax_final,
  h = horizonte_futuro,
  xreg = xreg_futuro,
  level = c(80, 95)
)

orden_arimax_final <- forecast::arimaorder(fit_arimax_final)

pronostico_futuro <- xreg_futuro_df %>%
  dplyr::mutate(
    Semana_Pronosticada = dplyr::row_number(),
    Precio_Pronosticado = as.numeric(fc_arimax_futuro$mean),
    Limite_Inferior_80 = as.numeric(
      fc_arimax_futuro$lower[, 1]
    ),
    Limite_Superior_80 = as.numeric(
      fc_arimax_futuro$upper[, 1]
    ),
    Limite_Inferior_95 = as.numeric(
      fc_arimax_futuro$lower[, 2]
    ),
    Limite_Superior_95 = as.numeric(
      fc_arimax_futuro$upper[, 2]
    ),
    Orden_ARIMAX = paste0(
      "ARIMAX(",
      unname(orden_arimax_final["p"]), ",",
      unname(orden_arimax_final["d"]), ",",
      unname(orden_arimax_final["q"]),
      ")"
    )
  ) %>%
  dplyr::select(
    Semana_Pronosticada,
    Fecha,
    Precio_Pronosticado,
    Limite_Inferior_80,
    Limite_Superior_80,
    Limite_Inferior_95,
    Limite_Superior_95,
    Fecha_Clima_Rezagada,
    temp_lag1,
    precip_lag1,
    viento_lag1,
    Fuente_Clima_Rezagada,
    Orden_ARIMAX
  )

# ================================================================
# 12. Gráficos del pronóstico futuro
# ================================================================

# Preparar el histórico observado.
historico_grafico <- precio_modelado %>%
  dplyr::arrange(Fecha) %>%
  dplyr::transmute(
    Fecha,
    Precio = Precio_Observado,
    Serie = "Histórico"
  )

# Conectar el pronóstico con el último precio observado.
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

# Mostrar el histórico y las 12 semanas pronosticadas.
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
    size = 5.5
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
    axis.title.x = ggplot2::element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(t = 10)
    ),
    axis.title.y = ggplot2::element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(r = 10)
    ),
    axis.text.x = ggplot2::element_text(
      size = 15,
      angle = 45,
      hjust = 1,
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = 15,
      colour = "black"
    ),
    legend.position = "top",
    legend.text = ggplot2::element_text(size = 25),
    legend.key.width = grid::unit(1.3, "cm"),
    panel.grid.major = ggplot2::element_line(
      colour = "gray84",
      linewidth = 0.4
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black",
      linewidth = 0.7
    ),
    plot.margin = ggplot2::margin(8, 10, 8, 10)
  ) +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      override.aes = list(linewidth = 1)
    )
  )

# Mostrar únicamente las 12 semanas pronosticadas.
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
    size = 5,
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
    axis.title.x = ggplot2::element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(t = 12)
    ),
    axis.title.y = ggplot2::element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(r = 12)
    ),
    axis.text.x = ggplot2::element_text(
      size = 13,
      angle = 45,
      hjust = 1,
      vjust = 1,
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = 15,
      colour = "black"
    ),
    panel.grid.major = ggplot2::element_line(
      colour = "gray84",
      linewidth = 0.4
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black",
      linewidth = 0.7
    ),
    plot.margin = ggplot2::margin(12, 12, 10, 12)
  )

print(g_historico_futuro)
print(g_pronostico_futuro)

# ================================================================
# 13. Controles y exportación
# ================================================================

controles_datos <- data.frame(
  Indicador = c(
    "Registros diarios de precio",
    "Registros diarios de clima",
    "Semanas totales",
    "Semanas observadas",
    "Semanas modelables",
    "Semanas de entrenamiento ARIMAX",
    "Semanas de prueba",
    "Semanas evaluadas",
    "Fecha final de entrenamiento",
    "Fecha inicial de prueba",
    "Semana imputada en entrenamiento",
    "Valor imputado en entrenamiento",
    "Error estándar de imputación",
    "Semana no observada en prueba",
    "Valor de apoyo en prueba",
    "Error estándar del apoyo",
    "Variables exógenas",
    "Semanas del modelo final",
    "Inicio del pronóstico futuro",
    "Fin del pronóstico futuro"
  ),
  Valor = c(
    as.character(nrow(precio)),
    as.character(nrow(clima)),
    as.character(nrow(precio_semanal)),
    as.character(sum(precio_semanal$Semana_Observada)),
    as.character(nrow(full_data_arimax)),
    as.character(nrow(train_data)),
    as.character(nrow(test_data)),
    as.character(sum(test_data$Evaluar_Test)),
    format(fecha_corte, "%Y-%m-%d"),
    format(fecha_inicio_test, "%Y-%m-%d"),
    format(fecha_faltante_train, "%Y-%m-%d"),
    format(valor_imputado_train, digits = 12),
    format(error_estandar_train, digits = 12),
    format(fecha_faltante_test, "%Y-%m-%d"),
    format(valor_apoyo_test, digits = 12),
    format(error_estandar_test, digits = 12),
    paste(variables_arimax, collapse = ", "),
    as.character(nrow(base_final_arimax)),
    format(min(fechas_futuras), "%Y-%m-%d"),
    format(max(fechas_futuras), "%Y-%m-%d")
  )
)

# Describir las variables exógenas.
detalle_variables_arimax <- data.frame(
  Variable = variables_arimax,
  Descripcion = c(
    "Temperatura promedio de la semana anterior",
    "Precipitación total de la semana anterior",
    "Velocidad media del viento de la semana anterior"
  ),
  Unidad = c("°C", "mm", "km/h")
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

# Exportar un Excel exclusivo de ARIMAX.
writexl::write_xlsx(
  list(
    Metricas_Test = metricas_arimax,
    Predicciones_Test = predicciones_test,
    Pronostico_Futuro = pronostico_futuro,
    Clima_Futuro = clima_futuro,
    Verificacion_Documento = verificacion_documento,
    Ordenes_Rolling = ordenes_rolling,
    Coeficientes_Rolling = coeficientes_rolling,
    Variables_ARIMAX = detalle_variables_arimax,
    Faltantes_Clima = faltantes_clima_antes,
    Controles_Datos = controles_datos,
    Versiones = versiones_paquetes,
    Base_Modelo_ARIMAX = full_data_arimax,
    Base_Semanal = base_semanal_arimax
  ),
  path = "Resultados_ARIMAX_Semanal.xlsx"
)

# Guardar el gráfico de prueba.
ggplot2::ggsave(
  filename = "arimax_s_test.pdf",
  plot = g_test,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar el histórico y el pronóstico futuro.
ggplot2::ggsave(
  filename = "Grafico_arimax_s_hist_y_pred.pdf",
  plot = g_historico_futuro,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar únicamente las 12 semanas futuras.
ggplot2::ggsave(
  filename = "Grafico_arimax_s_solo_pred.pdf",
  plot = g_pronostico_futuro,
  width = 7,
  height = 4.8,
  units = "in"
)

# Mostrar resultados principales.
print(metricas_arimax)
print(verificacion_documento)
print(pronostico_futuro)

cat(
  "\nResultados guardados en: ",
  normalizePath(getwd(), winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)
