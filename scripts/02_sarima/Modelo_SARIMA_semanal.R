# ================================================================
# MODELO SARIMA SEMANAL - PAPA SUPER CHOLA
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
  library(forecast)
  library(Metrics)
  library(ggplot2)
  library(writexl)
})

options(stringsAsFactors = FALSE)

# =========================================================
# 1) Carga de datos históricos del precio
# =========================================================


# Leer el Excel ubicado junto al archivo .R.
full_precio <- read_excel(
  "Papa_03-01-2022_al_30-06-2026.xlsx"
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

# Convertir formatos y ordenar.
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
# 3. Agregación semanal
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
# 4. División temporal e imputación causal
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

fecha_faltante_train <- semana_faltante_train$Fecha[[1]]

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

fecha_faltante_test <- semana_faltante_test$Fecha[[1]]

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
# 5. Base utilizada por SARIMA
# ================================================================

# SARIMA usa el objetivo observado, igual que en el Rmd.
precio_features <- precio_modelado %>%
  dplyr::arrange(Fecha) %>%
  dplyr::mutate(
    Precio_Objetivo = Precio_Observado
  )

# Conservar las 208 semanas modelables.
filas_modelables <- seq.int(
  ventana_maxima + 1L,
  nrow(precio_features)
)

full_data_sarima <- precio_features %>%
  dplyr::slice(filas_modelables)

train_data <- full_data_sarima %>%
  dplyr::filter(Conjunto == "Entrenamiento") %>%
  dplyr::arrange(Fecha)

test_data <- full_data_sarima %>%
  dplyr::filter(Conjunto == "Prueba") %>%
  dplyr::arrange(Fecha)

# Controles del protocolo.
stopifnot(
  nrow(full_data_sarima) == 208L,
  nrow(train_data) == 166L,
  nrow(test_data) == 42L,
  sum(test_data$Evaluar_Test) == 41L,
  sum(is.na(test_data$Precio_Objetivo)) == 1L
)

# ================================================================
# 6. Funciones de métricas
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
# 7. Rolling forecast SARIMA de un paso
# ================================================================

# Reestimar SARIMA en cada semana de prueba.
rolling_forecast_sarima <- function(train_df, test_df) {
  history_df <- train_df %>%
    dplyr::filter(!is.na(Precio_Objetivo)) %>%
    dplyr::arrange(Fecha)
  
  test_df <- test_df %>%
    dplyr::arrange(Fecha)
  
  preds <- rep(NA_real_, nrow(test_df))
  ordenes <- vector("list", nrow(test_df))
  
  for (i in seq_len(nrow(test_df))) {
    y_hist <- stats::ts(
      history_df$Precio_Objetivo,
      frequency = 52
    )
    
    # Configuración literal del Rmd del tutor.
    fit <- forecast::auto.arima(
      y_hist,
      seasonal = TRUE,
      stepwise = TRUE,
      approximation = FALSE
    )
    
    preds[i] <- as.numeric(
      forecast::forecast(fit, h = 1)$mean[1]
    )
    
    orden <- forecast::arimaorder(fit)
    
    ordenes[[i]] <- data.frame(
      Fecha = test_df$Fecha[i],
      Iteracion = i,
      p = unname(orden["p"]),
      d = unname(orden["d"]),
      q = unname(orden["q"]),
      P = unname(orden["P"]),
      D = unname(orden["D"]),
      Q = unname(orden["Q"]),
      Frecuencia = unname(orden["Frequency"]),
      AICc = as.numeric(fit$aicc)
    )
    
    # Incorporar la observación real al historial.
    history_df <- dplyr::bind_rows(
      history_df,
      test_df[i, ]
    )
  }
  
  list(
    predicciones = preds,
    ordenes = dplyr::bind_rows(ordenes)
  )
}

resultado_rolling <- rolling_forecast_sarima(
  train_df = train_data,
  test_df = test_data
)

pred_sarima <- resultado_rolling$predicciones
ordenes_rolling <- resultado_rolling$ordenes

# ================================================================
# 8. Predicciones y métricas
# ================================================================

y_real_test <- test_data$Precio_Objetivo

metricas_sarima <- calc_metrics(
  real = y_real_test,
  pred = pred_sarima,
  nombre_modelo = "SARIMA"
)

# Preparar el detalle semanal.
predicciones_test <- test_data %>%
  dplyr::transmute(
    Fecha,
    Conjunto,
    Semana_Observada,
    Evaluar_Test,
    Precio_Real = Precio_Objetivo,
    Precio_Predicho = pred_sarima,
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
# 9. Gráfico de observados y predichos
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
      color = "Predicción SARIMA"
    ),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      y = Precio_Predicho,
      color = "Predicción SARIMA"
    ),
    size = 1
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Precio observado" = "black",
      "Predicción SARIMA" = "#0072B2"
    )
  ) +
  ggplot2::scale_x_date(
    date_breaks = "3 months",
    date_labels = "%m-%Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(
      floor(
        min(
          c(
            predicciones_test$Precio_Real,
            predicciones_test$Precio_Predicho
          ),
          na.rm = TRUE
        )
      ),
      ceiling(
        max(
          c(
            predicciones_test$Precio_Real,
            predicciones_test$Precio_Predicho
          ),
          na.rm = TRUE
        )
      ),
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
  Valor_Documento = c(1.91, 2.55, 9.49, 9.63, 0.01)
)

# Comparar al mismo nivel de redondeo del documento.
verificacion_documento <- metricas_sarima %>%
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
# 9.1. Pronóstico futuro de 12 semanas
# ================================================================

# Usar las 232 semanas y los valores ya establecidos.
serie_final_sarima <- precio_modelado %>%
  dplyr::arrange(Fecha) %>%
  dplyr::transmute(
    Fecha,
    Precio_Modelo = Precio_Para_Rezagos
  )

stopifnot(
  nrow(serie_final_sarima) == 232L,
  !anyNA(serie_final_sarima$Precio_Modelo),
  max(serie_final_sarima$Fecha) == as.Date("2026-06-15")
)

# Reajustar SARIMA con toda la serie modelable disponible.
y_final_sarima <- stats::ts(
  serie_final_sarima$Precio_Modelo,
  frequency = 52
)

fit_sarima_final <- forecast::auto.arima(
  y_final_sarima,
  seasonal = TRUE,
  stepwise = TRUE,
  approximation = FALSE
)

# Generar las próximas 12 semanas con intervalos del 80 % y 95 %.
horizonte_futuro <- 12L

fc_sarima_futuro <- forecast::forecast(
  fit_sarima_final,
  h = horizonte_futuro,
  level = c(80, 95)
)

fechas_futuras <- seq.Date(
  from = max(serie_final_sarima$Fecha) + 7,
  by = "week",
  length.out = horizonte_futuro
)

pronostico_futuro <- data.frame(
  Semana_Pronosticada = seq_len(horizonte_futuro),
  Fecha = fechas_futuras,
  Precio_Pronosticado = as.numeric(fc_sarima_futuro$mean),
  Limite_Inferior_80 = as.numeric(fc_sarima_futuro$lower[, 1]),
  Limite_Superior_80 = as.numeric(fc_sarima_futuro$upper[, 1]),
  Limite_Inferior_95 = as.numeric(fc_sarima_futuro$lower[, 2]),
  Limite_Superior_95 = as.numeric(fc_sarima_futuro$upper[, 2])
)

# Registrar el orden elegido para el modelo final.
orden_sarima_final <- forecast::arimaorder(fit_sarima_final)

pronostico_futuro$Orden_SARIMA <- paste0(
  "SARIMA(",
  unname(orden_sarima_final["p"]), ",",
  unname(orden_sarima_final["d"]), ",",
  unname(orden_sarima_final["q"]),
  ")(",
  unname(orden_sarima_final["P"]), ",",
  unname(orden_sarima_final["D"]), ",",
  unname(orden_sarima_final["Q"]),
  ")[",
  unname(orden_sarima_final["Frequency"]),
  "]"
)

# ================================================================
# 9.2. Gráficos del pronóstico futuro
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
# 10. Controles y exportación
# ================================================================

controles_datos <- data.frame(
  Indicador = c(
    "Registros diarios",
    "Semanas totales",
    "Semanas observadas",
    "Semanas modelables",
    "Semanas de entrenamiento SARIMA",
    "Semanas de prueba",
    "Semanas evaluadas",
    "Fecha final de entrenamiento",
    "Fecha inicial de prueba",
    "Semana imputada en entrenamiento",
    "Valor imputado en entrenamiento",
    "Error estándar de imputación",
    "Semana no observada en prueba",
    "Valor de apoyo en prueba",
    "Error estándar del apoyo"
  ),
  Valor = c(
    as.character(nrow(precio)),
    as.character(nrow(precio_semanal)),
    as.character(sum(precio_semanal$Semana_Observada)),
    as.character(nrow(full_data_sarima)),
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
    format(error_estandar_test, digits = 12)
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



# Exportar un Excel exclusivo de SARIMA.
write_xlsx(
  list(
    Metricas_Test = metricas_sarima,
    Predicciones_Test = predicciones_test,
    Pronostico_Futuro = pronostico_futuro,
    Verificacion_Documento = verificacion_documento,
    Ordenes_Rolling = ordenes_rolling,
    Controles_Datos = controles_datos,
    Versiones = versiones_paquetes,
    Base_Modelo_SARIMA = full_data_sarima,
    Base_Semanal = precio_modelado
  ),
  path = "Resultados_SARIMA_Semanal.xlsx"
)

# Guardar el gráfico junto al script.
ggplot2::ggsave(
  filename = "sarima_s_test.pdf",
  plot = g_test,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar el histórico y el pronóstico futuro.
ggplot2::ggsave(
  filename = "Grafico_sarima_s_hist_y_pred.pdf",
  plot = g_historico_futuro,
  width = 7,
  height = 4.8,
  units = "in"
)

# Guardar únicamente las 12 semanas futuras.
ggplot2::ggsave(
  filename = "Grafico_sarima_s_solo_pred.pdf",
  plot = g_pronostico_futuro,
  width = 7,
  height = 4.8,
  units = "in"
)

# Mostrar resultados principales.
print(metricas_sarima)
print(verificacion_documento)
print(pronostico_futuro)

cat(
  "\nResultados guardados en: ",
  normalizePath(getwd(), winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)

