# Scripts de los modelos predictivos

Esta carpeta reúne los scripts en R utilizados para desarrollar, entrenar y evaluar los modelos de predicción semanal del precio de la papa súper chola en Riobamba.

## Estructura

- [`01_arima`](./01_arima): modelo ARIMA semanal.
- [`02_sarima`](./02_sarima): modelo SARIMA semanal.
- [`03_arimax`](./03_arimax): modelo ARIMAX semanal con variables climáticas.
- [`04_sarimax`](./04_sarimax): modelo SARIMAX semanal con variables climáticas.
- [`05_random_forest_precio`](./05_random_forest_precio): modelo Random Forest basado en el historial del precio.
- [`06_random_forest_clima`](./06_random_forest_clima): modelo Random Forest con variables climáticas.
- [`07_xgboost_precio`](./07_xgboost_precio): modelo XGBoost basado en el historial del precio.
- [`08_xgboost_clima`](./08_xgboost_clima): modelo XGBoost con variables climáticas.

Cada subcarpeta contiene el script correspondiente y un archivo README con su descripción. El modelo XGBoost semanal basado únicamente en el historial del precio fue seleccionado como el modelo final de la investigación.
