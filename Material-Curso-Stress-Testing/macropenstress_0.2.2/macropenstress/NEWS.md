## macropenstress 0.1.9

- `escenario_var_stress()` permite que los escenarios condicionales tengan menos filas que el horizonte `h`.
- `forecast_var_condicional()` completa los horizontes faltantes con dinámica VAR recursiva o baseline mediante `fill_method`.
- Valores `NA` en trayectorias condicionadas se interpretan como no imponer esa variable en ese período.

## macropenstress 0.1.6

- `escenario_arima_shock()` incorpora `fechas` y `n_hist` para graficar historia observada con eje temporal real.
- El gráfico ARIMA ancla baseline y adverso en el último dato observado para eliminar el espacio entre histórico y pronóstico.

# macropenstress 0.1.4

- Agrega `conjunta_resumen = c("path", "media", "mediana")` en `escenario_var_stress()`.
- Permite representar la trayectoria conjunta extrema como promedio o mediana del conjunto severo, reduciendo ruido Monte Carlo en gráficos regulatorios.

## macropenstress 0.1.3

- `escenario_var_stress()` ahora permite pasar `severity_by_path`, un vector de severidad precomputado por simulación para seleccionar trayectorias conjuntas extremas definidas por el usuario.

## macropenstress 0.1.2

- Ajusta el gráfico de `escenario_var_stress()` para presentar histórico, baseline y escenarios adversos en un único eje temporal por variable.
- Elimina la separación por columnas de enfoque y ancla todas las trayectorias futuras en el último dato histórico para mostrar continuidad temporal.
- Agrega `datos_grafico` al objeto de salida para facilitar auditoría y personalización del gráfico.

# macropenstress 0.1.0

- Carga de base macro-previsional del curso desde CSV incluido.
- Función `escenario_arima_shock()` para escenarios baseline/adverso con perfil k_h.
- Función `escenario_var_stress()` con enfoques marginal, trayectoria conjunta extrema y VAR condicional.
- Función auxiliar `forecast_var_condicional()`.
- Ejemplos originales del curso incluidos en `inst/examples`.


## macropenstress 0.2.0

- Mejora `cargar_datos_pensiones()` con nombres pedagógicos y compatibilidad: `datos`, `macro_raw`, `macro`, `modelo`, `datos_modelo`, `pensiones`.
- `macro` y `modelo` apuntan a la base transformada para ARIMA/VAR.
