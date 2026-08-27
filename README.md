# tfg-simulacion-ruleta-estadistica
# Simulación estadística de la ruleta europea

TFG del Grado en Estadística (Universidad de Salamanca, 2026), dirigido por Dra. María Teresa Cabero Morán.

## Qué hace este proyecto

Un simulador de ruleta europea programado en R que combina teoría de la probabilidad, simulación de Monte Carlo y muestreo estadístico para demostrar empíricamente por qué la banca tiene ventaja matemática sobre el jugador a largo plazo.

## Metodología

- **Simulador propio en R**: generación aleatoria de tiradas, motor de apuestas (sencillas, docenas/columnas, pleno) y resolución de pagos.
- **Simulaciones de Monte Carlo** de hasta 20.000 iteraciones por escenario, calculando esperanza matemática, varianza, error cuadrático medio (MSE) e intervalos de confianza al 95%.
- **Análisis de sensibilidad** del riesgo de ruina en función del tamaño de la apuesta.
- **Muestreo estadístico** (aleatorio simple y por conglomerados) sobre 40 jugadores simulados en Excel, con estimación de proporciones e intervalos de confianza.

## Resultados principales

- El saldo medio converge, en los tres escenarios de apuesta, hacia el margen teórico de la banca (-2,7%), validando el Teorema Central del Límite y la Ley de los Grandes Números.
- La probabilidad de ruina crece de forma **no lineal** con el tamaño de la apuesta: de prácticamente 0% con apuestas bajas a un **88,5%** con apuestas de 500€ sobre un capital de 1.000€.
- El muestreo sobre 40 jugadores simulados arrojó una tasa de bancarrota del 65%, coherente con el modelo teórico.

## Herramientas

R (base, `sample()`, gráficos base) · Excel (muestreo complementario)
