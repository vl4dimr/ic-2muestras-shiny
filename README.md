# Estimación interválica con dos muestras — App Shiny

Calculadora interactiva e interpretada para docencia en **Bioestadística**
(Maestría en Salud Pública, Escuela de Posgrado — UNAP).

Tres módulos:

- **A · Medias independientes** — IC de la diferencia de medias (t-Student / Welch).
- **B · Medias pareadas** — IC del cambio medio sobre las diferencias intra-sujeto.
- **C · OR y RR (tabla 2×2)** — cocientes con IC en escala log, RD y NNH.

Cada resultado sale **interpretado en lenguaje claro**, con la regla de oro
(el IC de una diferencia se lee frente al **0**; el de un cociente, frente al **1**).

Solo usa **R base + shiny**. Datos de los ejemplos: Hosmer & Lemeshow (1989) y
Student (1908).

**Ejecutar en local:** `shiny::runApp("app.R")`

Docente: Milton Vladimir Mamani Calisaya.
