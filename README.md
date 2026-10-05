# 1ACC0216-TB1-2026-2-grupo01

# TB1 – Análisis exploratorio de reservas hoteleras (Hotel Booking Demand)

**Curso:** 1ACC0216 Fundamentos de Data Science (UPC, 2026-02) · **Grupo 01**

## Integrantes
- Magallanes Arias, Cristian Marcelo – u202423870
- Calampa Bardales, Jhoyner Arnold – u202117290
- Mesías Huatuco, Adrián Ricardo – u202421946
- Gonzales Rios, Omar Estefano – u202423014

## Objetivo
Explorar, limpiar y visualizar el dataset *Hotel Booking Demand* con R/RStudio para responder:
1. ¿Qué tipo de hotel concentra una mayor cantidad de reservas y cómo se comportan sus cancelaciones?
2. ¿Cómo varía la cantidad de reservas a lo largo de los meses y qué patrones de demanda se observan según el tipo de hotel?

## Dataset
Reservas de un City Hotel (Lisboa) y un Resort Hotel (Algarve), llegadas del 01/07/2015 al 31/08/2017. 119,390 filas y 32 variables (Antonio, de Almeida y Nunes, 2019, *Data in Brief*, 22, 41–49, https://doi.org/10.1016/j.dib.2018.11.126).
- `data/hotel_bookings_original.csv`: archivo original, sin modificar.
- `data/hotel_bookings_preparado.csv`: versión limpia (`hotel_limpio`, 87,396 reservas sin duplicados exactos).

## Estructura
```text
1ACC0216-TB1-2026-2-grupoXX/
│
├── README.md                          # Documentación principal del proyecto
├── data/                              # Directorio de conjuntos de datos
│   ├── hotel_bookings_original.csv    # Dataset base sin procesar
│   └── hotel_bookings_preparado.csv   # Dataset limpio tras la etapa de preparación
│
├── code/                              # Directorio de scripts
│   └── upc-grupoXX-tb1-codigo.R       # Código fuente principal en R
│
└── output/                            # Directorio de resultados
    └── gráficos/                      # Visualizaciones exportadas por el script

## Conclusiones principales
- El City Hotel concentra más reservas (61.1%) y cancela más (30.0% vs. 23.5% del Resort).
- Las reservas canceladas se hicieron con más anticipación (medianas 75 y 91 días vs. 40 y 34); Online TA y Groups son los segmentos con más cancelaciones.
- En 2016 enero fue el mes más bajo y agosto el más alto en ambos hoteles; el Resort tiene una temporada de verano mucho más marcada en tarifa y cancelaciones.
- Quitar los 31,994 duplicados cambia los porcentajes de cancelación pero no el orden entre hoteles.

## Licencia
Código y documentos bajo licencia MIT. El dataset pertenece a sus autores originales (Antonio et al., 2019; licencia CC BY 4.0).
