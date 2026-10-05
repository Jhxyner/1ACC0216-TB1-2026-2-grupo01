# ============================================================
# FUNDAMENTOS DE DATA SCIENCE (1ACC0216) - CICLO 2026-02
# TB1: Análisis exploratorio, visualización y comunicación de datos
# Caso: Hotel Booking Demand
# Grupo 01
# Integrantes:
#   Magallanes Arias, Cristian Marcelo - u202423870
#   Calampa Bardales, Jhoyner Arnold   - u202117290
#   Mesías Huatuco, Adrián Ricardo     - u202421946
#   Gonzales Rios, Omar Estefano       - u202423014
#
# Preguntas de negocio asignadas (Grupo 1):
#   1. ¿Qué tipo de hotel concentra una mayor cantidad de reservas y
#      cómo se comportan sus cancelaciones?
#   2. ¿Cómo varía la cantidad de reservas a lo largo de los meses y
#      qué patrones de demanda se observan según el tipo de hotel?
#
# Nota: abrir el proyecto de RStudio en la carpeta principal del
# repositorio (donde están Data/, code/ y output/) y guardar/abrir
# este script con codificación UTF-8 para que se vean las tildes.
# ============================================================

# ---- 0. Preparación del entorno ------------------------------
rm(list = ls(all = TRUE))
graphics.off()
cat("\014")

# Instalar una sola vez si fuera necesario
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("e1071", dependencies = TRUE)
# install.packages("patchwork", dependencies = TRUE)
# install.packages("scales", dependencies = TRUE)
library(tidyverse)
library(ggplot2)
library(e1071)
library(patchwork)
library(scales)

# Carpetas de salida
dir.create("output/gráficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)

# Estilo de los gráficos: a color
# Un color por hotel (azul = City Hotel, naranja = Resort Hotel)
color_hotel <- c("City Hotel" = "#2C7BB6", "Resort Hotel" = "#F28E2B")
# Tonos más claros para boxplots (así se ve la línea de la mediana)
color_caja <- c("City Hotel" = "#9ECAE1", "Resort Hotel" = "#FDD0A2")
# Estado de la reserva: verde = no cancelada, rojo = cancelada
color_estado <- c("No cancelada" = "#66C2A5", "Cancelada" = "#E15759")
# Versión clara del estado para boxplots
color_estado_caja <- c("No cancelada" = "#B2E2D2", "Cancelada" = "#F4A6A7")
# Color único para gráficos de una sola categoría
color_unico <- "#4E79A7"
tema_tb1 <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 8, hjust = 0),
        legend.position = "bottom")
fuente <- "Fuente: Hotel Booking Demand (Antonio et al., 2019). Elaboración propia."

guardar <- function(grafico, nombre, ancho = 8, alto = 4.8) {
  ggsave(filename = paste0("output/gráficos/", nombre, ".png"),
         plot = grafico, width = ancho, height = alto,
         units = "in", dpi = 300, bg = "white")
}

# ---- 1. Carga del dataset original ---------------------------
# Los textos "NULL" y "NA" se leen como valores faltantes (NA)
hotel_bookings <- read_csv(
  "Data/hotel_bookings.csv",
  na = c("", "NA", "NULL"),
  show_col_types = FALSE
)

# ---- 2. Inspección inicial -----------------------------------
dim(hotel_bookings)
nrow(hotel_bookings)
ncol(hotel_bookings)
names(hotel_bookings)
head(hotel_bookings)
glimpse(hotel_bookings)
str(hotel_bookings)
summary(hotel_bookings)

# Muestra de registros para el informe (Tabla 3)
muestra <- hotel_bookings |>
  select(hotel, is_canceled, lead_time, arrival_date_year,
         arrival_date_month, stays_in_week_nights, adults,
         market_segment, deposit_type, adr) |>
  head(5)
muestra
write.csv(muestra, "output/tablas/t03_muestra.csv", row.names = FALSE)

# Tipos de datos que lee R y conversiones necesarias
tipos <- data.frame(
  variable = names(hotel_bookings),
  tipo_en_R = sapply(hotel_bookings, function(x) class(x)[1]),
  row.names = NULL
)
tipos
write.csv(tipos, "output/tablas/t04_tipos_R.csv", row.names = FALSE)

# ---- 3. Calidad de datos -------------------------------------
# 3.1 Completitud: valores faltantes por variable
faltantes <- data.frame(
  variable = names(hotel_bookings),
  cantidad = colSums(is.na(hotel_bookings)),
  porcentaje = round(colMeans(is.na(hotel_bookings)) * 100, 2)
)
rownames(faltantes) <- NULL
faltantes <- faltantes[faltantes$cantidad > 0, ]
faltantes
write.csv(faltantes, "output/tablas/t05_faltantes.csv", row.names = FALSE)

# Los 4 registros sin dato de niños
hotel_bookings |>
  filter(is.na(children)) |>
  select(hotel, adults, children, babies, is_canceled)

# 3.2 Unicidad: no existe un ID de reserva, la unidad de análisis es
# la reserva y se buscan filas idénticas en todas las columnas
cat("Número de registros:", nrow(hotel_bookings), "\n")
cat("Registros duplicados:", sum(duplicated(hotel_bookings)), "\n")
cat("Porcentaje de duplicados:",
    round(mean(duplicated(hotel_bookings)) * 100, 2), "%\n")

duplicado <- duplicated(hotel_bookings)
dup_segmento <- hotel_bookings |>
  mutate(duplicado = duplicado) |>
  group_by(market_segment) |>
  summarise(reservas = n(),
            duplicados = sum(duplicado),
            porcentaje = round(sum(duplicado) / n() * 100, 1),
            .groups = "drop") |>
  arrange(desc(porcentaje))
dup_segmento
write.csv(dup_segmento, "output/tablas/t06_duplicados_segmento.csv", row.names = FALSE)

dup_hotel <- hotel_bookings |>
  mutate(duplicado = duplicado) |>
  group_by(hotel) |>
  summarise(reservas = n(),
            duplicados = sum(duplicado),
            porcentaje = round(sum(duplicado) / n() * 100, 1),
            .groups = "drop")
dup_hotel

dup_deposito <- hotel_bookings |>
  mutate(duplicado = duplicado) |>
  group_by(deposit_type) |>
  summarise(reservas = n(),
            duplicados = sum(duplicado),
            porcentaje = round(sum(duplicado) / n() * 100, 1),
            .groups = "drop")
dup_deposito

# Gráfico 1: duplicados por segmento de mercado
g1 <- ggplot(dup_segmento,
             aes(x = reorder(market_segment, porcentaje), y = porcentaje)) +
  geom_col(fill = color_unico) +
  geom_text(aes(label = paste0(porcentaje, "%")), hjust = -0.15, size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90)) +
  labs(title = "Los registros duplicados se concentran en pocos segmentos",
       subtitle = "Porcentaje de filas repetidas dentro de cada segmento de mercado",
       x = "Segmento de mercado", y = "Registros duplicados (%)",
       caption = fuente) +
  tema_tb1
g1
guardar(g1, "g01_duplicados_segmento", alto = 4)

# 3.3 Consistencia
# a) categorías "Undefined" en variables categóricas
hotel_bookings |> count(meal, sort = TRUE)
hotel_bookings |> count(market_segment, sort = TRUE)
hotel_bookings |> count(distribution_channel, sort = TRUE)

# b) coherencia entre is_canceled y reservation_status
table(hotel_bookings$is_canceled, hotel_bookings$reservation_status)

# c) fechas: se arma la fecha de llegada con año, mes y día
hotel_bookings |> count(arrival_date_month)
fechas_prueba <- hotel_bookings |>
  mutate(
    mes_num = match(arrival_date_month, month.name),
    fecha_llegada = as.Date(paste(arrival_date_year, mes_num,
                                  arrival_date_day_of_month, sep = "-")),
    fecha_estado = as.Date(reservation_status_date),
    noches = stays_in_weekend_nights + stays_in_week_nights,
    fecha_salida = fecha_llegada + noches
  )
cat("Fechas de llegada que no se pudieron armar:",
    sum(is.na(fechas_prueba$fecha_llegada)), "\n")
range(fechas_prueba$fecha_llegada)

# Reservas con check-out cuya fecha de salida no coincide con
# llegada + noches
fechas_prueba |>
  filter(reservation_status == "Check-Out", fecha_estado != fecha_salida) |>
  nrow()
# Reservas canceladas con fecha de estado posterior a la llegada
fechas_prueba |>
  filter(reservation_status != "Check-Out", fecha_estado > fecha_llegada) |>
  nrow()

# d) habitación asignada distinta a la reservada (solo informativo)
sum(hotel_bookings$assigned_room_type != hotel_bookings$reserved_room_type)

# 3.4 Validez
# a) tarifa diaria (adr)
cat("adr negativo:", sum(hotel_bookings$adr < 0), "\n")
cat("adr igual a 0:", sum(hotel_bookings$adr == 0), "\n")
hotel_bookings |>
  filter(adr < 0 | adr > 1000) |>
  select(hotel, adr, market_segment, arrival_date_year,
         arrival_date_month, stays_in_week_nights, is_canceled)
quantile(hotel_bookings$adr, c(0.25, 0.50, 0.75, 0.99))
# b) reservas sin ningún huésped y sin noches
hotel_bookings |>
  mutate(total_huespedes = adults + ifelse(is.na(children), 0, children) + babies) |>
  summarise(sin_huespedes = sum(total_huespedes == 0),
            sin_adultos = sum(adults == 0),
            sin_noches = sum(stays_in_weekend_nights + stays_in_week_nights == 0))
# c) rangos de otras variables
summary(hotel_bookings[, c("lead_time", "adults", "children", "babies",
                           "days_in_waiting_list",
                           "required_car_parking_spaces")])

# 3.5 Exactitud: no hay una regla de negocio ni una fuente externa para
# contrastar las tarifas, así que solo se revisó la coherencia interna
# (secciones 3.3 b y c).

# ---- 4. Preparación de los datos: hotel_limpio ---------------
# El dataset original NO se modifica. Todas las decisiones se
# aplican en una función para poder repetirlas con y sin duplicados.
preparar <- function(df) {
  df |>
    mutate(
      # Nombres de los meses en orden de calendario
      mes_num = match(arrival_date_month, month.name),
      mes = factor(mes_num, levels = 1:12,
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
      # Fechas
      fecha_llegada = as.Date(paste(arrival_date_year, mes_num,
                                    arrival_date_day_of_month, sep = "-")),
      fecha_mes = as.Date(paste(arrival_date_year, mes_num, "01", sep = "-")),
      reservation_status_date = as.Date(reservation_status_date),
      # Valores faltantes
      children = ifelse(is.na(children), median(children, na.rm = TRUE), children),
      country = ifelse(is.na(country), "Desconocido", country),
      agent = ifelse(is.na(agent), "Sin agencia", as.character(agent)),
      company = ifelse(is.na(company), "Sin empresa", as.character(company)),
      # Categorías inconsistentes
      meal = ifelse(meal == "Undefined", "SC", meal),
      market_segment = ifelse(market_segment == "Undefined", "No definido", market_segment),
      distribution_channel = ifelse(distribution_channel == "Undefined", "No definido", distribution_channel),
      # Variables nuevas
      noches_totales = stays_in_weekend_nights + stays_in_week_nights,
      total_huespedes = adults + children + babies,
      sin_huespedes = total_huespedes == 0,
      cero_noches = noches_totales == 0,
      estado_cancelacion = factor(if_else(is_canceled == 1, "Cancelada", "No cancelada"),
                                  levels = c("No cancelada", "Cancelada")),
      # Tarifa depurada: valores no positivos o extremos pasan a NA
      adr_depurado = ifelse(adr <= 0 | adr >= 5000, NA, adr)
    ) |>
    mutate(
      # Variables categóricas como factor
      across(c(hotel, meal, country, market_segment, distribution_channel,
               reserved_room_type, assigned_room_type, deposit_type, agent,
               company, customer_type, reservation_status), as.factor)
    )
}

# Versión sin duplicados (la que se usa en el análisis principal)
hotel_limpio <- preparar(distinct(hotel_bookings))
# Versión con duplicados (solo para la comparación de sensibilidad)
hotel_con_dup <- preparar(hotel_bookings)

cat("Filas del original:", nrow(hotel_bookings), "\n")
cat("Filas de hotel_limpio:", nrow(hotel_limpio), "\n")
cat("Filas eliminadas:", nrow(hotel_bookings) - nrow(hotel_limpio), "\n")

# Verificación antes y después
print("=== DESPUÉS: faltantes que quedan en hotel_limpio ===")
colSums(is.na(hotel_limpio))[colSums(is.na(hotel_limpio)) > 0]
hotel_limpio |> count(meal)
hotel_limpio |> count(market_segment)
hotel_limpio |> count(distribution_channel)
hotel_limpio |> summarise(sin_huespedes = sum(sin_huespedes),
                          cero_noches = sum(cero_noches),
                          adr_cero_o_menos = sum(adr <= 0),
                          adr_depurado_NA = sum(is.na(adr_depurado)))
str(hotel_limpio)

# Guardar la versión preparada
write_csv(hotel_limpio, "Data/hotel_bookings_preparado.csv")
list.files("Data/", pattern = "preparado")

# ---- 5. Análisis univariado ----------------------------------
# 5.1 Variable categórica: tipo de hotel
tabla_hotel <- hotel_limpio |>
  count(hotel) |>
  mutate(porcentaje = round(n / sum(n) * 100, 2))
tabla_hotel
write.csv(tabla_hotel, "output/tablas/t07_reservas_hotel.csv", row.names = FALSE)

g2 <- tabla_hotel |>
  ggplot(aes(x = reorder(hotel, n), y = n, fill = hotel)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(format(n, big.mark = ","), " (", porcentaje, "%)")),
            hjust = -0.1, size = 3.3) +
  scale_fill_manual(values = color_hotel) +
  scale_y_continuous(limits = c(0, 70000), labels = comma) +
  coord_flip() +
  labs(title = "El City Hotel concentra más reservas que el Resort Hotel",
       subtitle = "Número de reservas sin duplicados (n = 87,396)",
       x = "Tipo de hotel", y = "Número de reservas", caption = fuente) +
  tema_tb1
g2
guardar(g2, "g02_reservas_por_hotel", alto = 3.2)

# Otras variables categóricas relevantes
tabla_estado <- hotel_limpio |> count(estado_cancelacion) |> mutate(porcentaje = round(n / sum(n) * 100, 2))
tabla_estado
hotel_limpio |> count(reservation_status) |> mutate(porcentaje = round(n / sum(n) * 100, 2))
tabla_segmento <- hotel_limpio |> count(market_segment, sort = TRUE) |> mutate(porcentaje = round(n / sum(n) * 100, 2))
tabla_segmento
write.csv(tabla_segmento, "output/tablas/t07b_segmento_mercado.csv", row.names = FALSE)
hotel_limpio |> count(deposit_type, sort = TRUE) |> mutate(porcentaje = round(n / sum(n) * 100, 2))
hotel_limpio |> count(country, sort = TRUE) |> mutate(porcentaje = round(n / sum(n) * 100, 2)) |> head(5)

# 5.2 Variables numéricas: medidas descriptivas por hotel
desc_lead <- hotel_limpio |>
  group_by(hotel) |>
  summarise(variable = "lead_time",
            n = n(),
            media = round(mean(lead_time), 2),
            mediana = round(median(lead_time), 2),
            desv_est = round(sd(lead_time), 2),
            minimo = min(lead_time),
            maximo = max(lead_time),
            asimetria = round(skewness(lead_time, type = 2), 2),
            .groups = "drop")
desc_noches <- hotel_limpio |>
  group_by(hotel) |>
  summarise(variable = "noches_totales",
            n = n(),
            media = round(mean(noches_totales), 2),
            mediana = round(median(noches_totales), 2),
            desv_est = round(sd(noches_totales), 2),
            minimo = min(noches_totales),
            maximo = max(noches_totales),
            asimetria = round(skewness(noches_totales, type = 2), 2),
            .groups = "drop")
desc_adr <- hotel_limpio |>
  filter(!is.na(adr_depurado)) |>
  group_by(hotel) |>
  summarise(variable = "adr_depurado",
            n = n(),
            media = round(mean(adr_depurado), 2),
            mediana = round(median(adr_depurado), 2),
            desv_est = round(sd(adr_depurado), 2),
            minimo = min(adr_depurado),
            maximo = max(adr_depurado),
            asimetria = round(skewness(adr_depurado, type = 2), 2),
            .groups = "drop")
desc_num <- rbind(desc_lead, desc_noches, desc_adr)
desc_num
write.csv(desc_num, "output/tablas/t08_descriptivo_numericas.csv", row.names = FALSE)

# Gráfico 2b: distribución de la anticipación (lead_time)
g3 <- ggplot(hotel_limpio, aes(x = lead_time)) +
  geom_histogram(binwidth = 30, fill = color_unico, color = "white") +
  facet_wrap(~ hotel) +
  scale_y_continuous(labels = comma) +
  labs(title = "La mitad de las reservas se hace con 50 días de anticipación o menos",
       subtitle = "Distribución de los días entre la reserva y la llegada (cada barra agrupa 30 días)",
       x = "Anticipación de la reserva (días)", y = "Número de reservas",
       caption = fuente) +
  tema_tb1
g3
guardar(g3, "g03_histograma_lead_time", alto = 3.8)

# 5.3 Valores atípicos (regla 1.5 x RIC, como en la Hoja 4-1)
limites_iqr <- function(x, nombre) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  ric <- q3 - q1
  inf <- q1 - 1.5 * ric
  sup <- q3 + 1.5 * ric
  data.frame(variable = nombre,
             q1 = as.numeric(q1), q3 = as.numeric(q3),
             limite_inferior = as.numeric(inf),
             limite_superior = as.numeric(sup),
             n_atipicos = sum(x < inf | x > sup, na.rm = TRUE),
             porcentaje = round(sum(x < inf | x > sup, na.rm = TRUE) /
                                  sum(!is.na(x)) * 100, 2),
             maximo = max(x, na.rm = TRUE))
}
atipicos <- rbind(
  limites_iqr(hotel_limpio$lead_time, "lead_time"),
  limites_iqr(hotel_limpio$adr_depurado, "adr_depurado"),
  limites_iqr(hotel_limpio$noches_totales, "noches_totales")
)
atipicos
write.csv(atipicos, "output/tablas/t09_atipicos_iqr.csv", row.names = FALSE)

# Casos extremos que conviene revisar
hotel_limpio |> filter(lead_time > 600) |> count(hotel)
# La regla del RIC no sirve para adults (Q1 = Q3 = 2), se revisan los casos grandes
hotel_limpio |> filter(adults >= 10) |>
  select(hotel, adults, market_segment, customer_type, is_canceled)
hotel_limpio |> filter(noches_totales > 30) |> count(hotel)
hotel_limpio |> filter(adr >= 5000) |>
  select(hotel, adr, is_canceled, noches_totales)
hotel_limpio |> filter(lead_time > 365) |>
  summarise(reservas = n(),
            porc_canceladas = round(mean(is_canceled) * 100, 1))

g4a <- ggplot(hotel_limpio, aes(x = hotel, y = lead_time, fill = hotel)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = color_caja) +
  labs(title = "Anticipación de la reserva", x = NULL, y = "Días") +
  tema_tb1
g4b <- ggplot(hotel_limpio, aes(x = hotel, y = adr_depurado, fill = hotel)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = color_caja) +
  labs(title = "Tarifa diaria promedio", x = NULL, y = "Tarifa (adr)") +
  tema_tb1
g4 <- (g4a | g4b) +
  plot_annotation(
    title = "Hay valores extremos en la anticipación y en la tarifa diaria",
    subtitle = "Boxplots por tipo de hotel (tarifa solo con valores mayores que 0 y menores que 5,000)",
    caption = fuente,
    theme = theme(plot.title = element_text(face = "bold", size = 11),
                  plot.subtitle = element_text(size = 9),
                  plot.caption = element_text(size = 8, hjust = 0))
  )
g4
guardar(g4, "g04_boxplots_atipicos", alto = 4)

# ---- 6. Pregunta 1: hotel con más reservas y cancelaciones ----
# 6.1 Cancelaciones por hotel
canc_hotel <- hotel_limpio |>
  group_by(hotel) |>
  summarise(reservas = n(),
            canceladas = sum(is_canceled),
            tasa_cancelacion = round(mean(is_canceled) * 100, 2),
            .groups = "drop")
canc_hotel
write.csv(canc_hotel, "output/tablas/t10_cancelacion_hotel.csv", row.names = FALSE)

estado_hotel <- hotel_limpio |>
  count(hotel, reservation_status) |>
  group_by(hotel) |>
  mutate(porcentaje = round(n / sum(n) * 100, 2)) |>
  ungroup()
estado_hotel
write.csv(estado_hotel, "output/tablas/t11_estado_hotel.csv", row.names = FALSE)

composicion <- hotel_limpio |>
  count(hotel, estado_cancelacion) |>
  group_by(hotel) |>
  mutate(porcentaje = round(n / sum(n) * 100, 1)) |>
  ungroup()

g5 <- ggplot(composicion, aes(x = hotel, y = porcentaje, fill = estado_cancelacion)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(porcentaje, "%"), color = estado_cancelacion),
            position = position_stack(vjust = 0.5), size = 3.5,
            show.legend = FALSE) +
  scale_fill_manual(values = color_estado) +
  scale_color_manual(values = c("No cancelada" = "black", "Cancelada" = "white")) +  # color de las etiquetas
  labs(title = "El City Hotel tiene un mayor porcentaje de reservas canceladas",
       subtitle = "Porcentaje de reservas canceladas y no canceladas dentro de cada hotel",
       x = "Tipo de hotel", y = "Porcentaje de reservas", fill = NULL,
       caption = fuente) +
  tema_tb1
g5
guardar(g5, "g05_cancelacion_por_hotel", alto = 4)

# 6.2 Anticipación según cancelación
lead_canc <- hotel_limpio |>
  group_by(hotel, estado_cancelacion) |>
  summarise(reservas = n(),
            media = round(mean(lead_time), 1),
            mediana = median(lead_time),
            q1 = quantile(lead_time, 0.25),
            q3 = quantile(lead_time, 0.75),
            .groups = "drop")
lead_canc
write.csv(lead_canc, "output/tablas/t12_lead_time_cancelacion.csv", row.names = FALSE)

g6 <- ggplot(hotel_limpio, aes(x = hotel, y = lead_time, fill = estado_cancelacion)) +
  geom_boxplot() +
  scale_fill_manual(values = color_estado_caja) +
  labs(title = "Las reservas canceladas se hicieron con más anticipación",
       subtitle = "Días entre la reserva y la llegada, según tipo de hotel y estado de la reserva",
       x = "Tipo de hotel", y = "Anticipación (días)", fill = NULL,
       caption = fuente) +
  tema_tb1
g6
guardar(g6, "g06_lead_time_cancelacion", alto = 4.2)

# 6.3 Cancelaciones según segmento de mercado y hotel
canc_segmento <- hotel_limpio |>
  group_by(hotel, market_segment) |>
  summarise(reservas = n(),
            tasa_cancelacion = round(mean(is_canceled) * 100, 1),
            .groups = "drop") |>
  arrange(hotel, desc(tasa_cancelacion))
canc_segmento
write.csv(canc_segmento, "output/tablas/t13_cancelacion_segmento.csv", row.names = FALSE)

g7 <- canc_segmento |>
  filter(reservas >= 100) |>
  ggplot(aes(x = reorder(market_segment, tasa_cancelacion), y = tasa_cancelacion,
             fill = hotel)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(tasa_cancelacion, "%")),
            position = position_dodge(width = 0.9), hjust = -0.1, size = 2.8) +
  scale_fill_manual(values = color_hotel) +
  scale_y_continuous(limits = c(0, 45)) +
  coord_flip() +
  labs(title = "Online TA y Groups tienen las tasas de cancelación más altas",
       subtitle = "Porcentaje de reservas canceladas por segmento de mercado (segmentos con al menos 100 reservas)",
       x = "Segmento de mercado", y = "Reservas canceladas (%)", fill = NULL,
       caption = fuente) +
  tema_tb1
g7
guardar(g7, "g07_cancelacion_segmento", alto = 4.6)

# Casos que requieren revisión: tipo de depósito
canc_deposito <- hotel_limpio |>
  group_by(hotel, deposit_type) |>
  summarise(reservas = n(),
            tasa_cancelacion = round(mean(is_canceled) * 100, 1),
            .groups = "drop")
canc_deposito
write.csv(canc_deposito, "output/tablas/t14_cancelacion_deposito.csv", row.names = FALSE)

# ---- 7. Pregunta 2: reservas por mes y patrones de demanda ---
# 7.1 Serie mensual (año-mes de llegada) por hotel
serie_mensual <- hotel_limpio |>
  count(fecha_mes, hotel, name = "reservas")
serie_mensual
write.csv(serie_mensual, "output/tablas/t15_serie_mensual.csv", row.names = FALSE)

g8 <- ggplot(serie_mensual, aes(x = fecha_mes, y = reservas,
                                color = hotel, linetype = hotel, shape = hotel)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = color_hotel) +
  scale_x_date(
    breaks = seq(as.Date("2015-07-01"), as.Date("2017-07-01"), by = "3 months"),
    labels = function(x) paste(c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul",
                                 "Ago", "Sep", "Oct", "Nov", "Dic")[as.integer(format(x, "%m"))],
                               format(x, "%Y"))
  ) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(title = "Desde diciembre de 2015, el City Hotel recibe más reservas que el Resort Hotel cada mes",
       subtitle = "Reservas sin duplicados según mes de llegada, julio 2015 a agosto 2017",
       x = "Mes de llegada", y = "Número de reservas",
       color = NULL, linetype = NULL, shape = NULL, caption = fuente) +
  tema_tb1 +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g8
guardar(g8, "g08_serie_mensual", alto = 4.4)

# 7.2 Perfil mensual con el año 2016
# 2016 es el único año con los 12 meses completos. Julio y agosto
# aparecen en 3 años y los demás meses en 2, por eso no se mezclan años.
mensual_2016 <- hotel_limpio |>
  filter(arrival_date_year == 2016) |>
  group_by(hotel, mes) |>
  summarise(reservas = n(),
            tasa_cancelacion = round(mean(is_canceled) * 100, 1),
            adr_promedio = round(mean(adr_depurado, na.rm = TRUE), 1),
            .groups = "drop")
mensual_2016
write.csv(mensual_2016, "output/tablas/t16_resumen_mensual_2016.csv", row.names = FALSE)

g9 <- ggplot(mensual_2016, aes(x = mes, y = reservas, fill = hotel)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = color_hotel) +
  scale_y_continuous(labels = comma) +
  labs(title = "En 2016 la demanda de ambos hoteles fue más alta en agosto y más baja en enero",
       subtitle = "Número de reservas sin duplicados según mes de llegada, año 2016",
       x = "Mes de llegada", y = "Número de reservas", fill = NULL,
       caption = fuente) +
  tema_tb1
g9
guardar(g9, "g09_reservas_por_mes_2016", alto = 4.2)

# 7.3 Cancelaciones y tarifa promedio por mes (2016)
g10a <- ggplot(mensual_2016, aes(x = mes, y = tasa_cancelacion, group = hotel,
                                 color = hotel, linetype = hotel, shape = hotel)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = color_hotel) +
  labs(title = "Reservas canceladas (%)", x = "Mes de llegada", y = "Porcentaje",
       color = NULL, linetype = NULL, shape = NULL) +
  tema_tb1
g10b <- ggplot(mensual_2016, aes(x = mes, y = adr_promedio, group = hotel,
                                 color = hotel, linetype = hotel, shape = hotel)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = color_hotel) +
  labs(title = "Tarifa diaria promedio (adr)", x = "Mes de llegada", y = "Tarifa",
       color = NULL, linetype = NULL, shape = NULL) +
  tema_tb1
g10 <- (g10a | g10b) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "El Resort Hotel sube mucho su tarifa y sus cancelaciones en verano; el City Hotel cambia menos",
    subtitle = "Resultados por mes de llegada y tipo de hotel, año 2016",
    caption = fuente,
    theme = theme(plot.title = element_text(face = "bold", size = 11),
                  plot.subtitle = element_text(size = 9),
                  plot.caption = element_text(size = 8, hjust = 0),
                  legend.position = "bottom")
  )
g10
guardar(g10, "g10_cancelacion_tarifa_mes", ancho = 9, alto = 4.2)

# Mes de mayor y menor demanda por hotel (2016)
mensual_2016 |>
  group_by(hotel) |>
  filter(reservas == max(reservas) | reservas == min(reservas)) |>
  select(hotel, mes, reservas, tasa_cancelacion, adr_promedio)

# Reservas que sí se concretaron (no canceladas) por mes en 2016
mensual_2016_efectivas <- hotel_limpio |>
  filter(arrival_date_year == 2016, is_canceled == 0) |>
  count(hotel, mes, name = "reservas_no_canceladas")
mensual_2016_efectivas |>
  group_by(hotel) |>
  filter(reservas_no_canceladas == max(reservas_no_canceladas) |
           reservas_no_canceladas == min(reservas_no_canceladas))

# Totales por año y hotel (2015 y 2017 son años incompletos)
hotel_limpio |> count(arrival_date_year, hotel)

# ---- 8. Sensibilidad: resultados con y sin duplicados --------
sens_hotel <- rbind(
  hotel_limpio |>
    group_by(hotel) |>
    summarise(version = "Sin duplicados", reservas = n(),
              tasa_cancelacion = round(mean(is_canceled) * 100, 1),
              .groups = "drop"),
  hotel_con_dup |>
    group_by(hotel) |>
    summarise(version = "Con duplicados", reservas = n(),
              tasa_cancelacion = round(mean(is_canceled) * 100, 1),
              .groups = "drop")
) |>
  group_by(version) |>
  mutate(porcentaje_reservas = round(reservas / sum(reservas) * 100, 1)) |>
  ungroup()
sens_hotel
write.csv(sens_hotel, "output/tablas/t17_sensibilidad_duplicados.csv", row.names = FALSE)

sens_pico <- rbind(
  hotel_limpio |> filter(arrival_date_year == 2016) |> count(hotel, mes) |>
    group_by(hotel) |> filter(n == max(n) | n == min(n)) |>
    mutate(version = "Sin duplicados"),
  hotel_con_dup |> filter(arrival_date_year == 2016) |> count(hotel, mes) |>
    group_by(hotel) |> filter(n == max(n) | n == min(n)) |>
    mutate(version = "Con duplicados")
)
sens_pico
write.csv(sens_pico, "output/tablas/t18_sensibilidad_pico.csv", row.names = FALSE)

# Julio de 2015 en el City Hotel: efecto de los duplicados
hotel_con_dup |> filter(fecha_mes == as.Date("2015-07-01")) |> count(hotel)
hotel_limpio |> filter(fecha_mes == as.Date("2015-07-01")) |> count(hotel)

# ---- 9. Resultados clave para el informe ---------------------
cat("\n==== Resumen ====\n")
cat("Reservas City:", sum(hotel_limpio$hotel == "City Hotel"),
    " Resort:", sum(hotel_limpio$hotel == "Resort Hotel"), "\n")
print(canc_hotel)
print(lead_canc)

