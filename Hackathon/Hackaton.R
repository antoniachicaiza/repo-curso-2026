library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
#Cargamos los datos
#Base DESIP
proyectos<- read_excel("/Users/antonia/Library/Mobile Documents/com~apple~CloudDocs/UBA/ciencia de datos/Hackaton/base_completa.xlsx", sheet = "Proyectos")
glimpse(proyectos)

#Base SIPA 
sipa <- read_excel("/Users/antonia/Library/Mobile Documents/com~apple~CloudDocs/UBA/ciencia de datos/Hackaton/SIPA_ordenado agosto.xlsx", sheet = "A.5.2", skip=2)
glimpse(sipa)

# DESIP -------------------------------------------------------------------
#Nos quedamos con los datos que vamos a estar usando
#Nos sacamos de encima proyectos en evaluación y no aprobados porque no tienen disponible el dato de empleo
proyectos1 <- proyectos |>
  select(id_proyecto, 
         sector, 
         provincia, 
         empleos_directos_indirectos_informados, 
         estado_administrativo)|>
           filter(estado_administrativo == "Aprobado")
unique(proyectos1$provincia)
#vemos que existen proyectos que están localizados en más de una provincia, separamos las observaciones
#Decisión metodológica: el número de empleos declarados se divide en partes iguales para el número de provincias que reporta el proyecto
#La decisión es cuestionable si pensamos que no existe manera de afirmar que la distribución de empleos generados es uniforme,
#pero hay que elegir algo y la otra opción es imputar la totalidad a la provincia que aparece primero.

#Separamos los proyectos localizados en más de una provincia en observaciones individuales
proyectosxprov <- proyectos1|>
  separate_rows(provincia, sep = "; ")|>
  group_by(id_proyecto) |>
  mutate(
    n_provincias = n(),
    empleoxprov = empleos_directos_indirectos_informados/n_provincias,
  )|>
  ungroup()
#Calculamos el empleo directo e indirecto informado total por provincia
#Generamos missings (deberían ser cero, dado que limpiamos la base al inicio)
empleo_tot_xprov <- proyectosxprov|>
  group_by(provincia)|>
  summarise(empleo_rigixprov = sum(empleoxprov, na.rm =TRUE),
            missing = any(is.na(empleoxprov)))
empleo_tot_xprov %>% arrange(desc(empleo_rigixprov))

# SIPA --------------------------------------------------------------------

#Nos quedamos con el dato de empleo registrado por provincia del último periodo disponible, es un dato provisorio, así que está sujeto a cambios menores en el futuro (mayo 2026)
#Ponemos en unidades comparables la serie (ahora está en miles de personas)
sipa_may2026 <- sipa |>
  mutate(Periodo = as.Date(suppressWarnings(as.numeric(Período)), origin = "1899-12-30")) |>
  filter(Periodo == max(Periodo, na.rm = TRUE)) |>
  select(-Período, -starts_with("...")) |>
  rename_with(~ str_replace_all(., "[\r\n]+", " ") %>% str_squish())|>
  mutate(across(where(is.numeric), ~ .x * 1000))

glimpse(sipa_may2026)

#Pasamos a long la serie
sipa_long<- sipa_may2026|>
  select(-Periodo, -`TOTAL PAIS`)|>
  pivot_longer(
    cols = everything(),
    names_to = "provincia",
    values_to = "empleo_privado_sipa"
  )
sipa_long

#Normalizamos los valores de provincia en la base sipa para hacer el join
sipa_long <- sipa_long |>
  mutate(provincia_norm = stringi::stri_trans_general(provincia, "Latin-ASCII")|>
           str_to_lower())

sipa_long

#Normalizamos los valores de provincia en la base desip...
proyectosxprov <- proyectosxprov |>
  mutate(provincia_norm = stringi::stri_trans_general(provincia, "Latin-ASCII") |>
           str_to_lower())

proyectosxprov |> select(provincia, provincia_norm) |> distinct()


empleo_tot_xprov <- empleo_tot_xprov |>
  mutate(provincia_norm = stringi::stri_trans_general(provincia, "Latin-ASCII") |>
           str_to_lower())

#Hacemos join por provincia
empleo_comparado <- empleo_tot_xprov |>
  left_join(sipa_long |>
              select(provincia_norm, empleo_privado_sipa), 
            by = "provincia_norm") |>
  mutate(ratio = empleo_rigixprov / empleo_privado_sipa * 100)

empleo_comparado

#Miramos el resultado ordenado
empleo_comparado |>
  select(provincia, empleo_rigixprov, empleo_privado_sipa, ratio) |>
  arrange(desc(ratio))

# Visualización de datos --------------------------------------------------
# Barras de proporción de empleo ------------------------------------------
barrasxprov<-ggplot(empleo_comparado, aes(y = reorder(provincia, ratio))) +
  geom_col(aes(x = 1), fill = "grey85") +
  geom_col(aes(x = ratio / 100), fill = "#1b3a5c") +
  geom_text(aes(x = ratio / 100, label = paste0(round(ratio, 1), "%")),
            hjust = -0.15, size = 3, color = "#1b3a5c",
            family = "poppins", fontface = "bold") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = NULL, y = NULL) + 
  theme(text = element_text(family = "poppins"),
        panel.grid.major.x = element_line(color = "white", linewidth = 0.15),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 11))
barrasxprov <- barrasxprov +
  labs(
    title = "Empleo proyectado por RIGI como % del empleo \n privado formal provincial"
  ) +
  theme(
    plot.title = element_text(family = "poppins", face = "bold", size = 14, hjust = 0.5)
  )
ggsave("/Users/antonia/Library/Mobile Documents/com~apple~CloudDocs/UBA/ciencia de datos/Hackaton/empleo_rigi_por_provincia.jpg", plot = barrasxprov, width = 7, height = 6, dpi = 300)


# Mapa de calor proporción empleo planeado -------------------------------------------
#Traemos el mapa de provincias de Argentina
argentina <- ne_states(country = "Argentina", returnclass = "sf")

#Nos quedamos con lo necesario y normalizamos el nombre de provincia, igual que con SIPA
argentina_sel <- argentina |>
  select(name) |>
  mutate(provincia_norm = stringi::stri_trans_general(name, "Latin-ASCII") %>% 
           str_to_lower())

#Cruzamos el mapa con los datos de empleo RIGI/SIPA
mapa_datos <- argentina_sel |>
  left_join(empleo_comparado |> select(provincia_norm, empleo_rigixprov, ratio), 
            by = "provincia_norm")

#Chequeo: confirmamos que las provincias con NA sean las que no tienen proyecto aprobado
mapa_datos |> 
  st_drop_geometry() |> 
  filter(is.na(ratio)) |> 
  select(name)

#Agrupamos el ratio en categorías para que el mapa no pierda variación entre provincias chicas
mapa_datos <- mapa_datos |>
  mutate(ratio_categoria = cut(ratio, 
                               breaks = c(0, 1, 5, 25, 100),
                               labels = c("0-1%", "1-5%", "5-25%", "25-55%"),
                               include.lowest = TRUE))

#Color de etiqueta según qué tan oscuro es el fondo de cada categoría
mapa_datos <- mapa_datos |>
  mutate(color_texto = if_else(ratio_categoria %in% c("5-25%", "25-55%"), "white", "#08306b"))

#Mapa de empleo RIGI relativo al empleo formal privado provincial
mapa_relativo <- ggplot(mapa_datos) +
  geom_sf(aes(fill = ratio_categoria), color = "white", linewidth = 0.3) +
  geom_sf_text(data = mapa_datos |> filter(!is.na(ratio)),
               aes(label = paste0(round(ratio, 1), "%"), color = color_texto),
               family = "poppins", fontface = "bold", size = 1.5) +
  scale_fill_manual(values = c("0-1%" = "#deebf7", "1-5%" = "#9ecae1", 
                               "5-25%" = "#4292c6", "25-55%" = "#08306b"),
                    na.value = "grey90",
                    na.translate = TRUE,
                    name = NULL) +
  scale_color_identity() +
  theme_void(base_family = "poppins") +
  theme(legend.position = "right")
#Le agregamos título y nota aclaratoria
mapa_relativo <- mapa_relativo +
  labs(
    title = str_wrap("Empleo proyectado por RIGI como % del empleo privado formal provincial", width = 45),
    caption = str_wrap("Empleos directos e indirectos informados por proyectos aprobados (may. 2026) respecto al empleo privado formal personas con empleo asalariado en el sector privado por provincia, SIPA (may 2026)",
      width = 90
    )
  ) +
  theme(
    plot.title = element_text(family = "poppins", face = "bold", size = 13, hjust = 0.5),
    plot.caption = element_text(family = "poppins", size = 7, color = "grey40", hjust = 0)
  )
mapa_relativo
ggsave("/Users/antonia/Library/Mobile Documents/com~apple~CloudDocs/UBA/ciencia de datos/Hackaton/mapa empleo relativo.jpg", plot = mapa_relativo, width = 7, height = 6, dpi = 300)

# Mapa de calor empleo planeado absolutos ---------------------------------
#Agrupamos el empleo absoluto en categorías, con cortes acordes a la escala de personas
mapa_datos <- mapa_datos |>
  mutate(empleo_categoria = cut(empleo_rigixprov, 
                                breaks = c(0, 1000, 5000, 10000, 50000),
                                labels = c("0-1.000", "1.000-5.000", "5.000-10.000", "10.000-45.000"),
                                include.lowest = TRUE),
         color_texto_abs = if_else(empleo_categoria %in% c("5.000-10.000", "10.000-45.000"), "white", "#08306b"))

#Mapa de empleo RIGI en términos absolutos
mapa_absoluto <- ggplot(mapa_datos) +
  geom_sf(aes(fill = empleo_categoria), color = "white", linewidth = 0.3) +
  geom_sf_text(data = mapa_datos |> filter(!is.na(empleo_rigixprov)),
               aes(label = scales::comma(round(empleo_rigixprov), big.mark = "."), color = color_texto_abs),
               family = "poppins", fontface = "bold", size = 1.5) +
  scale_fill_manual(values = c("0-1.000" = "#deebf7", "1.000-5.000" = "#9ecae1", 
                               "5.000-10.000" = "#4292c6", "10.000-45.000" = "#08306b"),
                    na.value = "grey90",
                    na.translate = TRUE,
                    name = NULL) +
  scale_color_identity() +
  theme_void(base_family = "poppins") +
  theme(legend.position = "right") +
  labs(
    title = str_wrap("Empleo proyectado por RIGI, en cantidad de personas", width = 45),
    caption = str_wrap(
      "Empleos directos e indirectos informados por proyectos aprobados (may. 2026), por provincia.",
      width = 90
    )
  ) +
  theme(
    plot.title = element_text(family = "poppins", face = "bold", size = 13, hjust = 0.5),
    plot.caption = element_text(family = "poppins", size = 7, color = "grey40", hjust = 0)
  )

mapa_absoluto
ggsave("/Users/antonia/Library/Mobile Documents/com~apple~CloudDocs/UBA/ciencia de datos/Hackaton/mapa empleo absoluto.jpg", plot = mapa_absoluto, width = 7, height = 6, dpi = 300)


