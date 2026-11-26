# Carga de librerías y directorio de trabajo ------------------------------
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(tidyr)
setwd("/Volumes/Crucial X9/Ciencia de datos/bases")

# Carga de bases ----------------------------------------------------------
base <- read_csv("MLER.csv",
                 col_select = c(id_trabajador, id_relacion, tiempo, rem_tot,
                                r34, r32, letra, tramo_empleo, fnacim, sexo),
                 col_types = cols(.default = col_integer(), rem_tot = col_double()))
filas_total <- nrow(base)   
#Decisión metodológica: analizamos datos hasta diciembre de 2019 para evitar años de pandemia
base <- base |> filter(tiempo <= 201912)
filas_2019 <- nrow(base)  
jerarquia <- base |> distinct(r34, r32, letra)
n_partes <- 20
dir.create("conteos", showWarnings = FALSE)                       
saveRDS(c(filas_total = filas_total, filas_2019 = filas_2019),    
        "conteos/inicio.rds")
partes_base <- split(base, base$id_trabajador %% n_partes)
rm(base); invisible(gc())

ipc_empalme<-read_excel("IPC_empalme1.xlsx")
ipc_empalme <- ipc_empalme|>
  mutate(tiempo = year(Date) * 100 + month(Date))

# Diccionario de ramas ----------------------------------------------------
dic_r34 <- read_excel("anexo_estadistico_y_descriptores.xlsx",
                      sheet = "DESC_R34", skip = 3,
                      col_names = c("r34", "nombre_r34"))
dic_r34 <- dic_r34 |> left_join(jerarquia, by = "r34")

# Umbrales ----------------------------------------------------------------
ipc_base <- ipc_empalme$`IPC Empalme base 2016`[ipc_empalme$tiempo == 202504]
umbral_j2j <- 3
piso_abr03 <- 12   # $ brutos de abril 2003 Umbral arbitrario de salario mínimo (Kennedy, Paola y Weksler, 2025, p. 15)
ipc_abr03 <- ipc_empalme$`IPC Empalme base 2016`[ipc_empalme$tiempo == 200304]
piso_real <- piso_abr03 * ipc_base / ipc_abr03

procesar_parte <- function(base) {
  
  # Deflactor de salario ----------------------------------------------------
  
  base <- base |>
    left_join(ipc_empalme |> select(tiempo, ipc = `IPC Empalme base 2016`),
              by = "tiempo")
  
  base <- base |>
    mutate(rem_real = rem_tot / ipc * ipc_base)
  
  
  # identificamos diciembre y junio -----------------------------------------
  #Decisión metodológica: no tomamos para el análisis meses con pago de aguinaldo porque no es posible determinar el monto del mismo
  #Decisión metodológica: Siguiendo la metodología de Kennedy Paola y Weksler, se fija un piso salarial para excluir remuneraciones exageradamente bajas que pueden corresponder a errores en el registro.
  #El piso equivale a 12 pesos brutos en abril de 2003 (tomado de Kennedy también)
  base <- base |>
    mutate(mes = tiempo %% 100,
           sac = as.integer(mes %in% c(6, 12))) # 1=junio o diciembre, 0=otro
  
  base <- base |>
    mutate(rem_comparable = if_else(sac == 0 & rem_real >= piso_real, rem_real, NA_real_))
  
  # Tratamiento del pluriempleo ---------------------------------------------
  #En los casos de full overlap entre relaciones laborales (relaciones que arrancan y terminan al mismo tiempo), se conserva la de mayor ingreso.
  #En los casos donde el overlap es parcial, nos quedamos con la relación más antigua para evitar inventar transiciones inexistentes hacia y desde empleos temporarios superpuestos a uno de larga duración.
  #Si uno de los empleos está en la rama de educación, nos quedamos con el otro. 
  base <- base |>
    mutate(anio    = tiempo %/% 100,
           mes_abs = anio * 12 + mes)
  
  rango_relacion <- base |>
    group_by(id_trabajador, id_relacion) |>
    summarise(inicio_rel   = min(mes_abs),
              fin_rel      = max(mes_abs),
              rem_prom_rel = mean(rem_comparable, na.rm = TRUE),
              .groups = "drop")
  
  pluri <- base |>
    left_join(rango_relacion, by = c("id_trabajador", "id_relacion")) |>
    group_by(id_trabajador, tiempo) |>
    filter(n() > 1) |>
    summarise(educacion        = any(r32 == 80) & any(r32 != 80),
              overlap_perfecto = n_distinct(inicio_rel) == 1 & n_distinct(fin_rel) == 1,
              .groups = "drop") |>
    mutate(regla = case_when(educacion        ~ "Educacion",
                             overlap_perfecto ~ "Mayor salario (Kennedy)",
                             TRUE             ~ "Más antiguo"))
  
  base <- base |>
    left_join(rango_relacion, by = c("id_trabajador", "id_relacion")) |>
    arrange(id_trabajador, tiempo, r32 == 80, inicio_rel, desc(fin_rel), desc(rem_prom_rel)) |>
    group_by(id_trabajador, tiempo) |>
    slice(1) |>
    ungroup()
  
  #Control:
  nrow(base) #es el número de observaciones
  base |> count(id_trabajador, tiempo) |> filter(n > 1) |> nrow() #debe dar cero, es el número de trabajadores con pluriempleo
 
   # Identificador de Job Spell ----------------------------------------------
  #un job spell es un tramo continuo con un mismo empleador.
  #Decisión metodológica: Identificamos el inicio de un tramo si:
  #1. Cambia el empleo: el id_relacion es distinto al del mes anterior.
  
  base <- base |>
    arrange(id_trabajador, mes_abs) |>
    group_by(id_trabajador) |>
    mutate(nuevo_spell = id_relacion != lag(id_relacion, default = -1) |
             mes_abs - lag(mes_abs, default = -99) > umbral_j2j + 1,
           spell = cumsum(nuevo_spell)) |>
    ungroup()
  
  
  #Agrupamos spells. Y computamos el salario de cada uno.
  #Decisión metodológica: Se computan dos salarios por cada job spell:
  #1.salario de inicio: mediana de 2 a 4 meses desde el inicio de la relación en términos reales excluyendo junio, diciembre y salarios debajo del umbral arbitrario.
  #2.salario al final: mediana de los 3 meses previos al cambio de relación en términos reales excluyendo junio, diciembre y debajo del umbral arbitrario.
  spells <- base |>
    group_by(id_trabajador, spell) |>
    summarise(
      id_relacion = first(id_relacion),
      inicio      = first(mes_abs),
      fin         = last(mes_abs),
      tiempo_fin  = last(tiempo),
      r34    = first(r34),
      r32    = first(r32),
      tamano = first(tramo_empleo),
      sal_final  = median(rem_comparable[mes_abs >= fin - 3 & mes_abs < fin], na.rm = TRUE),
      sal_inicio = median(rem_comparable[mes_abs > inicio & mes_abs <= inicio + 3], na.rm = TRUE),
      .groups = "drop")
  
  #Agrupamos Spells encadenados:
  #Definición: Un episodio es una transición de un spell al siguiente. Es una cadena de Markov simple, no tomamos cadenas de orden superior.
  episodios<- spells|>
    arrange(id_trabajador, inicio)|>
    group_by(id_trabajador)|>
    mutate(id_rel_dest = lead(id_relacion),
           inicio_dest = lead (inicio),
           r34_dest = lead(r34),
           r32_dest = lead(r32),
           tamano_dest = lead(tamano),
           sal_dest = lead(sal_inicio))|>
    ungroup()|>
    rename(r34_orig = r34, r32_orig = r32,
           tamano_orig = tamano, sal_orig = sal_final)
  
  # Decisión metodológica: un episodio entra al análisis si la salida del empleo de origen
  # ocurre entre enero de 2003 y agosto de 2019. Así, cada salida tiene 3 meses de
  # seguimiento observados antes del corte de la base (diciembre de 2019).
  episodios <- episodios |>
    filter(tiempo_fin >= 200301,
           tiempo_fin <= 201908) |>
    mutate(nonemp_months = inicio_dest - fin - 1,
           tipo = if_else(!is.na(nonemp_months) & nonemp_months <= umbral_j2j, "j2j", "no_j2j"),
           recall = id_rel_dest == id_relacion,
           dlog_wage = log(sal_dest) - log(sal_orig))
  
  #Armado de tabla
  personas <- base |> distinct(id_trabajador, fnacim, sexo)
  episodios <- episodios |>
    left_join(personas, by = "id_trabajador") |>
    mutate(edad = tiempo_fin %/% 100 - fnacim)

parte <- base$id_trabajador[1] %% 20
  saveRDS(list(persona_mes = nrow(base),
               personas    = n_distinct(base$id_trabajador),
               spells      = nrow(spells),
               pluri       = pluri),
          paste0("conteos/parte_", parte, ".rds"))
  episodios 
}

episodios <- list()
for (k in names(partes_base)) {
  cat("Parte", k, "\n")
  episodios[[k]] <- procesar_parte(partes_base[[k]])
  partes_base[[k]] <- NULL; invisible(gc())   
}
episodios <- bind_rows(episodios)


# Matriz de flujos --------------------------------------------------------
#Nos quedamos con los episodios job-to-job que tienen un destino que sea otra rama y contamos transiciones entre ramas
flechas <- episodios |>
  filter(tipo == "j2j",
         !recall,
         r34_orig != 0, r34_dest != 0) |>
  count(tipo, r34_orig, r34_dest, name = "episodios")

# Nombres de las ramas ----------------------------------------------------
flechas <- flechas |>
  left_join(dic_r34 |> select(r34, nombre_orig = nombre_r34), by = c("r34_orig" = "r34")) |>
  left_join(dic_r34 |> select(r34, nombre_dest = nombre_r34), by = c("r34_dest" = "r34"))

# Matriz de transición ----------------------------------------------------
transicion <- flechas |>
  group_by(tipo, r34_orig) |>
  mutate(prob = episodios / sum(episodios)) |>
  ungroup()

#Control:
transicion |>
  group_by(tipo, r34_orig) |>
  summarise(suma = sum(prob), .groups = "drop") |>
  pull(suma) |>
  range()
View(transicion)

# Matriz de transición en long --------------------------------------------
matriz_j2j <- transicion |>
  filter(tipo == "j2j") |>
  select(nombre_orig, nombre_dest, prob) |>
  pivot_wider(names_from  = nombre_dest,   # cada destino pasa a ser una columna
              values_from = prob,       # lo que va en las celdas
              values_fill = 0,          # los pares sin episodios quedan en 0
              names_sort  = TRUE) |>    # columnas ordenadas por código
  arrange(nombre_orig)

View(matriz_j2j)


# Guardamos la red --------------------------------------------------------
write_csv(episodios, "episodios_mler.csv")
write_csv(flechas,   "flechas_mler.csv")
