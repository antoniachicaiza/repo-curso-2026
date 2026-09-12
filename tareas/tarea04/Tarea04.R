
# Data transformation -----------------------------------------------------
library(nycflights13)
library(tidyverse)
library(maps)
#install.packages("maps")
#install.packages("Lahman")
#install.packages("babynames")
flights
glimpse(flights)
#int (integer), dbl(real numbers), dttm (date time), chr (string/character)
# Dplyr basics, verbs for data manipulation -------------------------------
#El argumento siempre es una tabla
#Las columnas suelen ser nombres de variables y las filas son observaciones
#El pipe es un "then"
#Hay verbos específicos para filas y verbos específicos para columnas.
flights |>
  filter(dest == "IAH") |>
  group_by(year,month, day)|>
  summarise(
    arr_delay = mean (arr_delay, na.rm = TRUE)
  )

# Verbs for rows ----------------------------------------------------------
# Filter ------------------------------------------------------------------
#Llama filas dependiendo de los valores de las columnas/variables.El primer argumento es la tabla y los siguientes son las condiciones para mantener la fila.
flights |>
  filter(dep_delay > 120)
#Filter con operaciones lógicas
# Conjunción: & "," 
flights |> 
  filter(month == 1, day == 1)
flights |> 
  filter(month == 1 & day == 1)
#Disyunción: | '$in$ c(x,x)'
flights |> 
  filter(month == 1 | month == 2)
flights |> 
  filter(month %in% c(1,2))
#Filter no modifica la tabla, si se quiere conservar el resultado debe generarse un objeto
jan1 <- flights |> 
  filter (month == 1, day == 1)
jan1

# Arrange -----------------------------------------------------------------
#Cambia el orden de las filas dependiendo del valor de las variables en las columnas.
#Ej. year 1 month 1 day 1 otras variables
#.   year 1 month 1 day 2 otras variables
#.   year 1 month 2 day 1 otras variables
#.   year 1 month 2 day 2 otras variables
#Por default arrange ordena de menor a mayor, para cambiarlo usamos arrange(desc(variable1, variable2, etc))
flights |> 
  arrange(year, month, day, dep_time)
#desc(variable) solo reconoce un argumento, es decir solo acepta una variable
flights |> 
  arrange (desc(dep_delay))

# Distrinct ---------------------------------------------------------------
#encuentra filas con valores únicos en la tabla
#Para eliminar observaciones repetidas
flights |>
  distinct()
#Para encontrar pares de salidas y destinos únicos
flights |> 
  distinct(origin, dest)
#Para mantener las demás columnas al filtrar combinaciones únicas, usamos .keep_all = TRUE
flights |> 
  distinct(origin, dest, .keep_all = TRUE)

# Count -------------------------------------------------------------------
#Para contar el número de observaciones con una combinación, usamos count. 
#sort = TRUE ordena descendentemente por frecuencia
flights |> 
  count(origin, dest, sort = TRUE)

# Exercises ---------------------------------------------------------------
#Find all the flights that:
#had an arrival delay of two or more hours
flights |>
  filter(arr_delay >= 120)
#flew to Houston IAH HOU
flights |> 
  filter (dest == "IAH" | dest == "HOU") 
#were operated by United, Amerian or delta
flights |> 
  filter (carrier == "UA" | carrier == "AA" | carrier == "DL") 
#departed in summer
flights|>
  filter(month %in% c(7,8,9)) 
#arrived more than two hours late but left on time
flights|> 
  filter (arr_delay >= 120 & dep_delay <= 0)
#were delayed by at least an hour but made up over 30 minutes in flight. 
flights |> 
  filter(dep_delay >= 60, dep_delay - arr_delay > 30 )


# Verbs for columns -------------------------------------------------------

# Mutate ------------------------------------------------------------------
#Creates new columns derived from the existing columns. 
#para generar más de una columna, se separa con comas cada variable
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time*60
  )
#Por default las columnas nuevas se agregan al final de la tabla. Con '.before' se agregan al inicio
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time*60,
    .before = 1
  )
# =1 lo pone en la primera columna, si quisiera ver la variable nueva en la segunda columna basta con .before = 2
#Se puede usar también la función .after = columna. 
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed= distance / air_time, 
    .after = day
  )
#Se puede llamar únicamente las columnas creadas usando .keep = "used"
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    speed = distance / air_time *60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )
# Select ------------------------------------------------------------------
#Changes the existing columns
flights |> 
  select (year, month, day)
#Seleccionar las columnas dentro de un intervalo.
flights |> 
  select (year : day)
#Seleccionar todas las columnas excluyendo un intervalo. Usamos !
flights |> 
  select(! month: day)
#Seleccionar columnas por tipo de dato
flights |> 
  select(where (is.character)) 
#Seleccionar y renombrar columnas
flights |> 
  select(til_num = tailnum)
# Rename ------------------------------------------------------------------
#Changes the names of the columns
#Para cambiar nombres de variables y seguir viendo las demás variables, se usa rename
flights |> 
  rename(tail_num = tailnum) |> 
  relocate (tail_num)
# Relocate ----------------------------------------------------------------
#Changes positions of the columns
flights |> 
  relocate (time_hour, air_time)
#Usando .before y .after
flights |> 
  relocate (year:dep_time, .after = time_hour)
flights |> 
  relocate (starts_with ("arr"), .before = dep_time)

# Exercises ---------------------------------------------------------------

#Ways to select  dep_time, sched_dep_time and dep_delay
flights |> 
  select (dep_time:dep_delay)
flights |> 
  select (dep_time, dep_delay, sched_dep_time)
flights |>
  select (starts_with("dep_")| starts_with ("sched_d"))
flights |> 
  select (contains("dep"))
#specifying the same variable multiple times...
flights |> 
  select (dep_time, dep_time, dep_time)
#Any_of
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |> 
  select(any_of(variables))
#Default with upper and lower case
flights |> select(contains("TIME"))
#Spot the mistake
flights |> 
  select(tailnum) |>
  arrange(arr_delay)
#Changing the order of the pipe
flights |> 
  arrange(arr_delay) |> 
  select(tailnum)

# The Pipe ----------------------------------------------------------------

flights |> 
  filter(dest == "IAH") |>
  mutate(speed = distance / air_time*60) |>
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange (desc(speed))
#The fastest flights to Houston's AIH airport are from United Airlines
#Without the pipe, nesting is the other option
arrange(
  select(
    mutate(
      filter(
        flights, 
        dest == "IAH"
      ),
      speed = distance / air_time * 60
    ),
    year:day, dep_time, carrier, flight, speed
  ),
  desc(speed)
)
#Using intermediate objects 
#Usar objetos intermedios innútiles llena de "basura" nuestro espacio de trabajo.
flights1 <- filter(flights, dest == "IAH")
flights2 <- mutate(flights1, speed = distance / air_time * 60)
flights3 <- select(flights2, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

# Groups ------------------------------------------------------------------
#Group by: divides the dataset into groups based on a chosen variable.
flights |> 
  group_by(month)
#Summarise
flights |> 
  group_by(month) |>
  summarize (
    avg_delay = mean(dep_delay)
  )
#Removing NA values with na.rm= TRUE
flights |> 
  group_by(month) |>
  summarize (
    avg_delay = mean(dep_delay, na.rm= TRUE)
  )
#Para ver el número de observaciones en cada grupo... n=n()
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )
#Slice functions: extracts rows, first, last, min, max, random
flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 2) |>
  relocate(dest)
#Grouping by multiple variables
daily <- flights |>  
  group_by(year, month, day)
daily
daily_flights <- daily |> 
  summarize(n = n())
daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
  )
daily |> 
  ungroup()
daily |> 
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    flights = n()
  )

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = month
  )
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )

# Exercises ---------------------------------------------------------------
#Which carrier has the worst average delays? 
flights1 <- flights |>
  group_by(carrier, dest) |>
  summarise(
    delay=mean(arr_delay, na.rm = TRUE),
    n= n()
    )
arrange(flights1, desc(delay))
#Most delayed upon departure to each destination flights
flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1) |>
  relocate(dest)
#How do delays vary over the course of the day
flights |>
  group_by(hour = sched_dep_time %/% 100) |>
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(x = hour, y = avg_delay)) +
  geom_line()
#Negative argument in slice_min
#Trae todas las filas menos la que tiene el calor más alto
flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = -1) |>
  relocate(dest)
#Count: returns the number of observations for each distinct category
flights|>
  count(month, day)
#Agregates and sample size
batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters

batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) + 
  geom_smooth(se = FALSE)
batters |> 
  arrange(desc(performance))

# Joins -------------------------------------------------------------------
# Primary and foreign key ------------------------------------------------
airlines
airports
#Verificamos que la correspondencia es unívoca en la clave primaria
planes |> 
  count(tailnum) |> 
  filter(n > 1)
weather |> 
  count(time_hour, origin) |> 
  filter(n > 1)
#Verificamos que no haya missing values
planes |> 
  filter(is.na(tailnum))
weather |> 
  filter(is.na(time_hour) | is.na(origin))
#Surrogate key
flights2 <- flights |> 
  mutate(id = row_number(), .before = 1)
flights2

# Basic Joins -------------------------------------------------------------
flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2
#Left join: agrega columnas a las observaciones existentes
flights2 |>
  left_join(airlines)
flights2 |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))
flights2 |> 
  left_join(planes |> select(tailnum, type, engines, seats))
#Si no existe correspondencia en una fila, el default es NA
flights2 |> 
  filter(tailnum == "N3ALAA") |> 
  left_join(planes |> select(tailnum, type, engines, seats))
#Specifying join keys
flights2 |> 
  left_join(planes)
flights2 |> 
  left_join(planes, join_by(tailnum))
flights2 |> 
  left_join(airports, join_by(dest == faa))
flights2 |> 
  left_join(airports, join_by(origin == faa))
#Filtering joins
#Semi-join: keeps all the rows in x that have a match un y
airports |> 
  semi_join(flights2, join_by(faa == origin))
airports |> 
  semi_join(flights2, join_by(faa == dest))
#Anti-join: returns all rows in x that don't have a match in y. Useful for finding missing values
flights2 |> 
  anti_join(airports, join_by(dest == faa)) |> 
  distinct(dest)
flights2 |>
  anti_join(planes, join_by(tailnum)) |> 
  distinct(tailnum)

# How do joins work -------------------------------------------------------
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

df1 <- tibble(key = c(1, 2, 2), val_x = c("x1", "x2", "x3"))
df2 <- tibble(key = c(1, 2, 2), val_y = c("y1", "y2", "y3"))

df1 |> 
  inner_join(df2, join_by(key))
#Non-equi-joins
x |> inner_join(y, join_by(key == key), keep = TRUE)
#Cross joins
df <- tibble(name = c("John", "Simon", "Tracy", "Max"))
df |> cross_join(df)
#Inequality joins
df <- tibble(id = 1:4, name = c("John", "Simon", "Tracy", "Max"))
df |> inner_join(df, join_by(id < id))
#Rolling joins
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03"))
)
set.seed(123)
employees <- tibble(
  name = sample(babynames::babynames$name, 100),
  birthday = ymd("2022-01-01") + (sample(365, 100, replace = TRUE) - 1)
)
employees
employees |> 
  left_join(parties, join_by(closest(birthday >= party)))
employees |> 
  anti_join(parties, join_by(closest(birthday >= party)))
#Overlap joins
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-11", "2022-10-02", "2022-12-31"))
)
parties

parties |> 
  inner_join(parties, join_by(overlaps(start, end, start, end), q < q)) |> 
  select(start.x, end.x, start.y, end.y)

parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-10", "2022-10-02", "2022-12-31"))
)

employees |> 
  inner_join(parties, join_by(between(birthday, start, end)), unmatched = "error")

# Exercises ---------------------------------------------------------------
#1
delay <- flights |>
  group_by(hour = sched_dep_time %/% 100, day, month, year) |>
  summarise(
    average_delay = mean(dep_delay, na.rm=TRUE)
  )|>
  ungroup()
max48 <- delay |> 
  slice_max(average_delay, n= 48)
#2
top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)
top_dest

flights |>
  filter(dest %in% top_dest$dest)
#3
flights|>
  anti_join(weather, join_by(time_hour, origin)) 
#No todos los vuelos tienen correspondencia entre weather y la hora de salida del aeropuerto

#4
missing_tailnum<-flights|>
  anti_join(planes, join_by(tailnum))
missing_tailnum

missing_tailnum |> 
  filter(!is.na(tailnum)) |>
  count(carrier, sort = TRUE)
  
#5: hay 18 aviones que fueron pilotados por más de un carrier
flown_by <- flights|>
  group_by(tailnum)|>
  summarise(carriers = list (unique(carrier)))
flown_by
flown_by|>
  mutate(num_carriers = lengths(carriers))|>
  filter(num_carriers > 1)
#6
airports_origin <- airports|>
  rename(origin_lat = lat,
       origin_lon =lon,
       origin=faa)
airports_origin
airports_dest <- airports |>
  rename(dest_lat = lat, 
         dest_lon = lon,
         dest = faa)
airports_dest

lat_lon_flights<- flights |>
  left_join(airports_origin, join_by(origin))|>
  left_join(airports_dest, join_by(dest))
#7
delay_dest <- flights |>
  group_by(dest) |>
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE))
delay_dest_geo <- delay_dest |>
  left_join(airports, join_by(dest == faa))
delay_dest_geo |>
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  geom_point(mapping = aes(color=avg_delay)) +
  coord_quickmap()
#8
delayed13<-flights|>
  filter(year == 2013,
         month == 6,
         day == 13)
resumen13 <- delayed13|>
  group_by(dest)|>
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE))
delayed13_geo <- resumen13|>
  left_join(airports, join_by(dest==faa))
delayed13_geo |>
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  geom_point(mapping = aes(color = avg_delay)) +
  coord_quickmap()    
            
            