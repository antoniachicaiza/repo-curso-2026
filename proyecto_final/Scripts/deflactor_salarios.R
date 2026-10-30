library(readr)
library(readxl)
library(lubridate)
# Deflactor salarios ------------------------------------------------------
setwd("/Volumes/Crucial X9/Ciencia de datos/bases")
base<-read_csv("mler_1000.csv")
View(base)
ipc_empalme<-read_excel("/Users/antonia/Library/Mobile Documents/com~apple~CloudDocs/UBA/Econometría II/TP1/IPC_empalme1.xlsx")
head(base)
ipc_empalme <- ipc_empalme|>
  mutate(tiempo = year(Date) * 100 + month(Date))
base <- base |>
  left_join(ipc_empalme |> select(tiempo, ipc = `IPC Empalme base 2016`),
            by = "tiempo")
View(base)

ipc_base <- ipc_empalme$`IPC Empalme base 2016`[ipc_empalme$tiempo == 202504]
base <- base |>
  mutate(rem_real = rem_tot / ipc * ipc_base)
View(base)

# identificamos diciembre y junio -----------------------------------------
base <- base |>
  mutate(mes = tiempo %% 100,
         sac = as.integer(mes %in% c(6, 12))) # 1=junio o diciembre, 0=otro
View(base) 

