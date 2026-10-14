library(edgar)
library(tidytext)
library(tidyverse)
library(textdata)

# Descarga de datos con la API de EDGAR -----------------------------------
#generamos el user agent para poder usar la API
ua <- "Antonia Chicaiza antonia.chicaiza@gmail.com"
getMasterIndex(filing.year = 2025, useragent = ua)
load("edgar_MasterIndex/2025master.Rda")

#Identificamos a las empresas argentinas usando ciks (hay empresas con registradas en inglés)
ciks_arg <- c(904851,   # YPF
              932470,   # Telecom Argentina
              1114700,  # Grupo Galicia (Nasdaq)
              1395213,  # Edenor
              1517399,  # Supervielle
              1711375,  # Loma Negra
              1717161,  # Central Puerto
              913059,   # BBVA Argentina
              933267,   # IRSA
              1347426,  # Banco Macro
              1469395,  # Pampa Energía
              931427)   # TGS
arg <- year.master |>
  filter(form.type == "20-F", cik %in% ciks_arg)
nrow(arg) #12

getFilings(cik.no = unique(arg$cik), form.type = "20-F",
           filing.year = 2025, quarter = 1:4,
           downl.permit = "y", useragent = ua)
list.files("edgar_Filings", recursive = TRUE)

nombres <- tibble(
  cik = c(904851, 932470, 1114700, 1395213, 1517399, 1711375,
          1717161, 913059, 933267, 1347426, 1469395, 931427),
  empresa = c("YPF", "Telecom", "Galicia", "Edenor", "Supervielle", "Loma Negra",
              "Central Puerto", "BBVA Arg", "IRSA", "Macro", "Pampa", "TGS"))
nombres

leer_20f <- function(ruta) {
  read_file(ruta) |>
    str_extract("(?s)<TEXT>.*?</TEXT>") |>
    str_remove_all("(?s)<ix:header>.*?</ix:header>") |>
    str_replace_all("<[^>]+>", " ") |>
    str_replace_all("&[#a-z0-9]+;", " ") |>
    str_squish() |>
    str_to_lower()
}

archivos <- list.files("edgar_Filings", recursive = TRUE, full.names = TRUE)

textos <- tibble(ruta = archivos) |>
  mutate(cik   = as.numeric(str_extract(basename(ruta), "^[0-9]+")),
         texto = map_chr(ruta, leer_20f)) |>
  left_join(nombres, by = "cik") |>
  select(empresa, texto)
textos
# Análisis de texto -------------------------------------------------------

#palabras por reporte, buscamos conseguir el total de palabras por reporte para poder armar una medida relativa. 
totales <- textos|>
  unnest_tokens(word, texto)|>
  count(empresa, name = "total")
totales

#Separamos términos de una palabra y de dos palabras para poder buscarlas como unigrams y bigrams respectivamente.
words1<- c("inflation", "devaluation", "default", "restructuring", "imf", "embi")
words2<- c("exchange controls", "capital controls", "price controls", "country risk")

#Tokenizamos en unigrams y buscamos la intersección entre los terminos del texto y los términos en la lista words1.
incidencia_words1 <-textos|>
  unnest_tokens(termino, texto)|>
  filter(termino %in% words1)|>
  count(empresa, termino)
incidencia_words1

#Tokenizamos en bigrams y buscamos la intersección entre los bigrams del texto y los bigrams en la lista words2. 
incidencia_words2 <-textos|>
  unnest_tokens(termino, texto, token = "ngrams", n=2)|>
  filter(termino %in% words2)|>
  count(empresa, termino)
incidencia_words2

#Unimos ambos conteos y normalizamos (multiplico por 10.000 para poder ver los decimales)
incidencia <- bind_rows(incidencia_words1, incidencia_words2)|>
  left_join(totales, by = "empresa")|>
  mutate(normalizacion = n/total*10000)
incidencia

print(incidencia|>
       arrange(desc(normalizacion)), n=120)
#Inflación es la palabra que más incidencia tiene en 11 de los 12 reportes, seguida por default e imf

#tf_idf
#Tokenizamos y sacamos stop words y números
tokens<- textos|>
  unnest_tokens(word, texto)|>
  filter(!str_detect(word, "\\d"))|>
  anti_join(stop_words, by = "word")
tokens

#calculamos tf_idf para ver qué tan frecuente es una palabra en un documento respecto al número de documentos en los que la palabra en el corpus, de modo que observamos qué palabras son más distintivas de un texto. 
tfidf<- tokens|>
  count(empresa, word)|>
  bind_tf_idf(word, empresa, n)
tfidf

#facet wrap con las 10 palabras con más tf_idf de cada empresa. 
tfidf|>
  group_by(empresa)|>
  slice_max(tf_idf, n=10)|>
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, empresa))) + 
  geom_col() + 
  scale_y_reordered() + 
  facet_wrap(~empresa, scales = "free")
print(tfidf |>
        arrange(desc(tf_idf)), n=20)
#Cemento es la palabra más representativa del reporte de Loma negra, obviando palabras que repiten el nombre de la empresa, palabras como financiero para galicia o mall y tenants para irsa son representativas de sus reportes. 

#análisis de sentimientos
#Cargamos el diccionario
lm <- get_sentiments("loughran") 

#Buscamos la intersección entre el diccionario y los textod tokenizados
sentimiento <- tokens |>
  inner_join(lm, by = "word", relationship = "many-to-many") |>
  count(empresa, sentiment) |>
  left_join(totales, by = "empresa")|>
  mutate(normalizacion = n / total * 10000)
sentimiento

#Facet wrap de los seis sentimientos y la frecuencia por empresa.
grafico_sent <- sentimiento |>
  ggplot(aes(normalizacion, reorder_within(empresa, normalizacion, sentiment))) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~sentiment, scales = "free") +
  labs(x = "Palabras cada 10.000", y = NULL,
       title = "Sentimiento Loughran-McDonald en 20-F 2025")
grafico_sent

print(sentimiento|>
        arrange(desc(normalizacion)), n=40)
#los sentimientos negativos predominan en la mayoría (10 de 12) de los reportes, en empresas como YPF y Central Puerto predominan sentimientos litigios. En bancos son comunes los sentimientos de incertidumbre en los reportes (aunque no es la categoría principal).
