# Librerías ---------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(textdata)
library(topicmodels)
library(igraph)
library(ggraph)
library(readr)
library(tidyverse)
library(tidyr)
library(lubridate)
# Base --------------------------------------------------------------------
articles<-read_csv("/Users/antonia/Documents/cienciadatos520/tareas/tarea05/DATA-T9-mckinsey-mind-the-gap-articles-20251020.csv")
view(articles)

# Arreglo de base ---------------------------------------------------------
articles1<-articles |>
  mutate(id = row_number(),
         year = year(date),
         article_text = article_text |>
           str_replace_all("’", "'") |>
           str_remove("^Brought to you by[^\n]*\n") |>
           str_remove(regex("Welcome to the latest edition of Mind the Gap.*?—Alex and Axel",
                            dotall = TRUE)))
article_words <- articles1|>
  unnest_tokens(output = word, input =article_text )
article_words  
#Sacamos stop words y palabras de alta frecuencia con bajo contenido informativo como también nombres de autores y senior partners que aparecieron después en el análisis
firmas <- c("partner", "partners", "senior", "managing", "coauthors")
autores <- c("aaron", "smet", "bryan", "hancock", "erica", "coe",
             "kweilin", "ellingrud", "brooke", "weddle", "bill", "schaninger")
lowcontext <- tibble(word = c("gen", "z", "z's", "zers", "mckinsey", "percent"))
cleaned_words <- article_words |>
  anti_join(stop_words, by = "word")|>
  anti_join(lowcontext, by = "word")|>
  filter(!word %in% firmas, !word %in% autores)
cleaned_words

# Descriptivo -------------------------------------------------------------
doc_freq <- cleaned_words |>
  distinct(id, word) |>
  count(word, sort = TRUE) |>
  mutate(prop_docs = n / n_distinct(cleaned_words$id))

doc_freq
#Los artículos tienen formatos compartidos, se repiten palabras como time, week social, en general se referencian encuestas compañías e investigaciones, un tema frecuente es el mundo del trabajo. 

articles_tfidf <- cleaned_words |>
  count(id, title, word) |>
  bind_tf_idf(word, id, n) |>
  arrange(desc(tf_idf))

articles_tfidf
#El tf_idf de las palabras nos deja ver que en general se tratan temas heterogéneos entre artículos, aunque siempre vinculados a estilo de vida, consumo y ocio. 
largo <- article_words |> count(id, name = "palabras")
summary(largo$palabras)
#El rango intercuantil nos deja ver que en general los artículos tienen la misma cantidad de palabras.

# Análisis de Sentimientos con diccionario binario ------------------------
sentiments_bin <- cleaned_words |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(id, date, year, title, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(sentiment = positive - negative,
         sentiment_rel = (positive - negative) / (positive + negative))
#Graficamos
ggplot(sentiments_bin, aes(date, sentiment, fill = sentiment > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(vars(year), scales = "free_x")
#En general los sentimientos encontrados son positivos, esto puede estar relacionado con el público al que están dirigidos los artículos y los temas que suelen desarrollar. 
#Palabras más comunes
bing_word_counts <- cleaned_words |>
  inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") |>
  count(word, sentiment, sort = TRUE)
bing_word_counts

bing_word_counts |>
  group_by(sentiment) |>
  slice_max(n, n = 10) |>
  ungroup() |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(sentiment), scales = "free_y") +
  labs(x = "Contribución al sentimiento", y = NULL)
#Al mirar el gráfico se relativiza la predominancia de sentimientos positivos, palabras como loyalty, fast y beauty en realidad pueden no estar vinculadas a sentimientos sino a expresiones como "fast fashion" o "loyalty programs"
# Análisis de sentimiento con diccionario graduado ------------------------
sentiments_afinn <- cleaned_words |>
  inner_join(get_sentiments("afinn"), by = "word") |>
  group_by(id, date, year, title) |>
  summarise(afinn_suma = sum(value),
            afinn_prom = mean(value),
            .groups = "drop")

ggplot(sentiments_afinn, aes(date, afinn_suma, fill = afinn_suma > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(year), ncol = 4, scales = "free_x") +
  scale_x_date(date_labels = "%b")
#Se mantiene la predominancia de los sentimientos positivos aun con el diccionario graduado
# Topic modeling ---------------------------------------------------------

articles_dtm <- cleaned_words |>
  count(id, word) |>
  cast_dtm(id, word, n)

lda_10 <- LDA(articles_dtm, k = 10, control = list(seed = 1234))
lda_15 <- LDA(articles_dtm, k = 15, control = list(seed = 1234))

top_terms_10 <- tidy(lda_10, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 10) |>
  ungroup() |>
  arrange(topic, -beta)

top_terms_10 |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free") +
  scale_y_reordered()
#Se confirma la intuición de los temas tratados, aparecen temas a los que claramente podríamos llamar consumo, moda, deportes-salud, mercado laboral, etc. 
top_terms_15 <- tidy(lda_15, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 10) |>
  ungroup() |>
  arrange(topic, -beta)

top_terms_15 |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free") +
  scale_y_reordered()

# Análisis de Bigrams -----------------------------------------------------
article_bigrams <- articles1 |>
  unnest_tokens(bigram, article_text, token = "ngrams", n = 2) |>
  filter(!is.na(bigram)) |>
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- article_bigrams |>
  filter(!word1 %in% stop_words$word, !word1 %in% lowcontext$word,
         !word2 %in% stop_words$word, !word2 %in% lowcontext$word)

bigram_counts <- bigrams_filtered |>
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered |>
  unite(bigram, word1, word2, sep = " ")

bigrams_united

bigram_tf_idf <- bigrams_united |>
  count(title, bigram) |>
  bind_tf_idf(bigram, title, n) |>
  arrange(desc(tf_idf))
bigram_tf_idf

bigram_graph <- bigram_counts |>
  filter(!word1 %in% firmas, !word2 %in% firmas, !word1 %in% autores, !word2 %in% autores) |>
  filter(n > 5) |>
  graph_from_data_frame()

bigram_graph

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE)

not_sentiment <- article_bigrams |>
  filter(word1 == "not") |>
  inner_join(get_sentiments("bing"), by = c(word2 = "word")) |>
  count(word1, word2, sentiment, sort = TRUE)

not_sentiment

AFINN <- get_sentiments("afinn")
not_sentiment1 <- article_bigrams |>
  filter(word1 == "not") |>
  inner_join(AFINN, by = c(word2 = "word")) |>
  count(word2, value, sort = TRUE)
not_sentiment1
#las not words no son un problema, si bien aparecen junto a sentimientos, no es tan recurrente como para ser significativo o alterar las conclusiones

