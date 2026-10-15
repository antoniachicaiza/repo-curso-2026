Delfina Esponda, Antonia Chicaiza

library(igraph)
library(tidygraph)
library(ggraph)
library(readr)
# Cargamos los datos ------------------------------------------------------
path <- "./asoiaf/data/"
files <- list.files(path = path, full.names = TRUE)
files
cooc_all_edges <- read_csv(file.path(path, "asoiaf-all-edges.csv"))
cooc_all_edges
cooc_book5     <- read_csv(file.path(path, "asoiaf-book5-edges.csv"))
cooc_book5
# Quiénes son los personajes con mayor degree en el libro? ----------------
main_ch <- cooc_book5 %>%
  select(-Type) %>%
  gather(x, name, Source:Target) %>%
  group_by(name) %>%
  summarise(sum_weight = sum(weight)) %>%
  ungroup()

main_ch_l <- main_ch %>%
  arrange(desc(sum_weight)) %>%
  top_n(100, sum_weight)
main_ch_l

#Los personajes con conexiones más pesadas son:
#1 Jon-Snow                  761
#2 Daenerys-Targaryen        721
#3 Tyrion-Lannister          454
#4 Stannis-Baratheon         420
#5 Theon-Greyjoy             307
#la métrica no mide cantidad de conexiones, sino intensidad. Por lo que pueden haber personajes con pocas conexiones, pero muchas interacciones. 

cooc_book5_f <- cooc_book5 %>%
  filter(Source %in% main_ch_l$name & Target %in% main_ch_l$name)
cooc_book5_f

#Armado de red
as_tbl_graph(cooc_book5_f, directed = FALSE)

as_tbl_graph(cooc_book5_f, directed = FALSE) %>%
  activate(edges) %>% 
  filter(!edge_is_multiple())
#101 nodos, 352 conexiones, no hay nodos autoreferenciados

#Node ranking
as_tbl_graph(cooc_book5_f, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(n_rank_trv = node_rank_traveller()) %>%
  arrange(n_rank_trv)

#Centralidad
as_tbl_graph(cooc_book5_f, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(neighbors = centrality_degree()) %>%
  arrange(-neighbors)
#Los personajes con más conexiones en el quinto libro son:
#1 Daenerys-Targaryen        34
#2 Stannis-Baratheon         28
#3 Jon-Snow                  26
#4 Tyrion-Lannister          23
#5 Barristan-Selmy           19
#Daenerys-Targaryen es el personaje con más conexiones. Es decir, es el personaje con más centralidad.(la métrica no mide intensidad de las conexiones, solo número)

#Clustering
as_tbl_graph(cooc_book5_f, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(group = group_infomap()) %>% # una funcion que llama a un algoritmo de clustering INFOMAP
  arrange(-group)
#Se encuentran 9 comunidades. 

#Querying node types
set.seed(123)
as_tbl_graph(cooc_book5_f, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(center = node_is_center(),
         keyplayer = node_is_keyplayer(k = 10))
#Si center es true, el personaje es el centro de la red. 
#Si keyplayer es true, el personaje conecta grupos. 

#Node pairs
as_tbl_graph(cooc_book5_f, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(dist_to_center = node_distance_to(node_is_center()))
#Nos dice la distancia al centro de la red. Son personajes que están a X pasos del centro de la red. 


#Edge betweeness
as_tbl_graph(cooc_book5_f, directed = FALSE) %>%
  activate(edges) %>% 
  mutate(centrality_e = centrality_edge_betweenness())
# Mide cuántos caminos más cortos entre pares de personajes pasan por cada arista.

# The complete code -------------------------------------------------------

cooc_book5_f_graph <- as_tbl_graph(cooc_book5_f, directed = FALSE) %>%
  mutate(n_rank_trv = node_rank_traveller(),
         neighbors = centrality_degree(),
         group = group_infomap(),
         center = node_is_center(),
         dist_to_center = node_distance_to(node_is_center()),
         keyplayer = node_is_keyplayer(k = 10)) %>%
  activate(edges) %>% 
  filter(!edge_is_multiple()) %>%
  mutate(centrality_e = centrality_edge_betweenness())

cooc_book5_f_graph %>%
  activate(nodes) %>% # %N>%
  as.tibble()

cooc_book5_f_graph %>%
  activate(edges) %>% # %E>%
  as.tibble()

#Salidas:
#n_rank_trv	posición en el orden “viajante”
#neighbors	degree: con cuántos personajes distintos se conecta.
#group: los grupos en los que se asignan por mayor cantidad de relaciones entre sí. 
#center: si es el centro de la red.
#dist_to_center	a cuántos pasos está del centro	de la red.
#keyplayer: si es un personaje que conecta grupos. Está bien ubicado para conectar la red, permite llegar a varias zonas de la red. 

#Plotting with ggraph
set.seed(123)
layout <- create_layout(cooc_book5_f_graph, 
                        layout = "fr")


# Gráfico 1: red coloreada por grupo (pregunta 2)
ggraph(layout) + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(group)), size = 2) +
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_graph() +
  labs(title = "A Dance with Dragons (libro 5): red de personajes",
       subtitle = "Nodos coloreados por grupo (infomap)")

ggsave("Trabajos en clase/got_libro5_grupos.png", width = 14, height = 10, dpi = 300)

cols <- RColorBrewer::brewer.pal(3, "Set1")

# Con el algoritmo de infomap, vemso los 9 grupos
# Los más grandes son el Muro y la campaña de Stannis (26 personajes, liderado por Stannis y Jon) y Meereen (24, con Daenerys como centro)
# La estructura de la red refleja la estructura fragmentada del libro: tramas geográficamente separadas que casi no se cruzan. El algoritmo agrupa por interacción y no por familia o casa.



# Gráfico 2: centro de la red y distancia al centro
ggraph(layout) + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(center), size = dist_to_center)) +
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  scale_colour_manual(values = c(cols[2], cols[1])) +
  theme_graph() +
  labs(title = "A Dance with Dragons (libro 5): red de personajes",
       subtitle = "Nodos coloreados según si son el centro de la red")

ggsave("Trabajos en clase/got_libro5_centro.png", width = 14, height = 10, dpi = 300)

# muestra dónde está el centro de la red y qué tan lejos está cada personaje de él:
# El punto rojo es el centro: el personaje desde el cual llegás a cualquier otro en la menor cantidad de pasos. En este caso Bran Stark
# El tamaño de cada punto es su distancia al centro. Puntos chicos están cerca del núcleo; puntos grandes, en la periferia.

# para ver en una tabla a que grupo pertenece cada personaje y la cantidad de neighbors que tiene:
cooc_book5_f_graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  arrange(group, desc(neighbors)) %>%
  select(name, group, neighbors) %>%
  print(n = Inf)


## Hay algún personaje que sea keyplayer en el libro pero no lo sería si miramos los 5 libros juntos? Por que? -----

# armamos la red de los 5 libros juntos:
main_ch_all <- cooc_all_edges %>%
  select(-Type) %>%
  gather(x, name, Source:Target) %>%
  group_by(name) %>%
  summarise(sum_weight = sum(weight)) %>%
  ungroup() %>%
  top_n(100, sum_weight)

cooc_all_f <- cooc_all_edges %>%
  filter(Source %in% main_ch_all$name & Target %in% main_ch_all$name)

# chequeamos que sea la misma cant de nodos y de edges:
as_tbl_graph(cooc_all_f, directed = FALSE)

# keyplayers estables en cada red: Como el keyplayer cambia según la corrida, lo calculamos 20 veces y contamos cuántas veces sale cada personaje. Para no escribir el mismo código dos veces, armamos una función:
# La columna n dice en cuántas de las 20 corridas salió ese personaje como keyplayer.

contar_keyplayers <- function(edges, veces = 20) {
  g <- as_tbl_graph(edges, directed = FALSE)
  map(1:veces, ~ g %>%
        mutate(kp = node_is_keyplayer(k = 10)) %>%
        as_tibble() %>%
        filter(kp) %>%
        pull(name)) %>%
    unlist() %>%
    tibble(name = .) %>%
    count(name, sort = TRUE)
}

set.seed(123)
kp_b5  <- contar_keyplayers(cooc_book5_f)
kp_all <- contar_keyplayers(cooc_all_f)

kp_b5
# cuando corremos los keyplayers del libro 5 vemos que son estables. Ya que personajes como: Daenerys-Targaryen, Jon-Snow y Stannis-Baratheon salen en las 20 corridas. El resto no salen siempre, pero casi siempre

kp_all
# en la saga completa, los keyplayers son muy inestables, ya que la red está mucho mas interconctada, los personajes se cruzan a lo largo de 5 libros.
# hay 62 personajes distintos, minetras que en el libro 5 hat 35 personajes (en las 20 corridas)

# Como el algoritmo de keyplayer es heurístico y su resultado varía entre corridas,
# comparamos la frecuencia con la que cada personaje es elegido en 20 repeticiones.
comparacion <- full_join(kp_b5, kp_all, by = "name",
                         suffix = c("_libro5", "_saga")) %>%
  replace_na(list(n_libro5 = 0, n_saga = 0)) %>%
  arrange(desc(n_libro5))

comparacion %>% print(n = 15)

# Stannis Baratheon, Cersei Lannister y Theon Greyjoy son keyplayers estables en el
# libro 5 (aparecen en 15 a 20 de las 20 corridas del algoritmo), pero en la red de
# los cinco libros juntos lo son solo en 3 o 4 de 20. Aegon y Areo Hotah también son
# keyplayers en el libro 5, pero en la saga no entran entre los 100 personajes con
# más interacciones, por lo que quedan fuera de esa red.
# Un keyplayer es un personaje que conecta zonas de la red que de otro modo quedarían
# alejadas. La red del libro 5 está muy fragmentada (9 grupos que casi no se conectan
# entre sí), por lo que hay pocos personajes que unan esos grupos y se vuelven
# indispensables. En la saga completa, en cambio, los personajes se cruzan a lo largo
# de cinco libros y la red está mucho más interconectada: hay muchos caminos
# alternativos, y esos roles de conexión pueden cumplirlos otros personajes.
# Daenerys, Jon y Tyrion son keyplayers tanto en el libro 5 como en la saga: son los
# ejes estructurales de toda la historia.


## Para todos: quien fue el key player principal de su libro? Cómo cambia el protagonista de la red según avanza la saga? Qué nos dice eso? -----

# Characters divided by books ---------------------------------------------

for (i in 1:5) {
  cooc <- read_csv(paste0(path, "asoiaf-book", i, "-edges.csv")) %>%
    mutate(book = paste0("book_", i)) %>%
    filter(Source %in% main_ch_all$name & Target %in% main_ch_all$name)
  
  assign(paste0("coocs_book_", i), cooc)
}

set.seed(123)

cooc_books_1_graph <- as_tbl_graph(coocs_book_1, directed = FALSE) %>%
  mutate(book = "Book 1: A Game of Thrones",
         keyplayer = node_is_keyplayer(k = 10))

cooc_books_2_graph <- as_tbl_graph(coocs_book_2, directed = FALSE) %>%
  mutate(book = "Book 2: A Clash of Kings",
         keyplayer = node_is_keyplayer(k = 10))

cooc_books_3_graph <- as_tbl_graph(coocs_book_3, directed = FALSE) %>%
  mutate(book = "Book 3: A Storm of Swords",
         keyplayer = node_is_keyplayer(k = 10))

cooc_books_4_graph <- as_tbl_graph(coocs_book_4, directed = FALSE) %>%
  mutate(book = "Book 4: A Feast for Crows",
         keyplayer = node_is_keyplayer(k = 10))

cooc_books_5_graph <- as_tbl_graph(coocs_book_5, directed = FALSE) %>%
  mutate(book = "Book 5: A Dance with Dragons",
         keyplayer = node_is_keyplayer(k = 10))

# Combinar y graficar los keyplayers

set.seed(123)
cooc_books_1_graph %>% 
  bind_graphs(cooc_books_2_graph) %>%
  bind_graphs(cooc_books_3_graph) %>%
  bind_graphs(cooc_books_4_graph) %>%
  bind_graphs(cooc_books_5_graph) %>%
  ggraph(layout = "fr") + 
  facet_nodes(~ book, scales = "free", ncol = 1) +
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link(aes(edge_width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(keyplayer)), size = 3) +
  geom_node_text(aes(label = name), color = "black", size = 3, repel = TRUE) +
  theme_graph() +
  scale_colour_manual(values = c(cols[2], cols[1]))

ggsave("Trabajos en clase/got_keyplayers_5libros.png", width = 12, height = 30, dpi = 300)


# para ver mejor la info del grafico, vamos a hacer la tabla:
set.seed(123)
kp_por_libro <- map_dfr(1:5, function(i) {
  contar_keyplayers(get(paste0("coocs_book_", i))) %>%
    mutate(libro = i)
})

kp_por_libro %>%
  group_by(libro) %>%
  slice_max(n, n = 5) %>%
  print(n = Inf)


# Libro	Keyplayer(s) principal(es)	                             Veces de 20
# 1	    Eddard Stark	                                                17
# 2	    Arya Stark, Daenerys, Theon (empatados)	                      20
# 3	    Robb Stark	                                                  17
# 4	    Arianne Martell, Brienne, Samwell, Stannis (empatados)	      20
# 5	    Cersei, Daenerys (empatadas)	                                20

# El protagonista cambia en cada libro. Ningún personaje es el keyplayer principal en más de un libro
# Algunos personajes se mantienen entre los más importantes en varios libros: Tyrion
# Lannister está en el top 5 de los libros 1 a 3 y Daenerys en los libros 2, 3 y 5.
# El libro 4 se diferencia del resto: sus key players no coinciden con los de los
# demás libros, lo que sugiere que se centra en otro conjunto de personajes.
# Esto nos dice que la saga no tiene un único protagonista: el centro de la red se
# desplaza a medida que avanza la historia y distintos personajes ganan o pierden
# protagonismo. Además, los key players son más estables en los últimos libros
# (varios aparecen en 20 de 20 corridas), lo que indica que esas redes están más
# fragmentadas y dependen de pocos personajes para conectar sus distintas tramas.
# Nota: para esta comparación se usó el top 100 de personajes de la saga completa,
# por eso los key players del libro 5 difieren de los de la pregunta 3, donde se
# usó el top 100 del libro 5.
