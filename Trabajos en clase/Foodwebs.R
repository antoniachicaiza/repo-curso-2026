#Valentina Cue, Leonardo Eguia, Antonia Chicaiza
# Carga de librerías ------------------------------------------------------
library(igraph)
library(igraphdata)
data("foodwebs")
g <- foodwebs$baywet
g
#IGRAPH ccb5ef1 DNW- 128 2106 -- Florida Bay Trophic Exchan
#La red es dirigida (D),los nodos tienen nombre (N) y es weighted (W), no es bipartita. 

# Descriptiva de nodos y conexiones ---------------------------------------
V(g) #128 nodos
E(g) #2106 conexiones
is_directed(g) #TRUE
is.weighted(g) #TRUE
is_connected(g) #TRUE
as_adjacency_matrix(g, attr="weight")
componentes <- components(g)
componentes$no #1 La red es una sola pieza, no hay nodos sueltos, todos están conectados con al menos otro nodo.
componentes$size #Null
table(componentes$csize) #128, 1
max(componentes$csize) / vcount(g) #1. Como se trata de un solo componente, tiene sentido que represente la totalidad de la red. 

# ¿Cómo se conecta la red? ------------------------------------------------
# Promedio y mediana ------------------------------------------------------

#Grado promedio y la mediana; Son parecidos o muy distintos? Qué dice eso sobre 
#la red?  

grados <- degree(g)
mean(grados)
median(grados)

#El grado promedio (32,91) y la mediana (31) son bastante similares, con una 
#diferencia de apenas 1,91 conexiones. Esto sugiere que la distribución de 
#grados no presenta una asimetría muy marcada en sus medidas centrales, aunque 
#podrían existir algunos nodos altamente conectados.


# Histograma --------------------------------------------------------------

#Histograma de grados. Tiene cola larga?

hist(grados, breaks = 50, col = "steelblue", border = "white",
     main = "Distribución de grados (escala lineal)",
     xlab = "Grado", ylab = "Frecuencia")

#Sí, el histograma presenta una cola larga hacia la derecha. La mayoría de los
#nodos concentra sus grados entre 10 y 40, mientras que unos pocos alcanzan 
#valores cercanos a 100. Esto indica que la red presenta cierta heterogeneidad, 
#con algunas especies mucho más conectadas que otras.



# Nodos mas conectados ----------------------------------------------------

#Cuáles son  los 5 nodos más conectados (hubs?)  Tiene sentido que sean hubs?

top5<- sort(grados, decreasing = TRUE)[1:5]
data.frame(grado       = top5)

#Los cinco nodos más conectados son Respiration (110), Water POC (98), 
#Predatory Shrimp (76), Pink Shrimp (73) y Other Demersal Fishes (65). 
#Tiene sentido que sean hubs porque representan componentes con numerosas 
#interacciones dentro de la red trófica. Respiration y Water POC participan en
#el flujo de materia y energía, mientras que los camarones y peces mantienen 
#múltiples relaciones alimentarias. Esto demuestra que algunos nodos tienen un 
#papel estructural más conectado que otros.

# Densidad ----------------------------------------------------------------

#Densidad? Es una red densa o esparsa?

edge_density(g)

#La densidad de la red baywet es de 0,1296 (12,96 %), lo que indica que solo una
#pequeña proporción de las conexiones posibles está presente. Por lo tanto, se 
#trata de una red relativamente poco densa, donde las interacciones tróficas no 
#ocurren entre todas las especies.


# Estructura de la red ----------------------------------------------------

## Coeficiente de clustering global. Es alto o bajo?
transitivity(g, type = "global") # 0.3119154

red_er <- sample_gnm(vcount(g), ecount(g), directed = TRUE)
red_conf <- sample_degseq(
  out.deg = degree(g, mode = "out"),
  in.deg  = degree(g, mode = "in"),
  method  = "edge.switching.simple")

metricas <- function(x) {
  c(clustering    = transitivity(x, type = "global"),
    asortatividad = assortativity_degree(x, directed = TRUE),
    dist_promedio = mean_distance(x, directed = TRUE))}

round(rbind(Real              = metricas(g),
            "Erdős–Rényi"     = metricas(red_er),
            Configuracional   = metricas(red_conf)), 3)

# El coeficiente de clustering es moderado (0,312) para su distribucion de grados
# comparando con modelos nulos aleatorios, es decir, existe una probabilidad
# de 0,31 de que dos vecinos de un nodo aleatorio se enlacen entre si por los que
# se puede concluir que este coeficiente responde a una consecuencia natural
# de la densidad de conexiones y la distribucion de grados de sus hubs

## Cuál es la distancia promedio entre nodos?
mean_distance(g) #6.52552

## Cuántas comunidades encontró
com <- cluster_louvain(as.undirected(g))
length(com) #4
sizes(com) 
#Community sizes
#1  2  3  4 
#27 30  4 67 