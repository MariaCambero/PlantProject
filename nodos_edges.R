# Cargar librerias

library(dplyr)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

# leo mis datos "enlaces"

enlaces <- read.csv(file = "edges1.csv")
str(enlaces)
typeof(enlaces$node1)
typeof(enlaces$node2)
typeof(enlaces$experimentally)
str(enlaces$node1)
length(enlaces)
typeof(enlaces)
dim(enlaces)
colnames(enlaces)


# leo mis datos "nodos"

nodos <- read.csv(file = "nodos2.csv")
str(nodos)
typeof(nodos)
dim(nodos)
colnames(nodos)

# Los hago red (Sin saber como, REVISAR COMANDOS)

g <- graph_from_data_frame(enlaces, directed = F)

g

plot(g)

# HASTA AQUI YA SE PUDO

# voy a usar tidygraph para manipular más fácil

g <- as_tbl_graph(g)
g
head(nodos)


# activo nodos, y les uno la informacion que tengo en mi objeto de nodos 

g <-
  g %>% 
  activate("nodes") %>% 
  left_join(y = nodos, by = c("name"="Protein"))
g


# Nueva Tabla









