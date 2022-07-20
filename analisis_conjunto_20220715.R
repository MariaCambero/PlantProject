# Cargar librerias

library(dplyr)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

# leo mis datos "enlaces"

# paso 1 ) metabolitos de G. glauca 
# paso 2 ) metabolitos - targets | swisstarget ### donde estan estos datos? 
# paso 3 ) targets - otras proteinas | string, valores experimentales 


g_proteinas <- read.csv(file = "edges1.csv") # targets - otras proteinas
g_proteinas <- g_proteinas %>% as_tibble()

g_proteinas <- 
  g_proteinas %>%
  filter(experimentally > 0)
  
g_proteinas <- graph_from_data_frame(g_proteinas, directed = F)


g_proteinas <- as_tbl_graph(g_proteinas)

# nodos2 contiene las relaciones entre los metabolitos y sus targets | de swisstarget

swisstarget <- read.csv(file = "nodos2.csv") %>% as_tibble()


# hay dos registros donde B no es numerico; vamos a quitarlos y volver B numerico

swisstarget <- 
  swisstarget %>% 
  filter(B!="NO SE ") %>% 
  mutate(B = as.integer(B))


# voy a convertir esto en otro edgelist / red



g_swisstarget <- 
  swisstarget %>% 
  select(-Fuente) %>% 
  pivot_longer(data = ., 
               cols = -Protein, 
               names_to = "metabolito", 
               values_to = "value"
               )


g_swisstarget <- 
  g_swisstarget %>% 
  filter(value!=0)

g_swisstarget <- 
  g_swisstarget %>% 
  select(-value) %>% 
  select(metabolito, Protein) %>% 
  graph_from_data_frame(d = ., directed = T) %>% 
  as_tbl_graph()

#plot(g_swisstarget)

#vamos a sacar un diccionario de si las proteinas son targets o no lo son (son segundos vecinos)

mis_targets <- swisstarget$Protein

g_proteinas <- 
  g_proteinas %>% 
  activate("nodes") %>% 
  mutate(target = name%in%mis_targets)

ggraph(g_proteinas)  +
  geom_edge_link(alpha = 0.05)  + 
  geom_node_point(aes(color = target))

# vamos a ver QUE metabolito está conectado a estos genes 

targets_b <- 
  swisstarget %>% 
  select(Protein, B) %>% 
  filter(B == 1) %>% 
  pull(Protein)

targets_c <- 
  swisstarget %>% 
  select(Protein, C) %>% 
  filter(C == 1) %>% 
  pull(Protein)

targets_d <- 
  swisstarget %>% 
  select(Protein, D) %>% 
  filter(D == 1) %>% 
  pull(Protein)

targets_e <- 
  swisstarget %>% 
  select(Protein, E) %>% 
  filter(E == 1) %>% 
  pull(Protein)

targets_f <- 
  swisstarget %>% 
  select(Protein, F) %>% 
  filter(F == 1) %>% 
  pull(Protein)

targets_g <- 
  swisstarget %>% 
  select(Protein, G) %>% 
  filter(G == 1) %>% 
  pull(Protein)

targets_i <- 
  swisstarget %>% 
  select(Protein, I) %>% 
  filter(I == 1) %>% 
  pull(Protein)

#le pongo si son targets
g_proteinas <- 
  g_proteinas %>% 
  activate("nodes") %>% 
  mutate(target = name%in%mis_targets,
         target_b = name%in%targets_b,
         target_c = name%in%targets_c,
         target_d = name%in%targets_d,
         target_e = name%in%targets_e,
         target_f = name%in%targets_f,
         target_g = name%in%targets_g,
         target_i = name%in%targets_i
  )


# vamos a calcular medidas de centralidad y detectar comunidades 

g_proteinas <- 
  g_proteinas %>% 
  activate("nodes") %>% 
  mutate(grado = centrality_degree(),
         bc    = centrality_betweenness()
         )

tabla_analisis_nodos <- 
  g_proteinas %>% 
  activate("nodes") %>% 
  as_tibble()

tabla_analisis_nodos %>% 
  arrange(desc(grado))

tabla_analisis_nodos %>% 
  arrange(desc(bc))


