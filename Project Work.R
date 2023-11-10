library(igraph)
library(magrittr)
library(dplyr)
library(widyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggraph)
library(ggrepel)
library(readr)

# Ideas
# 1. Which actors have worked together?
# 2. Which critics have reviewed the same movies?
# 3. Which movies have the same actors?
# 4. Which movies have the same critics?
# 5. Which actors have worked with the same directors?

# Idea 2
# change this path to where you saved the data
df.meta <- read_csv("Data/rotten_tomatoes_meta.csv")

df.meta %>% 
  select(movie_title, critic_name) %>%
  head()

df.movie_critics <- df.meta %>% 
  # keep only movies with information on critics
  filter(!is.na(critic_name)) %>% 
  # filter good reviews
  filter(review_type = 'Fresh') %>%
  # count the number of actors (new variable)
  select(rotten_tomatoes_link, critic_name, movie_title)

df.movie_critics %>% 
  head()

df.critics_count <- df.movie_critics %>% 
  count(critic_name, sort = T)

# get top critics
df.top_critics <- df.critics_count #%>% 
  #slice_tail(prop = 0.85)

# connect critics by appearances in the same movie (new dataframe)
df.edge_list <- df.movie_critics %>% 
  # keep only top actors
  filter(critic_name %in% df.top_critics$critic_name) %>% 
  # count the co-occurrence of actors
  pairwise_count(
    # critics are paired up ...
    item = movie_title, 
    # ... by movies
    feature = critic_name,
    # count only one occurrence per movie
    upper = FALSE,
    # don't count actor_A & actor_A
    diag = FALSE
  )

# only include edges with more than 4 co-occurrences
g.critics_by_movies_filtered <- df.edge_list %>% 
  filter(n >=20) %>% 
  # build a graph from the adjacency matrix (undirected)
  igraph::graph_from_data_frame(directed = F)

# inspect the graph/network:
# number of vertices/nodes
length(V(g.critics_by_movies_filtered))
# number of edges/ties
length(E(g.critics_by_movies_filtered))
# network diameter
diameter(g.critics_by_movies_filtered)
# network density
edge_density(g.critics_by_movies_filtered)
# look at highest degree actors
head(sort(degree(g.critics_by_movies_filtered), decreasing = T))

# quick visualization
g.critics_by_movies_filtered %>%
  # specify the layout, you can also try "kk" or "fr"
  ggraph(layout = "stress") +
  # draw edges as straight lines
  geom_edge_link0()  +
  # draw nodes as points
  geom_node_point() +
  # remove the gray background and any axes and grids
  theme_void()

# closer look at the largest component:
# get all components
comps <- components(g.critics_by_movies_filtered)
# number of the largest component
l_comp <- which.max(comps$csize)
# node ids of actors in largest compnent:
l_comp_ids <- V(g.critics_by_movies_filtered)[comps$membership == l_comp]
# create subgraph with largest component only
g.l_comp <- induced_subgraph(g.critics_by_movies_filtered, l_comp_ids)

write_graph(g.critics_by_movies_filtered, file='critics_by_movies_filtered.graphml', format='graphml')

# quick visualization
g.l_comp %>%
  ggraph(layout = "stress") +
  geom_edge_link0()  +
  geom_node_point() +
  geom_node_text(
    aes(label=name),
    size=2
  ) +
  theme_void()

# clusters within the largest component:
clust <- cluster_fast_greedy(g.l_comp)
V(g.l_comp)$color <- clust$membership
g.l_comp %>%
  ggraph(layout = "stress") +
  geom_edge_link0()  +
  geom_node_point(
    aes(color=factor(color))
  ) +
  geom_node_text(
    aes(label=name),
    size=2
  ) +
  theme_void()
V(g.l_comp)$critic_name
