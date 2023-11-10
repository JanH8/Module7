# packages

#install.packages("igraph")
#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("widyr")
#install.packages("tidyr")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("ggraph")
#install.packages("ggrepel")
#install.packages("readr")

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


# Read data

# change this path to where you saved the data
df.movies <- read_csv("Data/rotten_tomatoes_movies.csv")

# inspect the dataset - what's in here and what can we use?

# dimensions -> rows and columns
df.movies %>% 
    dim()
# column/variable/feature names
df.movies %>% 
    colnames()

# looking at the content
View(df.movies)

# can you think of Network Analysis related questions?
# can you think of possible "hidden networks" in this data?














# Example Case: Actor - Movie Network
# -> what information do we need, which columns are relevant?











df.movies %>% 
    select(movie_title, actors) %>%
    head()

# Create data frame with relevant information

# create a new dataframe
df.movie_actors <- df.movies %>% 
    # keep only movies with information on actors
    filter(!is.na(actors)) %>% 
    # split the character vector at the commas into a list of actor names
    # (new variable)
    mutate(actors_list = str_split(actors, regex(", (?!Jr.)"))) %>% 
    # count the number of actors (new variable)
    mutate(actors_n = unlist(lapply(actors_list, length)))

# expand the dataframe by the list of actors
df.movie_actors <- df.movie_actors %>% 
    slice(rep(1:n(), actors_n)) %>% 
    mutate(actor = unlist(df.movie_actors$actors_list)) %>% 
    # keep only movie id and actor
    select(rotten_tomatoes_link, actor, movie_title)
View(df.movie_actors)

# movies per actor
df.actor_count <- df.movie_actors %>% 
    count(actor, sort = T)

# get top actors
df.top_actors <- df.actor_count %>% 
    slice_head(prop = 0.15)

# connect actors by appearances in the same movie (new dataframe)
df.edge_list <- df.movie_actors %>% 
    # keep only top actors
    filter(actor %in% df.top_actors$actor) %>% 
    # count the co-occurrence of actors
    pairwise_count(
        # actors are paired up ...
        item = actor, 
        # ... by movies
        feature = rotten_tomatoes_link,
        # count only one occurrence per movie
        upper = FALSE,
        # don't count actor_A & actor_A
        diag = FALSE
    )

# only include edges with more than 4 co-occurrences
g.actors_by_movies_filtered <- df.edge_list %>% 
    filter(n >= 5) %>% 
    # build a graph from the adjacency matrix (undirected)
    igraph::graph_from_data_frame(directed = F)

# inspect the graph/network:
# number of vertices/nodes
length(V(g.actors_by_movies_filtered))
# number of edges/ties
length(E(g.actors_by_movies_filtered))
# network diameter
diameter(g.actors_by_movies_filtered)
# network density
edge_density(g.actors_by_movies_filtered)
# look at highest degree actors
head(sort(degree(g.actors_by_movies_filtered), decreasing = T))


# quick visualization
g.actors_by_movies_filtered %>%
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
comps <- components(g.actors_by_movies_filtered)
# number of the largest component
l_comp <- which.max(comps$csize)
# node ids of actors in largest compnent:
l_comp_ids <- V(g.actors_by_movies_filtered)[comps$membership == l_comp]
# create subgraph with largest component only
g.l_comp <- induced_subgraph(g.actors_by_movies_filtered, l_comp_ids)

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
V(g.l_comp)$name

write_graph(g.critics_by_movies_filtered, file='critics_by_movies_filtered.graphml', format='graphml')

# additional review data
df.meta <- read_csv("Data/rotten_tomatoes_critic_reviews.csv")
View(df.meta)

df.merge <- merge(
    df.movies,
    df.meta,
    by='rotten_tomatoes_link'
)
View(df.merge)

# look at the merged Dataframe, what changed?
# what additional metdata could we use?
# how can we leverage network structures and text data?