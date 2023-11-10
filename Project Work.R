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

# Comments after presentation (Disney+)
# - Research question was formulated a little too broad - also it was formulated so that the answer is always "yes" (can we do xxx?)
# - Suggestion 1: add more variables to the network analysis, for example genres or production firms etc. to see if there are more non-obvious relations 
# - Suggestion 2: add the other streaming services like Netflix, Amazon Prime to the dataset, so that it becomes visible what movies are still available to grab for Disney+
# - Suggestion 3: scale variables for better comparison

# Idea 2
# change this path to where you saved the data
df.meta <- read_csv("Data/rotten_tomatoes_meta.csv")

# load disney data
df.disney <- read_csv("Data/disney_plus_titles.csv")


df.disney_movies = df.disney %>% 
  filter(type == 'Movie') %>%
  select(title)

df.meta %>% 
  select(movie_title, critic_name) %>%
  head()

# calculate similarity between movie titles and disney titles
df.meta$disney = ifelse(df.meta$movie_title %in% df.disney_movies$title, 1, 0)

# set disney 1 when title contains star wars or marvel
df.meta$disney = ifelse(str_detect(df.meta$movie_title, 'Star Wars'), 1, df.meta$disney)


# count disney films and not disney films, distinct films
count_films = df.meta %>% 
  # distinct films
  distinct(movie_title, disney) %>%
  # count movies where disney is 1
  count(disney) %>%
  # rename column
  rename(count = n) %>%
  # change disney to character
  mutate(disney = as.character(disney))

# plot results, color by disney
count_films %>% 
  ggplot(aes(x = disney, y = count, fill = disney)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('grey', 'blue')) +
  labs(x = 'Disney', y = 'Number of Movies', title = 'Non-Disney vs. Disney Movies')

# show different review types
df.meta %>% 
  select(review_type) %>%
  distinct()

df.movie_critics <- df.meta %>% 
  # keep only movies with information on critics
  filter(!is.na(critic_name)) %>% 
  # filter good reviews
  filter(review_type == 'Fresh') %>%
  # count the number of actors (new variable)
  select(rotten_tomatoes_link, critic_name, movie_title, disney)

df.movie_critics %>% 
  head()

#df.critics_count <- df.movie_critics %>% 
#  count(critic_name, sort = T)

# get top critics
#df.top_critics <- df.critics_count #%>% 
#slice_tail(prop = 0.85)

# connect critics by appearances in the same movie (new dataframe)
df.edge_list <- df.movie_critics %>% 
  # count the co-occurrence of critics
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

df.edge_list$disney1 = ifelse(df.edge_list$item1 %in% df.disney_movies$title, 1, 0)
df.edge_list$disney1 = ifelse(str_detect(df.edge_list$item1, 'Star Wars'), 1, df.edge_list$disney1)

df.edge_list$disney2= ifelse(df.edge_list$item2 %in% df.disney_movies$title, 1, 0)
df.edge_list$disney2 = ifelse(str_detect(df.edge_list$item2, 'Star Wars'), 1, df.edge_list$disney2)

# filter where disney 1 or disney 2 is 1
df.edge_list = df.edge_list %>% 
  filter(disney1 == 1 | disney2 == 1)

# only include edges with more than 20 co-occurrences
g.critics_by_movies_filtered <- df.edge_list %>% 
  filter(n >=100) %>% 
  # build a graph from the adjacency matrix (undirected)
  igraph::graph_from_data_frame(directed = F)

# add attributes to vertices
V(g.critics_by_movies_filtered)$disney <- ifelse(V(g.critics_by_movies_filtered)$name %in% df.disney_movies$title, 1, 0)
V(g.critics_by_movies_filtered)$disney = ifelse(str_detect(V(g.critics_by_movies_filtered)$name, 'Star Wars'), 1, V(g.critics_by_movies_filtered)$disney)


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

write_graph(g.critics_by_movies_filtered, file='critics_by_movies_filtered_disney_connections.graphml', format='graphml')


## DELETE DISNEY CONNECTIONS
# filter where disney 1 or disney 2 is 1
df.edge_list_no_disney = df.edge_list %>% 
  filter(disney1 != disney2)

# only include edges with more than 20 co-occurrences
g.critics_by_movies_filtered_no_disney <- df.edge_list_no_disney %>% 
  filter(n >=100) %>% 
  # build a graph from the adjacency matrix (undirected)
  igraph::graph_from_data_frame(directed = F)

# add attributes to vertices
V(g.critics_by_movies_filtered_no_disney)$disney <- ifelse(V(g.critics_by_movies_filtered_no_disney)$name %in% df.disney_movies$title, 1, 0)
V(g.critics_by_movies_filtered_no_disney)$disney = ifelse(str_detect(V(g.critics_by_movies_filtered_no_disney)$name, 'Star Wars'), 1, V(g.critics_by_movies_filtered_no_disney)$disney)


# inspect the graph/network:
# number of vertices/nodes
length(V(g.critics_by_movies_filtered_no_disney))
# number of edges/ties
length(E(g.critics_by_movies_filtered_no_disney))
# network diameter
diameter(g.critics_by_movies_filtered_no_disney)
# network density
edge_density(g.critics_by_movies_filtered_no_disney)
# look at highest degree actors
head(sort(degree(g.critics_by_movies_filtered_no_disney), decreasing = T))

# quick visualization
g.critics_by_movies_filtered_no_disney %>%
  # specify the layout, you can also try "kk" or "fr"
  ggraph(layout = "stress") +
  # draw edges as straight lines
  geom_edge_link0()  +
  # draw nodes as points
  geom_node_point() +
  # remove the gray background and any axes and grids
  theme_void()

write_graph(g.critics_by_movies_filtered_no_disney, file='critics_by_movies_filtered_disney_connections_no_disney.graphml', format='graphml')


# get the 10 non disney movies that have the most common critcs with disney movies
df.ranking = df.edge_list_no_disney %>% 
    # set item1 null where disney1 is not 1
    mutate(item1 = ifelse(disney1 == 1, NA, item1)) %>%
    mutate(item2 = ifelse(disney2 == 1, NA, item2)) %>%
    mutate(item1 = ifelse(is.na(item1), item2, item1)) %>%
  select(item1,n) %>%
  # summarize by item 1 and sum n
  group_by(item1) %>%
  summarize(n = sum(n)) %>%
  # sort by n
  arrange(desc(n)) %>%
  rename(title = item1)

head(df.ranking)

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
