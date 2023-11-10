# Read data
source("Scripts/dependencies.R")
df.movies <- read_csv("Data/rotten_tomatoes_movies.csv") 

# Create data frame with relevant information
df.movie_genres <- df.movies %>% 
    filter(!is.na(genres)) %>% 
    mutate(genres_list = str_split(genres, ", ")) %>% 
    mutate(genres_n = unlist(lapply(genres_list, length)))

df.movie_genres <- df.movie_genres %>% 
    slice(rep(1:n(), genres_n)) %>% 
    mutate(genre = unlist(df.movie_genres$genres_list)) %>% 
    select(rotten_tomatoes_link, genre, tomatometer_rating) 

# Count genres
df.movie_genres_count <- df.movie_genres %>% 
    count(genre, sort = T)

# Make graph from genre connections
g.movie_genres <- df.movie_genres %>% 
    pairwise_count(item = genre, feature = rotten_tomatoes_link) %>% 
    graph_from_data_frame(
        directed = F,
        vertices = df.movie_genres_count
    )

# Visualize
g.movie_genres %>% 
    ggraph(.) +
    geom_edge_link0() +
    geom_node_point() +
    theme_void()

g.movie_genres %>% 
    ggraph(., layout = "fr", weights = n) +
    geom_edge_link0(aes(edge_alpha = n)) +
    geom_node_point(aes(
        size = V(g.movie_genres)$n,
        color = betweenness(g.movie_genres)
    )) +
    geom_node_text(
        aes(label = name), 
        repel = TRUE, 
        point.padding = unit(.5, "lines")) +
    theme_void() +
    labs(
        size = "Genre count",
        edge_alpha = "Edge count",
        color = "Betweenness"
    )

sort(betweenness(g.movie_genres))

# Get average rating for genre combination
f.get_genre_pair_rating <- function(genre.1, genre.2) {
    message(genre.1, " / ", genre.2)
    res <- df.movie_genres |> 
        group_by(rotten_tomatoes_link) |> 
        filter(genre %in% c(genre.1, genre.2)) |> 
        filter(n() == 2) |> 
        ungroup() |> 
        arrange(rotten_tomatoes_link) |> 
        summarise(tomatometer_rating = mean(tomatometer_rating, na.rm =T))
    return(res$tomatometer_rating)
}
g.movie_genres_rating <- df.movie_genres |> 
    pairwise_count(
        item = genre, 
        feature = rotten_tomatoes_link,
        upper = F) |> 
    rowwise() %>% 
    mutate(tomatometer_rating = f.get_genre_pair_rating(item1, item2)) |> 
    graph_from_data_frame(
        directed = F,
        vertices = df.movie_genres_count
    )

# Visualize
g.movie_genres_rating %>% 
    ggraph(., layout = "fr", weights = tomatometer_rating+1) +
    geom_edge_link(aes(edge_alpha = n, edge_color = tomatometer_rating)) +
    geom_node_point(aes(
        size = V(g.movie_genres_rating)$n
    )) +
    geom_node_label(
        aes(label = name), 
        repel = TRUE, 
        point.padding = unit(.5, "lines")) +
    scale_edge_color_gradient(low = "red", high = "green") +
    theme_void() +
    labs()

