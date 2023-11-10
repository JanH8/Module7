# Read data
source("Scripts/dependencies.R")
df.reviews <- read_csv("Data/rotten_tomatoes_critic_reviews.csv")

# Create data frame with relevant information
df.movie_reviews <- df.reviews %>% 
    filter(!is.na(review_type) & !is.na(critic_name)) %>% 
    select(rotten_tomatoes_link, review_type, critic_name) %>%
    filter(!duplicated(.))
View(df.movie_reviews)

# Count occurance of critics
df.critics_count <- df.movie_reviews %>% 
    count(critic_name, sort = T)

# Get top critics (over 80% of reviews)
df.top_critics <- df.critics_count %>% 
    slice_head(prop = .1)

# Get fresh reviews from top critics
df.movie_reviews_fresh <- df.movie_reviews %>% 
    filter(critic_name %in% df.top_critics$critic_name) %>% 
    filter(review_type == "Fresh")

# Get rotten reviews from top critics
df.movie_reviews_rotten <- df.movie_reviews %>%
    filter(critic_name %in% df.top_critics$critic_name) %>% 
    filter(review_type == "Rotten")

# Combine all reviews, fresh reviews and rotten reviews
df.critics_by_reviews <- df.movie_reviews %>% 
    filter(critic_name %in% df.top_critics$critic_name) %>%
    pairwise_count(item = critic_name, feature = rotten_tomatoes_link, upper = FALSE) %>%
    rename(total = n) |> 
    left_join(
        (df.movie_reviews_fresh %>% 
            pairwise_count(item = critic_name, feature = rotten_tomatoes_link, upper = FALSE) %>%
            rename(fresh = n)),
        by = c("item1", "item2")
    ) |> left_join(
        (df.movie_reviews_rotten %>% 
             pairwise_count(item = critic_name, feature = rotten_tomatoes_link, upper = FALSE) %>%
             rename(rotten = n)),
        by = c("item1", "item2")
    ) %>%
    mutate(across(where(is.numeric), replace_na, 0)) %>%
    mutate(disagree = total - (fresh + rotten)) %>%
    mutate(agree = fresh + rotten)

# Make graph
g.critics_by_reviews <- df.critics_by_reviews |> 
    filter(total >= 500) |> 
    graph_from_data_frame(directed = F, vertices = df.top_critics)

# Calculate agreement rate as edge attribute
E(g.critics_by_reviews)$p_agreement <- (E(g.critics_by_reviews)$total - E(g.critics_by_reviews)$agree) / E(g.critics_by_reviews)$total

# Visualize
g.critics_by_reviews %>% 
    ggraph(., layout = "fr", weights = p_agreement) +
    geom_edge_link0() +
    geom_node_point(aes(size = n)) +
    theme_void()

# Visualize
g.critics_by_reviews %>% 
    delete.vertices(
        .,
        degree(.) <= 1
    ) %>%
    ggraph(., layout = "fr", weights = p_agreement) +
    geom_edge_link(aes(edge_color = p_agreement)) +
    geom_node_point(aes(size = n)) +
    scale_edge_color_gradient(low = "red", high = "green") +
    theme_void()

# Get critics that are close to each other, weighted by rate of aggreement
df.distances <- distances(
    g.critics_by_reviews, 
    v = V(g.critics_by_reviews)[V(g.critics_by_reviews)$name == "Roger Ebert"],
    weights = E(g.critics_by_reviews)$p_agreement
)
df.distances <- tibble(
    critic = rownames(t(df.distances)),
    d = as.numeric(t(df.distances))
)
df.distances %>%
    filter(d != Inf) |> 
    arrange(d)

