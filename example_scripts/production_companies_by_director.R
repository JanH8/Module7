# Read data
source("Scripts/dependencies.R")
df.movies <- read_csv("Data/rotten_tomatoes_movies.csv") 

# Create data frame with relevant information
df.production_companies_directors <- df.movies %>%
    filter(!is.na(production_company)) %>%
    filter(!is.na(directors)) %>%
    select(production_company, directors)

# Count production companies
df.production_companies_count <-  df.production_companies_directors %>% 
    count(production_company, sort = T)

# Count directors
df.directors_count <- df.production_companies_directors %>% 
    count(directors, sort = T)

# Get top directors and companies
df.top_production_companies <- df.production_companies_count %>% 
    slice_head(prop = 0.1)
df.top_directors <- df.directors_count %>% 
    slice_head(prop = 0.6)

# Make graph
g.production_companies <- df.production_companies_directors %>% 
    filter(production_company %in% df.top_production_companies$production_company) %>% 
    filter(directors %in% df.top_directors$directors) %>% 
    pairwise_count(item = production_company, feature = directors, upper = FALSE) %>% 
    igraph::graph_from_data_frame(directed = F, vertices = df.top_production_companies)

length(V(g.production_companies))
length(E(g.production_companies))

# Delete nodes with less than 2 connections
g.production_companies <- delete.vertices(
    g.production_companies, 
    degree(g.production_companies) < 2
)

cluster.production_companies <- cluster_walktrap(g.production_companies, weights = E(g.production_companies)$n)
cluster.production_companies <- cluster_fast_greedy(g.production_companies, weights = E(g.production_companies)$n)
# There are other clustering algorithms included in igraph:

# cluster_edge_betweenness	    Community structure detection based on edge betweenness
# cluster_fast_greedy	        Community structure via greedy optimization of modularity
# cluster_fluid_communities	    Community detection algorithm based on interacting fluids
# cluster_infomap	            Infomap community finding
# cluster_label_prop	        Finding communities based on propagating labels
# cluster_leading_eigen	        Community structure detecting based on the leading eigenvector of the community matrix
# cluster_leiden	            Finding community structure of a graph using the Leiden algorithm of Traag, van Eck & Waltman.
# cluster_louvain	            Finding community structure by multi-level optimization of modularity
# cluster_optimal	            Optimal community structure
# cluster_spinglass	            Finding communities in graphs based on statistical meachanics

# Check the specifics with e.g. ?cluster_spinglass()

g.production_companies %>% 
    ggraph(., layout = "fr", weights = n) +
    geom_edge_link0(aes(edge_alpha = n)) +
    geom_node_point(aes(
        size = n, 
        color = as.character(membership(cluster.production_companies))
    )) +
    theme_void()

g.production_companies_filtered <- delete.vertices(
    g.production_companies,
    membership(cluster.production_companies) != 2
)

g.production_companies_filtered %>% 
    ggraph(., layout = "fr", weights = n) +
    geom_edge_link0(aes(edge_alpha = n)) +
    geom_node_point(aes(
        size = n
    )) +
    geom_node_label(
        aes(label = name),
        repel = TRUE,
        point.padding = unit(.5, "lines")) +
    theme_void()

ggsave(
    plot = last_plot(), 
    file = "Output/Production_companies.png", 
    scale = 2.5, 
    height = 5000, 
    width = 5000, 
    units = "px"
)