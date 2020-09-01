multiGraph1 <- make_ring(n/2)
multiGraph2 <- make_star(n/2, mode="undirected")
multiGraph <- compose(multiGraph1, multiGraph2)

# degree's avarage
degreeAvarageMultiGraph <- (2 * (E(multiGraph) %>% length()))/n

# avarage of pathes length
avarageShortestPathsMultiGraph <- mean_distance(multiGraph, FALSE, FALSE)

# edge's avarage
edgesAvarageMultiGraph <- (E(multiGraph) %>% length())/n

# clustering coefficient
localClusteringCoefficientMultiGraph <- transitivity(multiGraph, type = "localundirected")
localClusteringCoefficientMultiGraphAvarage <- transitivity(multiGraph, type = "localaverageundirected")
globalClusteringCoefficientMultiGraph <- transitivity(multiGraph, type = "globalundirected")

