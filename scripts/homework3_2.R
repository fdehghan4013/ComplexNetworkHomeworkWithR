wattz30Nodes <- 30
wattzStrogatzGraph <- sample_smallworld(1, wattz30Nodes, 4, 1)


# degree's avarage
degreeAvarageWattzStrogatzGraph <- (2 * (E(wattzStrogatzGraph) %>% length()))/wattz30Nodes

# avarage of pathes length
avarageShortestPathsWattzStrogatzGraph <- mean_distance(wattzStrogatzGraph, FALSE, FALSE)

# edge's avarage
edgesAvarageWattzStrogatzGraph <- (E(wattzStrogatzGraph) %>% length())/wattz30Nodes

# clustering coefficient and average must be calculate
localClusteringCoefficientWattzStrogatzGraph <- transitivity(wattzStrogatzGraph, type = "localundirected")
localClusteringCoefficientAverageWattzStrogatzGraph <- transitivity(wattzStrogatzGraph, type = "localaverageundirected")
globalClusteringCoefficientWattzStrogatzGraph <- transitivity(wattzStrogatzGraph, type = "globalundirected")

# with 1000 node
wattz1000Nodes <- 1000
wattzStrogatzGraphWith1000Node <- sample_smallworld(1, wattz1000Nodes, 4, 1)

# degree's avarage
degreeAvarageWattzStrogatzGraphWith1000Node <- (2 * (E(wattzStrogatzGraphWith1000Node) %>% length()))/wattz1000Nodes

# avarage of pathes length
avarageShortestPathsWattzStrogatzGraphWith1000Node <- mean_distance(wattzStrogatzGraphWith1000Node, FALSE, FALSE)

# edge's avarage
edgesAvarageWattzStrogatzGraphWith1000Node <- (E(wattzStrogatzGraphWith1000Node) %>% length())/wattz1000Nodes

# clustering coefficient and average must be calculate
localClusteringCoefficientWattzStrogatzGraphWith1000Node <- transitivity(wattzStrogatzGraphWith1000Node, type = "localundirected")
localClusteringCoefficientAverageWattzStrogatzGraphWith1000Node <- transitivity(wattzStrogatzGraphWith1000Node, type = "localaverageundirected")
globalClusteringCoefficientWattzStrogatzGraphWith1000Node <- transitivity(wattzStrogatzGraphWith1000Node, type = "globalundirected")

# with 100000 node
wattz100000Nodes <- 100000
wattzStrogatzGraphWith100000Node <- sample_smallworld(1, wattz100000Nodes, 4, 1)

# degree's avarage
degreeAvarageWattzStrogatzGraphWith100000Node <- (2 * (E(wattzStrogatzGraphWith100000Node) %>% length()))/wattz100000Nodes
# avarage of pathes length
avarageShortestPathsWattzStrogatzGraphWith100000Node <- mean_distance(wattz100000Nodes, FALSE, FALSE)

# edge's avarage
edgesAvarageWattzStrogatzGraphWith100000Node <- (E(wattzStrogatzGraphWith100000Node) %>% length())/wattz100000Nodes

# clustering coefficient and average must be calculate
localClusteringCoefficientWattzStrogatzGraphWith100000Node <- transitivity(wattzStrogatzGraphWith100000Node, type = "localundirected")
localClusteringCoefficientAverageWattzStrogatzGraphWith100000Node <- transitivity(wattzStrogatzGraphWith100000Node, type = "localaverageundirected")
globalClusteringCoefficientWattzStrogatzGraphWith100000Node <- transitivity(wattzStrogatzGraphWith100000Node, type = "globalundirected")

