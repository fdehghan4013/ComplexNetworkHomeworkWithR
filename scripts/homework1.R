p <- (2 * ln(n)) / n

# complete graph
completeGraph <- make_full_graph(n)
completeGraphDegree <- degree(completeGraph)

# erdos.reyni graph
erdosRenyiGraph <- sample_gnp(n, p)
erdosRenyiGraphDegree <- degree(erdosRenyiGraph)

# complementery graph
complementeryGraph <- complementer(erdosRenyiGraph)
complementeryGraphDegree <- degree(complementeryGraph)

# erdos.reyni adjacency matrix
erdosRenyiMatrix <- as_adjacency_matrix(erdosRenyiGraph, type = "both")

erdosRenyiMatrixPow2 <- powMatrix(erdosRenyiMatrix, 2)
erdosRenyiMatrixPow5 <- powMatrix(erdosRenyiMatrix, 5)
erdosRenyiMatrixPow15 <- powMatrix(erdosRenyiMatrix, 15)
erdosRenyiMatrixPow30 <- powMatrix(erdosRenyiMatrix, 30)

# Eccentricity
eccentricityErdosReyniGraph <- eccentricity(erdosRenyiGraph)
radiusErdosReyniGraph <- radius(erdosRenyiGraph)
diameterErdosReyniGraph <- diameter(erdosRenyiGraph, directed=FALSE)
girthErdosReyniGraph <- girth(erdosRenyiGraph)

# density
density <- edge_density(erdosRenyiGraph)

# induced graph
collectionOfVwithDegreeMoreThan3 <- numeric()
removedV <- numeric()

for (i in 1:length(erdosRenyiGraphDegree)) {
  if (erdosRenyiGraphDegree[i] >= 3) collectionOfVwithDegreeMoreThan3 <- c(collectionOfVwithDegreeMoreThan3, i)
  else removedV <- c(removedV, i)
}
inducedErdosRenyiGraph <- induced_subgraph(erdosRenyiGraph, collectionOfVwithDegreeMoreThan3)


# layouts 
homework1Layout <- layout.fruchterman.reingold(completeGraph)
homework1InducedGraphLayout <- homework1Layout
if (length(removedV) != 0) {
  homework1InducedGraphLayout <- homework1Layout[-as.numeric(removedV),]
}
