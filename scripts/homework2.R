erdosRenyiGraphWithDirection <- sample_gnp(n, log(n)/n, directed = TRUE)
erdosRenyiGraphWithDirectionInputDegree <- erdosRenyiGraphWithDirection %>% degree(mode = "in")

# transposed
newEdgeVector <- numeric()
for (e in E(erdosRenyiGraphWithDirection)) {
  newEdgeVector <- newEdgeVector %>% c(
    V(erdosRenyiGraphWithDirection)[get.edgelist(erdosRenyiGraphWithDirection)[e,]][2], 
    V(erdosRenyiGraphWithDirection)[get.edgelist(erdosRenyiGraphWithDirection)[e,]][1]
    )
}

erdosRenyiGraphWithDirectionTransposed <- erdosRenyiGraphWithDirection %>% 
                                                  delete_edges(E(erdosRenyiGraphWithDirection)) %>% 
                                                  add_edges(newEdgeVector)


# subdivision
subdivisionGraph <- erdosRenyiGraphWithDirection
newEdgeVector <- numeric()
for (e in E(erdosRenyiGraphWithDirection)) {
  subdivisionGraph <- subdivisionGraph %>% add_vertices(1)
  newVIndex <- length(V(subdivisionGraph))
  newEdgeVector <- c(newEdgeVector, V(erdosRenyiGraphWithDirection)[get.edgelist(erdosRenyiGraphWithDirection)[e,]][1], newVIndex, newVIndex, V(erdosRenyiGraphWithDirection)[get.edgelist(erdosRenyiGraphWithDirection)[e,]][2])
}
subdivisionGraph <- subdivisionGraph %>% 
  delete_edges(E(subdivisionGraph)) %>% 
  add_edges(newEdgeVector)

# erdos.reyniDirection adjacency matrix
erdosRenyiMatrixWithDirection <- as_adjacency_matrix(erdosRenyiGraphWithDirection, type = "both")

# Eccentricity
eccentricityErdosReyniGraphWithDirection <- eccentricity(erdosRenyiGraphWithDirection)
radiusErdosReyniGraphWithDirection <- radius(erdosRenyiGraphWithDirection)
diameterErdosReyniGraphWithDirection <- diameter(erdosRenyiGraphWithDirection)
girthErdosReyniGraphWithDirection <- girth(erdosRenyiGraphWithDirection)

# induced graph
collectionOfVwithDegreeMoreThan2 <- numeric()
removedV <- numeric()

for (i in 1:length(erdosRenyiGraphWithDirectionInputDegree)) {
  if (erdosRenyiGraphWithDirectionInputDegree[i] >= 2) collectionOfVwithDegreeMoreThan2 <- c(collectionOfVwithDegreeMoreThan2, i)
  else removedV <- c(removedV, i)
}
inducedErdosRenyiGraphWithDirection <- induced_subgraph(erdosRenyiGraphWithDirection, collectionOfVwithDegreeMoreThan2)

# layouts 
homework2Layout <- layout.fruchterman.reingold(completeGraph)
homework2InducedGraphLayout <- homework2Layout
if (length(removedV) != 0) {
  homework2InducedGraphLayout <- homework2Layout[-as.numeric(removedV),]
}

#hamilton cycle
erdosRenyiGraphWithDirectionEdges <- numeric()
for (e in E(erdosRenyiGraphWithDirection)) {
  erdosRenyiGraphWithDirectionEdges <- erdosRenyiGraphWithDirectionEdges %>% 
    c(
      V(erdosRenyiGraphWithDirection)[get.edgelist(erdosRenyiGraphWithDirection)[e,]][1], 
      V(erdosRenyiGraphWithDirection)[get.edgelist(erdosRenyiGraphWithDirection)[e,]][2]
    )
}

hamiltonianGraph <- erdosRenyiGraphWithDirection
hamiltonianEdges <- hamiltonian(erdosRenyiGraphWithDirectionEdges, cycle=TRUE)


