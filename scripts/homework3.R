erdosRenyiGraphWithDirectionAndWeighted <- sample_gnp(n, log(n)/n, directed = TRUE)
weights <- length(E(erdosRenyiGraphWithDirectionAndWeighted)) %>% runif(min = 0, max = 99) %>% floor()
E(erdosRenyiGraphWithDirectionAndWeighted)$weight <- weights
erdosRenyiGraphWithDirectionAndWeightedDegree <- degree(erdosRenyiGraphWithDirectionAndWeighted)

headCollection <- matrix(0, nrow = E(erdosRenyiGraphWithDirectionAndWeighted) %>% length(), ncol = 3)
for (e in E(erdosRenyiGraphWithDirectionAndWeighted)) {
  headCollection[e, 1] <- V(
    erdosRenyiGraphWithDirectionAndWeighted
  )[get.edgelist(erdosRenyiGraphWithDirectionAndWeighted)[e,]][1]
  headCollection[e, 2] <- V(
    erdosRenyiGraphWithDirectionAndWeighted
  )[get.edgelist(erdosRenyiGraphWithDirectionAndWeighted)[e,]][2]
  headCollection[e, 3] <- E(erdosRenyiGraphWithDirectionAndWeighted)[e]$weight
}


#subGraph
collectionOfEwithWeightLessThan30 <- numeric()
newWeights <- numeric()

for (e in E(erdosRenyiGraphWithDirectionAndWeighted)) {
  if (E(erdosRenyiGraphWithDirectionAndWeighted)[e]$weight < 30) {
    collectionOfEwithWeightLessThan30 <- collectionOfEwithWeightLessThan30 %>% c(
      V(erdosRenyiGraphWithDirectionAndWeighted)[get.edgelist(erdosRenyiGraphWithDirectionAndWeighted)[e,]][1], 
      V(erdosRenyiGraphWithDirectionAndWeighted)[get.edgelist(erdosRenyiGraphWithDirectionAndWeighted)[e,]][2]
    )
    newWeights <- newWeights %>% c(E(erdosRenyiGraphWithDirectionAndWeighted)[e]$weight)
  }
}
subEderdosRenyiGraphWithDirectionAndWeighted <- erdosRenyiGraphWithDirectionAndWeighted %>% 
  delete_edges(E(erdosRenyiGraphWithDirectionAndWeighted)) %>% 
  add_edges(collectionOfEwithWeightLessThan30)
E(subEderdosRenyiGraphWithDirectionAndWeighted)$label <- newWeights

#MST
MSTOfErdosRenyiGraphWithDirectionAndWeighted <- mst(erdosRenyiGraphWithDirectionAndWeighted)


# Eccentricity
eccentricityerdosRenyiGraphWithDirectionAndWeighted <- eccentricity(erdosRenyiGraphWithDirectionAndWeighted)
radiuserdosRenyiGraphWithDirectionAndWeighted <- radius(erdosRenyiGraphWithDirectionAndWeighted)
diametererdosRenyiGraphWithDirectionAndWeighted <- diameter(erdosRenyiGraphWithDirectionAndWeighted, directed=FALSE)
girtherdosRenyiGraphWithDirectionAndWeighted <- girth(erdosRenyiGraphWithDirectionAndWeighted)
