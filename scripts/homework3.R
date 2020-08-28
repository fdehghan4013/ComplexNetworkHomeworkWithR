erdosRenyiGraphWithDirectionAndWeighted <- sample_gnp(n, log(n)/n, directed = TRUE)
weights <- length(E(erdosRenyiGraphWithDirectionAndWeighted)) %>% runif(min = 0, max = 99) %>% floor()
E(erdosRenyiGraphWithDirectionAndWeighted)$weight <- weights
erdosRenyiGraphWithDirectionAndWeightedDegree <- erdosRenyiGraphWithDirectionAndWeighted %>% degree(mode = "in")

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
removedV <- numeric()

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
#E(subEderdosRenyiGraphWithDirectionAndWeighted)$label <- newWeights
E(subEderdosRenyiGraphWithDirectionAndWeighted)$weight <- newWeights

headCollectionOfSubGraph <- matrix(0, nrow = E(subEderdosRenyiGraphWithDirectionAndWeighted) %>% length(), ncol = 3)
for (e in E(subEderdosRenyiGraphWithDirectionAndWeighted)) {
  headCollectionOfSubGraph[e, 1] <- V(
    subEderdosRenyiGraphWithDirectionAndWeighted
  )[get.edgelist(subEderdosRenyiGraphWithDirectionAndWeighted)[e,]][1]
  headCollectionOfSubGraph[e, 2] <- V(
    subEderdosRenyiGraphWithDirectionAndWeighted
  )[get.edgelist(subEderdosRenyiGraphWithDirectionAndWeighted)[e,]][2]
  headCollectionOfSubGraph[e, 3] <- E(subEderdosRenyiGraphWithDirectionAndWeighted)[e]$weight
}
#MST

MSTOfErdosRenyiGraphWithDirectionAndWeighted <- mst(erdosRenyiGraphWithDirectionAndWeighted )

headCollectionOfMST <- matrix(0, nrow = E(MSTOfErdosRenyiGraphWithDirectionAndWeighted) %>% length(), ncol = 3)
for (e in E(MSTOfErdosRenyiGraphWithDirectionAndWeighted)) {
  headCollectionOfMST[e, 1] <- V(
    MSTOfErdosRenyiGraphWithDirectionAndWeighted
  )[get.edgelist(MSTOfErdosRenyiGraphWithDirectionAndWeighted)[e,]][1]
  headCollectionOfMST[e, 2] <- V(
    MSTOfErdosRenyiGraphWithDirectionAndWeighted
  )[get.edgelist(MSTOfErdosRenyiGraphWithDirectionAndWeighted)[e,]][2]
  headCollectionOfMST[e, 3] <- E(MSTOfErdosRenyiGraphWithDirectionAndWeighted)[e]$weight
}
# Eccentricity
eccentricityerdosRenyiGraphWithDirectionAndWeighted <- eccentricity(erdosRenyiGraphWithDirectionAndWeighted)
radiuserdosRenyiGraphWithDirectionAndWeighted <- radius(erdosRenyiGraphWithDirectionAndWeighted)
diametererdosRenyiGraphWithDirectionAndWeighted <- diameter(erdosRenyiGraphWithDirectionAndWeighted )
girtherdosRenyiGraphWithDirectionAndWeighted <- girth(erdosRenyiGraphWithDirectionAndWeighted)


# degree's avarage
degreeAvarageDirectedWithWeight <- (weights %>% sum())/n

# avarage of pathes length
avarageShortestPathsDirectedWithWeight <- mean_distance(erdosRenyiGraphWithDirectionAndWeighted, TRUE, FALSE)

# edge's avarage
edgesAvarageDirectedWithWeight <- (E(erdosRenyiGraphWithDirectionAndWeighted) %>% length())/n

# clustering coefficient
localClusteringCoefficientDirectedWithWeight <- transitivity(erdosRenyiGraphWithDirectionAndWeighted, type = "local", weights = weights)
localClusteringCoefficientAvarageDirectedWithWeight <- transitivity(erdosRenyiGraphWithDirectionAndWeighted, type = "localaverage", weights = weights)
globalClusteringCoefficientDirectedWithWeight <- transitivity(erdosRenyiGraphWithDirectionAndWeighted, type = "global", weights = weights)
