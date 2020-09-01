snobbishComplexNetworkGraph <- function (n, p, q) {
  redGraph <- erdos.renyi.game(n , p)
  blueGraph <- erdos.renyi.game(n , p)


  # combining two graph

  V(redGraph)$name <- 1:n
  V(blueGraph)$name <- (n + 1):(2*n)

  # set colors for each graph
  V(redGraph)$color <- 'red'
  V(blueGraph)$color <- 'blue'

  attrs <- rbind(as_data_frame(redGraph, "vertices"), as_data_frame(blueGraph, "vertices")) %>% unique()
  el <- rbind(as_data_frame(redGraph), as_data_frame(blueGraph))

  newGraph <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)

  # calculate all of the posibilities for edges between blue and red nodes
  allOfEdgesBetweenTwoColors <- list()

  for (i in 1:n) {
    for (j in (n + 1):(2*n)) {
      allOfEdgesBetweenTwoColors[[length(allOfEdgesBetweenTwoColors) + 1]] <- list(i, j)
    }
  }

  # now take a sample of edges with the given probality
  randomEdgesBetweenBlueAndRed <- sample(
    allOfEdgesBetweenTwoColors, ceiling(q * length(allOfEdgesBetweenTwoColors))
  )


  # now we add the edges to the main graph (if the given probality doesn't equal to zero)
  newGraphConnectedEdges <- numeric()
  for (i in randomEdgesBetweenBlueAndRed) {
    newGraphConnectedEdges <- c(newGraphConnectedEdges, i[1], i[2])
  }

  return(newGraph %>%  add_edges(newGraphConnectedEdges))

}

snobbishComplexNetworkGraphConnected <- function (n, p) {
  q <- 2*ln(2*n)/(2*n)
  return(snobbishComplexNetworkGraph(n, p, q))
}

getSnobbishBlueGraph <- function(g) {
  count <- length(V(g))
  n <- count/2
  return(induced_subgraph(g, (n+1):count))
}
# with 60 node and not connected
snobbish30Node <- 30
snobbishWith60Node <- snobbishComplexNetworkGraph(snobbish30Node, 0.5, 0)
snobbishWith60NodeDegree <- degree(snobbishWith60Node)
#newGraphHistogram <- hist(newGraphDegree)
degreeAvarageSnobbishComplexNetworkWith60Node <- (2 * (E(snobbishWith60Node) %>% length()))/(2*snobbish30Node)

# with 600 node
snobbish600Node <- 300
snobbishComplexNetworkGraphConnectedWith600Node <- snobbishComplexNetworkGraphConnected(snobbish600Node, 0.5)

snobbishComplexNetworkGraphConnectedWith600NodeDegree <- degree(snobbishComplexNetworkGraphConnectedWith600Node)


# degree's avarage
degreeAvarageSnobbishComplexNetworkWith600Node <- (2 * (E(snobbishComplexNetworkGraphConnectedWith600Node) %>% length()))/(2*snobbish600Node)

# blue subgraph
blueGraph <- getSnobbishBlueGraph(snobbishComplexNetworkGraphConnectedWith600Node)

blueGraphDegree <- degree(blueGraph)

#bluegraphHistogram <- hist(blueGraphDegree)

blueGraphAvarageDegree <- (2 * (E(blueGraph) %>% length()))/n


