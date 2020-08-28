n <- 30
p <- 0.5
q <- 0

redGraph <- erdos.renyi.game(n , p)
blueGraph <- erdos.renyi.game(n , p)


# combining two graph

V(redGraph)$name <- 1:30
V(blueGraph)$name <- 31:60

# set colors for each graph
V(redGraph)$color <- 'red'
V(blueGraph)$color <- 'blue'


attrs <- rbind(as_data_frame(redGraph, "vertices"), as_data_frame(blueGraph, "vertices")) %>% unique()
el <- rbind(as_data_frame(redGraph), as_data_frame(blueGraph))

newGraph <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)

newGraphDegree <- degree(newGraph)

newGraphHistogram <- hist(newGraphDegree)

newGraphAverageDegree <- (2 * (E(newGraph) %>% length()))/(2*n)

# blue subgraph
blueGraphDegree <- degree(blueGraph)

bluegraphHistogram <- hist(blueGraphDegree)

blueGraphAvarageDegree <- (2 * (E(blueGraph) %>% length()))/n

# now we increase the q
q <- 0.14 # 2*ln(countOf(n))/contOf(n)

allOfEdgesBetweenTwoColors <- list()

for (i in 1:30) {
  for (j in 31:60) {
    allOfEdgesBetweenTwoColors[[length(allOfEdgesBetweenTwoColors) + 1]] <- list(i, j)
  }
}

randomEdgesBetweenBlueAndRed <- sample(
  allOfEdgesBetweenTwoColors, ceiling(q * length(allOfEdgesBetweenTwoColors))
)


newGraphConnectedEdges <- numeric()
for (i in randomEdgesBetweenBlueAndRed) {
  newGraphConnectedEdges <- c(newGraphConnectedEdges, i[1], i[2])
}

newGraphConnected <- newGraph %>%  add_edges(newGraphConnectedEdges)
