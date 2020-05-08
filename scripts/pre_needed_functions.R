library(igraph)

# some useful functions

# ln(n)
ln <- function(n) {
  return(log(n, base = exp(1)))
}

# plot a graph
drawGraph <- function(graph, layout = NA) {
  plot.igraph(graph, edge.arrow.size=.3, vertex.label=NA, layout = layout)
}

powMatrix <- function(m, pow) {
  if (pow == 1) return(m)
  
  result <- m
  for (i in 2:pow) {
    result <- result %*% m
  }
  return(result)
}


n <- 30
