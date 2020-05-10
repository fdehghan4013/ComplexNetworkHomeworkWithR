multiGraph1 <- make_ring(n/2)
multiGraph2 <- make_star(n/2, mode="undirected")
multiGraph <- compose(multiGraph1, multiGraph2)

