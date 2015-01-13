## initialize world
dimX <- 5
dimY <- 5
numOfPolygons <- dimX * dimY

## initialize polygons
poly <- matrix(nrow = dimY, ncol = dimX)
rel <- matrix(rep(1, numOfPolygons), nrow = numOfBlocks, ncol = numOfPolygons)
diag(rel) <- 0
rel[3, 4] <- 3
rel[8, 9] <- 3
rel[13, 14] <- 3
rel[18, 19] <- 3
rel[23, 24] <- 3
rel[8, 13] <- 2
rel[9, 14] <- 2
rel[10, 15] <- 2
rel[12, 13] <- 2
rel[8, 13] <- 2
rel[12, 17] <- 2
rel[16, 17] <- 2
rel[21, 22] <- 2

## initialize clusters
seeds <- c(21, 2, 18, 4, 20)
clust <- list(seeds = seeds, conquered = array())
