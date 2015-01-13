## initialize
dimX <- 5
dimY <- 5
numOfBlocks <- dimX * dimY
blocks <- matrix(nrow = dimY, ncol = dimX)

## populate blocks
rel <- matrix(rep(1, numOfBlocks), nrow = numOfBlocks, ncol = numOfBlocks)
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