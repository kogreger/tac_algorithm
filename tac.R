##
## tac.R
##
## Version 0.1.20150118
## Author: Konstantin Greger
##
##
## Implementation of the Traverse-and-Conquer Algorithm (TaC) by K. Greger and 
## M. Heinrichs.
## 
## The algorithm allows to spatially cluster a study area based on certain 
## spatial units (e.g. building blocks) which are delineated by obstacles 
## (e.g. roads) of different obstacle classes (e.g. road classes). The clusters 
## are generated starting from a number of seeds (one seed will result in one 
## cluster). The oulines of the resulting clusters will follow the obstacles. 
## This allows for a "fair" and meaningful spatial clustering based on existing 
## spatial structures.
##
## In this experimental implementation the world is created manually, but 
## builing on the spatial features in the SHP files grid.shp, roads.shp and 
## seeds.shp. These SHP files are also used for the visualization in the end. 
## Any changes to the world therefore would have to be performed in both the 
## SHP files and the initialization section of this R code file.
##
## None of the functions have any error handing, so they will crash (or worse) 
## if used improperly. Use at your own risk!
##


### visualization routine
tacVisualize <- function() {
    # merge cluster result
    shpGrid$clust <- cells$clust
    # plot result on map
    plot(shpGrid, lwd = .1, col = shpGrid$clust + 1)     # grid cells
    plot(shpRoads,                                       # roads
         add = TRUE, 
         col = "white", 
         lwd = shpRoads$class * 5)
    plot(shpSeeds, add = TRUE)                           # seeds
    invisible(text(coordinates(shpGrid),                 # grid cell labels
                   labels = as.character(shpGrid$id), 
                   cex = .7))
    cat ("Press [enter] to continue")
    line <- readline()
}


### initialize
### the artificial test world consists of 21 hexagonal cells in a 6x5 grid, 
### the distances between cell centroids are all the same (see grid.shp)
numOfCells <- 21
numOfObstacleClasses <- 3
numOfSeeds <- 4

## relations
## what are the obstacle classes between neighboring cells?
# initialize all relations as obstacle class 0
rel <- matrix(0, nrow = numOfCells, ncol = numOfCells)
# set matrix diagonal to 0
diag(rel) <- 0
# set actual obstacle classes (see roads.shp)
rel[1, 2] <- 3
rel[1, 7] <- 1
rel[1, 8] <- 1
rel[2, 1] <- 3
rel[2, 3] <- 1
rel[2, 8] <- 3
rel[3, 2] <- 1
rel[3, 4] <- 1
rel[3, 8] <- 3
rel[3, 9] <- 3
rel[3, 10] <- 1
rel[4, 3] <- 1
rel[4, 5] <- 1
rel[4, 10] <- 1
rel[5, 4] <- 1
rel[5, 6] <- 1
rel[5, 10] <- 1
rel[5, 11] <- 1
rel[5, 12] <- 1
rel[6, 5] <- 1
rel[6, 12] <- 1
rel[7, 1] <- 1
rel[7, 8] <- 1
rel[7, 13] <- 1
rel[7, 14] <- 1
rel[8, 1] <- 1
rel[8, 2] <- 3
rel[8, 3] <- 3
rel[8, 7] <- 1
rel[8, 9] <- 2
rel[8, 14] <- 1
rel[9, 3] <- 3
rel[9, 8] <- 2
rel[9, 10] <- 3
rel[9, 14] <- 2
rel[9, 15] <- 1
rel[9, 16] <- 1
rel[10, 3] <- 1
rel[10, 4] <- 1
rel[10, 5] <- 1
rel[10, 9] <- 3
rel[10, 11] <- 1
rel[10, 16] <- 3
rel[11, 5] <- 1
rel[11, 10] <- 1
rel[11, 12] <- 1
rel[11, 16] <- 3
rel[11, 17] <- 1
rel[11, 18] <- 1
rel[12, 5] <- 1
rel[12, 6] <- 1
rel[12, 11] <- 1
rel[12, 18] <- 1
rel[13, 7] <- 1
rel[13, 14] <- 1
rel[13, 19] <- 1
rel[14, 7] <- 1
rel[14, 8] <- 1
rel[14, 9] <- 2
rel[14, 13] <- 1
rel[14, 15] <- 2
rel[14, 19] <- 1
rel[15, 9] <- 1
rel[15, 14] <- 2
rel[15, 16] <- 1
rel[15, 19] <- 2
rel[15, 20] <- 1
rel[16, 9] <- 1
rel[16, 10] <- 3
rel[16, 11] <- 3
rel[16, 15] <- 1
rel[16, 17] <- 3
rel[16, 20] <- 1
rel[17, 11] <- 1
rel[17, 16] <- 3
rel[17, 18] <- 1
rel[17, 20] <- 3
rel[17, 21] <- 1
rel[18, 11] <- 1
rel[18, 12] <- 1
rel[18, 17] <- 1
rel[18, 21] <- 1
rel[19, 13] <- 1
rel[19, 14] <- 1
rel[19, 15] <- 2
rel[20, 15] <- 1
rel[20, 16] <- 1
rel[20, 17] <- 3
rel[21, 17] <- 1
rel[21, 18] <- 1

## distances
## how far are the centroids of neighboring cells away?
# set all distances to 500m
dst <- matrix(0, nrow = numOfCells, ncol = numOfCells)
dst <- (rel != 0) * 500

## seeds
## which cells are the seeds located in? (see seeds2., seeds. & seeds4.shp)
if(numOfSeeds == 2) {         # 2 seeds
    seeds <- data.frame(id = seq(1:numOfSeeds), loc = c(8, 5))
} else if (numOfSeeds == 3) { # 3 seeds
    seeds <- data.frame(id = seq(1:numOfSeeds), loc = c(8, 16, 5))
} else if (numOfSeeds == 4) { # 4 seeds
    seeds <- data.frame(id = seq(1:numOfSeeds), loc = c(8, 16, 5, 4))
}

## cells
cells <- data.frame(id = seq(1:numOfCells), clust = rep(0, numOfCells))
cells$clust[seeds$loc] <- seeds$id

## visualization
library(maptools)
# load SHP files
shpGrid <- readShapePoly("grid.shp")
if(numOfSeeds == 2) {         # 2 seeds
    shpSeeds <- readShapePoints("seeds2.shp")
} else if (numOfSeeds == 3) { # 3 seeds
    shpSeeds <- readShapePoints("seeds.shp")
} else if (numOfSeeds == 4) { # 4 seeds
    shpSeeds <- readShapePoints("seeds4.shp")
}
shpRoads <- readShapeLines("roads.shp")
# order grid cells by cell id
shpGrid <- shpGrid[order(shpGrid$id), ]
tacVisualize()



### start algorithm
for(obstacleClass in 1:numOfObstacleClasses) {
    ## initialize
    iter <- 0                  # counter for iterations per obstacleClass
    everythingConquered = TRUE # flag to stop calculation
    
    ## run
    cat(paste0("Processing obstacleClass: ", obstacleClass, "\n"))
    while(!everythingConquered || iter == 0) {
        tacVisualize()
        iter <- iter + 1
        everythingConquered = TRUE
        attack <- data.frame(victim = 0,   # initialize dataframe for attacks
                             attacker = 0, 
                             dst = 0, 
                             cluster = 0)
        cat(paste0("--Iteration #", iter, "\n"))
        ## scout for suitable cells to be conquered
        for(cell in 1:numOfCells) {
            cat(paste0("  Cell ", cell, " "))
            if(cells$clust[cell] == 0) {
                cat(paste0("is waiting to be conquered.\n"))
            } else {
                cat(paste0("belongs to cluster ", cells$clust[cell], 
                           ", is now trying to conquer neighbors.\n"))
                # identify suitable candidates:
                # - neighboring cells
                # - not conquered yet
                # - currently crossable borders
                candidates <- subset(cells,
                                     dst[cell, ] != 0
                                     & cells$clust == 0
                                     & rel[cell, ] <= obstacleClass
                )
                if(dim(candidates)[1] == 0) {
                    cat(paste0("    No suitable candidates found.\n"))
                } else {
                    everythingConquered = FALSE
                    if(dim(candidates)[1] > 1) {
                        cat(paste0("    Found ", dim(candidates)[1], 
                                   " suitable cells to be conquered, ", 
                                   "selecting ideal victim.\n"))
                    }
                    # look-up distance between cell centroids
                    candidates$dst <- dst[cell, candidates$id]
                    # introduce random variable to handle ties
                    candidates$rnd <- sample(1:dim(candidates)[1], 
                                             dim(candidates)[1], 
                                             replace = FALSE)
                    # order candidates by suitability:
                    # - by minimum distance
                    # - by random variable in case of ties
                    candidates <- candidates[with(candidates, 
                                                  order(-dst, rnd)), ]
                    cat(paste0("    Attempting to attack cell ", 
                               candidates$id[1], ".\n"))
                    # collect all attacks (=attempted conqests)
                    attack <- rbind(attack, c(candidates$id[1],   # victim
                                              cell,               # attacker
                                              candidates$dst[1],  # distance
                                              cells$clust[cell])) # cluster of
                                                                  # attacker
                }
            }
        }
        ## fight actual battles for cells
        if(!everythingConquered) {
            # remove dummy row and order attacks by victim cell id
            attack <- subset(attack[order(attack$victim), ], victim != 0)
            cat(paste0("  Battles for ", length(unique(attack$victim)), 
                       " cells will be fought.\n"))
            # identify winner for each battled cell (victim)
            for(vic in unique(attack$victim)) {
                cat(paste0("    Battle for cell ", vic, ": "))
                battle <- subset(attack, victim == vic)
                if(dim(battle)[1] == 1) {
                    # there is only one attacker for this cell
                    cat(paste0("No conflict, no battle.\n"))
                } else {
                    # there are multiple attackers for this cell
                    # introduce random variable to handle ties
                    battle$rnd <- sample(1:dim(battle)[1], 
                                         dim(battle)[1], 
                                         replace = FALSE)
                    # order attackers:
                    # - by minimum distance
                    # - by random variable in case of ties
                    battle <- battle[with(battle, order(-dst, rnd)), ]
                    cat(paste0("Attacker ", battle$attacker[1], " won.\n"))
                }
                cat(paste0("    Attacker ", battle$attacker[1], 
                           " conquered cell ", cells$id[vic], ".\n"))
                # update cluster information in cell data
                cells$clust[vic] <- battle$cluster[1]
            }
        }
    }
    cat(paste0("  Everything conquered on obstacleClass ", obstacleClass, 
               ", climbing to next obstacleClass.\n"))
}