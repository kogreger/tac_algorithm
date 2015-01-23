##
## tac.R
##
## Version 0.3.20150123
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
## Change log:
## v.0.2.20150120   switched to swarm conquering
## v.0.3.20150123   use real data from database instead of dummy data
##                  disable visualization
##


### initialize
debug <- FALSE
## load data from database
if(debug) cat("Initializing TaC algorithm.")
library(maptools)
library(RPostgreSQL)
source("psqlHelper.R")
connection <- dbConnect(dbDriver("PostgreSQL"), 
                        host = "129.247.221.172", 
                        port = "5432", 
                        user = "postgres", 
                        password = "postgres", 
                        dbname = "tapas")
# relationships
if(debug) cat(".")
rel <- psqlGetTable(connection, "temp", "hamburg_relationships")
numOfRelationships <- dim(rel)[1]
numOfObstacleClasses <- range(rel$obst)[2] - range(rel$obst)[1] + 1
# seeds
if(debug) cat(".")
seeds <- psqlQuery(connection, 
                   paste0("SELECT s.gid id, c.gid loc ", 
                          "FROM temp.hamburg_seeds s, temp.hamburg_cells c ", 
                          "WHERE ST_Within(s.the_geom, c.the_geom) ", 
                          "ORDER BY s.gid"))
numOfSeeds <- dim(seeds)[1]
# cells
if(debug) cat(".")
cells <- psqlQuery(connection, 
                   paste0("SELECT DISTINCT(a) id ", 
                          "FROM temp.hamburg_relationships ", 
                          "ORDER BY a"))
cells$clust <- seeds$id[match(cells$id, seeds$loc)]
numOfCells <- dim(cells)[1]
dbDisconnect(connection)
# internal statistics
iter <- 0                                   # counter for iterations
tacStats <- data.frame(iter = integer(),    # data frame for general statistics
                       obstClass = integer(), 
                       battles = integer())
clustStats <- matrix(cells$clust)           # matrix for cluster statistics
if(debug) cat(".\n")
if(debug) cat("Initialization finished\n\n")



### start algorithm
if(debug) cat("Starting calculation\n")
for(obstClass in range(rel$obst)[1]:range(rel$obst)[2]) {
    ## initialize
    everythingConquered = TRUE # flag to stop calculation
    
    ## run
    if(debug) cat(paste0("----Processing obstClass: ", obstClass, "\n"))
    while(!everythingConquered || iter == 0) {
        iter <- iter + 1
        everythingConquered = TRUE
        attack <- data.frame(attacker = 0,   # initialize dataframe for attacks
                             victim = 0, 
                             dst = 0, 
                             cluster = 0)
        if(debug) cat(paste0("--Iteration #", iter, "\n"))
        ## scout for suitable cells to be conquered
        for(cell in cells$id) {
            if(debug) cat(paste0("  Cell ", cell, " "))
            if(with(cells, 
                    (clust[id == cell] == 0) | (is.na(clust[id == cell])))) {
                if(debug) cat(paste0("is waiting to be conquered.\n"))
            } else {
                if(debug) cat(paste0("belongs to cluster ", 
                                     with(cells, clust[id == cell]), 
                                     ", is now trying to conquer neighbors.\n"))
                # identify suitable candidates:
                # - neighboring cells
                # - currently crossable borders
                cands <- rel[rel$a == cell & 
                                 rel$dst != 0 & 
                                 rel$obst <= obstClass
                                 , ]
                # - and not conquered yet
                cands <- cands[is.na(cells$clust[cells$id %in% cands$b]), ]
                if(dim(cands)[1] == 0) {
                    if(debug) cat(paste0("    No suitable candidates found.\n"))
                } else {
                    everythingConquered = FALSE
                    candidateList <- character()
                    if(debug) cat(paste0("    Found ", dim(cands)[1], 
                                         " suitable cells to be conquered: "))
                    # collect all attacks (=attempted conqests)
                    for(cand in 1:dim(cands)[1]) {
                        candidateList <- paste(candidateList, 
                                               cands$b[cand], 
                                               sep = ", ")
                        attack <- rbind(attack, 
                                        c(cell,                      # attacker
                                          cands$b[cand],             # victim   
                                          cands$dst[cand],           # distance
                                          cells$clust[cells$id == cell]))
                                                                     # cluster
                    }
                    if(debug) cat(paste0(substr(candidateList, 
                                                3, 
                                                nchar(candidateList)), 
                                         "\n"))
                }
            }
        }
        ## fight actual battles for cells
        if(!everythingConquered) {
            # remove dummy row and order attacks by victim cell id
            attack <- subset(attack[order(attack$victim), ], victim != 0)
            if(debug) cat(paste0("  Battles for ", 
                                 length(unique(attack$victim)), 
                                 " cells will be fought.\n"))
            # identify winner for each battled cell (victim)
            for(vic in unique(attack$victim)) {
                if(debug) cat(paste0("    Battle for cell ", vic, ": "))
                battle <- subset(attack, victim == vic)
                if(dim(battle)[1] == 1) {
                    # there is only one attacker for this cell
                    if(debug) cat(paste0("No conflict, no battle.\n"))
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
                    if(debug) cat(paste0("Attacker ", 
                                         battle$attacker[1], 
                                         " won.\n"))
                }
                if(debug) cat(paste0("    Attacker ", 
                                     battle$attacker[1], 
                                     " conquered cell ", 
                                     battle$victim[1], ".\n"))
                # update cluster information in cell data
                cells$clust[cells$id == vic] <- battle$cluster[1]
            }
        }
        # update internal statistics
        cat(c(iter, obstClass, length(unique(attack$victim))))
        tacStats <- rbind(tacStats, c(iter, obstClass, length(unique(attack$victim))))
        clustStats <- cbind(clustStats, cells$clust)
    }
    if(debug) cat(paste0("  Everything conquered on obstClass ", 
                         obstClass, 
                         ", climbing to next obstClass.\n"))
}
if(debug) cat("Done.")