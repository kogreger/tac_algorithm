##
## tac.R
##
## Version 0.5.20150226
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
## None of the functions have any error handing, so they will crash (or worse) 
## if used improperly. Use at your own risk!
##
## Change log:
## v.0.2.20150120   switched to swarm conquering
## v.0.3.20150123   use real data from database instead of dummy data, disable
##                  map visualization
## v.0.4.20150126   switched to more effective cell handling using update-flag
## v.0.4.20150127   bug fix for complete model run
## v.0.5.20150129   further improved cell handling using completed-flag
## v.0.5.20150213   implemented execution timing
## v.0.5.20150227   bug fixes and significant performance improvements, fix of 
##                  tacStats visualization
##


### initialize
debug <- TRUE
sink(paste0("tac_", studyArea,".log"))
tacStartTime <- proc.time()
studyArea <- "hamburg"    # braunschweig, hamburg, test
## load data from database
cat(paste0("Initializing TaC algorithm v.0.5.20150226 for ", studyArea))
library(dplyr)
library(ggplot2)
library(maptools)
library(reshape2)
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
rel <- psqlGetTable(connection, "temp", paste0(studyArea, "_relationships"))
numOfRelationships <- nrow(rel)
numOfObstacleClasses <- max(rel$obst) - min(rel$obst) + 1
# seeds
if(debug) cat(".")
seeds <- psqlQuery(connection, 
                   paste0("SELECT s.gid id, c.gid loc ", 
                          "FROM temp.", studyArea, "_seeds s, ", 
                          "temp.", studyArea, "_cells c ", 
                          "WHERE ST_Within(s.the_geom, c.the_geom) ", 
                          "ORDER BY s.gid"))
numOfSeeds <- nrow(seeds)
# cells
if(debug) cat(".")
cells <- psqlQuery(connection, 
                   paste0("SELECT DISTINCT(a) id ", 
                          "FROM temp.", studyArea, "_relationships ", 
                          "ORDER BY a"))
cells$clust <- seeds$id[match(cells$id, seeds$loc)]
cells$surrounded <- FALSE
numOfCells <- nrow(cells)
invisible(dbDisconnect(connection))
# internal statistics
iter <- 0                                   # counter for iterations
tacStats <- data.frame(iter = integer(),    # data frame for general statistics
                       obstClass = integer(), 
                       battles = integer())
clustStats <- matrix(cells$clust)           # matrix for cluster statistics
if(debug) cat(".")
cat(" done.\n\n")



### start algorithm
cat("Starting calculation\n")
for(obstClass in min(rel$obst):max(rel$obst)) {
    ## initialize
    everythingConquered = FALSE    # flag to stop calculation
    # identify scouting cells
    scouting <- filter(cells, !is.na(clust), !surrounded) %>%
        collect %>% 
        .[["id"]]
    cells$updated[cells$id %in% scouting] <- TRUE
    
    ## run
    if(debug) cat(paste0("-- Processing obstClass: ", obstClass, "\n"))
    while(!everythingConquered) {
        iter <- iter + 1
        startTime <- proc.time()
        everythingConquered = TRUE
        if(debug) cat(paste0("- Iteration #", iter, "\n"))
        
        ## scout for suitable cells to be conquered by cells that have been 
        ##  updated in previous iteration (or from start)
        toDo <- cells %>% 
            filter(updated, !surrounded) %>% 
            collect %>% 
            .[["id"]]
        if(debug) cat(paste0(length(toDo), 
                             " cells will scout for cells to be conquered.\n"))
        # identify attacks:
        # - neighboring cells
        # - currently crossable borders
        # - and not conquered yet
        attacks <- filter(rel, a %in% toDo, dst != 0, obst <= obstClass) %>%
            left_join(cells, by = c("a" = "id")) %>% 
            left_join(cells, by = c("b" = "id")) %>% 
            filter(is.na(clust.y)) %>% 
            select(attacker = a, victim = b, dst, cluster = clust.x) %>% 
            arrange(victim)
        # don't check attacker cells in next iteration
        cells$updated[cells$id %in% toDo] <- FALSE
        if(nrow(attacks) != 0) 
            everythingConquered = FALSE
                
        ## fight actual battles for cells
        if(!everythingConquered) {
            if(debug) cat(paste0("Battles for ", 
                                 nrow(group_by(attacks, victim) %>% 
                                          summarize(attackers = n())), 
                                 " cells will be fought.\n"))
            # identify winner for each victim cell using minimum distance of
            #  attacker cell
            conquests <- group_by(attacks, victim) %>% 
                filter(rank(dst) == 1) %>% 
                select(attacker, victim, cluster)
            # assign conquered victims to winning attacker's cluster:
            #  - update cluster information in cell data
            #  - mark cell to be checked in next iteration
            cells[cells$id %in% 
                      conquests$victim, c(2, 4)] <- list(conquests$cluster, 
                                                         TRUE)
            # check for cells whose neighbors have all been conquered
            notSurrounded <- 
                filter(cells, !surrounded) %>% 
                left_join(rel, by = c("id" = "a")) %>% 
                left_join(cells, by = c("b" = "id")) %>% 
                group_by(id, clust.y) %>% 
                summarize(count = n()) %>% 
                filter(is.na(clust.y)) %>% 
                collect %>% 
                .[["id"]]
            surrounded <- 
                filter(cells, !surrounded, !is.na(clust), 
                       !(id %in% notSurrounded)) %>% 
                collect %>% 
                .[["id"]]
            # set surrounded flag
            cells$surrounded[cells$id %in% surrounded] <- TRUE
            if(debug)
                if(length(surrounded) > 0) 
                    cat(paste0("All neighboring cells of cells ", 
                               paste(surrounded, collapse = ', '), 
                               " have been conquered, hence these cells will ", 
                               "not be checked in upcoming iterations.\n"))
        }
        
        # update internal statistics
        if(debug)
            cat(paste0(c("--", 
                         "\nIteration:        ", 
                         iter, 
                         "\nObstacle class:   ", 
                         obstClass, 
                         "\nConquering cells: ", 
                         length(toDo), 
                         "\nAttacked cells:   ", 
                         nrow(group_by(attacks, victim) %>% 
                                  summarize(victims = n())), 
                         "\nSurrounded cells: ", 
                         length(surrounded), 
                         "\nCalculation time: ", 
                         round(as.double((proc.time() - startTime)[3]), 2), 
                         "\n--\n\n")))
        tacStats <- rbind(tacStats, c(iter, 
                                      obstClass, 
                                      length(toDo), 
                                      nrow(group_by(attacks, victim) %>% 
                                               summarize(victims = n())), 
                                      length(surrounded), 
                                      filter(cells, surrounded) %>% 
                                          summarize(cumSurr = n()) %>% 
                                          collect %>% 
                                          .[["cumSurr"]], 
                                      round(as.double((proc.time() - 
                                                           startTime)[3]), 2)))
        clustStats <- cbind(clustStats, cells$clust)
        ## export resulting cluster data
        write.csv2(cells, paste0("cells_output_", studyArea,".csv"))
    }
    if(debug) cat(paste0("-- Everything conquered on obstClass ", 
                         obstClass, 
                         ", climbing to next obstClass.\n"))
}



### wrap up
## internal statistics
# finish up
names(tacStats) <- c("iter", "obstClass", "toDo", "victims", "surr", "cumSurr", 
                     "procTime")
# visualize internal statistics
png(filename = paste0("tacStats_", studyArea,".png"),
    width = 17, height = 11.15, units = "cm", res = 300, pointsize = 10)
ggplot(melt(select(tacStats, iter, obstClass, toDo, cumSurr), 
            id = c("iter", "obstClass"))) + 
    geom_line(aes(x = iter, 
                  y = value, 
                  linetype = variable, 
                  color = factor(obstClass)), 
    ) + 
    scale_linetype_manual(values = c("dashed", "solid"), 
                          labels = c("Scouting", "Completed")) + 
    scale_x_continuous(name = "Iterations") + 
    scale_y_continuous(name = "Cells") + 
    guides(linetype = guide_legend(title = "Cells"), 
           color = guide_legend(title = "Obstacle Class")) + 
    theme_bw()
dev.off()
## export resulting cluster data
write.csv2(cells, paste0("cells_output_", studyArea,".csv"))

cat(paste0("Done in ", round(as.double((proc.time() - tacStartTime)[3]), 2), " seconds."))