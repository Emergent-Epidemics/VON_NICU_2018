#Figures and Analysis
#SV Scarpino 
#May 7th 2018

#libraries
library(ggplot2)
library(lmtest)
library(MASS)
library(lme4)
library(stats)
library(ggmap)
library(maptools)
library(gpclib)
library(sp)
library(adehabitatHR)
library(wesanderson)
library(rgeos)
library(polyclip)
library(igraph)

#########
#Globals#
#########

#########################
#Data and Acc. functions#
#########################
dat <- read.csv("data/nicuWithHierarchyLevel-M03-D22.csv")

info_node <- read.csv("data/infoNode-split0.csv")

born_at_3_4 <- read.csv("data/born at 3 or 4.csv")

community <- read.csv("data/byCommunity-april-12.csv")

nicu_states_class <- read.csv("data/statepolicy.csv")

by_state_frac <- read.csv("data/by-community-with-frac-state-nov-13-17.csv")

hosp_level <- read.csv("data/hospital-level.csv")

transfer_percents <- read.csv("data/by_community_trasnfer2.csv")

source("acc code and functions.R")

#######################################################################################################Excluding small communnities (1-2) transfers and Puerto Rico because there's an issue with the transfer data#########################################################################################
######################################################################################################

rm_coms <- c(3, 4, 14, 29, 33)
rm_coms_locs <- which(community$groupID %in% rm_coms)
community <- community[-rm_coms_locs,]

###################################
#Cartoon map figure (currently F1)#
###################################
unq_coms <-  unique(info_node$level2)
unq_coms <- na.omit(unq_coms)
unq_coms <- unq_coms[-which(unq_coms %in% rm_coms)]
                    
usa <- readShapePoly("cb_2015_us_state_20m/cb_2015_us_state_20m.shp")
pal <- wes_palette("FantasticFox", n = length(unq_coms), type = "continuous")
pal <- sample(pal, length(pal))

quartz(width = 8,height = 5)
plot(usa, lwd = 0.2, xlim = c(-123, -69),  ylim = c(35, 40), border = "#000000")

coms <- c()
for(thresh in c(90)){
  for(i in 1:length(unq_coms)){
    # if(! unq_coms[i] %in% use){
    #   next
    # }
    use.i <- which(info_node$level2 == unq_coms[i])
    
    #points.i <- SpatialPoints(cbind(jitter(x = dat$longitudeitude[use.i]), jitter(dat$latitudeitude[use.i])))
    mat.i <- cbind(info_node$longitude[use.i], info_node$latitude[use.i])
    mat.i <- na.omit(mat.i)
    points.i <- SpatialPoints(mat.i)
    
    if(length(use.i) <= 5){
      pol.i <-  gConvexHull(points.i)
      coords.i <- coordinates(pol.i)
      if(is(coords.i)[1] != "matrix"){
        coords.i <-  coords.i[[1]][[1]]
      }
    }else{
      pol.i <- mcp(points.i, thresh)
      coords.i <- fortify(pol.i)[,c("long","lat")]
    }
    
    if(nrow(coords.i) < 2){next}
    
    pol.clip.i <- polyoffset(list(list(x = coords.i[,1], y = coords.i[,2])), delta = 0.5, jointype = "round")
    
    if(length(pol.clip.i) == 0){
      polygon(x = coords.i[,1], y = coords.i[,2], col = paste0(pal[i], "10"), border = "#00000000") 
    }else{
      polygon(pol.clip.i[[1]], col = paste0(pal[i], "90"), border = "#00000000")
    } 
  }
}

########################################
#Distribution of h index (currently F2)#
########################################
hist(community$vonEntropy, breaks = 20, xlab = "Regionalization index", main = "", col = "gray")

#######################
#Network Metrics Plots#
#######################
#F3
quartz()
layout(matrix(c(1,1,1,2,2,2,3,3,4,4,5,5), nrow = 2, byrow = TRUE))
plot(community$vonPageRank, community$vonEntropy, col = "#666666", pch = 16, main = "", bty = "n", ylab = "h index")
plot(community$vonBweenNess, community$vonEntropy, col = "#1b9e77", pch = 16, main = "", bty = "n", ylab = "h index")
plot(community$flowHierarchy, community$vonEntropy, col = "#7570b3", pch = 16, main = "", bty = "n", ylab = "h index")
plot(community$reciprocity, community$vonEntropy, col = "#d6604d", pch = 16, main = "", bty = "n", ylab = "h index")
boxplot(community$vonEntropy ~ community$n_layers, col = "#67a9cf")

#F4
quartz()
layout(matrix(1))
cols <- wes_palette("Royal1", n = 2, type = "continuous")
cols_points <- paste0(cols, "75")
plot(community$vonEntropy, 100-community$transfer_prop, pch = 16, main = "", bty = "n", xlab = "h index", ylim = c(0, 100), col = cols_points[1], cex = 1.2)
points(community$vonEntropy, 100-community$transfer_prop, col = cols[1], cex = 1.2)
abline(lm(100-community$transfer_prop~community$vonEntropy), col = cols[1], lwd = 3, lty = 3)

#################################
#Prop. Not Transfered Regression#
#################################
community$ratio_VON <- community$VONhosps/community$total_hosp
m <- lm(transfer_prop ~ vonEntropy + back_trans_reimburse + ratio_VON, data = community)
m1 <- lm(transfer_prop ~ vonPageRank + back_trans_reimburse + ratio_VON, data = community)
m2 <- lm(transfer_prop ~ vonBweenNess + back_trans_reimburse + ratio_VON, data = community)
m3 <- lm(transfer_prop ~ flowHierarchy + back_trans_reimburse + ratio_VON, data = community)
m4 <- lm(transfer_prop ~ as.factor(n_layers) + back_trans_reimburse + ratio_VON, data = community)

full_dat <- community[,c("vonEntropy", "vonPageRank", "vonBweenNess", "flowHierarchy", "n_layers", "back_trans_reimburse", "ratio_VON")]
summary(lm(community$transfer_prop ~ ., data = full_dat))

#######################
#Network Metrics Table#
#######################
tab_dat <- community[,c("vonEntropy", "n_layers", "vonPageRank", "vonBweenNess", "reciprocity", "flowHierarchy", "ratio_VON", "total_hosp")]

summary(lm(1-community$transfer_prop ~ as.matrix(tab_dat)))

apply(tab_dat, 2, median, na.rm = TRUE)
apply(tab_dat, 2, mean, na.rm = TRUE)
apply(tab_dat, 2, summary, na.rm = TRUE)

summary(lm(community$vonEntropy~community$vonPageRank))
summary(lm(community$vonEntropy~community$vonBweenNess))
summary(lm(community$vonEntropy~community$flowHierarchy))
summary(lm(community$vonEntropy~community$reciprocity))
summary(aov(community$vonEntropy~as.factor(community$n_layers)))
TukeyHSD(aov(community$vonEntropy~as.factor(community$n_layers)))
by(community$vonEntropy, as.factor(community$n_layers), mean)