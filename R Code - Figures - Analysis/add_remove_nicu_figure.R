#SV Scarpino
#Comparison of edge add/remove for https://arxiv.org/abs/1802.02855
#June 2018

#set working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))#sets working directory to source file location

###########
#Libraries#
###########
library(glmnet)
library(ggplot2)
library(wesanderson)

######
#Data#
######
files <- list.files("result-randomization-region/")

data <- list()
for(i in files){
  data[[i]] <- read.csv(paste0("result-randomization-region/", i), header = TRUE)
}

type <- c()
percent <- c()
metric <- c()
error <- c()
for(i in 1:length(data)){
  name_split.i <- unlist(strsplit(names(data)[i], "_"))
  
  errors.i <- c()
  use.cols.data.i <- grep("X", colnames(data[[i]]))
  use.original.i <- which(colnames(data[[i]]) == "X.1")
  rm.col.i <- which(colnames(data[[i]]) == "X")
  use.cols.data.i <- use.cols.data.i[which(! use.cols.data.i %in% c(use.original.i, rm.col.i))]
  for(j in use.cols.data.i){
    error.ij <- (data[[i]][,j] - data[[i]][,use.original.i])
    errors.i <- c(errors.i, error.ij)
  }
  
  type.i <- name_split.i[1]
    
  percent.i <- name_split.i[2]
  if(nchar(percent.i) == 1){
    percent.i <- paste0("0",percent.i)
  }
  
  if(percent.i == 10){ #just presenting 5 and 20
    next
  }
  
  metric.i <- unlist(strsplit(name_split.i[3], ".csv"))
  
  type <- c(type, rep(type.i, length(errors.i)))
  percent <- c(percent, rep(percent.i, length(errors.i)))
  metric <- c(metric, rep(metric.i, length(errors.i)))
  error <- c(error, errors.i)
}

percent <- as.factor(percent)
dat.plot <- data.frame(type, percent, metric, error)

cols <- wes_palette(name = "Darjeeling2", n = 2)

quartz(width = 6, height = 5)
p <- ggplot(dat.plot, aes(x = percent, y = error, fill = metric))
p + geom_violin() + facet_wrap(~type)+scale_fill_manual(values = cols, guide_legend(title = "Metric")) + xlab("Percent") + ylab("Error (Simulated - Actual)") + theme(legend.position = c(0.1, 0.85), legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 14), axis.title = element_text(colour = "black", size = 15), panel.grid.minor = element_line(colour = "#00000050",linetype = 3), panel.grid.major = element_line(colour = "#00000060", linetype = 3)) + scale_x_discrete(expand = c(0.01,0.01)) + scale_y_continuous(expand = c(0.01,0.01), limits = c(-2, 2))


mod <- aov(error~metric*type*percent)
Tukey_mod <- TukeyHSD(mod)
Tukey_mod$`metric:type:percent`