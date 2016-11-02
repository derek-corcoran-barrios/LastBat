
#load packages
library(unmarked)
library(caret)
library(ggplot2)
library(texreg)
#load each species model
best2.My.Th2 <- readRDS("best2.My.Th2.rds")
best2.Ta.Br2 <- readRDS("best2.Ta.Br2.rds")
best2.Pa.He2 <- readRDS("best2.Pa.He2.rds")
best2.My.Yu2 <- readRDS("best2.My.Yu2.rds")
best2.My.Lu2 <- readRDS("best2.My.Lu2.rds")
best2.My.Ev2 <- readRDS("best2.My.Ev2.rds")
best2.My.Ca2 <- readRDS("best2.My.Ca2.rds")
best2.La.No2 <- readRDS("best2.La.No2.rds")
best2.La.Ci2 <- readRDS("best2.La.Ci2.rds")
best2.La.Bl2 <- readRDS("best2.La.Bl2.rds")
best2.Eu.Ma2 <- readRDS("best2.Eu.Ma2.rds")
best2.Ep.Fu2 <- readRDS("best2.Ep.Fu2.rds")




#load prprocessing and variables
preprocov <- readRDS("preprocov.rds")
sampling.cov <- readRDS("sampling.cov.rds")

#generate a dataframe filed with means called output
means <- colMeans(sampling.cov, na.rm = TRUE)
output <- matrix(ncol=ncol(sampling.cov), nrow=65)

for(i in 1:ncol(sampling.cov)){
  output[,i] <-  rep(means[i], times = 65)
  
}
output <- data.frame(output)
colnames(output) <- colnames(sampling.cov)

##generate max min matrix

maxmin <- matrix(ncol=ncol(sampling.cov), nrow=2)
rownames(maxmin) <- c("max", "min")
colnames(maxmin) <- colnames(sampling.cov)

for(i in 1:ncol(sampling.cov)){
  maxmin[1,i] <-  max(sampling.cov[,i], na.rm = TRUE)
  maxmin[2,i] <-  min(sampling.cov[,i], na.rm = TRUE)
}

#generate dataframe of predictions with what happens inside the burned area
OutputForest <- output
OutputForest$forest_dist <- seq(from = min(sampling.cov$forest_dist, na.rm = TRUE), to = max(sampling.cov$forest_dist, na.rm = TRUE), length.out = 65)
OutputForest$fire_dist <- 0
BurnPredicted <- predict(best2.My.Th2, type = "state", predict(preprocov, OutputForest))
OutputForestmyth <- cbind(OutputForest, BurnPredicted)

#generate dataframe of predictions with what happens in the forest
OutputFire <- output
OutputFire$fire_dist <- seq(from = min(sampling.cov$fire_dist, na.rm = TRUE), to = max(sampling.cov$fire_dist, na.rm = TRUE), length.out = 65)
OutputFire$forest_dist <- 0
OutputFire$Burn.intensity.Canopy <- 0
OutputFire$Burn.intensity.basal <- 0
OutputFire$Burn.intensity.soil <- 0
FirePredicted <- predict(best2.My.Th2, type = "state", predict(preprocov, OutputFire))
OutputFiremyth <- cbind(OutputFire, FirePredicted)

#Join and make inside of fire negative
EdgeOutput <- data.frame(Distance = c((exp(OutputForestmyth$forest_dist[length(OutputForestmyth$forest_dist):1])*-1),exp(OutputFiremyth$fire_dist)), Occupancy = c(OutputForestmyth$Predicted[length(OutputForestmyth$forest_dist):1], OutputFiremyth$Predicted), lower = c(OutputForestmyth$lower[length(OutputForestmyth$forest_dist):1], OutputFiremyth$lower), upper = c(OutputForestmyth$upper[length(OutputForestmyth$forest_dist):1], OutputFiremyth$upper)) 


#MYTH
mythresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ labs(title = "MYTH") + ylim(c(0,1)) + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")

#Tabr

BurnPredicted <- predict(best2.Ta.Br2, type = "state", predict(preprocov, OutputForest))
OutputForesttabr <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.Ta.Br2, type = "state", predict(preprocov, OutputFire))
OutputFiretabr <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForesttabr$forest_dist[length(OutputForesttabr$forest_dist):1])*-1),exp(OutputFiretabr$fire_dist)), Occupancy = c(OutputForesttabr$Predicted[length(OutputForesttabr$forest_dist):1], OutputFiretabr$Predicted), lower = c(OutputForesttabr$lower[length(OutputForesttabr$forest_dist):1], OutputFiretabr$lower), upper = c(OutputForesttabr$upper[length(OutputForesttabr$forest_dist):1], OutputFiretabr$upper)) 

tabrresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "TABR") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")

#Pahe

BurnPredicted <- predict(best2.Pa.He2, type = "state", predict(preprocov, OutputForest))
OutputForestpahe <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.Pa.He2, type = "state", predict(preprocov, OutputFire))
OutputFirepahe <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForestpahe$forest_dist[length(OutputForestpahe$forest_dist):1])*-1),exp(OutputFirepahe$fire_dist)), Occupancy = c(OutputForestpahe$Predicted[length(OutputForestpahe$forest_dist):1], OutputFirepahe$Predicted), lower = c(OutputForestpahe$lower[length(OutputForestpahe$forest_dist):1], OutputFirepahe$lower), upper = c(OutputForestpahe$upper[length(OutputForestpahe$forest_dist):1], OutputFirepahe$upper)) 

paheresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "PAHE") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")

#Myyu

BurnPredicted <- predict(best2.My.Yu2, type = "state", predict(preprocov, OutputForest))
OutputForestmyyu <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.My.Yu2, type = "state", predict(preprocov, OutputFire))
OutputFiremyyu <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForestmyyu$forest_dist[length(OutputForestmyyu$forest_dist):1])*-1),exp(OutputFiremyyu$fire_dist)), Occupancy = c(OutputForestmyyu$Predicted[length(OutputForestmyyu$forest_dist):1], OutputFiremyyu$Predicted), lower = c(OutputForestmyyu$lower[length(OutputForestmyyu$forest_dist):1], OutputFiremyyu$lower), upper = c(OutputForestmyyu$upper[length(OutputForestmyyu$forest_dist):1], OutputFiremyyu$upper)) 

myyuresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "MYYU") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")

#Mylu

BurnPredicted <- predict(best2.My.Lu2, type = "state", predict(preprocov, OutputForest))
OutputForestmylu <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.My.Lu2, type = "state", predict(preprocov, OutputFire))
OutputFiremylu <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForestmylu$forest_dist[length(OutputForestmylu$forest_dist):1])*-1),exp(OutputFiremylu$fire_dist)), Occupancy = c(OutputForestmylu$Predicted[length(OutputForestmylu$forest_dist):1], OutputFiremylu$Predicted), lower = c(OutputForestmylu$lower[length(OutputForestmylu$forest_dist):1], OutputFiremylu$lower), upper = c(OutputForestmylu$upper[length(OutputForestmylu$forest_dist):1], OutputFiremylu$upper)) 

myluresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "MYLU") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")

#Myev

BurnPredicted <- predict(best2.My.Ev2, type = "state", predict(preprocov, OutputForest))
OutputForestmyev <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.My.Ev2, type = "state", predict(preprocov, OutputFire))
OutputFiremyev <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForestmyev$forest_dist[length(OutputForestmyev$forest_dist):1])*-1),exp(OutputFiremyev$fire_dist)), Occupancy = c(OutputForestmyev$Predicted[length(OutputForestmyev$forest_dist):1], OutputFiremyev$Predicted), lower = c(OutputForestmyev$lower[length(OutputForestmyev$forest_dist):1], OutputFiremyev$lower), upper = c(OutputForestmyev$upper[length(OutputForestmyev$forest_dist):1], OutputFiremyev$upper)) 

myevresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "MYEV") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")

#Myca

BurnPredicted <- predict(best2.My.Ca2, type = "state", predict(preprocov, OutputForest))
OutputForestmyca <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.My.Ca2, type = "state", predict(preprocov, OutputFire))
OutputFiremyca <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForestmyca$forest_dist[length(OutputForestmyca$forest_dist):1])*-1),exp(OutputFiremyca$fire_dist)), Occupancy = c(OutputForestmyca$Predicted[length(OutputForestmyca$forest_dist):1], OutputFiremyca$Predicted), lower = c(OutputForestmyca$lower[length(OutputForestmyca$forest_dist):1], OutputFiremyca$lower), upper = c(OutputForestmyca$upper[length(OutputForestmyca$forest_dist):1], OutputFiremyca$upper)) 

mycaresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "MYCA") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")


#Lano

BurnPredicted <- predict(best2.La.No2, type = "state", predict(preprocov, OutputForest))
OutputForestlano <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.La.No2, type = "state", predict(preprocov, OutputFire))
OutputFirelano <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForestlano$forest_dist[length(OutputForestlano$forest_dist):1])*-1),exp(OutputFirelano$fire_dist)), Occupancy = c(OutputForestlano$Predicted[length(OutputForestlano$forest_dist):1], OutputFirelano$Predicted), lower = c(OutputForestlano$lower[length(OutputForestlano$forest_dist):1], OutputFirelano$lower), upper = c(OutputForestlano$upper[length(OutputForestlano$forest_dist):1], OutputFirelano$upper)) 

lanoresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "LANO") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")


#Laci

BurnPredicted <- predict(best2.La.Ci2, type = "state", predict(preprocov, OutputForest))
OutputForestlaci <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.La.Ci2, type = "state", predict(preprocov, OutputFire))
OutputFirelaci <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForestlaci$forest_dist[length(OutputForestlaci$forest_dist):1])*-1),exp(OutputFirelaci$fire_dist)), Occupancy = c(OutputForestlaci$Predicted[length(OutputForestlaci$forest_dist):1], OutputFirelaci$Predicted), lower = c(OutputForestlaci$lower[length(OutputForestlaci$forest_dist):1], OutputFirelaci$lower), upper = c(OutputForestlaci$upper[length(OutputForestlaci$forest_dist):1], OutputFirelaci$upper)) 

laciresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "LACI") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")


#Labl

BurnPredicted <- predict(best2.La.Bl2, type = "state", predict(preprocov, OutputForest))
OutputForestlabl <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.La.Bl2, type = "state", predict(preprocov, OutputFire))
OutputFirelabl <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForestlabl$forest_dist[length(OutputForestlabl$forest_dist):1])*-1),exp(OutputFirelabl$fire_dist)), Occupancy = c(OutputForestlabl$Predicted[length(OutputForestlabl$forest_dist):1], OutputFirelabl$Predicted), lower = c(OutputForestlabl$lower[length(OutputForestlabl$forest_dist):1], OutputFirelabl$lower), upper = c(OutputForestlabl$upper[length(OutputForestlabl$forest_dist):1], OutputFirelabl$upper)) 

lablresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "LABL") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")


#Euma

BurnPredicted <- predict(best2.Eu.Ma2, type = "state", predict(preprocov, OutputForest))
OutputForesteuma <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.Eu.Ma2, type = "state", predict(preprocov, OutputFire))
OutputFireeuma <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForesteuma$forest_dist[length(OutputForesteuma$forest_dist):1])*-1),exp(OutputFireeuma$fire_dist)), Occupancy = c(OutputForesteuma$Predicted[length(OutputForesteuma$forest_dist):1], OutputFireeuma$Predicted), lower = c(OutputForesteuma$lower[length(OutputForesteuma$forest_dist):1], OutputFireeuma$lower), upper = c(OutputForesteuma$upper[length(OutputForesteuma$forest_dist):1], OutputFireeuma$upper)) 

eumaresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "EUMA") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")


#Epfu

BurnPredicted <- predict(best2.Ep.Fu2, type = "state", predict(preprocov, OutputForest))
OutputForestepfu <- cbind(OutputForest, BurnPredicted)
FirePredicted <- predict(best2.Ep.Fu2, type = "state", predict(preprocov, OutputFire))
OutputFireepfu <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForestepfu$forest_dist[length(OutputForestepfu$forest_dist):1])*-1),exp(OutputFireepfu$fire_dist)), Occupancy = c(OutputForestepfu$Predicted[length(OutputForestepfu$forest_dist):1], OutputFireepfu$Predicted), lower = c(OutputForestepfu$lower[length(OutputForestepfu$forest_dist):1], OutputFireepfu$lower), upper = c(OutputForestepfu$upper[length(OutputForestepfu$forest_dist):1], OutputFireepfu$upper)) 

epfuresp <- ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5) + geom_line(size = 1)+ ylim(c(0,1))+ labs(title = "EPFU") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ geom_vline(xintercept = 0, linetype = "longdash")



#combine plots

library(gridExtra)

grid.arrange(mythresp, tabrresp, myevresp, mycaresp, lanoresp, laciresp, lablresp, epfuresp, eumaresp, myluresp, myyuresp, paheresp, ncol = 3)