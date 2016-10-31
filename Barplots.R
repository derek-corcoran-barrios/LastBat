library(unmarked)
library(caret)
library(ggplot2)
library(texreg)

best2.My.Lu2 <- readRDS("best2.My.Lu2.rds")
best2.My.Th2 <- readRDS("best2.My.Th2.rds")

preprocov <- readRDS("preprocov.rds")
sampling.cov <- readRDS("sampling.cov.rds")

#generate a dataframe filed with means
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
#####Burn canopy
OutputBurn <- output
OutputBurn$Burn.intensity.Canopy <- seq(from = min(sampling.cov$Burn.intensity.Canopy, na.rm = TRUE), to = max(sampling.cov$Burn.intensity.Canopy, na.rm = TRUE), length.out = 65)
BurnPredicted <- predict(best2.My.Lu2, type = "state", predict(preprocov, OutputBurn))
OutputBurn <- cbind(OutputBurn, BurnPredicted)

library(ggthemr)
ggthemr("chalk", type="outer", layout="scientific", spacing=2)

g1<- ggplot(OutputBurn, aes(x = Burn.intensity.Canopy, y = Predicted)) + geom_ribbon(aes(ymax = upper, ymin = lower), fill = "darkslategray2", alpha = 0.5) + ylab("Occupancy") + xlab ("Burn Intensity Canopy")  + geom_line(color= "white", size =1.15)

#####Distance to forest
OutputForest <- output
OutputForest$forest_dist <- seq(from = min(sampling.cov$forest_dist, na.rm = TRUE), to = max(sampling.cov$forest_dist, na.rm = TRUE), length.out = 65)
BurnPredicted <- predict(best2.My.Lu2, type = "state", predict(preprocov, OutputForest))
OutputForest <- cbind(OutputForest, BurnPredicted)
library(ggthemr)
ggthemr("chalk", type="outer", layout="scientific", spacing=2)
g2 <-ggplot(OutputForest, aes(x = exp(forest_dist), y = Predicted))  + geom_ribbon(aes(ymax = upper, ymin = lower), fill = "darkslategray2", alpha = 0.5) + ylab("Occupancy") + xlab ("Distance to Forest") + geom_line(color= "white", size =1.15)

summarised_df1 <- readRDS("summarised_df1.rds")

ggthemr("chalk", type="outer", layout="clear", spacing=1)

mean_fire <- summarised_df1$mean[rep(seq(from = 2, to = 34, by = 2), each = 2)]
limits <- aes(ymax = mean + std_error, ymin=mean-std_error)
g<-ggplot(summarised_df1,aes(y = mean, x = reorder(variable, -mean_fire), fill = FIRE))
g<-g+geom_bar(stat = "identity",position = position_dodge(), size = 1.5)+ scale_fill_manual(values=c('#999999','#E69F00'))
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(0.9)) + ylab("Occupancy") + xlab("Species")

g


##############################################################################
####################Distance to fire edge#####################################
##############################################################################
library(unmarked)
library(caret)
library(ggplot2)
library(texreg)

best2.My.Th2 <- readRDS("best2.My.Th2.rds")

preprocov <- readRDS("preprocov.rds")
sampling.cov <- readRDS("sampling.cov.rds")

#generate a dataframe filed with means
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


OutputForest <- output
OutputForest$forest_dist <- seq(from = min(sampling.cov$forest_dist, na.rm = TRUE), to = max(sampling.cov$forest_dist, na.rm = TRUE), length.out = 65)
OutputForest$fire_dist <- 0
BurnPredicted <- predict(best2.My.Th2, type = "state", predict(preprocov, OutputForest))
OutputForest <- cbind(OutputForest, BurnPredicted)

OutputFire <- output
OutputFire$fire_dist <- seq(from = min(sampling.cov$fire_dist, na.rm = TRUE), to = max(sampling.cov$fire_dist, na.rm = TRUE), length.out = 65)
OutputFire$forest_dist <- 0
FirePredicted <- predict(best2.My.Th2, type = "state", predict(preprocov, OutputFire))
OutputFire <- cbind(OutputFire, FirePredicted)

EdgeOutput <- data.frame(Distance = c((exp(OutputForest$forest_dist[length(OutputForest$forest_dist):1])*-1),exp(OutputFire$fire_dist)), Occupancy = c(OutputForest$Predicted[length(OutputForest$forest_dist):1], OutputFire$Predicted), lower = c(OutputForest$lower[length(OutputForest$forest_dist):1], OutputFire$lower), upper = c(OutputForest$upper[length(OutputForest$forest_dist):1], OutputFire$upper))

library(ggthemr)
ggthemr("chalk", type="outer", layout="scientific", spacing=2)
ggplot(EdgeOutput, aes(x = Distance, y = Occupancy))  + geom_ribbon(aes(ymax = upper, ymin = lower), fill = "darkslategray2", alpha = 0.5) + geom_line(color= "white", size =1.15)



########################################################################
###########################PAPER FIGURES#########################
####################################


#Barplot

summarised_df1 <- readRDS("summarised_df1.rds")

mean_fire <- summarised_df1$mean[rep(seq(from = 2, to = 34, by = 2), each = 2)]
limits <- aes(ymax = mean + std_error, ymin=mean-std_error)
g<-ggplot(summarised_df1,aes(y = mean, x = reorder(variable, -mean_fire), fill = FIRE))
g<-g+geom_bar(stat = "identity",position = position_dodge(), size = 1.5)+ scale_fill_grey(start = 0.3, end = .8)
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(0.9)) + ylab("Occupancy") + xlab("Species") + theme_classic() + theme(panel.grid.major = element_blank()) 

g + scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(), 
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

##### alt 2

g<-ggplot(summarised_df1,aes(y = mean, x = reorder(variable, mean_fire), fill = FIRE))
g<-g+geom_bar(stat = "identity",position = position_dodge(), size = 1.5)+ scale_fill_grey(start = 0.3, end = .8)
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(0.9)) + ylab("Occupancy") + xlab("Species") + theme_classic() + theme(panel.grid.major = element_blank()) 

g + scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(), 
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1)) + coord_flip()