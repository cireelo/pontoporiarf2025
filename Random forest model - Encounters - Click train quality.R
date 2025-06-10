####COMPARISON BETWEEN PONTOPORIA ENCOUNTERS   
#PER MOD-QUALITY TRAINS PERCENTAGE WITH RANDOM FOREST#######

#Loading packages:

packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "ggfortify", "gamlss", 
              "reshape2", "knitr", "gamlss.add", "gamlss.dist", "vegan", 
              "permute", "ggplot2", "dplyr", "BiodiversityR", "ggsci", "readxl",
              "ggpubr", "tidyverse",  "cluster", "MASS","randomForest", 
              "randomForestExplainer","localModel","DALEX","DALEXtra","ROCR","lime","caret", "e1071","ROSE", "pROC", "janitor","Metrics")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

setwd("C:/Users/jmner/OneDrive/Documents/Mestrado UFSC/SOLAMAC 2020")

# Data acquisition (only traveling clicks)
cencqual<- read.csv("c_enc_quali.csv", header=TRUE)
# Removing irrelevant columns
cencqual2 <- cencqual[,-(15:26),drop=FALSE]

str(cencqual2)

# Data partition
set.seed(123)
indencqual <- sample(2, nrow(cencqual2), replace = TRUE, prob = c(0.7, 0.3))
trainencqual <- cencqual2[indencqual==1,] #70% of data will be train data
testencqual <- cencqual2[indencqual==2,] #30% of data will be test data

# Showing that the Random variable is truly random:
acf(cencqual2$Random)
pacf(cencqual2$Random)
difference.sign.test(cencqual2$Random)
runs.test(cencqual2$Random)
barplot(cencqual2$Random)

# Random Forest model
set.seed(222)
rfencqual <- randomForest(PercentModQ~., data=trainencqual,
                          ntree = 100,
                          mtry=24,
                          maxnodes=4,
                          importance = TRUE,
                          proximity = TRUE)
print(rfencqual)
attributes(rfencqual)
rfencqual$importanceSD

# Start with ntree = 200, then use "plot(rfencqual)" to choose the best ntree
# Start with default mtry, then use process at "#Tune mtry" to choose the best 
# mtry value 

###### Model evaluation:

# Error rate of Random Forest
plot(rfencqual)

# After 70 trees, model stops improving 

# Tune mtry
tuneencqual <- tuneRF(trainencqual[,-15], trainencqual[,15],
                      #(remove first variable, which is PercentModQ)      
                      stepFactor = 0.5, 
                      #(at each iteration, mtry is inflated or deflated by this value)
                      plot = TRUE, 
                      #(plot OOB error rate as a function of mtry)
                      ntreeTry = 200,
                      #(accordingo to plot(rfencqual), max. number of trees needed) 
                      trace = TRUE,
                      improve = 0.05)
#(relative improve in OOB error must be this much to continue)
#the best mtry value is 3

# No. of nodes for the trees
hist(treesize(rfencqual),
     main = "No. of Nodes for the Trees",
     col = "green")
# Most trees have about 12 nodes

# Variable Importance
varImpPlot(rfencqual,
           sort = T,
           main = "Importance of acoustic parameters for classification")
importance(rfencqual)
varUsed(rfencqual)
# Most important variables (model performs worse when they're removed)

# More relevant than the random variable: every variable, except:
# MafF, MaxSPL_db, BW, Hour_of_day, and Frange

# Extract Single Tree
getTree(rfencqual, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
set.seed(1)
distance.matrix.encqual <- as.dist(1-rfencqual$proximity)

set.seed(1)
mds.encqual <- cmdscale(distance.matrix.encqual, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
set.seed(1)
mds.var.encqual <- round(mds.encqual$eig/sum(mds.encqual$eig)*100, 1)
set.seed(1)
mds.values.encqual <- mds.encqual$points
mds.cencqual2 <- data.frame(Sample=rownames(mds.values.encqual),
                            X=mds.values.encqual[,1],
                            Y=mds.values.encqual[,2],
                            Status=trainencqual$PercentModQ)
set.seed(1)
ggplot(data=mds.cencqual2, aes(x=X, y=Y, label=Sample)) + 
  geom_point(aes(shape=Status,colour=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.encqual[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.encqual[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")

ggsave(file="rfencqual_mds_plot.png")

### Breakdown plot to see the model's variable attributions:

# Create explainer objects:
explainencqual <- DALEX::explain(model = rfencqual,  
                                 data = cencqual2[, -15],
                                 y = cencqual2$PercentModQ == "encqual", 
                                 label = "Random Forest")

library("randomForest")
library("DALEX") # just to avoid an error message related to "Method"
bd_rfencqual <- predict_parts(explainer = explainencqual,
                              new_observation = testencqual[2,],
                              type = "break_down")
bd_rfencqual 
plot(bd_rfencqual)

# Partial dependance profile:
pd_rfencqual <- model_profile(
  explainer = explainencqual, 
  variables = "Frange")
plot(pd_rfencqual)

### Interpreting model with LIME - 
### Local Interpretable Model-Agnostic Explanations

set.seed(1)
library("DALEXtra")
library("lime")
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_encqual <- predict_surrogate(explainer = explainencqual, 
                                  new_observation = testencqual[100,-15], 
                                  n_features = 15, 
                                  n_permutations = 1000,
                                  type = "lime")
plot(lime_encqual)
(as.data.frame(lime_encqual))

## Distribution of minimal PercentModQ of the forest model:
library('randomForestExplainer')
minPercentModQ_encqual <- min_PercentModQ_distribution(rfencqual)
save(minPercentModQ_encqual, file = "minPercentModQ_encqual.rda")
load("minPercentModQ_encqual.rda")
head(minPercentModQ_encqual, n = 10)
plot_min_PercentModQ_distribution(minPercentModQ_encqual, mean_sample = "relevant_trees", k = 15)

## Multi-way importance variable
importance_frameencqual <- measure_importance(rfencqual)
save(importance_frameencqual, file = "importance_frameencqual.rda")
load("importance_frameencqual.rda")
importance_frameencqual
plot_multi_way_importance(importance_frameencqual, size_measure = "p_value", no_of_labels = 5)

# More partial dependence plots
par(mfrow=c(2,2))
partialPlot(rfencqual, trainencqual, TPsum)
partialPlot(rfencqual, trainencqual, avSPL.T)
partialPlot(rfencqual, trainencqual, kHzMedian.T)
partialPlot(rfencqual, trainencqual, Minutes)
par(mfrow=c(1,1))

# Actual vs. predicted values plot:
par(mfrow=c(1,1))
actualAndPredictedData = data.frame(actualValue = testencqual$PercentModQ, 
                                    predictedValue = predict(rfencqual,testencqual))

ggplot(actualAndPredictedData,aes(x = actualValue, y = predictedValue)) +  geom_point() +  
  geom_abline(color = "blue")

# Root mean-square error (RMSE):
pred_pencqual <- predict(rfencqual, testencqual, index=2, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
head(pred_pencqual)
sqrt(mean((testencqual$PercentModQ -pred_pencqual)^2))

# Testing accuracy:
require(Metrics)
require(caret)
predicted_encqual<-predict(rfencqual,testencqual)
print(paste0('MAE: ' , mae(testencqual$PercentModQ,predicted_encqual)))
print(paste0('RMSE: ' ,caret::postResample(predicted_encqual , testencqual$PercentModQ)['RMSE']))
print(paste0('MSE: ' ,caret::postResample(predicted_encqual , testencqual$PercentModQ)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(predicted_encqual , testencqual$PercentModQ)['Rsquared']))


