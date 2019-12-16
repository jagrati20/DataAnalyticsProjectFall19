#Getting the location
getwd()

#Setting the location
setwd("/Users/jagratisharma/Desktop/Data\ Analytics")

library(ggplot2)
library(lattice)
library(caret)
library(DataExplorer)
library(caTools)
require(graphics)
library(ggplot2)
library(randomForest)
library(tree)
library("visNetwork")
library("sparkline")
library(rpart)
library(rpart.plot)
library(C50)

#Loading the data
cpudata<-read.csv("Dataset.csv", sep = " ", header = FALSE)
View(cpudata)
names(cpudata) <- c("time","lread","lwrite","scall","sread","swrite","fork","execute",
                       "rchar","wchar","pgout","ppgout","pgfree","pgscan","atch","pgin",
                       "ppgin","pflt","vflt","runqsz","runocc","freemem","freeswap","usr","sys",
                       "wio","idle")
View(cpudata)
cpudata<-cpudata[,-28]

str(cpudata)
#Viewing the data 
colnames(cpudata)
View(cpudata)
summary(cpudata)

#identifying if duplicate rows exist
duplicated(cpudata)
which(duplicated(cpudata))
#way 1
cpudatanew<-cpudata[!duplicated(cpudata),]
#hence no duplicates rows as cpudata and cpudatanew has same variables and observations

#check for NAs
which(is.na(cpudata))


#performing exploratory data analysis on the data

#drawing bar grpahs
#bargraph for Lread
counts <- table(cpudata$lread)
barplot(counts, main="Lread",
        xlab="Reads (transfers per second ) between system memory and user memory")

counts <- table(cpudata$lwrite)
barplot(counts, main="Lwrite",
        xlab="writes (transfers per second) between system memory and user memory")

counts <- table(cpudata$scall)
barplot(counts, main="Scall",
        xlab="Number of system calls of all types per second")

counts <- table(cpudata$sread)
barplot(counts, main="Sread",
        xlab="Number of system read calls per second")

counts <- table(cpudata$swrite)
barplot(counts, main="Swrite",
        xlab="Number of system write calls per second")

counts <- table(cpudata$fork)
barplot(counts, main="Fork",
        xlab="Number of system fork calls per second")

counts <- table(cpudata$exec)
barplot(counts, main="Execute",
        xlab="Number of system exec calls per second")

counts <- table(cpudata$rchar)
barplot(counts, main="RChar",
        xlab="Number of characters transferred per second by system read calls")

counts <- table(cpudata$wchar)
barplot(counts, main="Wchar",
        xlab="Number of characters transfreed per second by system write calls")

counts <- table(cpudata$pgout)
barplot(counts, main="Pgout",
        xlab="Number of page out requests per second")

counts <- table(cpudata$ppgout)
barplot(counts, main="PPgout",
        xlab="Number of pages, paged out per second")

counts <- table(cpudata$pgfree)
barplot(counts, main="PgFree",
        xlab="Number of pages per second placed on the free list")

counts <- table(cpudata$pgscan)
barplot(counts, main="PgScan",
        xlab="Number of pages checked if they can be freed per second")

counts <- table(cpudata$atch)
barplot(counts, main="Atch",
        xlab="Number of page attaches (satisfying a page fault by reclaiming a page in memory) per second")

counts <- table(cpudata$pgin)
barplot(counts, main="PgIn",
        xlab="Number of page-in requests per second")

counts <- table(cpudata$ppgin)
barplot(counts, main="Ppgin",
        xlab="Number of pages paged in per second")

counts <- table(cpudata$pflt)
barplot(counts, main="Pflt",
        xlab="Number of page faults caused by protection errors (copy-on-writes)")

counts <- table(cpudata$vflt)
barplot(counts, main="Vflt",
        xlab="Number of page faults caused by address translation")

counts <- table(cpudata$runqsz)
barplot(counts, main="Runqsz",
        xlab="Process run queue size")


counts <- table(cpudata$runocc)
barplot(counts, main="Runocc",
        xlab="Process run occurance")


counts <- table(cpudata$freemem)
barplot(counts, main="Freemem",
        xlab="Number of memory pages available to user processes")

counts <- table(cpudata$freeswap)
barplot(counts, main="Freeswap",
        xlab="Number of disk blocks available for page swapping")

counts <- table(cpudata$usr)
barplot(counts, main="Usr",
        xlab="Portion of time (%) that cpus run in user mode")

counts <- table(cpudata$sys)
barplot(counts, main="Sys",
        xlab="Portion of time (%) that cpus run in system mode")

counts <- table(cpudata$wio)
barplot(counts, main="Wio",
        xlab="Portion of time (%) that cpus are idle waiting for block IO")

counts <- table(cpudata$idle)
barplot(counts, main="Idle",
        xlab="Portion of time (%) that cpus are otherwise idle")


#plotting scatter plot
plot(cpudata$lread, cpudata$sys, main="Scatterplot sys and lread",
     xlab="Lread ", ylab="System mode ", pch=20)

plot(cpudata$lwrite, cpudata$sys, main="Scatterplot sys and lwrite",
     xlab="lwrite", ylab="System mode ", pch=20)

plot(cpudata$scall, cpudata$sys, main="Scatterplot sys and scall",
     xlab="scall", ylab="System mode ", pch=20)

plot(cpudata$sread, cpudata$sys, main="Scatterplot sys and sread",
     xlab="sread", ylab="System mode ", pch=20)

plot(cpudata$swrite, cpudata$sys, main="Scatterplot sys and swrite",
     xlab="swrite", ylab="System mode ", pch=20)

plot(cpudata$fork, cpudata$sys, main="Scatterplot sys and fork",
     xlab="fork", ylab="System mode ", pch=20)

plot(cpudata$exec, cpudata$sys, main="Scatterplot sys and exec",
     xlab="exec", ylab="System mode ", pch=20)

plot(cpudata$rchar, cpudata$sys, main="Scatterplot sys and rchar ",
     xlab="rchar", ylab="System mode ", pch=20)

plot(cpudata$wchar, cpudata$sys, main="Scatterplot sys and wchar",
     xlab="wchar", ylab="System mode ", pch=20)

plot(cpudata$pgout, cpudata$sys, main="Scatterplot sys and pgout",
     xlab="", ylab="System mode ", pch=20)

plot(cpudata$ppgout, cpudata$sys, main="Scatterplot sys and ppgout",
     xlab="ppgout", ylab="System mode ", pch=20)

plot(cpudata$pgfree, cpudata$sys, main="Scatterplot sys and pgfree",
     xlab="pgfree", ylab="System mode ", pch=20)

plot(cpudata$pgscan, cpudata$sys, main="Scatterplot sys and pgscan",
     xlab="pgscan", ylab="System mode ", pch=20)

plot(cpudata$atch, cpudata$sys, main="Scatterplot sys and atch",
     xlab="atch", ylab="System mode ", pch=20)

plot(cpudata$pgin, cpudata$sys, main="Scatterplot sys and pgin",
     xlab="pgin", ylab="System mode ", pch=20)

plot(cpudata$ppgin, cpudata$sys, main="Scatterplot sys and ppgin",
     xlab="ppgin", ylab="System mode ", pch=20)

plot(cpudata$pflt, cpudata$sys, main="Scatterplot sys and pflt",
     xlab="pflt", ylab="System mode ", pch=20)

plot(cpudata$vflt, cpudata$sys, main="Scatterplot sys and vflt",
     xlab="vflt", ylab="System mode ", pch=20)

plot(cpudata$runqsz, cpudata$sys, main="Scatterplot sys and runqsz",
     xlab="runqsz", ylab="System mode ", pch=20)

plot(cpudata$runocc, cpudata$sys, main="Scatterplot sys and runocc",
     xlab="runocc", ylab="System mode ", pch=20)

plot(cpudata$freemem, cpudata$sys, main="Scatterplot sys and freemem",
     xlab="freemem", ylab="System mode ", pch=20)

plot(cpudata$freeswap, cpudata$sys, main="Scatterplot sys and freeswap",
     xlab="freeswap", ylab="System mode ", pch=20)

plot(cpudata$usr, cpudata$sys, main="Scatterplot sys and usr",
     xlab="usr", ylab="System mode ", pch=20)

plot(cpudata$wio, cpudata$sys, main="Scatterplot sys and wio",
     xlab="wio", ylab="System mode ", pch=20)

plot(cpudata$idle, cpudata$sys, main="Scatterplot sys and idle",
     xlab="idle", ylab="System mode ", pch=20)


#Removing the Columns with null values 
#removing WIO
cpudata<-cpudata[,-26]
cpudata

#removing Idle
cpudata<-cpudata[,-26]
cpudata

#removing TIME
cpudata<-cpudata[,-1]
cpudata

#removing USR mode time spent
cpudata<-cpudata[,-23]
cpudata


View(cpudata)
str(cpudata)

#Mising value treatment
#install.packages("Amelia")
print(require(Amelia))
missmap(cpudata)


#HeatMap
heatmap(as.matrix(cpudata.subset),
        scale = "column",
        col=heat.colors(256),
        main = "Characteristics of CPUDATA",
        Rowv = NA,Colv = NA)



#storing data to a new dataset as cpudatanew and converitng the SYS column to factor
newdata<-cpudata
attach(newdata)
range(sys)
tempdata <- as.integer(sys)
for (i in 1:length(tempdata)) {
  
  if(tempdata[i] >=0 & tempdata[i] <=14){
    
    newdata$sysgroup[i] = "very low"
    
  } else if(tempdata[i] > 14 & tempdata[i] <= 28){
    
    newdata$sysgroup[i] = "low"
    
  }else if(tempdata[i] > 28 & tempdata[i] <= 42){
    
    newdata$sysgroup[i] = "low moderate"
    
  }else if(tempdata[i] > 42 & tempdata[i] <= 56){
    
    newdata$sysgroup[i] = "moderate"
    
  } else if(tempdata[i] > 56 & tempdata[i] <= 70){
    
    newdata$sysgroup[i] = "moderate high"
    
  }else if(tempdata[i] > 70 & tempdata[i] <= 84){
    
    newdata$sysgroup[i] = "high"
    
  }else { newdata$sysgroup[i] = "very high"}
  
}
View(newdata)
unique(newdata$sysgroup)
#making the new variable as a factor
newdata$sysgroup<-factor(newdata$sysgroup)

#cpudatanew<-cpudata
#cpudatanew[,24]<-as.factor(cpudata[,24])
#View(cpudatanew)

#Not Ran yet
#Copying data to new variable and removing all zeros
#cpudatanew<-cpudatanew[!(cpudatanew$sys == 0),]


#Cluster Analysis 
pairs(cpudata.subset)

plot_correlation(cpudata)

#Normalizing hte data
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

cpudata.n<-as.data.frame(lapply(cpudata, normalize))
head(cpudata.n)
#View(cpudata.n)

##calculate distance matrix (default is Euclidean distance)
distance = dist(cpudata.n)

# Hierarchical agglomerative clustering using default complete linkage 
cpudata.subset.hclust = hclust(distance)
plot(cpudata.subset.hclust)
plot(cpudata.subset.hclust,labels=cpudata.subset$sys,main='Default from hclust')
plot(cpudata.subset.hclust,hang=-1)

# Hierarchical agglomerative clustering using "average" linkage 
cpudata.subset.hclust<-hclust(distance,method="average")
plot(cpudata.subset.hclust,hang=-1)

# Cluster membership
member = cutree(cpudata.subset.hclust,3)
table(member)

#Characterizing clusters 
aggregate(cpudata.n,list(member),mean)

# Scree Plot
wss <- (nrow(cpudata.n)-1)*sum(apply(cpudata.n,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(cpudata.n, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

################################################################################
#                         K-means clustering
################################################################################

#Applying Kmeans
unique(cpudata.n$sys)
results<-kmeans(cpudata.n,3)
results
#size of clusters
results$size

#ploting
plot(cpudata.n[c('sys','fork')],col=results$cluster)

plot(cpudata.n[c('sys','lread')],col=results$cluster)


################################################################################
#                        Principle Component Analysis 
################################################################################
d<-cpudata[,]
pc<-princomp(d,cor = TRUE,scores = TRUE)
summary(pc)
biplot(pc, main="PCA",ellipse=TRUE, groups=d$fork)


################################################################################
#                        Partitioning data
################################################################################

#Partitioning data into testing and traininf set for Modelling using cpudata(
#this is without factor)

split=sample.split(cpudata$sys,SplitRatio=0.8)

training_set=subset(cpudata,split==TRUE)
testing_set=subset(cpudata,split==FALSE)


################################################################################
#                  Multivariate LINEAR Regression
################################################################################


#Applying multivariate regression using forward regression

regressor=lm(formula = training_set$sys~training_set$lread,data = training_set)
summary(regressor)

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite,data = training_set)
summary(regressor)
#not working

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall,data = training_set)
summary(regressor)
#yes

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread,data = training_set)
summary(regressor)
#yes

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite,
             data = training_set)
summary(regressor)
#not working

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+training_set$fork,
             data = training_set)
summary(regressor)
#clear yes

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec,data = training_set)
summary(regressor)
#yes


regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar,data = training_set)
summary(regressor)
#cleAR yes

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar,data = training_set)
summary(regressor)
#nice clearer

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin,data = training_set)
summary(regressor)
#clear yes

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin,data = training_set)
summary(regressor)
#yes


regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt,data = training_set)
summary(regressor)
#okay okay

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt,data = training_set)
summary(regressor)
#okay okay

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt+training_set$runqsz,
             data = training_set)
summary(regressor)
#okay

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt+training_set$runqsz+training_set$runocc,
             data = training_set)
summary(regressor)
#damn

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt+training_set$runqsz+training_set$runocc+
               training_set$freemem,
             data = training_set)
summary(regressor)

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt+training_set$runqsz+training_set$runocc+
               training_set$freemem+training_set$freeswap,
             data = training_set)
summary(regressor)

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt+training_set$runqsz+training_set$runocc+
               training_set$freemem+training_set$freeswap+training_set$pgout,
             data = training_set)
summary(regressor)


regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt+training_set$runqsz+training_set$runocc+
               training_set$freemem+training_set$freeswap+training_set$pgout+training_set$ppgout,
             data = training_set)
summary(regressor)

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt+training_set$runqsz+training_set$runocc+
               training_set$freemem+training_set$freeswap+training_set$pgout+training_set$ppgout+
               training_set$pgfree,
             data = training_set)
summary(regressor)

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt+training_set$runqsz+training_set$runocc+
               training_set$freemem+training_set$freeswap+training_set$pgout+training_set$ppgout+
               training_set$pgfree+training_set$pgscan,
             data = training_set)
summary(regressor)

regressor=lm(formula = training_set$sys~training_set$lread+training_set$lwrite+
               training_set$scall+training_set$sread+training_set$swrite+
               training_set$fork+training_set$exec+training_set$rchar+
               training_set$wchar+training_set$pgin+training_set$ppgin+
               training_set$pflt+training_set$vflt+training_set$runqsz+training_set$runocc+
               training_set$freemem+training_set$freeswap+training_set$pgout+training_set$ppgout+
               training_set$pgfree+training_set$pgscan+training_set$atch,
             data = training_set)
summary(regressor)


#taking all the variables
regressor=lm(formula = training_set$sys~.,
             data = training_set)
summary(regressor)


#hence taking the most dependent variables
regressor=lm(formula = training_set$sys~training_set$lread+training_set$scall+training_set$swrite+
               training_set$exec+training_set$rchar+training_set$wchar+training_set$pgout+
               training_set$pgin+training_set$ppgin+training_set$pflt+training_set$vflt+
               training_set$runqsz+training_set$runocc+training_set$freemem+training_set$freeswap,
             data = training_set)
summary(regressor)
plot(regressor)
confint(regressor)
sigma(regressor)/mean(testing_set$sys)


qqnorm(cpudata$sys)
qqline(cpudata$sys)
?qqnorm


ggplot(training_set,aes(y=ypredict,x=training_set$sys))+geom_point()+geom_smooth(method="lm")
#remove: atch,freeswap and lwrite


#Making a subset of the data and keeping only important columns
cpudata.subset <- c("lread","scall","swrite","exec","rchar","wchar","pgout","pgin","ppgin","pflt",
                    "vflt","runqsz","runocc","freemem","sys")
cpudata.subset <- cpudata[cpudata.subset]
str(cpudata.subset)
#View(cpudata.subset)

newdata.subset<-c("lread","scall","swrite","exec","rchar","wchar","pgout","pgin","ppgin","pflt",
                  "vflt","runqsz","runocc","freemem","sys","sysgroup")
newdata.subset <- newdata[newdata.subset]
str(newdata.subset)
View(newdata.subset)

#removing sys
cpudata.subset.f <- newdata.subset
cpudata.subset.f<-cpudata.subset.f[,-15]
View(cpudata.subset.f)



#updating training and testing datasets

set.seed(1234)
ind<-sample(2,nrow(cpudata.subset),replace=T,prob = c(0.8,0.2))
training_set<-cpudata.subset[ind==1,]
testing_set<-cpudata.subset[ind==2,]


##updating training and testing datasets when sys is a factor
set.seed(1234)
ind<-sample(2,nrow(cpudata.subset.f),replace=T,prob = c(0.8,0.2))
training_setf<-cpudata.subset.f[ind==1,]
testing_setf<-cpudata.subset.f[ind==2,]


################################################################################
#                                      KNN 
################################################################################


# KNN Model
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 3)
set.seed(333)
fitknn <- train(sysgroup ~.,
             data = training_setf,
             tuneGrid = expand.grid(k=10:82),
             method = 'knn',
             metric = 'Accuracy',
             trControl = trControl,
             preProc = c('center', 'scale'))

# Model Performance
fitknn
plot(fit, main="Knn Fit")
varImp(fitknn)
pred <- predict(fitknn, newdata = testing_setf)
plot(pred , testing_setf$sysgroup, main="knn")

Accuracytable <- table(pred, testing_setf$sys)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(Accuracytable)

################################################################################
#                                  KNN/method 2
################################################################################

# kNN
control<-trainControl(method = "cv",number=10)
metric<-"Accuracy"
set.seed(7)
fit.knn <- train(sysgroup~., data=training_setf, method="knn", metric=metric, trControl=control)
print(fit.knn)
predictions <- predict(fit.knn, testing_setf)
confusionMatrix(predictions, testing_setf$sysgroup)

################################################################################
#                            RANDOM FOREST 
################################################################################


set.seed(1234)
randomF1 = randomForest(sysgroup~., data = training_setf, importance = TRUE)
print(cpudata.subset$sys)
plot(randomF1)

randomF2 <- randomForest(sysgroup ~ ., data = training_setf, ntree = 500, mtry = 6, importance = TRUE)
plot(randomF2)
randomF2.pred <- predict(randomF2, newdata=testing_setf,type = "class")
randomF2.pred
confusionMatrix(randomF2.pred,testing_setf$sysgroup)

# Checking classification accuracy
#library(caret)
#print(postResample(pred = rffit2.pred, obs = testing_set$sys))
#confusionMatrix(rffit2.pred,sys)
importance(randomF2)
varImpPlot(randomF2)


Accuracytable<-table(randomF2.pred,testing_setf$sysgroup)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(Accuracytable)


################################################################################
#                             Random Forest-method 2
################################################################################
# Random Forest
control<-trainControl(method = "cv",number=10)
metric<-"Accuracy"
set.seed(7)
fit.rf <- train(sysgroup~., data=training_setf, method="rf", metric=metric, trControl=control)
print(fit.rf)
predictions <- predict(fit.rf, testing_setf)
confusionMatrix(predictions, testing_setf$sysgroup)




#roccurve

################################################################################
#                                     TREE
################################################################################

model_tree <- tree(sys ~ . , data = training_set)
summary(model_tree)
plot(model_tree, main="Tree")
text(model_tree, pretty = 0, cex = 0.8)


model_tree <- tree(sysgroup ~ . , data = training_setf)
summary(model_tree)
plot(model_tree)
text(model_tree, pretty = 0, cex = 0.8)


################################################################################
#                                       RPART 
################################################################################


#using RPART

# generate the decision tree model
dectionTreeModel <- rpart(sys~., training_set)
summary(cpudata.subset)
dectionTreeModel
rpart.plot(dectionTreeModel)
visTree(dectionTreeModel)



# generate the decision tree model
dectionTreeModel <- rpart(sysgroup~., training_setf)
dectionTreeModel
rpart.plot(dectionTreeModel)
visTree(dectionTreeModel)
predict.decision<-predict(dectionTreeModel,newdata=testing_setf, type = "class")  # factor
confusionMatrix(predict.decision,testing_setf$sysgroup)


################################################################################
#                               C50 
################################################################################


## Building the Classification Tree Models using the Quinlan's C5.0 algorithm

c50.fit  <- C5.0(x=training_setf, y=training_setf$sysgroup, trials = 10)
summary(c50.fit)
plot(c50.fit)
c50.fit.pred <- predict(c50.fit, newdata= testing_setf)
print(postResample(pred = c50.fit.pred, obs = testing_setf$sysgroup))
confusionMatrix(c50.fit.pred, testing_setf$sysgroup)



################################################################################
#                               Using Caret library
################################################################################



#using various forms in similar approach

# https://machinelearningmastery.com/machine-learning-in-r-step-by-step/


################################################################################
#                                  LDA 
################################################################################

control<-trainControl(method = "cv",number=10)
metric<-"Accuracy"
set.seed(7)

library(lda)
fit.lda <- train(sysgroup~., data=training_setf, method="lda", metric=metric,
                 preProc=c("center", "scale"), trControl=control)
print(fit.lda)
predictions <- predict(fit.lda, testing_setf)
confusionMatrix(predictions, testing_setf$sysgroup)


################################################################################
#                                  CART 
################################################################################

# CART
control<-trainControl(method = "cv",number=10)
metric<-"Accuracy"
set.seed(7)
fit.cart <- train(sysgroup~., data=training_setf, method="rpart", metric=metric, trControl=control)
print(fit.cart)
predictions <- predict(fit.cart, testing_setf)
confusionMatrix(predictions, testing_setf$sysgroup)
plot(fit.cart)


################################################################################
#                                  SVM
################################################################################
control<-trainControl(method = "cv",number=10)
metric<-"Accuracy"
set.seed(7)
fit.svm <- train(sysgroup~., data=training_setf, method="svmRadial", metric=metric, trControl=control)
print(fit.svm)
predictions <- predict(fit.svm, testing_setf)
confusionMatrix(predictions, testing_setf$sysgroup)
plot(fit.svm)


################################################################################
#                        summarize accuracy of models
################################################################################

results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
# compare accuracy of models
dotplot(results)




