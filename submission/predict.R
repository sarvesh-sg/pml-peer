library(doParallel)
cl<-makeCluster(detectCores())
registerDoParallel(cl)

training <- read.csv("pml-training.csv", na.strings = c("NA","#DIV/0!",""))
training <- training[, which(as.numeric(colSums(is.na(training)))==0)]
trainSubSet <- training[,8:ncol(training)]

library(caret)
inTrain <- createDataPartition(y=trainSubSet$classe,p=0.6, list=FALSE)
training1 <- trainSubSet[inTrain,]
testing1 <- trainSubSet[-inTrain,]
model <- train(classe ~ ., data = training1, method = "rf")
confusionMatrix(testing1$classe,predict(model,testing1))

result <- rfcv(training1, training1$classe,cv.fold=5)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

testing <- read.csv("pml-testing.csv", na.strings = c("NA","#DIV/0!",""))
testing <- testing[, which(as.numeric(colSums(is.na(testing)))==0)]
testSubSet <- testing[,9:ncol(testing)-1]
testSubSet$classe <- predict(model,testSubSet)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(testSubSet$classe)

#preProc <- preProcess(trainSubSet[,-ncol(trainSubSet)],method="pca",pcaComp=20)
#trainPC <- predict(preProc,trainSubSet[,-ncol(trainSubSet)])
#modelFit <- train(trainSubSet$classe ~ .,method="knn",data=trainPC)