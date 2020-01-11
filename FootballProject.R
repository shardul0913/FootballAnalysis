
if (! require(corrplot)){install.packages("corrplot"); library(corrplot)}
if (! require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if (! require(car)){install.packages("car"); library(car)}
if (! require(readxl)){install.packages("readxl"); library(readxl)}
if (! require(DAAG)){install.packages("DAAG"); library(DAAG)}
if (! require(randomForest)){install.packages("randomForest"); library(randomForest)}
if (! require(e1071)){install.packages("e1071"); library(e1071)}
if (! require(caret)){install.packages("caret"); library(caret)}
if (! require(rminer)){install.packages("rminer"); library(rminer)}
if (! require(boot)){install.packages("boot"); library(boot)}
if (! require(Metrics)){install.packages("Metrics"); library(Metrics)}
if (! require(class)){install.packages("class"); library(class)}
if (! require(FactoMineR)){install.packages("FactoMineR"); library(FactoMineR)}
if (! require(mstknnclust)){install.packages("mstknnclust"); library(mstknnclust)}
if (! require(clusterSim)){install.packages("clusterSim"); library(clusterSim)}
if (! require(lattice)){install.packages("lattice"); library(lattice)}


MyData = read_excel("EPL2013-2020.xlsx")
summary(MyData)

MyData1 = MyData[sample(nrow(MyData)),]

MyData1$Referee = factor(MyData$Referee,labels=paste('Q',1:43,sep=''))
MyData1$AwayTeam = factor(MyData$AwayTeam,labels=paste('Q',1:39,sep=''))
MyData1$Status = factor(MyData$Status,labels=c(1,2))
MyData1$HTR = factor(MyData$HTR,labels=c(1,2,3))
MyData1$FTR = factor(MyData$FTR,labels=c(1,2,3))

drops2 <- c("Referee","HomeTeam","Date","OR","LR")

NcorrplotData = MyData1[ , !(names(MyData1) %in% drops2)]
corrplotData = MyData1[ , !(names(MyData1) %in% drops2)]

####################################### Linear Regression #######################################

NcorrplotData$Status <- lapply(NcorrplotData$Status,as.integer);
NcorrplotData$FTR <- lapply(NcorrplotData$FTR,as.integer);
NcorrplotData$HTR <- lapply(NcorrplotData$HTR,as.integer);


NcorrplotData = as.data.frame(lapply(NcorrplotData, as.numeric))

dataMatrix <- cor(NcorrplotData)
cor_matrix <- cor(NcorrplotData, method = c("pearson")) 
corrplot(cor_matrix, method="number")
sapply(NcorrplotData, class)

## Understanding the variance and data distribution

summary(pca.corrplotData)
pca.corrplotData <- FAMD(corrplotData)


# The linear model train and test for the all numeric values
trainWith <- sample(1:nrow(NcorrplotData), nrow(NcorrplotData) * 0.7)
train.data.numeric <- NcorrplotData[trainWith, ]
# test data set
testWith <- -trainWith
test.data.numeric <- NcorrplotData[testWith, ]


################################### linear model ############################### 
linearMod = cv.lm(train.data.numeric, form.lm = formula(FTR ~ .), m=5)
summary(linearMod)


# Convert the  values to factors for random forest

################################## Random forest #######################################


corrplotData$FTR = factor(corrplotData$FTR) 
corrplotData$AwayTeam = factor(corrplotData$AwayTeam) 
corrplotData$HTR = factor(corrplotData$HTR)
corrplotData$Status = factor(corrplotData$Status)

##############################################################################################################################################################


trainWith <- sample(1:nrow(corrplotData), nrow(corrplotData) * 0.7)
train.data <- corrplotData[trainWith, ]
# test data set
testWith <- -trainWith
test.data <- corrplotData[testWith, ]

forestMod <- randomForest(train.data$FTR ~ .,data = train.data, ntree = 21, mtry = 10)
varImpPlot(forestMod,type=2)
forestMod.predict <- predict(forestMod, test.data)
plot(forestMod.predict)
print(forestMod)

test.error2 = c()
test.accuracy = c()
m_list1 = c()
m_list2 = c()
ob.list = rep(0, 100)

for(i in seq(5, 105, 5)){
  modelLoop = randomForest(train.data$FTR ~ .,data = train.data,ntree = i, mtry = 5)
  ran.for.tree.predict = predict(modelLoop,  test.data)
  m_list1 = append(m_list1, i)
  test.accuracy = append(test.accuracy, MLmetrics::Accuracy(ran.for.tree.predict, test.data$FTR))
}

for(i in c(2, 4, 6, 8, 10)){
  modelLoop = randomForest(train.data$FTR ~ ., data = train.data, ntree = 100, mtry = i)
  ran.for.mode.predict = predict(modelLoop, test.data) 
  ob.list = cbind(ob.list, modelLoop$err.rate[, c(1)])
  m_list2 = append(m_list2, i)
  test.error2 = append(test.error2, MLmetrics::Accuracy(ran.for.mode.predict, test.data$FTR))
}

plot.data = data.frame(m_list1, test.accuracy); names(plot.data) = c("model list", "Accuracy")

ggplot(plot.data, aes(x = m_list1, y = Accuracy)) + geom_line(color="red")+ geom_line(linetype = "dashed")+geom_point() + ggtitle("model list vs. Accuracy")

plot_dataOOB = data.frame(ob.list)[,-c(1)]; 
names(plot_dataOOB) = c("m_2", "m_4", "m_6", "m_8", "m_10")
plot_dataOOB$NTrees = seq(1, 100)

ggplot(plot_dataOOB, aes(NTrees)) + geom_line(aes(y = m_2, colour = "m = 2")) + 
  ggtitle("Total OOB graph error") + xlab("number of  trees") + ylab("Error of OOB") +
  geom_line(aes(y = m_4, colour = "m = 4")) +   geom_line(aes(y = m_6, colour = "m = 6")) + 
  geom_line(aes(y = m_8, colour = "m = 8"))+ geom_line(aes(y = m_10, colour = "m = 10"))


############################################## SVM ######################################################

# Add the SVM and try to visualize the data to understand the type of kernel required to fit

svm.data.numeric = scale(NcorrplotData[,-c(5)])
svm.data.numeric = as.data.frame(svm.data.numeric)
MyDataSVM = read_excel("EPL2013-2020.xlsx")
svm.data.numeric$FTR = as.factor(MyDataSVM$FTR)

trainWithSVM <- sample(1:nrow(svm.data.numeric), nrow(svm.data.numeric) * 0.7)
train.data.SVM <- svm.data.numeric[trainWithSVM, ]
# test data set
testWithSVM <- -trainWithSVM
test.data.SVM <- svm.data.numeric[testWithSVM, ]


set.seed(123)
model <- train(
  FTR ~., data = train.data.SVM, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(C = seq(1, 4, length = 20),sigma=seq(0.1, 0.9, length = 20))
)
plot(model)
predicted.classesSVM <- predict(model, test.data.SVM)
mean(predicted.classesSVM == test.data.SVM$FTR)

svm.test.data.numeric = scale(test.data.numeric)


#######################################KNN##################################

# Do a KNN prediction 


tuneDf = tune.train.data.numeric[,-4]  
scale.train.data.numeric = scale(tune.train.data.numeric[,-5])
scale.train.data.numeric = as.data.frame(scale.train.data.numeric)
scale.train.data.numeric$FTR = factor(tune.train.data.numeric[,5],levels = 1:3)

scale.test.data.numeric = scale(tune.test.data.numeric[,-5])
scale.test.data.numeric = as.data.frame(scale.test.data.numeric)
scale.test.data.numeric$FTR = factor(tune.test.data.numeric[,5],levels = 1:3)

set.seed(399)
knn.model <- knn(train = scale.train.data.numeric, test = scale.test.data.numeric,cl = scale.train.data.numeric$FTR, k = 6)
plot(knn.model)
confusionMatrix(zz,knn.model)

## KNN with cross validations

scale.data.numeric = scale(NcorrplotData[,-5])
scale.data.numeric = as.data.frame(scale.data.numeric)
MyDatanew = read_excel("EPL2013-2020.xlsx")
scale.data.numeric$FTR = as.factor(MyDatanew$FTR)

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 5,classProbs = TRUE) 
knnFit <- train(FTR ~ ., data = scale.data.numeric, method = "knn", trControl = ctrl,preProcess = c("center","scale"), tuneGrid   = expand.grid(k = 1:10))
plot(knnFit)

# LOOCV
#knn.model <- knn.cv(train = scale.train.data.numeric, cl = scale.train.data.numeric$FTR, k = 5, prob = FALSE, use.all = TRUE)


# Add the PCA on the mumerical data to reduce the variable numbers




#library(devtools)
#install_github("vqv/ggbiplot")
#library(ggbiplot)
# came across the multiple factor analysis


