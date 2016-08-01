# load the packages 
library(RCurl) 

# Download data from UCI repository 
repoURL <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data' 
dataDownload <- getURL(repoURL, ssl.verifypeer=FALSE)  
dsBC <- read.csv(textConnection(dataDownload), header=FALSE)  

# Adding a header to above downloaded data 
names(dsBC) <- c("ID","Clp_Thickness","Cell_Size","Cell_Shape","Marg_Adhesion","Sng_Cell_Size","Nuclei","Chromatin","Nucleoli","Mitoses","Class") 

# Display few rows of Breast Cancer Dataset 
head(dsBC) 

# Display number of rows of Breast Cancer Dataset 
head(dsBC, n=100)  

# Data Summary 
summary(dsBC)  

# Checking num of rows (observation) and columns (attribute) 
dim(dsBC)  

# Data type of attributes 
sapply(dsBC, class)  

# Class distribution 
cbind(freq=table(dsBC$Class), percentage=prop.table(table(dsBC$Class))*100) 

# Remove redundant variable ID 
dsBC <- dsBC[,-1]  

# Check dataset after removing ID 
head(dsBC) 

# convert input values to numeric  
for(i in 1:9) { 
  dsBC[,i] <- as.numeric(as.character(dsBC[,i])) 
} 

# Confirm Data type of attributes after conversion 
sapply(dsBC, class) 

# or
table(dsBC$Class)

bc <- dsBC
str(bc)

#Convert Class integer to label
bc$Class <- factor(bc$Class, levels = c(2, 4), labels = c("Benign", "Malignant"))

str(bc)

# To check class distribution 
round(prop.table(table(bc$Class)) * 100, digits = 1)

# Counts of Class
table(bc$Class)

# Checking data
summary(bc)
dim(bc)


# Remove missing values
bcNoMiss <- bc[complete.cases(bc),]


# --------------- Data Visualization Starts ----------


# histograms each attribute 
par(mfrow=c(3,3)) 
for(i in 1:9) 
{ hist(bcNoMiss[,i], main=names(bcNoMiss)[i]) }


# boxplots for each attribute 
par(mfrow=c(3,3)) 
for(i in 1:9) 
{ boxplot(bcNoMiss[,i], main=names(bcNoMiss)[i]) }


# bar plots of each variable by class 
par(mfrow=c(3,3)) 
for(i in 1:9) 
{ barplot(table(bcNoMiss$Class,bcNoMiss[,i]), main=names(bcNoMiss)[i], 
          legend.text=unique(bcNoMiss$Class))

# --------------- Data Visualization Ends ------------


# --------------- Model 1 (All 9 features) Starts -------------

# load the packages 
library(caret) 
library(mlbench) 

# Using dataset that has no missing values
dim(bcNoMiss)


set.seed(7)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(bcNoMiss$Class, p=0.80, list=FALSE)

# select 20% of the data for validation
bctest20 <- bcNoMiss[-validation_index,]

# use the remaining 80% of data to train model
bctrain80 <- bcNoMiss[validation_index,]


# create a list of 50% of the rows in the original dataset we can use for training
validation_index50 <- createDataPartition(bcNoMiss$Class, p=0.50, list=FALSE)

# select 50% of the data for validation
bctest50 <- bcNoMiss[-validation_index50,]

# use the remaining 50% of data to train model
bctrain50 <- bcNoMiss[validation_index50,]


dim(bctest50)
table(bctest50$Class)

# For 80-20 Split
# fit model with train data
set.seed(7)
fit <- knn3(Class~., data=bctrain80, k=7) 

# make predictions with test data
predictions <- predict(fit, bctest20, type="class") 

# summarize accuracy 
table(predictions, bctest20$Class) 
mean(predictions == bctest20$Class)

confusionMatrix(predictions, bctest20$Class)

# 10 Fold Cross Valiation for 80-20 split
control <- trainControl(method="repeatedcv", number=10, repeats=3) 
metric <- "Accuracy"
set.seed(7)
fit.knn <- train(Class~., data=bctrain80, method="knn", metric=metric, trControl=control) 
print(fit.knn)
plot(fit.knn)

control <- trainControl(method="repeatedcv", number=10, repeats=3) 
metric <- "Accuracy"
set.seed(7)
fit.knn <- train(Class~., data=bctest20, method="knn", metric=metric, trControl=control) 
print(fit.knn)
plot(fit.knn)



# For 50-50 Split
# fit model with train data
set.seed(7)
fit <- knn3(Class~., data=bctrain50, k=9) 

# make predictions with test data
predictions <- predict(fit, bctest50, type="class") 

# summarize accuracy 
table(predictions, bctest50$Class) 
mean(predictions == bctest50$Class)

confusionMatrix(predictions, bctest50$Class)


# 10 Fold Cross Valiation for 50-50 split
control <- trainControl(method="repeatedcv", number=10, repeats=3) 
metric <- "Accuracy"
set.seed(7)
fit.knn <- train(Class~., data=bctrain50, method="knn", metric=metric, trControl=control) 
print(fit.knn)
plot(fit.knn)

control <- trainControl(method="repeatedcv", number=10, repeats=3) 
metric <- "Accuracy"
set.seed(7)
fit.knn <- train(Class~., data=bctest50, method="knn", metric=metric, trControl=control) 
print(fit.knn)
plot(fit.knn)

# --------------- Model 1 (All 9 features) Ends -------------




# --------------- Model 2 (All features except Nuclei which has missing values) Starts -------------

# load the packages (Model 2)
library(caret) 
library(mlbench) 

# Using dataset that has missing values (Model 2)
summary(bc)
bc_m2 <- bc
summary(bc_m2)

# Remove Nuclei feature that has missing values (Model 2)
bc_m2 <- subset(bc_m2, select = -Nuclei)
str(bc_m2)


set.seed(7)

# create a list of 80% of the rows in the original dataset we can use for training (Model 2)
validation_index_m2 <- createDataPartition(bc_m2$Class, p=0.80, list=FALSE)

# select 20% of the data for validation (Model 2)
bctest20_m2 <- bc_m2[-validation_index_m2,]

# use the remaining 80% of data to train model (Model 2)
bctrain80_m2 <- bc_m2[validation_index_m2,]

# create a list of 50% of the rows in the original dataset we can use for training (Model 2)
validation_index50_m2 <- createDataPartition(bc_m2$Class, p=0.50, list=FALSE)

# select 50% of the data for validation (Model 2)
bctest50_m2 <- bc_m2[-validation_index50_m2,]

# use the remaining 50% of data to train model (Model 2)
bctrain50_m2 <- bc_m2[validation_index50_m2,]


dim(bctrain50_m2)
table(bctrain50_m2$Class)


# For 80-20 Split (Model 2)
# fit model with train data (Model 2)
set.seed(7)
fit <- knn3(Class~., data=bctrain80_m2, k=9) 

# make predictions with test data (Model 2)
predictions <- predict(fit, bctest20_m2, type="class") 

# summarize accuracy (Model 2)
table(predictions, bctest20_m2$Class) 
mean(predictions == bctest20_m2$Class)

confusionMatrix(predictions, bctest20_m2$Class)


# 10 Fold Cross Valiation for 80-20 split (Model 2)
control <- trainControl(method="repeatedcv", number=10, repeats=3) 
metric <- "Accuracy"
set.seed(7)
fit.knn <- train(Class~., data=bctrain80_m2, method="knn", metric=metric, trControl=control) 
print(fit.knn)
plot(fit.knn)

control <- trainControl(method="repeatedcv", number=10, repeats=3) 
metric <- "Accuracy"
set.seed(7)
fit.knn <- train(Class~., data=bctest20_m2, method="knn", metric=metric, trControl=control) 
print(fit.knn)
plot(fit.knn)



# For 50-50 Split (Model 2)
# fit model with train data (Model 2)
set.seed(7)
fit <- knn3(Class~., data=bctrain50_m2, k=5) 

# make predictions with test data (Model 2)
predictions <- predict(fit, bctest50_m2, type="class") 

# summarize accuracy (Model 2)
table(predictions, bctest50_m2$Class) 
mean(predictions == bctest50_m2$Class)

confusionMatrix(predictions, bctest50_m2$Class)


# 10 Fold Cross Valiation for 50-50 split (Model 2)
control <- trainControl(method="repeatedcv", number=10, repeats=3) 
metric <- "Accuracy"
set.seed(7)
fit.knn <- train(Class~., data=bctrain50_m2, method="knn", metric=metric, trControl=control) 
print(fit.knn)
plot(fit.knn)

control <- trainControl(method="repeatedcv", number=10, repeats=3) 
metric <- "Accuracy"
set.seed(7)
fit.knn <- train(Class~., data=bctest50_m2, method="knn", metric=metric, trControl=control) 
print(fit.knn)
plot(fit.knn)

# --------------- Model 2 (All features except Nuclei which has missing values) Ends -------------