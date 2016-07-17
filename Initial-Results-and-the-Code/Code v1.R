# Impute?
# Tuning kNN with box cox?
# Seed is making accuracy better.
# How to investigate 2 incorrect FN
## discuss TN (B) and TP (M). No cases of False Negative FN. 
## There are 2 cases of FP - False Positive where B coming as M
## Accuracy = TN + TP / total observaion (97+45/144) = 98.6
## understand which column is TN, TP etc

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

summary(bc)

dim(bc)

# Remove missing values
bcNoMiss <- bc[complete.cases(bc),]
dim(bcNoMiss)

# Set seed for shuffling data
set.seed(1234)

# Split data 80/20 to make train and test dataset
ind <- sample(2, nrow(bcNoMiss), replace=TRUE, prob=c(0.80, 0.20))

bcNoMiss.training <- bcNoMiss[ind==1, 1:9]
bcNoMiss.test <- bcNoMiss[ind==2, 1:9]

bcNoMiss.trainLabels <- bcNoMiss[ind==1, 10]
bcNoMiss.testLabels <- bcNoMiss[ind==2, 10]


# Apply kNN algorithm
#bc_pred <- knn(train = bcNoMiss.training, test = bcNoMiss.test, cl = bcNoMiss.trainLabels, k=7)

library(class)

accuracy <- rep(0, 10)
k <- 1:10
for(x in k){
  bc_pred <- knn(bcNoMiss.training, bcNoMiss.test, bcNoMiss.trainLabels, k = x)
  accuracy[x] <- mean(bc_pred == bcNoMiss.testLabels)
}

plot(k, accuracy, type = 'b')
plot(bc_pred)

table(bc_pred, bcNoMiss.testLabels)
mean(bc_pred == bcNoMiss.testLabels)

summary(bc_pred)

# Check ConfusionMatrix
library(gmodels)

CrossTable(x = bcNoMiss.testLabels, y = bc_pred, prop.chisq=FALSE)
