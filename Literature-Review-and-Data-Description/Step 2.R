
## Step 2 - Understanding of data and preprocessing ##

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
for(i in 1:9) { dsBC[,i] <- as.numeric(as.character(dsBC[,i])) }

# Confirm Data type of attributes after conversion
sapply(dsBC, class)