## Step 1 - Downloading data from UCI website ##

library(RCurl)

# Download data from UCI repository
repoURL <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data'
dataDownload <- getURL(repoURL, ssl.verifypeer=FALSE) 
dsBC <- read.csv(textConnection(dataDownload), header=FALSE) 

# Adding a header to above downloaded data
names(dsBC) <- c("ID","Clp_Thickness","Cell_Size","Cell_Shape","Marg_Adhesion","Sng_Cell_Size","Nuclei","Chromatin","Nucleoli","Mitoses","Class")

# Display few rows of Breast Cancer Dataset
head(dsBC)