#Load data to data frame
data<-read.csv("data.csv")
# Take a look at the data...
View(data)
# Explore names of columns
names(data)
# Number of rows
nrow(data)

# Let's get rid of the first column (user_id)
df<-data[c(-1)]
names(df)
nrow(df)
ncol(df)
sum(is.na(df))
df<-na.omit(df)
nrow(df)
ncol(df)

# We perform principal component analysis itself
PCA <- princomp(df,cor=TRUE) #, scores=TRUE, cor=TRUE)

# There seems to be a problem with PCA. Let's see if 
# covariance/correlation matrices are positive definite
install.packages("matrixcalc")
library(matrixcalc)

is.positive.definite(cor(df))
is.positive.definite(cov(df))
is.positive.semi.definite(cor(df))
is.positive.semi.definite(cov(df))


# Let's do exploratory analysis of eigenvalues and singular value 
# decomposition on covariance and correlation matices
# Covariance...
ev.cov<-eigen(cov(df))
ev.cov$values
svd.cov<-svd(cov(df))
svd.cov$d
# Correlation...
ev.cor<-eigen(cor(df))
ev.cor$values
svd.cor<-svd(cor(df))
svd.cor$d

# to be continued...


