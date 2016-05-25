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
# Let's see if there are any NA's in the data frame..
sum(is.na(df))
# ... and get rid of them
df<-na.omit(df)
nrow(df)
ncol(df)

# Before we proceed for PCA, let's scale our data
df.scaled <- scale(df)

# We perform principal component analysis itself
PCA <- princomp(df.scaled, scores=TRUE, cor=TRUE)

# There seems to be a problem with PCA; it looks like
# the covariance matrix is not postitive-definite
# Let's do exploratory analysis to figure this out
install.packages("matrixcalc")
library(matrixcalc)

is.positive.definite(cov(df.scaled))
#is.positive.semi.definite(cor(df))


# The previous tests indicate that covariance and 
# matrix is NOT positive definite.
# Let's do exploratory analysis of eigenvalues and 
# singular value 
# decomposition on it
# Covariance...
cov.matrix<-as.matrix(cov(df.scaled))
ev.cov<-eigen(cov.matrix)
ev.cov$values
#ev.cov$vectors
svd.cov<-svd(cov.matrix)
svd.cov$d
#cov.old<- ((svd.cov$u))  %*% (diag(svd.cov$d)) %*% (t(svd.cov$v))
is.positive.definite(cov.old)

# We observe that the last two eiganvalues, although 
# positive, they are smaller than the default R tolerance
# 1e-8. We can take two approaches here: 1. Change such
# eiganvalues to make positive definite or 2. Apply directly
# SVD and do PCA step by step. Let's do both

# APPROACH No.1 .- Modifying problematic eiganvalues
#
# Let's correct the eigenvalues that are causing issues..

ev.cov$values[38] <- 1e-7
ev.cov$values[39] <- 1e-7
#
# Let's recontruct the original matrix based on the 
# corrected principal values
rm(cov.new)
cov.new <-  (diag(ev.cov$values)) %*% 
    (matrix.inverse(ev.cov$vectors))
cov.new <- (ev.cov$vectors)  %*% cov.new


# Retesting...
is.positive.definite(cov.new)

# Now it seems the matrix is not symmetric. 
# Then let's see how "serious" the anti-symmetry is
# Let's see if there is any pair in which the a_ij - a_ji
# is greater than 10^-14
num_rows<-nrow(cov.new)

for (i in 1:num_rows){
    for (j in i:num_rows){
        diff = cov.new[i,j]-cov.new[j,i]
        if (abs(diff)>=1e-14) print(c(i,j,diff))
    }
}

# The former loop reveals the difference in a_ij and a_ji 
# is less than 10^-14. Let's then simply symmetrize the matrix
    
#install.packages("sna")
#library(sna)
#cov.new<-symmetrize(cov.new,"upper")

cov.new <- (cov.new + t(cov.new))/2.

# ... and check if now is positive definite
# Retesting...
is.positive.definite(cov.new)
eigen(cov.new)$values
svd(cov.new)$d

# Let's now do PCA by doing SVD
svd.cov.new <- svd(cov.new)
summary(svd.cov.new)
# Sum of principal values
princ.vals.sum <- sum(svd.cov.new$d)
# Cumulative sum of principal values
princ.vals.cumul<-cumsum(svd.cov.new$d)
princ.vals.cumul
rel.princ.vals.cumul <- princ.vals.cumul/princ.vals.sum
rel.princ.vals.cumul

plot(rel.princ.vals.cumul,type="lines",main="Cumulative Relative 
     Sum of Principal Values",xlab="Number of Princ. Val. in Sum",
     ylab="Cumulative Relative Sum") 

abline(h=0.9083777,col='red')
abline(v=12,col='red')

# We save the plot to file "elbow.png"
dev.copy(png, file="Cumul_Principal_Values.png", height=480, width=600)
## and switch off the device
dev.off()


