myNewData = read.csv("myNewData.csv")

sampleVector = c(5.5, 130)
cat("Sample vector is given as ", sampleVector, "\n")

EuclideanDist = function(a,b){
    return (sqrt(sum((a-b)^2)))
}

MahalanobisDist = function(x, mean, covariance ){
    diff= (x - mean)
    inv_covar = solve(covariance)
    return (sqrt(diff %*% inv_covar %*% diff))
}

male_mean_height = mean(myNewData$Height[myNewData$Gender == "Male"])
male_mean_weight = mean(myNewData$Weight[myNewData$Gender == "Male"])
male_means = c(male_mean_height, male_mean_weight)
cat("Male Means are ", male_means, "\n")

female_mean_height = mean(myNewData$Height[myNewData$Gender == "Female"])
female_mean_weight = mean(myNewData$Weight[myNewData$Gender == "Female"])
female_means = c(female_mean_height , female_mean_weight)
cat("Female Means are ",female_means, "\n")

covariance = cov(myNewData[, 1:2])
cat("Following is the covarinace matrix ", covariance , "\n")

euclid_male_dist = EuclideanDist (male_means , sampleVector)
cat("Calculated Euclidean Male Distance ", euclid_male_dist , "\n")
euclid_female_dist = EuclideanDist(female_means , sampleVector)
cat("Calculated Euclidean Female Distance ", euclid_female_dist , "\n")

if (euclid_female_dist>euclid_male_dist){
    cat("Sample vector belongs to MALE distribution by Euclidean distance \n \n")
} else{
    cat("Sample vector belongs to FEMALE distribution by Euclidean distance \n \n")
}

mahalanobis_male_dist = MahalanobisDist( sampleVector, male_means , covariance)
cat("Calculated Mahalanobis Male Distance ", mahalanobis_male_dist , "\n")
mahalanobis_female_dist = MahalanobisDist(sampleVector , female_means, covariance)
cat("Calculated Mahalanobis female Distance ", mahalanobis_female_dist , "\n")

if (mahalanobis_female_dist>mahalanobis_male_dist){
    cat("Sample vector belongs to MALE distribution by Mahalanobis distance \n")
} else{
    cat("Sample vector belongs to FEMALE distribution by Mahalanobis distance\n")
}

windows()
plot(myNewData$Height, myNewData$Weight, xlab = "Height", ylab = "Weight", main = "Male Female Distribution and Classification of sample vector")
points(male_means[1], male_means[2], col = "blue", pch = "+", cex = 2)
points(female_means[1], female_means[2], col = "red", pch = "+", cex = 2)
points(sampleVector[1], sampleVector[2], col = "green", pch = "*", cex = 2)
segments(x0 = sampleVector[1], y0 = sampleVector[2], x1 = male_means[1], y1 = male_means[2], col = "blue", lty = 2)
segments(x0 = sampleVector[1], y0 = sampleVector[2], x1 = female_means[1], y1 = female_means[2], col = "red", lty = 2)
legend("topleft", legend = c("Male Centroid", "Female Centroid", "Sample Vector"), 
       col = c("blue", "red", "green"), pch = c("+", "+", "*"))