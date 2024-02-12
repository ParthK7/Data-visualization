n = 200 
min_height = 0
max_height = 7
min_weight = 0
max_weight = 250


mean_height = (min_height + max_height)/ 2
mean_weight = (min_weight + max_weight)/ 2

sd_height = 1.5
sd_weight = 20

heights = rnorm(n, mean_height , sd_height)
heights = pmax(min_height,pmin(max_height,heights))

weights = rnorm(n, mean_weight , sd_weight)
weights = pmax(min_weight,pmin(max_weight,weights))

myData = data.frame(Height = heights, Weight = weights)

calculated_mean_height = mean(myData $Height)
calculated_mean_weight = mean(myData $Weight)

calculated_sd_height = sd(myData $Height)
calculated_sd_weight = sd(myData $Weight)

calculated_median_height = median(myData$Height)
calculated_median_weight = median(myData$Weight)

calc_min_height = min(myData $Height)
calc_max_height = max(myData $Height)

calc_min_weight = min(myData $Weight)
calc_max_weight = max(myData $Weight)

height_weight_covariance = cov(myData $Height, myData $Weight)

cat("\nMean Height=", calculated_mean_height)
cat("\nMin Height = ", calc_min_height)
cat("\nMax Height = ", calc_max_height)
cat("\nMedian height" , calculated_median_height)
cat("\nStandatd deviation of height data" , calculated_sd_height)

cat("\nMean Weight " , calculated_mean_weight)
cat("\nMin Weight ", calc_min_weight)
cat("\nMax weight " , calc_max_weight)
cat("\nMedian weight" , calculated_median_weight)
cat("\nStandard deviation of weight data" , calculated_sd_weight)

cat("\n  Covariance of the height and weight is ", height_weight_covariance)

windows()
hist (myData $Weight , xlab = "Weight", main = "Weight Data")
windows()
hist (myData $Height , xlab = "Height", main = "Height Data")
windows()
plot(myData $ Height, myData $ Weight , xlab = "Height ", ylab ="Weight", main = "Height vs Weight Plot for the sample")

