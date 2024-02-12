n = 200 
min_height = 0
max_height = 7
min_weight = 0
max_weight = 250

mean_height = (min_height + max_height)/ 2
mean_weight = (min_weight + max_weight)/ 2

sd_height = 1.5
sd_weight = 20

num_male = round(n * 0.7)
num_female = n - num_male

male_heights = rnorm(num_male, mean_height, sd_height)
male_heights = pmax(min_height, pmin(max_height, male_heights))
male_weights = rnorm(num_male, mean_weight, sd_weight)
male_weights = pmax(min_weight, pmin(max_weight, male_weights))

female_heights = rnorm(num_female, mean_height, sd_height)
female_heights = pmax(min_height, pmin(max_height, female_heights))
female_weights = rnorm(num_female, mean_weight, sd_weight)
female_weights = pmax(min_weight, pmin(max_weight, female_weights))

heights = c(male_heights, female_heights)
weights = c(male_weights, female_weights)
genders = c(rep("Male", num_male), rep("Female", num_female))

myNewData = data.frame(Height = heights, Weight = weights, Gender = genders)

male_mean_height = mean(myNewData$Height[myNewData$Gender == "Male"])
male_mean_weight = mean(myNewData$Weight[myNewData$Gender == "Male"])
male_sd_height = sd(myNewData$Height[myNewData$Gender == "Male"])
male_sd_weight = sd(myNewData$Weight[myNewData$Gender == "Male"])
male_median_height = median(myNewData$Height[myNewData$Gender == "Male"])
male_median_weight = median(myNewData$Weight[myNewData$Gender == "Male"])
male_min_height = min(myNewData$Height[myNewData$Gender == "Male"])
male_min_weight = min(myNewData$Weight[myNewData$Gender == "Male"])
male_max_height = max(myNewData$Height[myNewData$Gender == "Male"])
male_max_weight = max(myNewData$Weight[myNewData$Gender == "Male"])


cat("\nMale Statistics:\n")
cat("Height:\n")
cat("Mean:",male_mean_height,"\n")
cat("Standard Deviation:",male_sd_height,"\n")
cat("Median:",male_median_height,"\n")
cat("Min:",male_min_height,"\n")
cat("Max:",male_max_height,"\n")

cat("\nWeight:\n")
cat("Mean:",male_mean_weight,"\n")
cat("Standard Deviation:",male_sd_weight,"\n")
cat("Median:",male_median_weight,"\n")
cat("Min:",male_min_weight,"\n")
cat("Max:",male_max_weight,"\n")

female_mean_height = mean(myNewData$Height[myNewData$Gender == "Female"])
female_mean_weight = mean(myNewData$Weight[myNewData$Gender == "Female"])
female_sd_height = sd(myNewData$Height[myNewData$Gender == "Female"])
female_sd_weight = sd(myNewData$Weight[myNewData$Gender == "Female"])
female_median_height = median(myNewData$Height[myNewData$Gender == "Female"])
female_median_weight = median(myNewData$Weight[myNewData$Gender == "Female"])
female_min_height = min(myNewData$Height[myNewData$Gender == "Female"])
female_min_weight = min(myNewData$Weight[myNewData$Gender == "Female"])
female_max_height = max(myNewData$Height[myNewData$Gender == "Female"])
female_max_weight = max(myNewData$Weight[myNewData$Gender == "Female"])

cat("\nFemale Statistics:\n")
cat("Height:\n")
cat("Mean:",female_mean_height,"\n")
cat("Standard Deviation:",female_sd_height,"\n")
cat("Median:",female_median_height,"\n")
cat("Min:",female_min_height,"\n")
cat("Max:",female_max_height,"\n")

cat("\nWeight:\n")
cat("Mean:",female_mean_weight,"\n")
cat("Standard Deviation:",female_sd_weight,"\n")
cat("Median:",female_median_weight,"\n")
cat("Min:",female_min_weight,"\n")
cat("Max:",female_max_weight,"\n")

height_weight_covariance = cov(myNewData$Height, myNewData$Weight)

cat("\nCovariance between Height and Weight:\n")
cat(height_weight_covariance,"\n")


windows()
plot(myNewData$Height, myNewData$Weight, col = ifelse(myNewData$Gender == "Male", "blue", "red"), 
     xlab = "Height", ylab = "Weight", main = "Height vs. Weight by Gender")
legend("topleft", legend = c("Male", "Female"), col = c("blue", "red"), pch = 1)

write.csv(myNewData, file = "myNewData.csv", row.names = FALSE)

#plotting histograms by superimposing them on each other for male vs female for heights and weights respectively
windows()
par(mfrow = c(1, 1))

hist(myNewData$Height[myNewData$Gender == "Male"], col = rgb(0, 0, 1, 0.5), xlab = "Height", main = "Male vs. Female Heights", ylim=c(0, 25), xlim=c(0, 8), 
     border = "blue", breaks = seq(0, 8, by = 0.5), 
     xaxt = "n", yaxt = "n", axes = FALSE)

hist(myNewData$Height[myNewData$Gender == "Female"], col = rgb(1, 0, 0, 0.5), xlab = "", add = TRUE, 
     border = "red", breaks = seq(0, 8, by = 0.5), axes = FALSE)

axis(1, at = seq(0, 8, by = 1), labels = TRUE, col.axis = "black")
axis(2, at = seq(0, 25, by = 5), labels = TRUE, col.axis = "black")
legend("topleft", legend = c("Male", "Female"), fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))


windows()
par(mfrow = c(1, 1))

hist(myNewData$Weight[myNewData$Gender == "Male"], col = rgb(0, 0, 1, 0.5), xlab = "Weight", main = "Male vs. Female Weights", 
     xlim = c(0, 250), border = "blue", breaks = seq(0, 250, by = 10), xaxt = "n", yaxt = "n", axes = FALSE)

hist(myNewData$Weight[myNewData$Gender == "Female"], col = rgb(1, 0, 0, 0.5), xlab = "", add = TRUE, 
     border = "red", breaks = seq(0, 250, by = 10), axes = FALSE)

axis(1, at = seq(0, 250, by = 50), labels = TRUE, col.axis = "black")
axis(2, labels = TRUE, col.axis = "black")
legend("topleft", legend = c("Male", "Female"), fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))


