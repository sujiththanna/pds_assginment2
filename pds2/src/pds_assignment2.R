#Reading the csv file


library(readr)
diabetes <- read_csv("diabetes.csv")
View(diabetes)


#Getting the info of complete csv
summary(diabetes)

#Total rows
nrow(diabetes)

#Clean data
#Check missing values
any(is.na(diabetes))

#check null values
any(is.null(diabetes))

#------------------------------------------------------------------------
#A)
#set a seed (to ensure work reproducibility) 
set.seed(70)


# Take a random sample of 25 observations
sample <- diabetes[sample(nrow(diabetes), 25, replace = FALSE), ]

#printing the Sample data
sample


#Getting the info of sample data
summary(sample)

#Total rows
nrow(sample)

#finding the mean of the samples and population
data_mean_glucose <- mean(diabetes$Glucose)
data_max_glucose <- max(diabetes$Glucose)
sample_mean_glucose <- mean(sample$Glucose)
sample_max_glucose <- max(sample$Glucose)

#printing the values
sprintf("%f is the Population mean glucose", data_mean_glucose)
sprintf("%f is the Population max glucose", data_max_glucose)
sprintf("%f is the sample mean glucose", sample_mean_glucose)
sprintf("%f is the sample max glucose", sample_max_glucose)


# Create a bar chart to compare the statistics
stats <- c(data_mean_glucose, sample_mean_glucose, data_max_glucose, sample_max_glucose)
names(stats) <- c("P.Mean", "S.Mean", "P.Max", "S.Max")
barplot(stats, main="Comparison of Glucose Statistics", ylab="Glucose", col=c("red", "blue", "red", "blue"), ylim=c(0, 250))

#--------------------------------------------------------------------------------------------------


#B)
#98th percentile of BMI


#summary
summary(diabetes)


#extract BMI
bmi <- diabetes$BMI
bmi

#Calculate the 98th percentile of the BMI column for both the population and the sample
data_98th_percentile <- quantile(bmi, 0.98)
samp_98th_percentile <- quantile(sample(bmi, length(bmi), replace=TRUE), 0.98)

data_98th_percentile
samp_98th_percentile



#Plot the BMI distribution
hist(bmi, breaks=20, xlab="BMI", main="BMI Distribution for Population",col="lightgreen",border = "brown")
abline(v=data_98th_percentile, col="darkblue", lty=2)
legend("topright", legend=c("98th percentile"), col="darkblue", lty=2)

hist(sample(bmi, length(bmi), replace=TRUE), breaks=20, xlab="BMI", main="BMI Distribution for Sample", col="lightblue",border = "black")
abline(v=samp_98th_percentile, col="violet", lty=2)
legend("topright", legend=c("98th percentile"), col="violet", lty=2)


#----------------------------------------------------------------------------------------------------

#C)Using bootstrap (replace= True), create 500 samples (of 150 observation each) from the
#population and find the average mean, standard deviation and percentile for BloodPressure and
#compare this with these statistics from the population for the same variable.


#stats of population i.e mean , sd , percentile of bp
mean_bp <- mean(diabetes$BloodPressure)
sd_bp <- sd(diabetes$BloodPressure)
percentile_bp <- quantile(diabetes$BloodPressure, c(0.25, 0.5, 0.75))
sprintf("%f, %f, %f are the mean_bp, SD_bp, percentile_bp(25,50,75) of the population ",mean_bp,sd_bp,percentile_bp )


# Create empty vectors to store bootstrap results
bp_mean_boot <- rep(NA, 500)
bp_sd_boot <- rep(NA, 500)
bp_quant_boot <- matrix(NA, nrow = 3, ncol = 500)


for (i in 1:500) {
  sample_i <- sample(diabetes$BloodPressure, size = 150, replace = TRUE)
  bp_mean_boot[i] <- mean(sample_i)
  bp_sd_boot[i] <- sd(sample_i)
  bp_quant_boot[, i] <- quantile(sample_i, probs = c(0.25, 0.5, 0.75))
}


# Compare population and bootstrap statistics for BloodPressure
par(mfrow = c(3, 2))
hist(diabetes$BloodPressure, main = "Population", xlab = "BloodPressure")
hist(bp_mean_boot, main = "Bootstrap Mean", xlab = "BloodPressure")
abline(v = mean_bp, col = "blue")
hist(bp_sd_boot, main = "Bootstrap SD", xlab = "BloodPressure")
abline(v = sd_bp, col = "blue")
matplot(bp_quant_boot, type = "l", main = "Bootstrap Percentiles", xlab = "Sample",
        ylab = "BloodPressure", lty = 1, col = 1:3)
lines(percentile_bp, lty = 2, col = "blue")


#We can observe that the bootstrap means, standard deviations, and 
#percentiles are all fairly near to the population values by comparing the histograms and 
#line plots. This implies that the bootstrap samples are representative of the
#population and that we can reasonably
#estimate the statistics for Blood Pressure using them.
