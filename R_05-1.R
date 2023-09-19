data(mtcars)

# A.Calculate the mean and median of the miles per gallon (mpg) values.
mean_mpg <- mean(mtcars$mpg)
median_mpg <- median(mtcars$mpg)
cat("A. Mean MPG:", mean_mpg)
cat("   Median MPG:", median_mpg)

# B.Identify the mode of the number of cylinders (cyl) in the dataset.
mode_cyl <- as.numeric(names(sort(table(mtcars$cyl), decreasing = TRUE)[1]))
cat("B. Mode of Cylinders:", mode_cyl)

# C.Compute the first quartile (Q1) and third quartile (Q3) of the car weights (wt).
Q1_wt <- quantile(mtcars$wt, 0.25)
Q3_wt <- quantile(mtcars$wt, 0.75)
cat("C. Q1 Weight:", Q1_wt)
cat("   Q3 Weight:", Q3_wt)

# D.Determine the 75th percentile of the car horsepower (hp) values.
percentile_75_hp <- quantile(mtcars$hp, 0.75)
cat("D. 75th Percentile HP:", percentile_75_hp)

# E.Calculate the standard deviation of the miles per gallon (mpg) values.
sd_mpg <- sd(mtcars$mpg)
cat("E. Standard Deviation of MPG:", sd_mpg)

# F.Find the variance of the car displacements (disp).
variance_disp <- var(mtcars$disp)
cat("F. Variance of Displacements:", variance_disp)

# G.Determine the range of the car weights (wt).
range_wt <- range(mtcars$wt)
cat("G. Range of Weights:", range_wt)

# H.Compute the interquartile range (IQR) of the car horsepower (hp) values.
IQR_hp <- IQR(mtcars$hp)
cat("H. IQR of HP:", IQR_hp)

# I.Calculate the skewness of the miles per gallon (mpg) values.
skewness_mpg <- moments::skewness(mtcars$mpg)
cat("I. Skewness of MPG:", skewness_mpg)

# J.Determine the kurtosis of the car displacements (disp).
kurtosis_disp <- moments::kurtosis(mtcars$disp)
cat("J. Kurtosis of Displacements:", kurtosis_disp)

# K.Analyze the relationship between car weights and miles per gallon using appropriate statistical measures and visualizations.
correlation_wt_mpg <- cor(mtcars$wt, mtcars$mpg)
cat("K. Correlation between Weight and MPG:", correlation_wt_mpg)

plot(mtcars$wt, mtcars$mpg, main = "Weight vs. MPG", xlab = "Weight", ylab = "MPG", pch = 19)

# L.Compare the variability in car weights of automatic and manual transmission cars using various measures.
automatic_wt <- mtcars$wt[mtcars$am == 0]
manual_wt <- mtcars$wt[mtcars$am == 1]

# Perform comparisons (e.g., t-test, variance, etc.) between the two groups as needed.
# For example, you can calculate the means and perform a t-test:
mean_automatic_wt <- mean(automatic_wt)
mean_manual_wt <- mean(manual_wt)

cat("L. Comparison of Weight Variability between Automatic and Manual Transmission Cars:\n")
cat("   Mean Weight (Automatic):", mean_automatic_wt)
cat("   Mean Weight (Manual):", mean_manual_wt)

# Perform a t-test to compare means
t_test_result <- t.test(automatic_wt, manual_wt)
cat("   T-test p-value:", t_test_result$p.value)

