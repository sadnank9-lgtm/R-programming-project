# ==============================================================================
# Q2: Exploratory Analysis of the mtcars Dataset
# Topics: built-in datasets, descriptive statistics, subsetting, missing values
# ==============================================================================

data(mtcars)

# ---- (a) First rows of the dataset -------------------------------------------
head(mtcars)

# ---- (b) Descriptive statistics for mpg --------------------------------------
mean(mtcars$mpg)
median(mtcars$mpg)
var(mtcars$mpg)
sd(mtcars$mpg)

# ---- (c) Number of cars with more than 6 cylinders ---------------------------
sum(mtcars$cyl > 6)

# ---- (d) Cars with above-average mpg -----------------------------------------
avg_mpg  <- mean(mtcars$mpg)
high_mpg <- subset(mtcars, mpg > avg_mpg)
high_mpg

# ---- (e) Quartiles of horsepower ---------------------------------------------
quantile(mtcars$hp)

# ---- (f) Average mpg by number of cylinders ----------------------------------
aggregate(mpg ~ cyl, data = mtcars, FUN = mean)
tapply(mtcars$mpg, mtcars$cyl, mean)

# ---- (g) Handling missing values ---------------------------------------------
sum(is.na(mtcars))                 # total number of NAs (0 for mtcars)

# If missing values existed:
clean_data <- na.omit(mtcars)      # option 1: drop rows containing NA

mtcars$mpg[is.na(mtcars$mpg)] <-   # option 2: impute with the mean
  mean(mtcars$mpg, na.rm = TRUE)
