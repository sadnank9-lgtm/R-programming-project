# ==============================================================================
# R Programming Assignment
# Author: Sadnan
#
# Contents
#   Q1  Employee data frame analysis
#   Q2  Exploratory analysis of the mtcars dataset
#   Q3  Working with lists
#   Q4  Working with arrays
#   Q5  User-defined functions
#   Q6  Loops and control flow
#   Q7  Exam scores analysis
#
# Requirements: R >= 4.0, base R only (no external packages).
# ==============================================================================

# ==============================================================================
# Q1: Employee Data Frame Analysis
# Topics: data frames, descriptive statistics, aggregation, sorting, correlation
# ==============================================================================

# ---- 1. Create vectors -------------------------------------------------------
Employee         <- c("Alice", "Bob", "Carol", "David",
                      "Eve", "Frank", "Grace", "Heidi")
Age              <- c(29, 34, 41, 26, 38, 45, 31, 50)
Department       <- c("Sales", "IT", "Sales", "HR",
                      "IT", "Marketing", "HR", "Marketing")
Salary           <- c(45, 62, 55, 40, 70, 58, 47, 65)   # in thousands
Years_Experience <- c(3, 6, 10, 2, 8, 12, 4, 15)

# ---- 2. Build the data frame -------------------------------------------------
emp_df <- data.frame(Employee, Age, Department, Salary, Years_Experience)

str(emp_df)       # structure
summary(emp_df)   # summary statistics

# ---- 3. Salary statistics ----------------------------------------------------
mean(emp_df$Salary)
median(emp_df$Salary)
sd(emp_df$Salary)
range(emp_df$Salary)

# ---- 4. Employees with maximum and minimum experience ------------------------
emp_df[emp_df$Years_Experience == max(emp_df$Years_Experience), ]
emp_df[emp_df$Years_Experience == min(emp_df$Years_Experience), ]

# ---- 5. Department-wise average salary ---------------------------------------
# Method 1: aggregate()
aggregate(Salary ~ Department, data = emp_df, FUN = mean)

# Method 2: tapply()
tapply(emp_df$Salary, emp_df$Department, mean)

# ---- 6. New column: salary per year of experience ----------------------------
emp_df$Salary_per_Year_Exp <- emp_df$Salary / emp_df$Years_Experience

# ---- 7. Top 3 employees by salary --------------------------------------------
sorted_emp <- emp_df[order(-emp_df$Salary), ]
head(sorted_emp, 3)

# ---- 8. Correlation between age and salary -----------------------------------
cor(emp_df$Age, emp_df$Salary)


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


# ==============================================================================
# Q3: Working with Lists
# Topics: creating, accessing, modifying, and nesting lists
# ==============================================================================

# ---- (a) Create a list -------------------------------------------------------
student <- list(
  name   = "Rahim",
  age    = 21,
  marks  = c(78, 85, 90, 66),
  passed = TRUE
)
student

# ---- (b) Access the marks ----------------------------------------------------
student$marks

# ---- (c) Average of the marks ------------------------------------------------
mean(student$marks)

# ---- (d) Add a new element ---------------------------------------------------
student$grade <- "A"
student

# ---- (e) Remove an element ---------------------------------------------------
student$passed <- NULL
student

# ---- (f) Nested list ---------------------------------------------------------
students <- list(
  list(name = "Rahim", marks = c(78, 85, 90)),
  list(name = "Karim", marks = c(88, 76, 91)),
  list(name = "Sakib", marks = c(82, 80, 95))
)

# Name of the second student
students[[2]]$name


# ==============================================================================
# Q4: Working with Arrays
# Topics: 3-D arrays, indexing, apply(), logical replacement
# ==============================================================================

# ---- (a) Create a 3 x 3 x 2 array --------------------------------------------
arr1 <- array(1:18, dim = c(3, 3, 2))
arr1
# , , 1
#      [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9
#
# , , 2
#      [,1] [,2] [,3]
# [1,]   10   13   16
# [2,]   11   14   17
# [3,]   12   15   18

# ---- (b) Second layer --------------------------------------------------------
arr1[, , 2]

# ---- (c) Element at row 2, column 3, layer 1 ---------------------------------
arr1[2, 3, 1]      # [1] 8

# ---- (d) Sum of all elements -------------------------------------------------
sum(arr1)          # [1] 171

# ---- (e) Row sums and column sums (across both layers) -----------------------
apply(arr1, 1, sum)   # [1] 51 57 63   (row sums)
apply(arr1, 2, sum)   # [1] 39 57 75   (column sums)

# ---- (f) Replace all elements greater than 10 with 0 -------------------------
arr1[arr1 > 10] <- 0
arr1
# , , 2
#      [,1] [,2] [,3]
# [1,]   10    0    0
# [2,]    0    0    0
# [3,]    0    0    0


# ==============================================================================
# Q5: User-Defined Functions
# Topics: control flow in functions, return values, recursion
# ==============================================================================

# ---- (a) Prime number checker ------------------------------------------------
is_prime <- function(n) {
  if (n <= 1) return(FALSE)
  if (n <= 3) return(TRUE)            # 2 and 3 are prime
  if (n %% 2 == 0) return(FALSE)

  # Only need to test divisors up to sqrt(n)
  i <- 3
  while (i * i <= n) {
    if (n %% i == 0) return(FALSE)
    i <- i + 2
  }
  TRUE
}

is_prime(7)    # TRUE
is_prime(10)   # FALSE
is_prime(2)    # TRUE

# ---- (b) BMI calculator ------------------------------------------------------
# weight in kg, height in metres
bmi_calculator <- function(weight, height) {
  bmi <- weight / (height^2)

  category <- if (bmi < 18.5) {
    "Underweight"
  } else if (bmi < 25) {
    "Normal"
  } else if (bmi < 30) {
    "Overweight"
  } else {
    "Obese"
  }

  list(BMI = bmi, Category = category)
}

bmi_calculator(60, 1.70)

# ---- (c) Summary statistics function -----------------------------------------
stats_summary <- function(x) {
  list(
    Mean    = mean(x),
    Median  = median(x),
    SD      = sd(x),
    Minimum = min(x),
    Maximum = max(x)
  )
}

stats_summary(c(10, 20, 30, 40, 50))

# ---- (d) Recursive factorial -------------------------------------------------
factorial_r <- function(n) {
  if (n < 0) stop("n must be a non-negative integer")
  if (n == 0) return(1)
  n * factorial_r(n - 1)
}

factorial_r(5)   # 120


# ==============================================================================
# Q6: Loops and Control Flow
# Topics: for, while, repeat, nested loops, next, break
# ==============================================================================

# ---- (a) Even numbers from 1 to 50 -------------------------------------------
for (i in 1:50) {
  if (i %% 2 == 0) print(i)
}

# ---- (b) Sum of the first 20 natural numbers (while loop) --------------------
sum_n <- 0
i <- 1
while (i <= 20) {
  sum_n <- sum_n + i
  i <- i + 1
}
print(sum_n)        # 210

# Vectorised check
sum(1:20)           # 210

# ---- (c) Multiplication table (nested loops) ---------------------------------
for (i in 1:5) {
  for (j in 1:5) {
    cat(i, "x", j, "=", i * j, "  ")
  }
  cat("\n")
}

# ---- (d) Classify numbers as even or odd -------------------------------------
nums <- c(12, 7, 25, 4, 18, 9, 30)

for (n in nums) {
  if (n %% 2 == 0) {
    cat(n, "is Even\n")
  } else {
    cat(n, "is Odd\n")
  }
}

# ---- (e) repeat loop with break ----------------------------------------------
i <- 1
repeat {
  print(i)
  if (i == 10) break
  i <- i + 1
}

# ---- (f) Skip missing values with next ---------------------------------------
marks <- c(45, 38, NA, 90, 22)

for (m in marks) {
  if (is.na(m)) next            # skip NA values
  if (m >= 40) {
    cat("Pass\n")
  } else {
    cat("Fail\n")
  }
}


# ==============================================================================
# Q7: Exam Scores Analysis
# Topics: named vectors, data frames, custom functions, sapply, ifelse,
#         manual computation of mean / median / standard deviation
# ==============================================================================

# ---- Data --------------------------------------------------------------------
scores <- c(45, 67, 23, 89, 56, 34, 90, 12, 78, 65)
names(scores) <- paste0("Student", 1:10)

# ---- (a) Create a data frame -------------------------------------------------
df <- data.frame(
  Student = names(scores),
  Score   = as.numeric(scores)
)
print(df)

# ---- (b) Grading function ----------------------------------------------------
# A: >= 80 | B: 60-79 | C: 40-59 | F: < 40
grade_it <- function(score) {
  if (score >= 80) {
    "A"
  } else if (score >= 60) {
    "B"
  } else if (score >= 40) {
    "C"
  } else {
    "F"
  }
}

grade_it(45)   # "C"
grade_it(89)   # "A"

# ---- (c) Apply grade_it() to every student -----------------------------------
df$Grade <- sapply(df$Score, grade_it)
print(df)

# ---- (d) Pass / Fail classification (pass mark = 40) -------------------------
result <- ifelse(scores >= 40, "Pass", "Fail")
print(result)
print(table(result))

# ---- (e) Manual statistics (without mean(), median(), sd()) ------------------
n <- length(scores)

# Mean
total <- 0
for (x in scores) total <- total + x
mean_score <- total / n
print(mean_score)

# Median
sorted_scores <- sort(scores)
if (n %% 2 == 1) {
  median_score <- sorted_scores[(n + 1) / 2]
} else {
  median_score <- (sorted_scores[n / 2] + sorted_scores[n / 2 + 1]) / 2
}
print(median_score)

# Sample standard deviation
sum_squared <- 0
for (x in scores) sum_squared <- sum_squared + (x - mean_score)^2
sd_score <- sqrt(sum_squared / (n - 1))
print(sd_score)

# Verification against built-in functions
c(mean(scores), median(scores), sd(scores))


