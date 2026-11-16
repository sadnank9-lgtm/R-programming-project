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
