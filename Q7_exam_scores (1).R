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
