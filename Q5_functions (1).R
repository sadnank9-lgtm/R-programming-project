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
