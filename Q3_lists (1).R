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
