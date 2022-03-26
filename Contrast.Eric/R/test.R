test <- data.frame(
  lev = as.factor(c("A", "B", "C", "A", "B", "C")),
  val = 1:6
)
coe_matrix_one(
  test$lev,
  c("A", "B", "C"),
  matrix(c(1, -0.5, -0.5), nrow = 1, ncol = 3)
)
