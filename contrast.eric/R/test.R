test <- data.frame(
  lev = as.factor(c("A", "B", "C", "A", "B", "C")),
  lev2 = as.factor(c("x", "y", "z","x", "y", "z")),
  lev3 = as.factor(c("O", "M", "N","O", "M", "N")),
  val = 1:6
)
coe_matrix_one(
  test$lev,
  c("A", "B", "C"),
  matrix(c(1, -0.5, -0.5), nrow = 1, ncol = 3)
)


testl <- list(c("AXyXO", "CXxXN", "BXyXN"),c("CXyXO", "CXxXM", "BXzXN", "AXzXM"))
coe_matrix_multi(test[,1:3],
                 list(c("AXyXO", "CXxXN", "BXyXN"),
                      c("CXyXO", "CXxXM", "BXzXN", "AXzXM")),
                 list(c(1,-0.5,-0.5),
                      c(-1,-1,1,1)))
length(testl)


do.call(paste, c(test[1,], sep = "X"))

coe_matrix_multi(test[,1:2],
                 list(c("AXy", "CXx", "BXy"),
                      c("CXy", "CXx", "BXz", "AXz")),
                 list(c(1,-0.5,-0.5),
                      c(-1,-1,1,1)))

coe_matrix_multi(test[,1],
                 list(c("A", "C", "B"),
                      c("C", "B", "A")),
                 list(c(1,-0.5,-0.5),
                      c(-1,1,1)))
