test <- data.frame(
  lev = as.factor(c("A", "A", "A", "B", "B","B", "C", "C","C","C")),
  lev2 = as.factor(c("x", "y","z", "z", "x","y", "y", "z","z","x")),
  lev3 = as.factor(c("O", "M","N", "O", "M","N", "N", "O","M","N")),
  val = 10:19
)
# coe_matrix_one(
#   test$lev,
#   c("A", "B", "C"),
#   matrix(c(1, -0.5, -0.5), nrow = 1, ncol = 3)
# )


testl <- list(c("AXyXO", "CXxXN", "BXyXN"),c("CXyXO", "CXxXM", "BXzXN", "AXzXM"))
coe_matrix_multi(test[,1:3],
                 list(c("AXyXO", "CXxXN", "BXyXN"),
                      c("CXyXO", "CXxXM", "BXzXN", "AXzXM")),
                 list(c(1,-0.5,-0.5),
                      c(-1,-1,1,1)))
length(testl)


do.call(paste, c(test[1,], sep = "X"))

coe_matrix_multi(test[,1:2],
                 list(c("A<>y", "C<>x", "B<>y"),
                      c("C<>y", "C<>x", "B<>z", "A<>z")),
                 list(c(1,-0.5,-0.5),
                      c(-1,-1,1,1)))

coe_matrix_multi(test[,1],
                 list(c("A", "C", "B"),
                      c("C", "B", "A")),
                 list(c(1,-0.5,-0.5),
                      c(-1,1,1)))

grep("<>", "A<>B<>C", fixed = TRUE)

nchar(gsub("<>","","A"))
cellMean(test[,1:2],test[,4])
cellSize(test[,1:2],test[,4])

aggregate(x=list(Y=test[,4]),
          by=list(test[,1],test[,2]),
          FUN=mean)
aggregate(x=list(Y=test[,4]),
          by=list(test[,1],test[,2]),
          FUN=length)

l_hat(test[,1:2],
      test[,4],
      levels = list(c("A<>y", "C<>x", "B<>y"),
           c("C<>y", "C<>x", "B<>z", "A<>z")),
      coe = list(c(1,-0.5,-0.5),
           c(-1,-1,1,1)))
ComputeL(1:10,10:19)
