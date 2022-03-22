test <- data.frame(Lev = as.factor(c("A","B","C","A","B","C")),
                   Val = 1:6)
Coe.Matrix.one(test$Lev,
               c("A","B", "C"),
               matrix(c(1,-0.5,-0.5), nrow = 1, ncol = 3))
