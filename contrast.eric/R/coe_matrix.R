#### A function to generate coefficients matrix


#### In order to make your hypotheses to be correctly converted to a coefficient matrix, you need to follow the rules below:
#### levels: a list of character vectors indicating which levels of the factor your hypotheses are related to
#### coe: a list of numeric vectors indicating what the coefficients for each of your hypotheses, corresponding to the character vector in levels
#### col_factor: The columns of all different factors involved in the testing.

coe_matrix_multi <- function(colFactor, levels, coe){
      colFactor <- as.data.frame(colFactor)
      ## Create the empty coefficients table
      numFactor <- ifelse(is.null(ncol(colFactor)), 1, ncol(colFactor))
      l <- list()
      for (i in 1:numFactor) {
        l[[i]] <- as.character(unique(colFactor[,i]))
      }
      allcomb <- expand.grid(l)
      #print(allcomb)

      all_levels <- character()
      if(ncol(allcomb)==1){
        for(i in 1:nrow(allcomb)){
          all_levels[i] <- as.character(allcomb[i,1])
        }
      }else{
        for(i in 1:nrow(allcomb)){
          all_levels[i] <- do.call(paste, c(allcomb[i,], sep = "<>"))
        }
      }

      #print(all_levels)

      coe_matrix <- as.data.frame(matrix(
        data = 0,
        ncol = length(all_levels),
        nrow = length(coe)
      ))
      #print(coe_matrix)
      colnames(coe_matrix) <- all_levels

      ## put the selected coefficient list into the full coefficient matrix
      for (i in 1: length(levels)){
        #temp_coe_matrix <- matrix(coe[i], nrow = 1, ncol = length(coe[i]) )
        #tempcol <- levels[i]
        coe_matrix[i,][levels[[i]]] <- coe[[i]]
      }
      for(i in 1:nrow(coe_matrix)){
        rownames(coe_matrix)[i] <- paste("Contrast",i,sep = "")
      }
      return(coe_matrix)
}


### Ignore the function below: one level codes, which have been combined into the new function
# coe_matrix_one <- function(col_factor, levels, coe) {
#   ## Build a coefficient matrix for the selected levels
#   select_levels <- as.character(levels)
#   select_coe_matrix <- as.data.frame(coe)
#   colnames(select_coe_matrix) <- select_levels
#   ## Build a coefficient matrix for  all levels of the factor
#   all_levels <- as.character(unique(col_factor))
#   coe_matrix <- as.data.frame(matrix(
#     data = 0,
#     ncol = length(all_levels),
#     nrow = nrow(select_coe_matrix)
#   ))
#   colnames(coe_matrix) <- all_levels
#
#   ## Put the selected coefficient matrix into the full coefficient matrix
#   for (i in 1:ncol(select_coe_matrix)) {
#     tempcol <- colnames(select_coe_matrix)[i]
#     coe_matrix[tempcol] <- select_coe_matrix[tempcol]
#   }
#   return(coe_matrix)
# }

#### Let's try more than one factor, say a 2X2 design
