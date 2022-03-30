#### A very first trial for the package, let's say a contrast for a 3-group variable


#### In order to make your hypotheses to be correctly converted to a coefficient matrix, you need to follow the rules below:
#### levels: a character vector indicating which levels of the factor your hypotheses are related to
#### coe: a numeric matrix indicating what the coefficients for each of your hypotheses, corresponding to the character vector in levels
#### col_factor:

coe_matrix_one <- function(col_factor, levels, coe) {
  ## Build a coefficient matrix for the selected levels
  select_levels <- as.character(levels)
  select_coe_matrix <- as.data.frame(coe)
  colnames(select_coe_matrix) <- select_levels
  ## Build a coefficient matrix for  all levels of the factor
  all_levels <- as.character(unique(col_factor))
  coe_matrix <- as.data.frame(matrix(
    data = 0,
    ncol = length(all_levels),
    nrow = nrow(select_coe_matrix)
  ))
  colnames(coe_matrix) <- all_levels

  ## Put the selected coefficient matrix into the full coefficient matrix
  for (i in 1:ncol(select_coe_matrix)) {
    tempcol <- colnames(select_coe_matrix)[i]
    coe_matrix[tempcol] <- select_coe_matrix[tempcol]
  }
  return(coe_matrix)
}

#### Let's try more than one factor, say a 2X2 design

coe_matrix_multi <- function(col_factor, levels, coe){
      numFactor <- ncol(col_factor)
      if(numFactor==1){
            coe_matrix_one(col_factor, levels, coe)
      }
      if(numFactor>1){
            for (i in 1:numFactor) {
                  for (j in 1:length(unique(col_factor[i]))) {

                  }
            }
      }
      combn()
}