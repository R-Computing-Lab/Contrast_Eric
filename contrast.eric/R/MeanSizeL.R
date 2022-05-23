### A function to calculate cell means for all combinations of factor levels
cellMean <- function(colFactor, colDV){
      ## use aggregate() to calculate the cell means
      l.factor <- list()
      for (j in 1:ncol(colFactor)){
            l.factor[[j]] <- colFactor[,j]
            names(l.factor)[j] <- colnames(colFactor)[j]
      }
      means <- aggregate(x=list(Y=colDV),
                         by=l.factor,
                         FUN=mean)
      
      ## All the combinations of the factors
      numFactor <- ifelse(is.null(ncol(colFactor)), 1, ncol(colFactor))
      l <- list()
      for (i in 1:numFactor) {
            l[[i]] <- as.character(unique(colFactor[,i]))
      }
      allcomb <- expand.grid(l)
      
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
      
      # turn the means into a proper format
      all_levels_mean <- character()
      for (i in 1: nrow(means)){
            all_levels_mean[i] <- do.call(paste, c(means[i,1:ncol(colFactor)], sep = "<>"))
      }
      means_value <- data.frame(level = all_levels_mean,
                                value = means[,ncol(means)])
      
      mean_matrix <-as.data.frame(matrix(
            data = 0,
            ncol = length(all_levels),
            nrow = 1
      ))
      colnames(mean_matrix) <- all_levels
      
      # put means into all the combinations(those with)
      for (j in 1:ncol(mean_matrix)) {
            if(colnames(mean_matrix)[j] %in% all_levels_mean){
                  mean_matrix[1,j] <- means_value[match(colnames(mean_matrix)[j],means_value[,1]),2]
            }else{
                  mean_matrix[1,j] <- as.numeric(NA)
            }
      }
      rownames(mean_matrix)[1]<-"Mean"
      return(mean_matrix)
}
### A function to collect the sample size for each cell
cellSize <- function(colFactor, colDV){
      ## use aggregate() to calculate the cell means
      l.factor <- list()
      for (j in 1:ncol(colFactor)){
            l.factor[[j]] <- colFactor[,j]
            names(l.factor)[j] <- colnames(colFactor)[j]
      }
      sizes <- aggregate(x=list(Y=colDV),
                         by=l.factor,
                         FUN=length)
      
      ## All the combinations of the factors
      numFactor <- ifelse(is.null(ncol(colFactor)), 1, ncol(colFactor))
      l <- list()
      for (i in 1:numFactor) {
            l[[i]] <- as.character(unique(colFactor[,i]))
      }
      allcomb <- expand.grid(l)
      
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
      
      # turn the means into a proper format
      all_levels_sizes <- character()
      for (i in 1: nrow(sizes)){
            all_levels_sizes[i] <- do.call(paste, c(sizes[i,1:ncol(colFactor)], sep = "<>"))
      }
      sizes_value <- data.frame(level = all_levels_sizes,
                                value = sizes[,ncol(sizes)])
      
      sizes_matrix <-as.data.frame(matrix(
            data = 0,
            ncol = length(all_levels),
            nrow = 1
      ))
      colnames(sizes_matrix) <- all_levels
      
      # put means into all the combinations(those with)
      for (j in 1:ncol(sizes_matrix)) {
            if(colnames(sizes_matrix)[j] %in% all_levels_sizes){
                  sizes_matrix[1,j] <- sizes_value[match(colnames(sizes_matrix)[j],sizes_value[,1]),2]
            }else{
                  sizes_matrix[1,j] <- as.numeric(NA)
            }
      }
      rownames(sizes_matrix)[1]<-"N"
      return(sizes_matrix)
}

### Functions to compute weighted coefficient for two rows
ComputeL <- function(row1, row2){
      L <- numeric()
      for (i in 1:length(row1)){
            L[i] <- row1[i]*row2[i]
      }
      L <- as.numeric(L)
      sumL <- sum(L,na.rm = TRUE)
      return(sumL)
}

### Functions to calculate L hat for each contrast

#### 
#### 

l_hat <- function(colFactor,colDV, levels, coe){
      size_matrix <- cellSize(colFactor,colDV)
      #print(size_matrix)
      mean_matrix <- cellMean(colFactor,colDV)
      #print(mean_matrix)
      coe_matrix <- coe_matrix_multi(colFactor,levels, coe)
      #print(coe_matrix)
      all_matrix <- rbind(size_matrix,mean_matrix,coe_matrix)
      #print(all_matrix)
      all_matrix$L <- as.numeric(NA)
      for (i in 3:nrow(all_matrix)){
            all_matrix$L[i] <- ComputeL(all_matrix[2,],all_matrix[i,])
      }
      #print(all_matrix)
      
      return(
            print.data.frame(all_matrix, digits = 2)
      )
}
