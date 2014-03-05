#### Sampling method. #### 
# Systematic sampling method on a data.frame.

rw.sample <- function(df = NULL, n = NULL) {
  
  # Creating an `id` column. 
  a <- data.frame(1)
  colnames(a)[1] <- "id"
  
  # Adding each number to a column.
  for (i in 1:nrow(df)) { 
    x <- i
    a <- rbind(a, x)
  }
  
  # Joining the resulting column with the df provided.
  df <- cbind(a, df)
  
  # Function for sampling. 
  x <- as.numeric(df$id[sample(nrow(df), 1)])
  for (i in 2:n) {
    if (((x + n) > nrow(df)) == FALSE) {
      x <- x + n
    }
    if (((x + n) > nrow(df)) == TRUE) {
      x <- ((x + n) - nrow(df))
    }
    a <- rbind(a, x)
  }
  a <- data.frame(a)
  colnames(a)[1] <- "id"
  a <- merge(df, a)
  return(a)
}

