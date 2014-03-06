############### Sampling method. ################## 
# Systematic sampling method on a data.frame.     # 
# n (sample size) has to provided as a parameter. #
#                                                 #
# Author: Luis Capelo | capelo@un.org             #
#                                                 #
###################################################

rw.sample <- function(df = NULL, n = NULL) {
  # Creating an `id` column. 
  id.creator <- function(df = NULL) {
    a <- data.frame(1)
    for (i in 1:nrow(df)) { 
      x <- i
      a <- rbind(a, x)
    }
    a <- data.frame(a[1:nrow(a)-1,])
    df <- cbind(a, df)
    colnames(df)[1] <- "id"
    return(df)
  }
  df <- id.creator(df = df)
  
  # Function for sampling. 
  sampler <- function(df = NULL) {
    a <- data.frame()
    x <- as.numeric(df$id[sample(nrow(df), 1)])
    for (i in 1:n) {
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
  a <- sampler(df = df)
  return(a)
}