#### Sampling method. #### 

rw.sample <- function(df = NULL, n = NULL) {
  a <- df$id[sample(nrow(df), 1)]
  x <- as.numeric(a)
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


rw.id <- function (df = NULL) { 
  a <- data.frame(1)
  colnames(a)[1] <- "id"
  for (i in 1:nrow(df)) { 
    x <- i + 1
    a <- rbind(a, x)
  }
  return(a)
}