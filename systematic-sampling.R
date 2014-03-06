############### Sampling method. ##################
# Systematic sampling method on a data.frame.     #
# n (sample size) has to provided as a parameter. #
#                                                 #
# Author: Luis Capelo | capelo@un.org             #
#                                                 #
###################################################

rw.sample <- function(df = NULL, n = NULL) {

N <- nrow(df)
int <- (N / n)

  # Creating an `id` column.
  id.creator <- function(df = NULL) {
    print("Creatign 'id' columns -- using a highly inefficient method.")

    # Create progress bar.
    pb <- txtProgressBar(min = 0, max = N, style = 3)

    a <- data.frame(1)
    for (i in 1:nrow(df)) {
      Sys.sleep(0.1)

      # Update progress bar.
      setTxtProgressBar(pb, i)

      x <- i
      a <- rbind(a, x)
    }
    close(pb)
    a <- data.frame(a[1:nrow(a) - 1,])
    df <- cbind(a, df)
    colnames(df)[1] <- "id"
    return(df)
  }
  df <- id.creator(df = df)

  # Function for sampling.
  sampler <- function(df = NULL) {
    print("Sampling.")

    # Create progress bar.
    pb <- txtProgressBar(min = 0, max = N, style = 3)

    a <- data.frame()
    x <- as.numeric(df$id[sample(nrow(df), 1)])
    for (i in 1:n) {
      Sys.sleep(0.1)

      # Update progress bar.
      setTxtProgressBar(pb, i)

      if (((x + int) > nrow(df)) == FALSE) {
        x <- x + int
      }
      if (((x + int) > nrow(df)) == TRUE) {
        x <- ((x + int) - nrow(df))
      }
      a <- rbind(a, x)
    }
    close(pb)
    a <- data.frame(a)
    colnames(a)[1] <- "id"
    a <- merge(df, a)
    return(a)
  }
  a <- sampler(df = df)
  return(a)
}