getFile <- function (directory, id)
{
  
  filen <- paste (getwd(), "/", directory, "/", sprintf("%03d",as.numeric(id)), ".csv", sep="")
  dataf <- read.csv(filen)
  dataf
}

complete <- function (directory, id = 1:332)
{
  nobscount <- numeric(0)
  
  for (i in id)
  {
    ff <- getFile(directory, i)
    nobscount <- c(nobscount, sum(complete.cases(ff)))
  }
  data.frame(id = id, nobs = nobscount)
}