getFileData <- function(filen)
{
  fdata <- read.csv(filen)
  fdata
}

corr <- function(directory, threshold = 0)
{
  dirpath <- paste (getwd(), "/", directory, "/", sep = "")
  filenames = list.files(path = dirpath, full.names = TRUE)
  
  corrval <- c()
  
  for (i in 1:length(filenames))
  {
    
    data <- getFileData(filenames[i])
    aa <- data[complete.cases(data),]
    if (nrow(aa) > threshold)
    {
      corrval <- c(corrval, cor(aa$sulfate, aa$nitrate))
    }
    
  }
  
  corrval
}