
masivedataloader <- function (directory)
{
  # Retrieve all the files in the directory
  dirpath <- paste (getwd(), "/", directory, "/", sep = "")
  filenames = list.files(path = dirpath, full.names = TRUE)
  
  do.call("rbind",lapply(filenames, read.csv))
}


pollutantmean <- function (directory, pollutant, id = 1:332)
{
  
  mydata <- masivedataloader(directory)
  
  mean(mydata[mydata[,"ID"] >= min(id) & mydata[,"ID"] <= max(id) , pollutant], na.rm= TRUE)
  
}