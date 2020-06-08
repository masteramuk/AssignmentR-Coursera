complete <- function(directory, id = 1:332){
  ##reading the files from given directory
  folder <- directory
  
  ##get all files in the list as a list
  file_list <- list.files(path=folder, pattern="*.csv")
  
  ##get list of files based on the given id
  file_list2 <- cleanlist(folder, id, file_list)
  
  ##print(file_list2)
  ##get complete case by id and store in a vector
  df <- c()
  for(i in 1:length(file_list2)){
    ##read the files and load into data frame
    data <- read.csv(file_list2[i],stringsAsFactors = FALSE)
    ##print(data)
    ##get id
    idx <- data[1,"ID"]
    ##get complete case
    data_ok <- complete.cases(data)
    ##load into data frame and appebd the existing data frame
    ##print(paste(idx, " - ", sum(data_ok)))
    nobs <- sum(data_ok)
    df <- rbind(df, data.frame(id = idx, nobs))
  }
  return(df)
}

##we want to get only specifics files
cleanlist <- function(folder, id, file_list){
  file_list2 <- c()
  for (i in id)
    file_list2 <- c(file_list2, paste(folder,"/",file_list[i],sep=''))
  file_list2
}