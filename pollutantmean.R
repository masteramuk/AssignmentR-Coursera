pollutantmean <- function(directory, pollutant = "nitrate", id = 1: 332){
  ##reading the files from given directory
  folder <- directory
  
  ##get all files in the list as a list
  file_list <- list.files(path=folder, pattern="*.csv")
  
  ##get list of files based on the given id
  file_list2 <- cleanlist(folder, id, file_list)
  
  ##read all data in the second list and load into a data frame
  data <- 
    do.call("rbind", 
            lapply(file_list2, 
                   function(x){
                     read.csv(x,stringsAsFactors = FALSE)
                     }
                   )
            )
  
  ##get the data without NA
  clean_data <-na.omit(data[pollutant])
  
  ##return mean data
  mean_data <- mean(clean_data[,1])
  return(mean_data)
}

##we want to get only specifics files
cleanlist <- function(folder, id, file_list){
  file_list2 <- c()
  for (i in id)
    file_list2 <- c(file_list2, paste(folder,"/",file_list[i],sep=''))
  file_list2
}