corr <- function(directory, threshold = 0){
  ##reading the files from given directory
  folder <- directory
  
  ##get all files in the list as a list
  file_list <- list.files(path=folder, pattern="*.csv")
  
  ##format all file list name
  file_list2 <- getalllist(folder, file_list)
  
  cor_result <- numeric()
  ##for all files in file_list2
  for(i in 1:length(file_list2)){
    ##read the files and load into data frame
    data <- read.csv(file_list2[i],stringsAsFactors = FALSE)
    ##get the list of complete case
    data_ok <- complete.cases(data)
    nobs <- sum(data_ok)
    ##get correlation only if number of complete case more than the threshold
    if (nobs > threshold){
      cor_result <- c(cor_result,cor(data['nitrate'], data['sulfate'], use="complete.obs"))
      ##cor_result <- rbind(cor_result, data.frame(cor(data_ok['nitrate'], data_ok['sulfate'], use="complete.obs")))
    }
  }
  return(cor_result)
}

##we want to get only specifics files
getalllist <- function(folder, file_list = ""){
  file_list2 <- c()
  for (i in 1:length(file_list))
    file_list2 <- c(file_list2, paste(folder,"/",file_list[i],sep=''))
  file_list2
}