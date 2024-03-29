#Quick way to access files/folders when GLS data is housed following BAStag recommendations eg. colony name > GLS tag name > raw data/processing code

geoPath <- function(path1=path1, path2=path2, type=c("file", "folder")){
  folder1 <- list.files(path=path1)
  folder1 <- folder1[grep(".xlsx", folder1, invert=T)]
  folder1 <- folder1[grep("makefile", folder1, invert=T)]
  
  if(!is.na(path2)){
  folder2 <- list.files(path=path2)
  folder2 <- folder2[grep(".xlsx", folder2, invert=T)]
  folder2 <- folder2[grep("makefile", folder2, invert=T)]
  path <- c(paste0(path1, "/", folder1), paste0(path2, "/", folder2))
  file <- c(folder1, folder2)
  } else {
    path <- paste0(path1, "/", folder1)
    file <- folder1
  }
  if(type=="folder")return(path)
  if(type=="file")return(file)
}
