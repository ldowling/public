read_tableau <- function(file){
  #a custom read.table() col w/ specifications for TMW's Tableau reports format
  tab <- read.table(file=file, sep="\t",skipNul=TRUE, header=TRUE,
             fileEncoding="UCS-2LE",
             stringsAsFactors=FALSE,fill=TRUE, quote="\"")
  #checks for an all-NA last column with name `X` and removes it if present
  if(any(grepl("^X$",colnames(tab))) ){
    if(all(is.na(tab$X))){
      tab <- tab[-grep("^X$",colnames(tab))]
    }
  }
}