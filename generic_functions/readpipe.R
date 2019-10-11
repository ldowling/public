read.pipe <- function(file){
  read.table(file, sep="|", header=TRUE, stringsAsFactors=FALSE)
}
