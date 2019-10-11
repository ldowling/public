#returns values present in a pair of vectors that aren't in both vectors
find_missing <- function(a, b){
  if (length(a)==1){
    if (exists(as.character(a))) {
      a <- get(a)
      }
    }
  if (length(b)==1){
    if (exists(as.character(b))) {
      b <- get(b)
    }
  }
  
  missing_a <- a[which(!a%in%b)]
  missing_b <- b[which(!b%in%a)]
  missing <- list(A_not_B=if(length(missing_a)>0)missing_a,
                  B_not_A=if(length(missing_b)>0)missing_b)
  missing <- missing[-which(unlist(lapply(missing, is.null),use.names=FALSE))]
  return(missing)
}