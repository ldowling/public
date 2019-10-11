inject_bad <- function(x, n_bad=9, type="integer", custom=NULL){
  if ("integer"%in%type) {
    for (n in 1:n_bad) {
      x[sample(1:nrow(x),1),
        sample(1:ncol(x),1)] <- paste(rep(n,5), collapse="")
    }
  }
  if ("character"%in%type) {
    for (n in 1:n_bad) {
      x[sample(1:nrow(x),1),
        sample(1:ncol(x),1)] <- paste0("error_",n)
    }
  }
  if (!is.null(custom)) {
    for (n in 1:n_bad) {
      x[sample(1:nrow(x),1),
        sample(1:ncol(x),1)] <- custom
    }
  }
  return(x)
}