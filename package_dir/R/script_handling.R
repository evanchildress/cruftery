source_notify <- function(path) {
  pad <- max(c((40 - nchar(path)),0))
  msg <- paste(
    "Running .R file at: '", path, "'",
    paste(rep('.',pad),sep='', collapse=''),
  sep='', collapse='')
  cat(msg)
  source(path)
  msg <- paste("[DONE]\n", sep='', collapse='')
  cat(msg)
}



