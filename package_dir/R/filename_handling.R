drop_suffix <- function(path=NULL, sep='.') {
  if (is.null(path))
    stop("File path must be provided.")
  dir_name <- dirname(path)
  file_name <- basename(path)
  file_stub <- strsplit(x=file_name, split=sep, fixed=TRUE)
  file_stub <- sapply(file_stub,`[`,1)
  return(file_stub)
}





