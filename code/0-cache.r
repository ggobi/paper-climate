"%<-cache%" <- function(x, value) {
  name_expr <- substitute(x)
  if (!is.name(name_expr)) {
    stop("First argument should be unquoted name")
  } else {
    name <- as.character(name_expr)
  }
  path <- paste(name, ".rds", sep = "")
  
  if (file.exists(path)) {
    result <- readRDS(path)
  } else {
    result <- force(value)
    saveRDS(result, path)
  }
  assign(name, result, parent.frame())
}