pml_write_files <- function(x, path) {
  n <- length(x)
  for(i in 1:n){
    filename <- paste0("problem_id_",i,".txt")
    write.table(x[i],file = paste0(path, filename),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }
}