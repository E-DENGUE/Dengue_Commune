move.file <- function(from=filenames_full_path) {
  filenames_new_path <- gsub('Staging','Archive',from)
  
  todir <- dirname(filenames_new_path)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = filenames_new_path)
}
