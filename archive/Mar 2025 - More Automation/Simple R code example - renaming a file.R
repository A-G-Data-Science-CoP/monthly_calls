# Simple example of how to rename a file in R using base R code

# Set directory path where your file is located.
directory <- "C:/Users/MadisonMerzke/A-G Associates/Data Analytics Resources - R/R Best Practices/FEVS examples"

# Simple example renaming file /rename your own file
old_filename <- 'Ideas for FEVS analyses.docx' # Define then new name you want the file to have / Replace with the desired file name
new_filename <- 'Ideas for FEVS analyses_renamed.docx' # Define the current name of the file you want to rename / Replace with the actual file name

if (
  file.rename(
    file.path(directory, old_filename),
    file.path(directory, new_filename)
  )
) {
  cat("File renamed successfully!\n")
} else {
  cat("File renaming failed.\n")
}

# If you needed to rename a bunch of files, could do this iteratively by writing a function and then running it on a list of files using map() from purrr
