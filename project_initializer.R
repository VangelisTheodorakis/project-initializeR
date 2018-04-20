# Author: Vangelis Theodorakis
# Email:  van.theodorakis@gmail.com

# For testing purposes
testing <- FALSE

check.packages <- function(packages) {
  # Checks to see if desired packages are installed and if they are not, it installs them.
  # Args:
  #   package: A vector of the desired packages
  # Returns:
  #   TRUE by default
  #
  new.packages <- packages[ !(packages %in% installed.packages()[, "Package"])]
  if ( length(new.packages) )
    install.packages(new.packages, dependencies = TRUE)
  return (TRUE)
}

# Project Packages to be installed
packages <- c("here", "stringr", "rmarkdown")

# Install missing packages
check.packages(packages)

# Load all packages
lapply(packages, require, character.only = TRUE)

# The direcories to be created 
dirs.to.create <- c("src",
                    "data-input",
                    "data-output",
                    "info",
                    "reports")

# For testing purposes
if (testing == TRUE) {
  unlink(dirs.to.create, recursive = TRUE)
}

handle.dir.warning <- function(warning) {
  # Prints a warning message of the warning raised except when the file already exists.
  # In this case it prints a custom message
  # Args:
  #   warning: the warning message   
  #
  # Returns:
  #   FALSE by default
  #
  message <- warning[1]
  if ( grep('already exists', message) == TRUE ) {
    stringr.object <-  str_match(message, "\'(.*)\'")
    folder <-stringr.object[, 2]
    message(
      "Folder ", folder, " already exists under ", here(),
      " and was not created.")
  }
  else {
    message(warning)
  }
  return (FALSE)
}

handle.dir.error <- function(error) {
  # # Prints an error message of error raised during directory creation.
  # Args:
  #   error: the error message
  #
  # Returns:
  #   FALSE by default
  #
  cat(error, "\n")
  return (FALSE)  
}

handle.dir.success <- function(dir) {
  # Prints a message of success in stdout in case of successful 
  # directory creation.
  # Args:
  #   dir: the directory name
  #
  # Returns:
  #   TRUE on successful print
  #
  cat("Folder", dir, "was created under",
      paste(here(), ".\n", sep = ""))
  
  return (TRUE)
}

try.to.make.dir <- function(dir, handle.dir.warning, handle.dir.error, handle.dir.success) {
  # Tries to make a directory. If not successful it handles the raised error/warning.
  # Args:
  #   dir:                the directory name
  #   handle.dir.warning: a function to be called in case of warning 
  #   handle.dir.error:   a function to be called in case of error
  #   handle.dir.success: a function to be called in case of success
  #
  # Returns:
  #   TRUE or FALSE based on the fact that the direcory was created 
  #   or not
  #
  success <- tryCatch( {
                        dir.create(dir)
                        handle.dir.success(dir)
                        return (TRUE)
                      }, 
                      warning = handle.dir.warning,
                      error   = handle.dir.error)
  
  return (success)
}

# Create all the directories
directory.creation.status <- invisible( 
  lapply( dirs.to.create,
          try.to.make.dir,
          handle.dir.warning,
          handle.dir.error,
          handle.dir.success))

# Just for view purposes separator
cat("===============================================================\n")

# Turn directory.creation.status from list to vector
directory.creation.status <- unlist(directory.creation.status)

# The R scripts to be created
files.to.create <- c("main.R",
                     "functions.R",
                     "explore.R",
                     "initialize.R",
                     "load_data.R",
                     "pull_data_from_DB.R",
                     "build.R",
                     "analyze.R",
                     "build_ppt.R",
                     "prepare_markdown.R",
                     "prepare_shiny.R",
                     "markdown_report.Rmd",
                     "shiny_report.Rmd")

try.to.make.files <- function(file.name, path){
  # Tries to make a directory. If not successful it ( DOES NOT !) 
  # handles the raised error/warning ( YET !).
  # Args:
  #   file.name:  the name of the file to be created
  #   path:       the path to the src folder
  #
  # Returns:
  #   TRUE  if the file creation was successful or
  #   FALSE if is was not
  #
  new.file.name <- file.path(path, file.name)
  
  # For debugging purposes
  # unlink(new.file.name)
  
  if( file.exists(new.file.name) == TRUE) {
    success <- FALSE
    message(
      "File ", file.name, " already exists under ",path,
      " and was not created.")
  }
  else {
    success <- file.create(new.file.name)
    cat(
      "File", file.name, "was created under",
      paste(path, ".\n", sep = ""))
  }
  
  return (success)
}

# Create all the files
file.creation.status <- invisible( 
  lapply( files.to.create, try.to.make.files, here("src")) )

# Turn file.creation.status from list to vector
file.creation.status <- unlist(file.creation.status)

# Just for view purposes separator
cat("===============================================================\n")

# Path to file descriptions.csv
# Each row of the file contains  the file name [main.R, functions.R, etc]
# and the description of what each file should contain
#
file_descriptions.path <- file.path(here(), "file_descriptions.csv")

# Read the file descriptions csv
file_descriptions <- read.csv(file_descriptions.path)

fill.r.scripts <- function(file.line, path) {
  # Fills a script with the appropriate description
  # Args
  #   file.line:  the inline description of the file as contained
  #               in the file_descriptions.csv
  #   path:       the path to the src folder
  #
  # Returns:
  #   TRUE on successfull write operation
  #
  file.name <- file.line[[1]]
  file.to.write.path <- file.path(path, file.name)
  file.to.write.info <- file.info(file.to.write.path)
  file.to.write.size <- file.to.write.info$size
  
  if ( file.to.write.size == 0 ) {
    description <- str_wrap(file.line[[2]], width = 80)
    description <- str_split(description, "\n")
    
    description <- lapply(description, 
                          function(line) paste("# ", line, sep = ""))
    
    description <- unlist(description)
    
    write(description, 
          file = file.to.write.path,
          sep = "\n")
    
    return (TRUE)  
  }
  else {
    # message("File ", file.name , " under ", path,
    #         " is not empty and therefore was not overwritten.")
    return (FALSE)
  }
}

# Fill the files with the appropriate descriptions
file.filling.status <- invisible( 
  apply(file_descriptions, 1, fill.r.scripts, here("src")))

project.creation.status <-  function(statuses) {
  overal.status <- Reduce("&", statuses)
  if( overal.status == TRUE) {
    cat("Project Tree creation was done!\n")
  }
  else {
    cat("Project Tree creation was not done...\n")
    
  }
}

# Print the separator only if file filling has happened
if ( Reduce("&", file.filling.status) == TRUE ) {
  cat("===============================================================\n")
}

# Inform about the project creation status
project.creation.status(
                          c(directory.creation.status,
                          file.creation.status,
                          file.filling.status))

