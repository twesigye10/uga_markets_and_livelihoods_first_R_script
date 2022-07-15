## create sub directories for the project

sub_directories <- list("outputs", "inputs", "R", "support_files")

for(x in sub_directories){
  check_folder <- dir.exists(paste0(getwd(),"/",x))
  
  if (check_folder == FALSE) {
    dir.create(paste0("./",x), showWarnings = FALSE)
  }
  
}
