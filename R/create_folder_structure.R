## create sub directories for the project

create_folder_structure <- function(input_sub_folders = c("outputs", "inputs", "R", "support_files")) {
  sub_directories <- input_sub_folders
  # iterate the subfolders and create them
  for(x in sub_directories){
    # check folder existence
    check_folder <- dir.exists(paste0(getwd(),"/",x))
    
    if (check_folder == FALSE) {
      dir.create(paste0("./",x), showWarnings = FALSE)
      message(paste("Folder created :", x))
    } else{
      warning(paste("Folder already exists :", x))
    }
  }
}