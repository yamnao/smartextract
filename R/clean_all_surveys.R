#' Function to read all the SMART Survey & save the different structure in the right folder & generate issue folder
#'
#' @param smart_folder Folder where the .as file are saved
#' @param output_folder Folder where the results need to be saved
#' @export
f_sort_smart <- function(smart_folder, output_folder, country){
  admins_list <- f_location_possibility(country)
  # Generate and creat issue folder
  create_issue_folder(output_folder)
  # Generate and create a log file  for the raw folder
  log_file <- generate_log_file(output_folder)
  # List SMART Survey needed to be updated
  files <- list.files(smart_folder, pattern='\\.as$')
  # Loop over them
  for(file in files){
    # Extract the path of the file & its name
    file_path <- paste(smart_folder, file, sep="")
    name_file <- file
    # Read the .as content
    smart_content <- readLines(file_path)
    # Extract info in the .as content
    infos <- f_extract_info_smart(name_file, smart_content, admins_list)
    # Generate new name
    new_name <- f_generate_new_name(log_file, country, infos)
    # Extract nutrution, mortality and clusters infos
    nutrition_data <- f_extract_nutrition_data(smart_content)
    mortality_data <- f_extract_mortality_data(smart_content, infos[6])
    clusters_data <- f_extract_clusters_data(smart_content)
    # Update the logfile
    log_file <- f_update_log_file(log_file, name_file, new_name, infos, nutrition_data, mortality_data, clusters_data)
    # Save the smart data
    f_save_smart(smart_folder, output_folder, file, new_name,
                 infos, nutrition_data, mortality_data, clusters_data)
  }
  write.table(log_file, file = paste(output_folder, '/log_file.csv', sep=""), append = FALSE,
              sep=',', col.names=TRUE, row.names = FALSE)
}
