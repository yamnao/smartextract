#' Function to read all the SMART Survey & save the different structure in the right folder & generate issue folder
#'
#' @param smart_folder Folder where the .as file are saved
#' @param output_folder Folder where the results need to be saved
#' @export
f_sort_smart <- function(smart_folder, output_folder, country,
                         country_data){
  if(length(nrow(country_data)) != 0){
    admins_list <- country_data
  }else if(is.na(country_data)){
    admins_list <- f_location_possibility(country)
  }else{
    admins_list <- NA
  }

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
    print(name_file)
    # Read the .as content
    smart_content <- readLines(file_path)
    # Extract info in the .as content
    infos <- f_extract_info_smart(name_file, smart_content, admins_list)
    # Generate new name
    new_name <- f_generate_new_name(log_file, country, infos)
    new_name <- gsub(" ", "", new_name)
    new_name <- substr(new_name, start=1, stop=30)
    # Extract nutrution, mortality and clusters infos
    nutrition_data <- f_extract_nutrition_data(smart_content)
    mortality_data <- f_extract_mortality_data(smart_content, infos[9])
    clusters_data <- f_extract_clusters_data(smart_content)
    # Extract metadata
    metadata_data <- f_extract_metadata_data(smart_content)
    # Update the logfile
    log_file <- f_update_log_file(log_file, name_file, new_name, infos,
                                  nutrition_data, mortality_data, clusters_data,
                                  metadata_data)
    # Save the smart data
    f_save_smart(smart_folder, output_folder, file, new_name,
                 infos, nutrition_data, mortality_data, clusters_data)
  }
  write.table(log_file, file = paste(output_folder, '/metadata.csv', sep=""), append = FALSE,
              sep=',', col.names=TRUE, row.names = FALSE)
}
