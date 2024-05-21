#' Function to check the different issue files
#'
#' @param log_file log file
#' @param output_folder output_folder
#'
f_deal_with_issues <- function(country, output_folder, admins_list){
  admins_list <- f_location_possibility(country)
  files <- list.files(paste(output_folder, '/smart_with_issue', sep=""))
  log_file <- read.csv(paste(output_folder, '/log_file.csv', sep=""))
  for(file in files){
    type_of_issue <- log_file[which(log_file$old_name == file), 'type_of_issue'][1]
    # First issue possible --  Unknow Date
    if(grepl(type_of_issue,'Unknown Date')){
      year <- readline(prompt="Could you enter the year of the SMART Survey: ")
      month <- readline(prompt="Could you enter the month of the SMART Survey: ")
      if(year < as.integer(format(Sys.Date(), "%Y"))){
        # Update log file
        log_file[which(log_file$old_name == file), 'type_of_issue'] <- ""
        log_file[which(log_file$old_name == file), 'issue'] <- FALSE
        log_file[which(log_file$old_name == file), 'year_survey'] <- year
        log_file[which(log_file$old_name == file), 'month_survey'] <- month
        infos <- c(log_file[which(log_file$old_name == file), 'admin_level_survey'][1],
                   log_file[which(log_file$old_name == file), 'admin_name_survey'][1],
                   year, month)
        log_file[which(log_file$old_name == file), 'new_name'] <- paste(stringr::str_to_lower(country), '_',
                                                                       infos[1], '_',
                                                                       gsub("\u00a0", "", infos[2]), '_',
                                                                       infos[3],
                                                                       '_', infos[4], '_', 0, sep='')
        # Save the file and its content
        file_path <- paste(smart_folder, file, sep="")
        # Read the .as content
        smart_content <- readLines(file_path)
        # Extract nutrution, mortality and clusters infos
        nutrition_data <- f_extract_nutrition_data(smart_content)
        mortality_data <- f_extract_mortality_data(smart_content, log_file[which(log_file$old_name == file), 'type_survey'][1])
        clusters_data <- f_extract_clusters_data(smart_content)
        # Save the smart data
        folder_name <- paste(output_folder, '/', log_file[which(log_file$old_name == file), 'new_name'][1], sep="")
        check_and_generate_folder(folder_name)
        file.copy(paste(smart_folder, file, sep = ""),
                  folder_name)
        write.table(nutrition_data, file = paste(folder_name, '/', log_file[which(log_file$old_name == file), 'new_name'][1], 'nutrition_data.csv', sep=""),
                    append = FALSE, sep=',', col.names=TRUE, row.names = FALSE)
        write.table(mortality_data, file = paste(folder_name,'/', log_file[which(log_file$old_name == file), 'new_name'][1], 'mortality_data.csv', sep=""),
                    append = FALSE, sep=',', col.names=TRUE, row.names = FALSE)
        write.table(clusters_data, file = paste(folder_name, '/', log_file[which(log_file$old_name == file), 'new_name'][1], 'clusters_data.csv', sep=""),
                    append = FALSE, sep=',', col.names=TRUE, row.names = FALSE)
        unlink(paste(paste(output_folder, '/smart_with_issue/', sep=""), file, sep=""))
      }
    }else if(grepl(type_of_issue,'Unknown Admin')){
      print(paste('The name of the SMART Survey is: ', file, sep=""))
      admin_name <- readline(prompt="Probably the name is writing with some issue, could you provide any information of the admin level of this SMART ?")
      admin_name <- tolower(admin_name)
      # First be sure we are comparing everything without punctuations and spaces
      admin_list <- admins_list |>
        dplyr::mutate(eventual_name_low = tolower(gsub("[[:punct:][:blank:]]+", "", eventual_name)),
                      right_name_low = tolower(gsub("[[:punct:][:blank:]]+", "", right_name)))
      if(admin_name %in% admin_list$right_name_low){
        level <- admin_list[which(admin_name == admin_list$right_name_low), 'level'][1]
        log_file[which(log_file$old_name == file), 'type_of_issue'] <- ""
        log_file[which(log_file$old_name == file), 'issue'] <- FALSE
        log_file[which(log_file$old_name == file), 'admin_level_survey'] <- level
        log_file[which(log_file$old_name == file), 'admin_name_survey'] <- admin_name
        infos <- c(log_file[which(log_file$old_name == file), 'admin_level_survey'][1],
                   log_file[which(log_file$old_name == file), 'admin_name_survey'][1],
                   log_file[which(log_file$old_name == file), 'year_survey'][1],
                   log_file[which(log_file$old_name == file), 'month_survey'][1])
        log_file[which(log_file$old_name == file), 'new_name'] <- paste(stringr::str_to_lower(country), '_',
                                                                        infos[1], '_',
                                                                        gsub("\u00a0", "", infos[2]), '_',
                                                                        infos[3],
                                                                        '_', infos[4], '_', 0, sep='')
        # Save the file and its content
        file_path <- paste(smart_folder, file, sep="")
        # Read the .as content
        smart_content <- readLines(file_path)
        # Extract nutrution, mortality and clusters infos
        nutrition_data <- f_extract_nutrition_data(smart_content)
        mortality_data <- f_extract_mortality_data(smart_content, log_file[which(log_file$old_name == file), 'type_survey'][1])
        clusters_data <- f_extract_clusters_data(smart_content)
        # Save the smart data
        folder_name <- paste(output_folder, '/', log_file[which(log_file$old_name == file), 'new_name'][1], sep="")
        check_and_generate_folder(folder_name)
        file.copy(paste(smart_folder, file, sep = ""),
                  folder_name)
        write.table(nutrition_data, file = paste(folder_name, '/', log_file[which(log_file$old_name == file), 'new_name'][1], 'nutrition_data.csv', sep=""),
                    append = FALSE, sep=',', col.names=TRUE, row.names = FALSE)
        write.table(mortality_data, file = paste(folder_name,'/', log_file[which(log_file$old_name == file), 'new_name'][1], 'mortality_data.csv', sep=""),
                    append = FALSE, sep=',', col.names=TRUE, row.names = FALSE)
        write.table(clusters_data, file = paste(folder_name, '/', log_file[which(log_file$old_name == file), 'new_name'][1], 'clusters_data.csv', sep=""),
                    append = FALSE, sep=',', col.names=TRUE, row.names = FALSE)
        unlink(paste(paste(output_folder, '/smart_with_issue/', sep=""), file, sep=""))
      }
    }

  }
}
