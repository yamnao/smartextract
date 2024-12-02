#' Function to check the different issue files
#'
#' @param log_file log file
#' @param output_folder output_folder
#'
#' @export
f_deal_issues <- function(country, file, admin_name, log_file,
                          smart_folder, output_dir, country_data){
  # First select the right country
  # First option we are using a known country
  if(length(nrow(country_data)) == 0){
    admin_list <- f_location_possibility(country) |>
      dplyr::mutate(eventual_name_low = tolower(gsub("[[:punct:][:blank:]]+", "", eventual_name)),
                    right_name_low = tolower(gsub("[[:punct:][:blank:]]+", "", right_name)))
  }else{
    admin_list <- country_data |>
      dplyr::mutate(eventual_name_low = tolower(gsub("[[:punct:][:blank:]]+", "", eventual_name)),
                    right_name_low = tolower(gsub("[[:punct:][:blank:]]+", "", right_name)))
  }

  # And check if the name is corresponding
  if(admin_name %in% admin_list$right_name_low){
    level <- admin_list[which(admin_name == admin_list$right_name_low), 'level'][1]
    log_file[which(log_file$old_name == file), 'type_of_issue'] <- ""
    log_file[which(log_file$old_name == file), 'issue'] <- FALSE
    log_file[which(log_file$old_name == file), 'admin_level_survey'] <- level
    log_file[which(log_file$old_name == file), 'admin_name_survey'] <- admin_name
    infos <- c(log_file[which(log_file$old_name == file), 'admin_level_survey'][1],
               log_file[which(log_file$old_name == file), 'admin_name_survey'][1],
               log_file[which(log_file$old_name == file), 'min_year_survey'][1],
               log_file[which(log_file$old_name == file), 'min_month_survey'][1])
    name <- paste(stringr::str_to_lower(country), '_',
                  infos[1], '_',
                  gsub("\u00a0", "", infos[2]), '_',
                  infos[3],
                  '_', infos[4], sep='')
    pattern_found <- log_file[['new_name']][grepl(name, log_file[['new_name']])]
    if(length(pattern_found) == 0){
      name <- paste(stringr::str_to_lower(country), '_',
                    infos[2], '_',
                    gsub("\u00a0", "", infos[1]), '_',
                    infos[3],
                    '_', infos[4], '_', 0, sep='')
    }else{
      number <- max(as.integer(sapply(strsplit(pattern_found, "_"),"[[", 6)))
      name <- paste(stringr::str_to_lower(country), '_',
                    infos[2], '_',
                    gsub("\u00a0", "", infos[1]), '_',
                    infos[3],
                    '_', infos[4], '_', number+1, sep='')
    }

    log_file[which(log_file$old_name == file), 'new_name'] <- name
    # Save the file and its content
    file_path <- paste(smart_folder, file, sep="")
    # Read the .as content
    smart_content <- readLines(file_path)
    # Extract nutrution, mortality and clusters infos
    nutrition_data <- f_extract_nutrition_data(smart_content)
    mortality_data <- f_extract_mortality_data(smart_content, log_file[which(log_file$old_name == file), 'type_survey'][1])
    clusters_data <- f_extract_clusters_data(smart_content)
    # Save the smart data
    folder_name <- paste(output_dir, '/', log_file[which(log_file$old_name == file), 'new_name'][1], sep="")
    smartextract::check_and_generate_folder(folder_name)
    file.copy(paste(smart_folder, file, sep = ""),
              folder_name)
    write.table(nutrition_data, file = paste(folder_name, '/', log_file[which(log_file$old_name == file), 'new_name'][1], 'nutrition_data.csv', sep=""),
                append = FALSE, sep=',', col.names=TRUE, row.names = FALSE)
    write.table(mortality_data, file = paste(folder_name,'/', log_file[which(log_file$old_name == file), 'new_name'][1], 'mortality_data.csv', sep=""),
                append = FALSE, sep=',', col.names=TRUE, row.names = FALSE)
    write.table(clusters_data, file = paste(folder_name, '/', log_file[which(log_file$old_name == file), 'new_name'][1], 'clusters_data.csv', sep=""),
                append = FALSE, sep=',', col.names=TRUE, row.names = FALSE)
    unlink(paste(paste(output_dir, '/smart_with_issue/', sep=""), file, sep=""))
    write.table(log_file, file = paste(output_dir, 'metadata.csv', sep=""), append=FALSE,
                sep=',', col.names=TRUE, row.names = FALSE)


  }
}

