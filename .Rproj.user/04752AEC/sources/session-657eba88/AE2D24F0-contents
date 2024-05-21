#' Function to extraction nutrition content of a SMART Survey
#'
#' @param smart_content Content of smart survey
#' @return [data.frame] containing the nutrition information in the SMART
#' @export
f_extract_nutrition_data <- function(smart_content){
  pos <- grep("SURVDATE\t", smart_content, ignore.cas=TRUE, useBytes = TRUE)
  pos2 <- grep("\xa5Planning:", smart_content, ignore.cas=TRUE, useBytes = TRUE)
  data <- smart_content[(pos):(pos2-1)]
  data<- strsplit(data, split = "\t")
  data <- lapply(data, `length<-`, max(lengths(data)))
  data <- as.data.frame(do.call(rbind, data))
  colnames(data) <- data[1,]
  data <- data[-1, ]
  return(data)
}


#' Function to extraction mortality content of a SMART Survey
#'
#' @param smart_content Content of smart survey
#' @return [data.frame] containing the mortality information in the SMART
#' @export
f_extract_mortality_data <- function(smart_content, type_survey){
  if(type_survey == 'individual'){
    pos <- grep("\\Mor_individual:", smart_content, ignore.cas=TRUE, useBytes = TRUE)
    pos2 <- grep("\\Mor_individual_options:", smart_content, ignore.cas=TRUE, useBytes = TRUE)
    data <- smart_content[(pos+1):(pos2-1)]

    data<- strsplit(data, split = "\t")
    data <- lapply(data, `length<-`, max(lengths(data)))
    data <- as.data.frame(do.call(rbind, data))
    data[is.na(data)] <- ""
    colnames(data) <- data[1,]
    data <- data[-1, ]
    data <- data[rowSums(is.na(data)) != ncol(data), ]
    return(data)
  }else{
    pos <- grep("\\Mortality_new:", smart_content, ignore.cas=TRUE, useBytes = TRUE)
    pos2 <- grep("\\Mor_individual:", smart_content, ignore.cas=TRUE, useBytes = TRUE)
    data <- smart_content[(pos+1):(pos2-1)]
    data <- strsplit(data, split = "\t")
    data <- lapply(data, `length<-`, max(lengths(data)))
    data <- as.data.frame(do.call(rbind, data))
    data[is.na(data)] <- ""
    colnames(data) <- c('HH', 'Cluster',	'HH_members_Total', 'HH_members_u5', 'joined_HH_total',
                      'joined_HH_u5', 'left_HH_total', 'left_HH_u5', 'Births', 'Deaths_Total', 'Deaths_u5')
    return(data)
  }
}

#' Function to extraction mortality content of a SMART Survey
#'
#' @param smart_content Content of smart survey
#' @return [data.frame] containing the clusters information in the SMART
#' @export
f_extract_clusters_data <- function(smart_content){
  pos <- grep("Cluster", smart_content, ignore.cas=TRUE)[2]
  pos2 <- grep("\\Training_new:", smart_content, ignore.cas=TRUE, useBytes = TRUE)
  clusters_data <- smart_content[(pos+10):(pos2-1)]
  clusters_data<- strsplit(clusters_data, split = "\t")
  clusters_data <- lapply(clusters_data, `length<-`, max(lengths(clusters_data)))
  clusters_data <- as.data.frame(do.call(rbind, clusters_data))
  if(nrow(clusters_data) < 3){
    return(data.frame())
  }
  colnames(clusters_data) <- c('settlements', 'pop', 'nb_cluster')
  clusters_data <- dplyr::select(clusters_data, c('settlements', 'nb_cluster'))
  clusters_data[which(clusters_data$nb_cluster == ""),]$nb_cluster <- NA
  clusters_data <- tidyr::drop_na(clusters_data)
  clusters_data$settlements <- tolower(clusters_data$settlements)
  clusters_data$settlements <- stringr::str_replace_all(clusters_data$settlements , "[[:punct:]]", "")
  clusters_data$settlements <- stringr::str_replace_all(clusters_data$settlements , "[[:space:]]", "")
  clusters_data$settlements <- stringr::str_replace_all(clusters_data$settlements , "[[:digit:]]+", "")
  return(clusters_data)
}
