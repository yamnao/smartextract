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
  if(length(pos2) == 0){
    return(data.frame())
  }
  clusters_data <- smart_content[(pos+10):(pos2-1)]
  clusters_data<- strsplit(clusters_data, split = "\t")
  clusters_data <- lapply(clusters_data, `length<-`, max(lengths(clusters_data)))
  clusters_data <- as.data.frame(do.call(rbind, clusters_data))
  if(nrow(clusters_data) < 3){
    return(data.frame())
  }
  colnames(clusters_data) <- c('settlements', 'pop', 'nb_cluster')
  clusters_data <- dplyr::select(clusters_data, c('settlements', 'nb_cluster'))
  if(length(clusters_data[which(clusters_data$nb_cluster == ""),]$nb_cluster)!= 0){
    clusters_data[which(clusters_data$nb_cluster == ""),]$nb_cluster <- NA
  }
  clusters_data <- tidyr::drop_na(clusters_data)
  clusters_data$settlements <- tolower(clusters_data$settlements)
  clusters_data$settlements <- stringr::str_replace_all(clusters_data$settlements , "[[:punct:]]", "")
  clusters_data$settlements <- stringr::str_replace_all(clusters_data$settlements , "[[:space:]]", "")
  clusters_data$settlements <- stringr::str_replace_all(clusters_data$settlements , "[[:digit:]]+", "")
  return(clusters_data)
}

#' Function to extraction metadata content of a SMART Survey
#'
#' @param smart_content Content of smart survey
#' @return [data.frame] containing the metadata information in the SMART
#' @export
f_extract_metadata_data <- function(smart_content){
  #Recall days
  recall_days <-f_extract_recall_day(smart_content)
  ##Code for injury/unknown or violent death
  codes <- f_find_codes(smart_content)
  metadata_data <- data.frame(recall_days = recall_days)
  metadata_data$unk_code <- codes[1]
  metadata_data$inj_code <- codes[2]
  metadata_data$viol_code <- codes[3]
  return(metadata_data)
}

#' Function to extraction recall day of a SMART Survey
#'
#' @param smart_content Content of smart survey
#' @return [data.frame] containing the recall day in the SMART
f_extract_recall_day <- function(smart_content){
  pos <- grep("Cluster", smart_content, ignore.cas=TRUE)[2]
  return(strsplit(smart_content[pos+8], '\t')[[1]][1])
}

#' Find code to know the type of death if known
#'
#' @param smart_content Content of smart survey
#' @return [data.frame] 3 integers for three different types of death: unknow, injury and violence
f_find_codes <- function(smart_content){
  pos <- grep('Mor_individual_options', smart_content, ignore.case=TRUE)
  pos_unk <- grep('Unknown', smart_content, ignore.case = TRUE)
  pos_inj <- grep('Injury', smart_content, ignore.case = TRUE)
  pos_viol <- grep('Violence', smart_content, ignore.case = TRUE)
  if(length(pos_unk) != 0){
    score_unk <- pos_unk - pos
  }else{
    score_unk <- 0
  }
  if(length(pos_inj) != 0){
    score_inj <- pos_inj - pos
  }else{
    score_inj <- 0
  }
  if(length(pos_viol) != 0){
    score_viol <- pos_viol - pos
  }else{
    score_viol <- 0
  }
  return(c(score_unk, score_inj, score_viol))
}
