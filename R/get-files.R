# This function provides a file path when a transform input file is brought into 
#    Palantir Foundry's Code Workbook in NIDAP.

#' @title Get Path
#' @description This function returns the file path from a transform input 
#'        dataset in NIDAP Code Workbook
#' @details This function will return a file path from a transform input dataset 
#'        into a code template or code transform in the NIDAP instance of
#'        Foundry Code Workbook.
#' @param input transform input dataset to import
#' @param name name of input file, usually transform input format
#' @param type data type, by default, 'r'.
#' 
#' @export 
#' 
#' @return file path from which to read in data. Note that the original file 
#'    name will not be retained 

nidapGetPath <- function(input,
                     name,
                     type = 'r'){
  
  fs <- input$fileSystem()
  path <- fs$get_path(name, type)
  return(path)
}

# This function returns the file path or paths used for loading files brought 
#  into NIDAP code workbook.

#' @title Get Files
#' @description This function returns specific file paths for an input dataset  
#'       in transform input format containing either a single or multiple files. 
#' @details This method will copy files to the compute node and returns
#'        the original file names for reading into code workbook. 
#'
#' @param data Dataset to import, usually in transform input format 
#' @param ext File extension. For instance, "xlsx", "csv", "'txt, "rds"
#' @param type Data type, by default, 'r'
#' 
#' @export 
#' 
#' @return File name(s) to read into code template or code transform 
#' 

nidapGetFiles <- function(data,
                          ext,
                          type = 'r'){
  fs <- data$fileSystem()
  file.path <- fs$ls(glob = paste0("*.",ext))
  files <- lapply(file.path, function(file) { 
        return(c(file$path, fs$get_path(file$path, type))) })
  output.names<-list()
  for (item in files){
    file.copy(from=item[2], to=item[1], 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
    output.names <- append(output.names,item[1])
  }
  return(unlist(output.names))
}
