# gdrive.R
# list of functions to connect to gdrive


#' download.gdrive
#' 
#' download file from gdrive (NOTE: as of now, the file must be viewable to everyone)
#' Look at this site for further details of the issue:
#' https://syncwithtech.blogspot.com/p/direct-download-link-generator.html
#' 
#' @param f_name (default = "gdrive.file")string for what the file should be downloaded as
#' @param share_url (default = NULL) the share url of the file to download (ignored if download url is supplied)
#' @param download_url (deafult = NULL) the actual download url of the file to download
#' @param parent_dir (default = getwd()) the full path for the parent directory for the file to be downloaded to
#' @param content (default = FALSE) flag to determine whether to return the content or the file name
#' @param quiet (default = FALSE) flag to display warnings and status of function
#' @param ... extra parameters that will be passed to utils::download.file
#' @return string of the full file path of downloaded file (or content if the content flag is TRUE)
#' 
#' @examples 
#' TODO: add nice examples
#' @importFrom checkmate checkPathForOutput
#' @importFrom readr read_file
#' @importFrom stringr str_match
#' @export
download.gdrive <- function(f_name = "gdrive.file",
             share_url = NULL,
             download_url = NULL,
             parent_dir = getwd(),
             content=FALSE,
             quiet = FALSE,
             ...){
  # the str header for download url headers (will need to change over time)
  DOWNLOAD_URL_HEADER = "https://drive.google.com/uc?export=download&id="
  
  # if nothing is supplied yell at user
  if(is.null(share_url) && is.null(download_url))
  {
    stop("Missing share_url or download_url")
  }
  
  # case that the share url is supplied but not the download url
  if(!is.null(share_url) && is.null(download_url)){
    download_url = paste(DOWNLOAD_URL_HEADER,get_file_id.gdrive(share_url),sep="")
  }
  
  download_location = file.path(parent_dir,f_name)
  path_check = checkPathForOutput(download_location,overwrite = TRUE)
  if(path_check == TRUE){
    if(is.character(checkPathForOutput(download_location)) && !quiet){
      message("overwriting file")
    }
  } else {
    stop(path_check)
  }
  
  # wrap the download in a wrapper to capture errors and warnings and deal with them accordingly
  # addpted from the following SO response
  # https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
  err <- list()
  warn <- list()
  withCallingHandlers(
    tryCatch(download.file(download_url,download_location,quiet=quiet,...), error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  if(length(err)>0){
    FILE_NOT_FOUND_PATTERN = "^cannot open URL '.+'$"
    if(!is.na(str_match(err$message,FILE_NOT_FOUND_PATTERN))){
      stop(paste(err$message,"\nDo you have access?"))
    } else{
      stop(err)
    }
  } else if(length(warn)>0){
    FILE_SIZE_MISMATCH_PATTERN = "^downloaded length \\d+ != reported length \\d+$"
    if(!is.na(str_match(warn$message,FILE_SIZE_MISMATCH_PATTERN))){
      if (file.exists(download_location)) {
        file.remove(download_location)
        stop(paste(warn$message,"\nRemoving downloads... Do you have access?"))
      }
    }
  }
  
  if(!content){
    return(download_location)
  } else {
    return(read_file(download_location))
  }
}

#' get_file_id.gdrive
#' 
#' Extracts the file token from a gdrive share link
#' 
#' @param share_url a share url from gdrive
#' @return the file id token as a string
#' 
#' @importFrom stringr str_match
get_file_id.gdrive <- function(share_url = NULL){
  # the regex for share url (will need to change over time)
  SHARE_REGEX = "https://drive.google.com/file/d/(.+)/view\\?usp=sharing"
  return(str_match(share_url,SHARE_REGEX)[,2])
}
