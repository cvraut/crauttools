# gdrive.R
# list of functions to connect to gdrive


#' download.gdrive
#' 
#' download file from gdrive
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
  
  if(!checkPathForOutput(download_location,overwrite = TRUE)){
    stop(paste("download location: '",download_location,"' is an invalid file location on this system",sep=""))
  } else if(is.character(checkPathForOutput(download_location)) && !quiet) {
    message("overwriting file")
  }
  
  download.file(download_url,download_location,quiet=quiet,...)
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