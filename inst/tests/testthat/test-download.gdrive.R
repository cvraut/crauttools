library(crauttools)

# Delete test file if it exists
clean_up = function(fname = "gdrive.file"){
  if (file.exists(fname)) {
    file.remove(fname)
  }
}

test_that("missing parameters", {
  # missing share and download
  expect_error(download.gdrive(),"Missing share_url or download_url")
})

test_that("can get files from gdrive",{
  clean_up()
  # test file urls
  test_file1_share = "https://drive.google.com/file/d/1RYw0vUWHvaY7E_J3dgKd8DW6j0ez09fe/view?usp=sharing"
  test_file1_down = "https://drive.google.com/uc?export=download&id=1RYw0vUWHvaY7E_J3dgKd8DW6j0ez09fe"
  
  # test the share link
  expect_equal(download.gdrive(share_url=test_file1_share,content=TRUE,quiet = TRUE),"test download file")
  clean_up()
  
  # test the download link
  expect_equal(download.gdrive(download_url=test_file1_down,content=TRUE,quiet = TRUE),"test download file")
  clean_up()
})

test_that("access issues",{
  clean_up()
  # file exists in drive, but cannot be accessed by anyone
  secret_file1_share = "https://drive.google.com/file/d/1wCQklzEJ7KYz6Qf9PLt2q3SRXkGMvl7O/view?usp=sharing"
  CANNOT_ACCESS_FILE_MSG_PATTERN = "downloaded length \\d+ != reported length \\d+ \nRemoving downloads... Do you have access?"
  expect_error(download.gdrive(share_url=secret_file1_share,content=TRUE,quiet = TRUE),CANNOT_ACCESS_FILE_MSG_PATTERN)
  # file does not exist in drive (mistyped link)
  secret_file2_share = "https://drive.google.com/file/d/1wCQklzEJ7K_Mvl7O/view?usp=sharing"
  FILE_NOT_FOUND_PATTERN = "cannot open URL '.+'"
  expect_error(download.gdrive(share_url=secret_file2_share,content=TRUE,quiet = TRUE),FILE_NOT_FOUND_PATTERN)
  # parent dir for file writing cannot be written to
  ## place exists but I don't have write permissions
  illegal_dir = "/home/danbarke"
  illegal_file1_share = "https://drive.google.com/file/d/1tVsSidcXJaCYi4cFSdTRNyTeWR0Wy-2R/view?usp=sharing"
  ILLEGAL_FILE_LOCATION_PATTERN = paste("\\w*'",illegal_dir,"' not writeable",sep="")
  expect_error(download.gdrive(share_url = illegal_file1_share,parent_dir=illegal_dir, content=FALSE,quiet = TRUE),ILLEGAL_FILE_LOCATION_PATTERN)
  ## place just doesn't exist
  illegal_dir2 = "/home/++++"
  ILLEGAL_FILE_LOCATION_PATTERN_2 = "Path to file \\(dirname\\) does not exist: '.+' of '.+'"
  expect_error(download.gdrive(share_url = illegal_file1_share,parent_dir=illegal_dir2, content=FALSE,quiet = TRUE),ILLEGAL_FILE_LOCATION_PATTERN_2)
  clean_up()
})

