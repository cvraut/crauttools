library(crauttools)

test_that("missing parameters", {
  # missing share and download
  expect_error(download.gdrive(),"Missing share_url or download_url")
})

test_that("can get files from gdrive",{
  test_file1_share = "https://drive.google.com/file/d/1RYw0vUWHvaY7E_J3dgKd8DW6j0ez09fe/view?usp=sharing"
  test_file1_down = "https://drive.google.com/uc?export=download&id=1RYw0vUWHvaY7E_J3dgKd8DW6j0ez09fe"
  expect_equal(download.gdrive(share_url=test_file1_share,content=TRUE,quiet = TRUE),"test download file")
})

test_that("access issues",{
  # file exists in drive, but cannot be accessed by anyone
  
  # file does not exist in drive (mistyped link)
  
  # parent dir for file writing cannot be written to
})