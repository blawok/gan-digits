# library(imager)

if(!require(imager)){
  install.packages('imager')
  library(imager)
} else {
  library(imager)
}


data_prepare <- function(folder_path="3/",
                         destination_file = "threes_test.csv",
                         image_quantity = 10,
                         resizing_to = 28) {
  
  # loading all file names into one list
  files <- list.files(path=folder_path, pattern="*.PNG", full.names=TRUE, recursive=FALSE)
  
  df <- data.frame()
  image_number = 0
  
  # resizing images and transforming them into 1-dim vectors and appending them to one dataframe
  for (file in files) {
    if (image_number <= image_quantity) {
      dfTemp <-  file %>%
        load.image() %>%
        resize(., size_x=resizing_to, size_y=resizing_to) %>%
        as.matrix(.) %>%
        as.vector(.) %>%
        matrix(., nrow = 1) %>%
        data.frame(.)

      df <- rbind(df, dfTemp)
    }
    image_number = image_number + 1
  }
  
  # encoding values to either 0 or 255 for better distinction
  color_update <- function(x){
    if (x == 1) {
      x <- 0
    } else if (x == 0) {
      x <- 255
    }
  }
  
  df <- data.frame(apply(df, c(1,2), color_update))
  
  # saving file
  write.csv(df, file = destination_file)
}


# # example of usage
# data_prepare(folder_path="3/",
#              destination_file = "threes_full.csv",
#              image_quantity = 10,
#              resizing_to = 28)
