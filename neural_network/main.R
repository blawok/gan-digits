
setwd("C:/Users/Asus/GIT/gan_digits/neural_network")

main_network <- function(dataset_file_path = "data/trojki_full.csv",
                         prepare_data = FALSE,
                         image_quantity = 1000,
                         hidden_layers = 10,
                         learning_rate = 0.1,
                         batchsize = 300,
                         num_of_epochs = 20,
                         image_resolution = 28,
                         matrix_columns_from = 2,
                         matrix_columns_until = 785) {
  
  source("gan.R")
  source("data_prepare.R")
  
  if (isTRUE(prepare_data)){
    cat("Preparing data this may take aprox:", round(image_quantity*0.03), "sec")
    data_prepare(folder_path="3/",
                 destination_file = dataset_file_path,
                 image_quantity = image_quantity,
                 resizing_to = image_resolution)
  }
  
  train<-read.csv(dataset_file_path)
  x<-train[,matrix_columns_from:matrix_columns_until]
  x<-x/255
  x<-as.matrix(x)
  
  ### initialize model
  g_nn<<-nn_model(input_dim=image_resolution*image_resolution,
                  hidden=hidden_layers,
                  output_dim=image_resolution*image_resolution,
                  learningrate=learning_rate,
                  activationfun="relu",
                  output="sigm" )
  d_nn<<-nn_model(input_dim=image_resolution*image_resolution,
                  hidden=hidden_layers,
                  output_dim=1,
                  learningrate=learning_rate,
                  activationfun="relu",
                  output="sigm" )
  
  numdata<-dim(train)[1]
  num_f<-numdata* g_nn$input_dim
  num_d<-numdata* d_nn$input_dim
  
  ### traning GANs
  ganmodel<-gan(x,g_nn,d_nn,
                batchsize = batchsize,
                epoch = num_of_epochs,
                disc_step=1,
                display_generation_distribution = F,
                display_generation_image = F)
  ### If you stop training, stopped model will be saved "gan_model".
  gan_model$loss
  
  generation<-generator(gan_model,9)
  
  rotate <- function(x) t(apply(x, 2, rev))
  
  par(mfrow=c(3,3))
  lapply(1:6,
         function(q) image(
           rotate(matrix(unlist(generation[q,]),nrow = image_resolution, byrow = TRUE)),
           col=grey.colors(255)
         )
  )
  
  save(gan_model, file="test_model.RData")
  
}

# example of usage
main_network(dataset_file_path = "data/trojki_test_test_test.csv",
             image_quantity = 1000,
             hidden_layers = 10,
             learning_rate = 0.1,
             batchsize = 300,
             num_of_epochs = 20,
             image_resolution = 28,
             prepare_data = TRUE)
  

