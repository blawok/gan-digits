
if(!require(imager)){
  install.packages('imager')
  library(imager)
} else {
  library(imager)
}

generate_digits <- function(# simple generating digits with already computed model
                            digit_to_generate = NULL,
                            plot_data = FALSE,
                            save_data = FALSE,
                            csv_path = "generated_digits.csv",
                            save_data_as_png = FALSE,
                            num_of_digits_to_generate = 9,
                            
                            # route with network on demand
                            network_on_demand = FALSE,
                              prepare_data = FALSE,
                                data_folder_path = "3/",
                                dataset_file_path = "data/trojki_test_test_test.csv",
                                  image_quantity = 1000,
                                  hidden_layers = 10,
                                  learning_rate = 0.1,
                                  batchsize = 300,
                                  num_of_epochs = 20,
                                  image_resolution = 28,
                            save_model = FALSE,
                            model_save_path = "models/test_model.RData",
                            
                            # path for either saving the model or loading it
                            model_file_name = "neural_network/models/test_model.RData") {
  
  # setwd("..")
  setwd("C:/Users/Asus/GIT/gan_digits")
  
  if (isTRUE(network_on_demand)) {
    # route with network on demand
    
    source("neural_network/main.R")
    
    main_network(dataset_file_path = dataset_file_path,
                 prepare_data = prepare_data,
                 data_folder_path = data_folder_path,
                 image_quantity = image_quantity,
                 hidden_layers = hidden_layers,
                 learning_rate = learning_rate,
                 batchsize = batchsize,
                 num_of_epochs = num_of_epochs,
                 image_resolution = image_resolution,
                 matrix_columns_from = 2,
                 matrix_columns_until = 785,
                 plot_digits = FALSE,
                 save_model = save_model,
                 model_file_name = model_save_path)

    setwd("..")
  }
  
  
  if (is.numeric(digit_to_generate)) {
    # load utility functions
    source("neural_network/gan.R")
  }
  
  
  if (!is.numeric(digit_to_generate)) {
    
    print("Specify desired digit as an integer")
    
  } else if (digit_to_generate == 3) {
    
      # load desired model data
      load(model_file_name)
      
      # generate new data - specify model and number of new data elements
      generation<<-generator(gan_model,num_of_digits_to_generate)
    }
  
  
  if (!is.logical(plot_data) | !is.logical(save_data)) {
    
    print("'show_data' and 'save_data' have to be logical") 
  
    }
  
  
  plot_digit <- function(q) {
    image(rotate(matrix(unlist(generation[q,]),nrow = 28, byrow = TRUE)), col=grey.colors(255))
  }
  
  if (isTRUE(plot_data) & is.numeric(digit_to_generate) & digit_to_generate == 3) {
    # draw new data (remember to enlarge plot window!!!)
    par(mfrow=c(3,3))
    lapply(1:min(num_of_digits_to_generate, 9), plot_digit)
  }

  
  if (isTRUE(save_data) & is.numeric(digit_to_generate) & digit_to_generate == 3){
    # save data to a file
    write.csv(generation, csv_path)
  }
  
  save_digit_as_png <- function(q) {
    im <- as.cimg(rotate(matrix(unlist(generation[q,]),nrow = 28, byrow = TRUE)))
    paste(q, ".png")
    save.image(im, paste("created_digits/",q, ".png"))
  }
  
  if (isTRUE(save_data_as_png) & is.numeric(digit_to_generate) & digit_to_generate == 3){
    # save data as pngs
    lapply(1:min(num_of_digits_to_generate, 9), save_digit_as_png)
  }
  
}

# example of usage
generate_digits(digit_to_generate = 3,
                plot_data = TRUE,
                save_data = FALSE,
                save_data_as_png = TRUE,
                num_of_digits_to_generate = 11,
                
                # route with network on demand
                network_on_demand = TRUE,
                dataset_file_path = "data/trojki_6185.csv",
                # data_folder_path = "3/",
                # image_quantity = 1000,
                hidden_layers = 30,
                learning_rate = 0.1,
                batchsize = 300,
                num_of_epochs = 20,
                image_resolution = 28,
                # prepare_data = FALSE,
                save_model = TRUE,
                model_save_path = "models/test_from_generate.RData",
                
                model_file_name = "neural_network/models/test_from_generate.RData")
