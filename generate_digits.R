generate_digits <- function(digit_to_generate = NULL, show_data = FALSE, save_data = FALSE){
  
  if (is.numeric(digit_to_generate)) {
    # load utility functions
    source("gan.R")
  }
  
  if (!is.numeric(digit_to_generate)) {
    print("Specify desired digit as an integer")
  } else if (digit_to_generate == 3) {
      # load desired model data
      load("model_3.RData")
      
      # declare model hyperparameters
      g_nn<<-nn_model(input_dim=784,hidden=10,output_dim=784,learningrate=0.1,
                     activationfun="relu",output="sigm" )
      d_nn<<-nn_model(input_dim=784,hidden=10,output_dim=1,learningrate=0.1,
                     activationfun="relu",output="sigm" )
      
      # generate new data - specify model and number of new data elements
      generation<-generator(gan_model,6)
    }
  
  if (!is.logical(show_data) | !is.logical(save_data)) {
    print("'show_data' and 'save_data' have to be logical") 
  }
  
  if (isTRUE(show_data) & is.numeric(digit_to_generate)) {
    # draw new data (remember to enlarge plot window!)
    par(mfrow=c(3,3))
    lapply(1:6,
           function(q) image(
             rotate(matrix(unlist(generation[q,]),nrow = 28, byrow = TRUE)),
             col=grey.colors(255)
           )
    )
  }

  if (isTRUE(save_data) & is.numeric(digit_to_generate)){
    # save data to a file
    write.csv(generation, "generation_test.csv")
  }
  
  
}

generate_digits(digit_to_generate = 3, show_data = T)
