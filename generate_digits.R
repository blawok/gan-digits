# load utility functions
source("gan.R")
# load model data
load("model_3.RData")

# declare model hyperparameters
g_nn<-nn_model(input_dim=784,hidden=10,output_dim=784,learningrate=0.1,
               activationfun="relu",output="sigm" )
d_nn<-nn_model(input_dim=784,hidden=10,output_dim=1,learningrate=0.1,
               activationfun="relu",output="sigm" )

# generate new data - specify model and number of new data elements
generation<-generator(gan_model,6)

# draw new data (remember to enlarge plot window!)
par(mfrow=c(3,3))
lapply(1:6,
       function(q) image(
         rotate(matrix(unlist(generation[q,]),nrow = 28, byrow = TRUE)),
         col=grey.colors(255)
       )
)

# save data to a file
write.csv(generation, "generation_test.csv")