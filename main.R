
source("utils.R")

train<-read.csv("data/trojki_6185.csv")
x<-train[,2:785]
x<-x/255
x<-as.matrix(x)

### initialize model
g_nn<-nn_model(input_dim=784,hidden=10,output_dim=784,learningrate=0.1,
               activationfun="relu",output="sigm" )
d_nn<-nn_model(input_dim=784,hidden=10,output_dim=1,learningrate=0.1,
               activationfun="relu",output="sigm" )

numdata<-dim(train)[1]
num_f<-numdata* g_nn$input_dim
num_d<-numdata* d_nn$input_dim

### traning GANs
ganmodel<-gan(x,g_nn,d_nn,batchsize = 300,epoch = 1, disc_step=1,display_generation_distribution = F,display_generation_image = F)
### If you stop training, stopped model will be saved "gan_model".
gan_model$loss


generation<-generator(ganmodel,100)
## or if you stop traning
generation<-generator(gan_model,6)

hist(generation)

rotate <- function(x) t(apply(x, 2, rev))

par(mfrow=c(3,3))
lapply(1:6,
       function(q) image(
         # rotate(matrix(unlist(generation[q,]),nrow = 80, byrow = TRUE)),
         rotate(matrix(unlist(generation[q,]),nrow = 28, byrow = TRUE)),
         col=grey.colors(255)
       )
)


# 
# par(mfrow=c(1,1))
# image(
#   # rotate(matrix(unlist(generation[q,]),nrow = 32, byrow = TRUE)),
#   rotate(matrix(unlist(generation[3,]),nrow = 28, byrow = TRUE)),
#   # matrix(train[2,2:6401],nrow = 80, byrow = TRUE),
#   col=grey.colors(255)
# )





