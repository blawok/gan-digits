library(imager)

im <- load.image("3/number-0000004.png")
im2 <- resize(im, size_x=, size_y=1000)
vector <- as.vector(t(matrix(im2)))
plot(im2)
image((as.matrix(im2)))

save.image(im2, "trzy.png")
file <- readPNG("trzy.png")


files <- list.files(path="3/", pattern="*.PNG", full.names=TRUE, recursive=FALSE)
df <- data.frame()

for (file in files) {
  im <- load.image(file)
  im2 <- resize(im, size_x=28, size_y=28)
  vector <- as.vector(as.matrix(im2))
  print(file)
  dfTemp <- data.frame(matrix(vector,nrow = 1))
  df <- rbind(df, dfTemp)
}

write.csv(df, file = "trojki_6185.csv")


image(
  # rotate(matrix(unlist(generation[q,]),nrow = 32, byrow = TRUE)),
  # matrix(unlist(df[2,])),
  matrix(unlist(df[2,]),nrow = 80, ncol=80),
  col=grey.colors(255)
)
