####################################################################
# Implement an R program for PCA based Eigenface Generation        #
# Hw4 of CSC 591(Foundation of Data Science)                       #
# Author: Zexi chen(Unity ID: zchen22)                             #
####################################################################

rm(list=ls(all=T))

library(pixmap) # read the .pgm image

setwd("faces-corrected/faces-corrected/")

options(warn=-1)

file_list <- list.files()

for(file in file_list){
    temp  <- read.pnm(file)
    temp_vector <- as.vector(t(temp@grey))
    
    if(!exists("A")){
      A<- temp_vector
      image_row <- nrow(temp@grey)
      image_col <- ncol(temp@grey)
    }else{
      A <- cbind(A,temp_vector)
    }
}

# compute the mean for each row of the matrix
row_mean <- as.vector(rowMeans(A))

for(i in 1:nrow(A)){
  A[i,]<-A[i,]-row_mean[i]
}

# compute the A'*A
mat <- t(A) %*% A

#eigenvectors for the A'*A matrix
eig <- eigen(mat)

# convert the eigenvectors of the A'*A matrix to A*A' matrix
ori_eig <- A %*% eig$vectors

for(i in 1:10){
  x11()
  eigenface <- matrix(ori_eig[,i],image_col,image_row)
  eigenface = t(eigenface)
  image <- pixmapGrey(eigenface)
  plot(image,main=paste('The ',i,'th eigenface image'))
}




