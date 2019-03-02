library(png)
library(testthat)

#' Flip 
#' 
#' @param input_path string, path for the input png file 
#' @param output_path string, path for the output png file 
#' @param direction string, direction of flip 
#' 
#' @return a png file at the output path 
#' export 
#' 
#' @examples 
#' flip("test_imgs/flip/test_input.png","test_imgs/flip/test_output.png","v")

flip<-function(input_path,output_path,direction){
  
  assert("Please provide a string as the path for the input image file.", is.character(input_path))
  assert("Please provide a string as the path for the output image file.", is.character(output_path))
  assert("Invalid input for flip direction", direction %in% c("h","v"))
  
  
  #Reading image file 
  input_img = readPNG(input_path)
  
  row = dim(input_img)[1]
  col = dim(input_img)[2]
  R_input = input_img[,,1]
  G_input = input_img[,,2]
  B_input = input_img[,,3]
  R_output = R_input
  G_output = G_input
  B_output = B_input
  
  # horizental flip 
  if(direction == "h"){
    for(i in 1:row){
      for(j in 1:col){
        R_output[i,j] = R_input[i,col-j+1]
        G_output[i,j] = G_input[i,col-j+1]
        B_output[i,j] = B_input[i,col-j+1]
      }
    }
  }
  # vertical flip
  else if (direction == "v"){
    for(i in 1:row){
      for(j in 1:col){
        R_output[i,j] = R_input[row+1-i,j]
        G_output[i,j] = G_input[row+1-i,j]
        B_output[i,j] = B_input[row+1-i,j]
      }
    }
  }
  
  output_img <- array(c(R_output,G_output,B_output),dim = c(row,col,3))
  
  #Saved flipped image to output path
  writePNG(output_img,output_path)
  
}


