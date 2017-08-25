library(imager)
library(dplyr)
library(ggplot2)

#' Example
#' bounding_box(load.image(file = "holdout/images/img_1.png"))
bounding_box <- function(img){
    
    image_data <- img[,,,4]
    
    bounds_y <- 
        # get a [1:2, 1:280] matrix, the min x and max x for each line in the image
        apply(image_data, 1, 
              FUN = function(v){
                  vix <- which(v > 0); 
                  c(
                      min_x = if(length(vix) == 0) Inf else min(vix),
                      max_x = if(length(vix) == 0) -Inf else max(vix)
                  )
              }) %>% t
    
    min_y <- min(bounds_y[, 1])
    max_y <- max(bounds_y[, 2])
    
    bounds_x <- 
        # get a [1:2, 1:280] matrix, the min x and max x for each line in the image
        apply(image_data, 2, 
              FUN = function(v){
                  vix <- which(v > 0); 
                  c(
                      if(length(vix) == 0) Inf else min(vix),
                      if(length(vix) == 0) -Inf else max(vix)
                  )
              })
    
    min_x <- min(bounds_x[1, ])
    max_x <- max(bounds_x[2, ])
    
    list(min_x=min_x, max_x=max_x, min_y=min_y, max_y=max_y)
}




#' Returns the scaling factor to resize to target_size while maintaining aspect ratio
#' Example
#' img <- load.image(file = "holdout/images/img_1.png")
#' sf <- scale_factor(bounding_box(img), target_size=20)
#' new_img <- resize(img, size_x = round(nrow(img) * sf), size_y = round(ncol(img) * sf), interpolation_type = 5)
# ggplot(as.data.frame(new_img) %>% filter(cc==4), aes(x=x, y=y)) +
#   geom_raster(aes(fill=value)) +
#   scale_y_continuous(trans=scales::reverse_trans())
scale_factor <- function(bounding_box, target_size=20){
    w <- bounding_box$max_x - bounding_box$min_x
    h <- bounding_box$max_y - bounding_box$min_y
    20 / max(w, h)
}



#' Returns the center of mass of an image
#' Example
#' img <- load.image(file = "holdout/images/img_1.png")
#' sf <- scale_factor(bounding_box(img), target_size=20)
#' new_img <- resize(img, size_x = round(nrow(img) * sf), size_y = round(ncol(img) * sf), interpolation_type = 5)
#' center_of_mass(new_img[,,,4])
center_of_mass <- function(img){
    image_data <- img[,,,4]
    total_sum <- sum(image_data)
    center_x <- sum(image_data %*% 1:ncol(image_data)) / sum(image_data)
    center_y <- sum(1:nrow(image_data) %*% image_data) / sum(image_data)
    c(center_x=center_x, center_y=center_y)
}


center_point <- function(img){
    bb <- bounding_box(img)
    c(center_x = (bb$max_x + bb$min_x)*0.5,
      center_y = (bb$max_y + bb$min_y) * 0.5)
}


#' Image translation.
#' Example:
# img <- load.image(file = "holdout/images/img_6.png")
# ggplot(as.data.frame(img) %>% filter(cc==4), aes(x=x, y=y)) +
#     geom_raster(aes(fill=value)) +
#     scale_y_continuous(trans=scales::reverse_trans())
# sf <- scale_factor(bounding_box(img), target_size=20)
# new_img <-
#     resize(img, size_x = round(nrow(img) * sf), size_y = round(ncol(img) * sf), interpolation_type = 5) %>%
#     move_image(from=center_point(.), to=dim(.)[1:2]/2) %>%
#     crop.borders(nx = round((dim(.)[1] - 28)/2), ny = round((dim(.)[2] - 28)/2)) %>%
#     resize(size_x = 28, size_y = 28)
# ggplot(as.data.frame(new_img) %>% filter(cc==4), aes(x=x, y=y)) +
# geom_raster(aes(fill=value)) +
#    scale_y_continuous(trans=scales::reverse_trans())

move_image <- function(img, from, to){
    img %>% imshift(delta_x = from[1] - to[1], delta_y = to[2] - from[2])
}


