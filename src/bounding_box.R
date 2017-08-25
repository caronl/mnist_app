library(imager)


bounding_box <- function(image_data){
    
    bounds_x <- 
        # get a [1:2, 1:280] matrix, the min x and max x for each line in the image
        apply(image_data, 1, 
              FUN = function(v){
                  vix <- which(v > 0); 
                  c(
                      if(length(vix) == 0) Inf else min(vix),
                      if(length(vix) == 0) -Inf else max(vix)
                  )
              })
    
    min_x <- min(bounds_x[1, ])
    max_x <- max(bounds_x[2, ])
    
    bounds_y <- 
        # get a [1:2, 1:280] matrix, the min x and max x for each line in the image
        apply(image_data, 2, 
              FUN = function(v){
                  vix <- which(v > 0); 
                  c(
                      if(length(vix) == 0) Inf else min(vix),
                      if(length(vix) == 0) -Inf else max(vix)
                  )
              })
    
    min_y <- min(bounds_y[1, ])
    max_y <- max(bounds_y[2, ])
    
    list(min_x=min_x, max_x=max_x, min_y=min_y, max_y=max_y)
    
}

#bounding_box(load.image(file = "holdout/images/img_1.png")[,,,4])



