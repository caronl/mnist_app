interpret_results <- function(prediction)
{
    tibble(digit = as.integer(0:9),
           prob = as.numeric(round(prediction, 4)) * 100) %>% 
        arrange(desc(prob))
}
