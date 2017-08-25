image_matrix <- function(file_name)
{
    load.image(file = file_name) %>%
        resize(size_x = 28, size_y = 28, interpolation_type = 2) %>%
        "["(,,,4) %>%
        t() %>% 
        array(dim = c(1, 28, 28, 1))
}