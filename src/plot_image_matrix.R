plot_image_matrix <- function(image_matrix)
{
    image_matrix %>% 
        "["(1, 28:1,,) %>% 
        melt() %>% 
        ggplot(aes(x = X2, y = X1, fill = value)) + 
        geom_tile() +
        scale_fill_gradient(low = "white", high = "black") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position = "none")
}