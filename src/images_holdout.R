source("src/interpret_results.R")

model <- load_model_hdf5("model.hdf5")
labels <- readRDS("holdout/labels/labels.RDS")

holdout_set <- map_df(.x = list.files("holdout/images"),
                   .f = ~ list(name = .x,
                               label = labels[.x %>% match(labels$file_name), "label"],
                               pred = load.image(file = paste0("holdout/images/", .x)) %>%
                                   resize(size_x = 28, size_y = 28, interpolation_type = 2) %>%
                                   "["(,,,4) %>%
                                   t() %>%
                                   array(dim = c(1, 28, 28, 1)) %>% 
                                   model$predict_on_batch() %>% 
                                   interpret_results() %>% 
                                   top_n(1, prob) %>% 
                                   select(digit) %>% 
                                   as.numeric))

holdout_set

# labels <- rbind(labels, data.frame(file_name = "img_14.png", label = 8))
# saveRDS(labels, "holdout/labels/labels.RDS")
