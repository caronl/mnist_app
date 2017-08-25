source("src/interpret_results.R")

model <- load_model_hdf5("model.hdf5")

eval_set <- map2_df(.x = list.files("images"),
                    .y = readRDS("labels/labels.RDS"),
                    .f = ~ list(name = .x,
                                label = .y,
                                pred = load.image(file = paste0("images/", .x)) %>%
                                    resize(size_x = 28, size_y = 28, interpolation_type = 2) %>%
                                    "["(,,,4) %>%
                                    t() %>%
                                    array(dim = c(1, 784)) %>% 
                                    model$predict_on_batch() %>% 
                                    interpret_results() %>% 
                                    top_n(1, prob) %>% 
                                    select(digit) %>% 
                                    as.numeric))

eval_set

