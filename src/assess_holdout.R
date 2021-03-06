source("src/interpret_results.R")
source("src/preprocess_image.R")

model <- load_model_hdf5("model.hdf5")
labels <- readRDS("holdout/labels/labels.RDS")

holdout_set <- map_df(.x = list.files("holdout/images"),
                   .f = ~ list(name = .x,
                               label = labels[.x %>% match(labels$file_name), "label"],
                               pred = image_matrix(paste0("holdout/images/", .x)) %>%
                                   model$predict_on_batch() %>% 
                                   interpret_results() %>% 
                                   top_n(1, prob) %>% 
                                   select(digit) %>% 
                                   as.numeric))

holdout_set %>% filter(label != pred)
holdout_set %>%
    group_by(label) %>%
    dplyr::summarise(count = n(),
                     accuracy = round(mean(label == pred), 4) * 100)
holdout_set %>% summarise(accuracy = round(mean(label == pred), 4) * 100)

# labels <- rbind(labels, data.frame(file_name = "img_23.png", label = 0))
# saveRDS(labels, "holdout/labels/labels.RDS")
