# devtools::install_github("rstudio/keras")

# install anaconda 4.4 with python 3.6


library(keras)
library(abind)

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y


# dim(x_train) <- c(nrow(x_train), 784)
# dim(x_test) <- c(nrow(x_test), 784)
dim(x_train) <- c(dim(x_train), 1)
dim(x_test) <- c(dim(x_test), 1)

x_train <- x_train / 255
x_test <- x_test / 255


y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)


model <- keras_model_sequential() 
# model %>%
#     layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
#     layer_dropout(rate = 0.4) %>%
#     layer_dense(units = 128, activation = 'relu') %>%
#     layer_dropout(rate = 0.3) %>%
#     layer_dense(units = 10, activation = 'softmax')

model %>% 
    layer_conv_2d(32, kernel_size = c(3,3), activation = 'relu', input_shape = c(28, 28, 1)) %>% 
    layer_conv_2d(64, kernel_size = c(3,3), activation='relu') %>%
    layer_max_pooling_2d(pool_size=c(2,2)) %>%
    layer_dropout(rate = 0.25) %>% 
    layer_flatten() %>%
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
)

history <- model %>% fit(
    x_train, y_train, 
    epochs = 12, batch_size = 128, 
    validation_split = 0
)

plot(history)

loss_and_metrics <- model %>% evaluate(x_test, y_test)

save_model_hdf5(model, "model.hdf5")

#### Train model on full data

# x_full <- abind(x_train, x_test, along = 1)
# y_full <- abind(y_train, y_test, along = 1)
# 
# model %>% fit(x_full, y_full, epochs = 30, batch_size = 128)

