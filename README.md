# mnist_app

### app.R
This script contains the ui and server sides of the app.

### model.hdf5
This is the model created by train.R (see [src/](#src)).

### src/
This folder contains all the relevant R programs:

- train.R was used to train the model
- assess_holdout.R allows to assess 'real-life' performance of the model (see [holdout](#holdout))
- other files are helpers called by the other scripts

### holdout/
When ran locally, this app allows the user to save the images created by the user, as well as
their corresponding labels as identified by the user. This 'holdout' data can then be used for
validating that the model performs well in a different context than the original MNIST. Indeed,
the size of the digit, the width of the pen and the difference in the way the drawing was
obtained (real pen vs. drawing with a computer mouse) can all affect the 'real-life' performance
of the model.

### www/
This folder contains the GitHub "Fork Me" logo and the javascript signature pad used in the app. Please see
[szimek's GitHub repo](https://github.com/szimek/signature_pad) for the original source code.

### CSS_file.css
This file is necessary for the signature pad to work properly.
