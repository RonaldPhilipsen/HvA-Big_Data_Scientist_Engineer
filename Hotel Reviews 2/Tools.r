Require.packages <- function(list.of.packages) {
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
    if (length(new.packages)) {
        install.packages(new.packages)
    }

    suppressPackageStartupMessages(
        x <- lapply(list.of.packages, require, character.only = TRUE)
    )
}


test.stuff <- function() {
    #initialize the testing posts and set the x-param
    test_posts = data[(train_size + 1):nrow(data), 2]
    x_test = texts_to_matrix(tokenizer, test_posts, mode = 'freq')
    rm(test_posts)

    #initialize the testing labels and set the y-param
    test_tags = data[(train_size + 1):nrow(data), 1]
    y_test = to_categorical(test_tags)
    rm(test_tags)

    #evaluate the model's performance
    score <- evaluate(model, x_test, y_test, batch_size = batch_size, verbose = 1)

    print(paste0('Test loss:', score[1]))
    print(paste0('Test accuracy:', score[2]))

    #get rid of the testing parameters
    rm(x_test)
    rm(y_test)


    }