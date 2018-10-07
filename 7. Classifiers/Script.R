library(caret)
data(iris)

ds <- iris

validation_index <- createDataPartition(ds$Species, p=0.80, list = FALSE)
validation <- ds[-validation_index,]
ds <- ds[validation_index,]

dim(ds)
sapply(ds, class)
head(ds)

levels(ds$Species)


percentage <- prop.table(table(ds$Species)) * 100
cbind(freq = table(ds$Species), percentage = percentage)

summary(ds)

x <- ds[, 1:4]
y <- ds[, 5]

par(mfrow = c(1, 4))
for (i in 1:4) {
    boxplot(x[, i], main = names(iris)[i])
}

#featurePlot(x = x, y = y, plot = "ellipse")

#featurePlot(x = x, y = y, plot = "box")

# density plots for each attribute by class value
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
#featurePlot(x = x, y = y, plot = "density", scales = scales)

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"


# a) linear algorithms
set.seed(7)
fit.lda <- train(Species ~ ., data = ds, method = "lda", metric = metric, trControl = control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species ~ ., data = ds, method = "rpart", metric = metric, trControl = control)
# kNN
set.seed(7)
fit.knn <- train(Species ~ ., data = ds, method = "knn", metric = metric, trControl = control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species ~ ., data = ds, method = "svmRadial", metric = metric, trControl = control)
# Random Forest
set.seed(7)
fit.rf <- train(Species ~ ., data = ds, method = "rf", metric = metric, trControl = control)

# summarize accuracy of models
results <- resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results)

dotplot(results)