#load data
wbcd <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"))
#assing name to columns
colnames(wbcd) <- c("id", "class", "radius_mean", "radius_sd", "radius_largest", "texture_mean", "texture_sd", "texture_largest", "perimeter_mean", "perimeter_sd", "perimeter_largest", "area_mean", "area_sd", "area_largest", "smoothness_mean", "smoothness_sd", "smoothness_largest", "compactness_mean", "compactness_sd", "compactness_largest", "concavity_mean", "concavity_sd", "concavity_largest", "concavepoints _mean", "concavepoints _sd", "concavepoints _largest", "symmetry_mean", "symmetry_sd", "symmetry_largest", "fractaldimension_mean", "fractaldimension_sd", "fractaldimension_largest")
#remove the id column, you can also do it by using wbcd[-1]
wbcd_train <- wbcd[,-1]
#define transformation function
normalize <- function(x) {
    return ((x-min(x))/(max(x)-min(x)))
}
#normalize feature
wbcd_ftr <- as.data.frame(lapply(wbcd_train[2:31], normalize))
#prepare label
wbcd_label <- factor(wbcd_train$class, levels = c("B", "M"), labels = c("Benign", "Maligant"))
#prepare training set and test set
wbcd_train_ftr <- wbcd_ftr[1:469,]
wbcd_test_ftr <- wbcd_ftr[470:568,]
wbcd_train_label <- wbcd_label[1:469]
wbcd_test_label <- wbcd_label[470:568]

#load library for knn
(if (!require("class", quietly = TRUE) ) install.packages("class"))
library("class")

#load library for cross tables
(if (!require("gmodels", quietly = TRUE) ) install.packages("gmodels"))
library("gmodels")

#training
wbcd_pred <- knn(wbcd_train_ftr, wbcd_test_ftr, wbcd_train_label, 21)

#evaluate performance
CrossTable(wbcd_test_label,wbcd_pred,prop.chisq=FALSE)

#download caret for to partite data into train and test set
(if (!require("caret", quietly = TRUE)) install.packages("caret"))
library("caret")
train_set <- createDataPartition(wbcd_label, p = 0.8, list = FALSE)
wbcd_train_ftr <- wbcd_ftr[train_set,]
wbcd_train_label <- wbcd_label[train_set]
wbcd_test_ftr <- wbcd_ftr[-train_set,]
wbcd_test_label <- wbcd_label[-train_set]

wbcd_pred1 <- knn(wbcd_train_ftr, wbcd_test_ftr, wbcd_train_label,21)
CrossTable(wbcd_test_label, wbcd_pred1, prop.chisq = FALSE)

# cross validation
seed <- 9070
set.seed(seed)
cs_split <- createFolds(wbcd_label, k = 10, returnTrain = TRUE)
#test cross validation
knn_cros <- function(train_set,data_ftr,data_class) {
    trf <- data_ftr[train_set,]
    trl <- data_class[train_set]
    tef <- data_ftr[-train_set,]
    tel <- data_class[-train_set]
    pre <- knn(trf, tef, trl, 21)
    CrossTable(tel, pre, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)
}
knn_cros(train_set,wbcd_ftr,wbcd_label)
knn_cros(cs_split$Fold01, wbcd_ftr, wbcd_label)

for (m in cs_split) {
    knn_cros(m, wbcd_ftr, wbcd_label)
}
