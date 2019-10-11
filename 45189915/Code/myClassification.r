# Xin Chen 45189915

# code to complete Task 3

library("lattice") # Better Graphical function
library("ggplot2") # Data visualization
library("plotly") # Interactive data visualizations
library("psych") # For correlation visualizations
library("rattle") # Graphing decision trees
library("caret") # Machine learning
library("party") # Decision Tree

# 3.1 load the preprocessed data file from Task1 into a data frame
#     divide the dataset into "training" and "test" subsets randomly (70% and 30% respectively)
#     [we use all attributes in task 3]
ILPD_task3 = readRDS("./Data/ilpd_processed.Rda")

# change it type to numeric, with 1 represents female and 0 represents male
ILPD_task3$Gender <- as.numeric(ILPD_task3$Gender)
ILPD_task3$Gender[is.na(ILPD_task3$Gender)] <- 0
ILPD_task3[,"Gender"]<-factor(ILPD_task3[,"Gender"])

# divdie the dataset into "training" and "test" subsets randomly (70% and 30% respectively)
set.seed(45)
indices <- sample(2, nrow(ILPD_task3),replace = TRUE, prob = c(0.7, 0.3))
train_data <- ILPD_task3[indices == 1,]
test_data <- ILPD_task3[indices == 2,]


# 3.2 learn a classification tree from the training data using the default parameters of the ctree function from the "party" library
#     plot that classification tree and provide your comments on its structure (e.g., what are the important/unimportant variables? 
#     is there any knowledge we can infer from the tree representation that helps in differentiating between the classes?)
#     using the learned tree, predict the class labels of the test data
#     calculate the accuracy, precision and recall
ct <- ctree(Class ~., data = train_data)
plot(ct)
dev.copy(png,filename="Plot/task3.2.png");
dev.off ();
# confusion matrix
prediction_dt1 <- predict(ct, test_data)
matrix_dt1 <- table(prediction_dt1, test_data$Class)
TP = matrix_dt1[2,2]
FN = matrix_dt1[2,1]
FP = matrix_dt1[1,2]
TN = matrix_dt1[1,1]
# calculate the accuracy
accuracy_dt1 = sum(diag(matrix_dt1))/sum(matrix_dt1) # overall accuracy
# calculate the precision
precision_dt1 = TP/(TP + FP)
# calculate the recall
recall_dt1 = TP/(TP+FN)
# calculate the f1 score
f1_dt1 = (2 * precision_dt1 * recall_dt1)/(precision_dt1 + recall_dt1)

# correlation matrix
pairs.panels(ILPD_task3[,1:10], scale = TRUE, bg = c("red","green","blue")[ILPD_task3$Class], pch = 21, main = "Correlation Matrix of ILPD Data")
dev.copy(png,filename="Plot/task3.2(correlation maxtrix).png");
dev.off ();


# 3.3 try building your classification tree again via the ctree function but using parameters that are different from the default settings.
#     can you achieve better accuracy or more meaningful representation by tuning some parameters?
#     (note that in the ctree function, you can modify ctree_control parameters. execute ?ctree from RStudio console for the detailed documentation)
ct <- ctree(Class ~Sgot+AG_Ratio, data = train_data, controls = ctree_control(maxdepth = 6, minbucket = 5, mincriterion = 0.083))
plot(ct)
dev.copy(png,filename="Plot/task3.3.png");
dev.off ();
# confusion matrix
prediction_dt2 <- predict(ct, test_data)
matrix_dt2 <- table(prediction_dt2, test_data$Class)
TP = matrix_dt2[2,2]
FN = matrix_dt2[2,1]
FP = matrix_dt2[1,2]
TN = matrix_dt2[1,1]
# calculate the accuracy
accuracy_dt2 = sum(diag(matrix_dt2))/sum(matrix_dt2) # overall accuracy
# calculate the precision
precision_dt2 = TP/(TP + FP)
# calculate the recall
recall_dt2 = TP/(TP+FN)
# calculate the f1 score
f1_dt2 = (2 * precision_dt2 * recall_dt2)/(precision_dt2 + recall_dt2)


# 3.4 apply K-NN classification to predict the labels in the test subset and calculate the accuracy, precision and recall
#     particularly, try different values of K (e.g., K=1,2,3,4,5), and report your observations on the achieved classification

library("class")

# K=1
set.seed(45)
prediction_knn1  <- knn(train_data[,-11], test_data[,-11 ], train_data$Class, k=1, prob=TRUE)
# confusion matrix
matrix_knn1 <- table(prediction_knn1, test_data$Class)
TP = matrix_knn1[2,2]
FN = matrix_knn1[2,1]
FP = matrix_knn1[1,2]
TN = matrix_knn1[1,1]
# calculate the accuracy
accuracy_knn1 = sum(diag(matrix_knn1))/sum(matrix_knn1)
# calculate the precision
precision_knn1 = TP/(TP + FP)
# calculate the recall
recall_knn1 = TP/(TP+FN)
# calculate the f1 score
f1_knn1 = (2 * precision_knn1 * recall_knn1)/(precision_knn1 + recall_knn1)

# K=2
set.seed(45)
prediction_knn2  <- knn(train_data[,-11], test_data[,-11 ], train_data$Class, k=2, prob=TRUE)
# confusion matrix
matrix_knn2 <- table(prediction_knn2, test_data$Class)
TP = matrix_knn2[2,2]
FN = matrix_knn2[2,1]
FP = matrix_knn2[1,2]
TN = matrix_knn2[1,1]
# calculate the accuracy
accuracy_knn2 = sum(diag(matrix_knn2))/sum(matrix_knn2)
# calculate the precision
precision_knn2 = TP/(TP + FP)
# calculate the recall
recall_knn2 = TP/(TP+FN)
# calculate the f1 score
f1_knn2 = (2 * precision_knn2 * recall_knn2)/(precision_knn2 + recall_knn2)

# K=3
set.seed(45)
prediction_knn3  <- knn(train_data[,-11], test_data[,-11 ], train_data$Class, k=3, prob=TRUE)
# confusion matrix
matrix_knn3 <- table(prediction_knn3, test_data$Class)
TP = matrix_knn3[2,2]
FN = matrix_knn3[2,1]
FP = matrix_knn3[1,2]
TN = matrix_knn3[1,1]
# calculate the accuracy
accuracy_knn3 = sum(diag(matrix_knn3))/sum(matrix_knn3)
# calculate the precision
precision_knn3 = TP/(TP + FP)
# calculate the recall
recall_knn3 = TP/(TP+FN)
# calculate the f1 score
f1_knn3 = (2 * precision_knn3 * recall_knn3)/(precision_knn3 + recall_knn3)

# K=4
set.seed(45)
prediction_knn4  <- knn(train_data[,-11], test_data[,-11 ], train_data$Class, k=4, prob=TRUE)
# confusion matrix
matrix_knn4 <- table(prediction_knn4, test_data$Class)
TP = matrix_knn4[2,2]
FN = matrix_knn4[2,1]
FP = matrix_knn4[1,2]
TN = matrix_knn4[1,1]
# calculate the accuracy
accuracy_knn4 = sum(diag(matrix_knn4))/sum(matrix_knn4)
# calculate the precision
precision_knn4 = TP/(TP + FP)
# calculate the recall
recall_knn4 = TP/(TP+FN)
# calculate the f1 score
f1_knn4 = (2 * precision_knn4 * recall_knn4)/(precision_knn4 + recall_knn4)

# K=5
set.seed(45)
prediction_knn5  <- knn(train_data[,-11], test_data[,-11 ], train_data$Class, k=5, prob=TRUE)
# confusion matrix
matrix_knn5 <- table(prediction_knn5, test_data$Class)
TP = matrix_knn5[2,2]
FN = matrix_knn5[2,1]
FP = matrix_knn5[1,2]
TN = matrix_knn5[1,1]
# calculate the accuracy
accuracy_knn5 = sum(diag(matrix_knn5))/sum(matrix_knn5)
# calculate the precision
precision_knn5 = TP/(TP + FP)
# calculate the recall
recall_knn5 = TP/(TP+FN)
# calculate the f1 score
f1_knn5 = (2 * precision_knn5 * recall_knn5)/(precision_knn5 + recall_knn5)