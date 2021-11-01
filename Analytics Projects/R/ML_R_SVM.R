##svm
letters <- read.csv('letter-recognition.data', header = F)
head(letters)
letters$V1 <- as.factor(letters$V1)
str(letters)
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

library(kernlab)
letter_classifier <- ksvm(V1 ~ ., data = letters_train,
                            kernel = "vanilladot")
letter_classifier

letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$V1)
agreement <- letter_predictions == letters_test$V1
table(agreement)
prop.table(table(agreement))

letter_classifier_rbf <- ksvm(V1 ~ ., data = letters_train,
                              kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf,
                                  letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$V1
table(agreement_rbf)
prop.table(table(agreement_rbf))

##### mnist
mnist_train <- read.csv('mnist_train.csv', header = F)
#head(mnist_train)
#str(mnist_train)
mnist_train$V1 <- as.factor(mnist_train$V1)
#str(mnist_train)
table(mnist_train$V1)

mnist_test <- read.csv('mnist_test.csv', header = F)
mnist_test$V1 <- as.factor(mnist_test$V1)
#str(mnist_test)
table(mnist_test$V1)

library(kernlab)
mnist_classifier <- ksvm(V1 ~ ., data = mnist_train,
                          kernel = "vanilladot")
mnist_classifier

mnist_predictions <- predict(mnist_classifier, mnist_test)
head(mnist_predictions)

table(mnist_predictions, mnist_test$V1)
agreement <- mnist_predictions == mnist_test$V1
table(agreement)
prop.table(table(agreement))


mnist_classifier_rbf <- ksvm(V1 ~ ., data = mnist_train,
                              kernel = "rbfdot")
mnist_predictions_rbf <- predict(mnist_classifier_rbf,
                                  mnist_test)
agreement_rbf <- mnist_predictions_rbf == mnist_test$V1
table(agreement_rbf)
prop.table(table(agreement_rbf))

