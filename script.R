library(caret)
library(randomForest)
set.seed(777)

data <- read.csv("pml-training.csv", na.strings = c("", "#DIV/0!", "NA"), header = T)

# Going to use hold-out cross validation technique to determine out of sample error, which means splitting the set into a training and one test set
inTrain <- createDataPartition(y=data$classe, p=0.75, list=FALSE)

# Excluding unnecessary variables not related to movement type, but kind of user data ('X','user_name', timestamps) and
# related to algorithm of original research ('new_window', 'num_window')
training <- data[inTrain, -c(1:7)]
testing <- data[-inTrain, -c(1:7)]

# Basic exploration
dim(training)

# Excluding columns with a lot of NAs
naNums <- sapply(training, function(x) sum(is.na(x)))
table(naNums)

# As it may be seen from the table above only 57 variables don't have NAs at all, but all the rest have almost all NAs, so
training <- training[, naNums == 0]

# Removing zero variance covariates

nsv <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[,!nsv$nzv]

# Fitting model and evaluating
modFit <- randomForest(classe ~ ., data=training)
prediction <- predict(modFit, testing, type="response")
table(prediction == testing$classe)

# *https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm

# Testing for part 2 of assignment
test_data <- read.csv("pml-testing.csv", na.strings = c("", "#DIV/0!", "NA"), header = T)
prediction2 <- predict(modFit, test_data)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(prediction2)


