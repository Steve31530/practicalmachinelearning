####Background

#Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data
#about personal activity relatively inexpensively. These type of devices are part of the quantified self movement
#- a group of enthusiasts who take measurements about themselves regularly to improve their health, 
#to find patterns in their behavior, or because they are tech geeks. 
#One thing that people regularly do is quantify how much of a particular activity they do, 
#but they rarely quantify how well they do it. In this project, your goal will be to use data from 
#accelerometers on the belt, forearm, arm, and dumbell of 6 participants. 
#They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 
#More information is available from the website here: http://groupware.les.inf.puc-rio.br/har 
#(see the section on the Weight Lifting Exercise Dataset).


####Data Manipulation

#Data import
        #We first load the training data set from the course website.

        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                      "train.csv")
        
        train <- read.csv("train.csv", na.strings=c("NA","#DIV/0!", ""))
        
        #then the test set
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                      "final_test.csv")
        
        final_test <- read.csv("final_test.csv", na.strings=c("NA","#DIV/0!", ""))

#Cleaning Data
        
        #The original data sets has many fields which are blank or contain errors, 
        #In the previous stage the na.strings argument was used to transform all unrecognized characters into NA. 
        #Is the colSum is NA then that column is also eliminated as it would affect the prediction. 
        
        train <- train[,colSums(is.na(train)) == 0]
        final_test <- final_test[,colSums(is.na(final_test)) == 0]
        
        #Finally, the data sets contain many superfluous fields that will not impact on the prediction using the. 
        #the “classe” field. The "classe" variable in the training set identifies how they did the exercise. 
        #Identify fields that contain the four strings of interest: "arm", "dumbell", "belt" and "classe".
        #Belt, arm, dumbbell, and forearm variables that do not have any missing values in the test dataset 
        #will be predictor candidates.
        
        train <- subset(train, select = c(
                grep("arm",colnames(train)),
                grep("dumbell",colnames(train)),
                grep("belt",colnames(train)),
                grep("classe",colnames(train))
        )
        )
        
        final_test <- subset(final_test, select = c(
                grep("arm",colnames(final_test)),
                grep("dumbell",colnames(final_test)),
                grep("belt",colnames(final_test)),
                grep("classe",colnames(final_test))
        )
        )  
        
        #Check that outcome is a factor        
        class(train$classe)

####Partitioning Data
        
        #Before we fit our model, we use the caret package to partition the training data set into sub-training and sub-testing sets for the purpose of cross-validation.
        
        library(caret)
        
        #Check for near zero variance.
        nzv <- nearZeroVar(train, saveMetrics=TRUE)
        if (any(nzv$nzv)) nzv else message("No variables with near zero variance")
        
        set.seed(42)
        inTrain <- createDataPartition(y=train$classe,
                                       p=0.7, list=FALSE)
        training <- train[inTrain,]
        testing <- train[-inTrain,]
        
        #trainingModel <- train(classe ~ ., data=training, method="rf")
        
        
        trainingModel<-train(classe~.,data=training,method="rf",
                        trControl=trainControl(method="cv",number=5),
                        prox=TRUE,allowParallel=TRUE)
        
        trainingModel
        
        #Random Forest 
        
        #13737 samples
        #39 predictor
        #5 classes: 'A', 'B', 'C', 'D', 'E' 
        
        #No pre-processing
        #Resampling: Cross-Validated (5 fold) 
        #Summary of sample sizes: 10990, 10990, 10990, 10989, 10989 
        #Resampling results across tuning parameters:
                
        #        mtry  Accuracy   Kappa    
        #2    0.9879158  0.9847133
        #20    0.9866054  0.9830561
        #39    0.9814369  0.9765198
        
        #Accuracy was used to select the optimal model using  the largest value.
        #The final value used for the model was mtry = 2. 
        
####Evaluate the model on the training dataset
        
        trainingModel
        prediction <- predict(trainingModel, training)
        
        confusionMatrix(prediction, training$classe)
        
        #Confusion Matrix and Statistics
        
        #          Reference
        #Prediction    
        #     A    B    C    D    E
        #A 3906    0    0    0    0
        #B    0 2658    0    0    0
        #C    0    0 2396    0    0
        #D    0    0    0 2252    0
        #E    0    0    0    0 2525
        
        #Overall Statistics
        
        #Accuracy : 1          
        #95% CI : (0.9997, 1)
        #No Information Rate : 0.2843     
        #P-Value [Acc > NIR] : < 2.2e-16  
        
        #Kappa : 1          
        #Mcnemar's Test P-Value : NA         
        
        #Statistics by Class:
        
        #Class: A Class: B Class: C Class: D Class: E
        #Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
        #Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
        #Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
        #Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
        #Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
        #Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
        #Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
        #Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
        
#### Final Prediction on Testing Data
#Our prediction on the final testing data is as follows.
        
        predict(trainingModel, final_test, type = "raw")
        
        #[1] B A B A A E D B A A B C B A E E A B B B
        #Levels: A B C D E
        