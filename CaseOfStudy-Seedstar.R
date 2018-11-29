## Seedstars Case of Study 

library('gsheet')
library('ggplot2')
# Using this open source gsheet package to parse the google spreadsheet 
# More information at: https://github.com/maxconway/gsheet
getwd()
#test <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1YT0-PTttD-k1y8KHXwfHm5rkfOucIj-eGLvSiFOCchY/edit#gid=0')

dataset <- read.csv('R/case-of-study.csv', na.strings = c(""," ",NA))
is.na(dataset) <- dataset == "NA"

#test2<- read.csv('LoanStats3a.csv')
dataset$Education_Status <- as.factor(dataset$Education_Status)
dataset$Education_Status <- as.factor(dataset$Education_Status)
dataset$Amount <- as.numeric(dataset$Amount)
dataset$Gender <- as.factor(dataset$Gender)
dataset$Reason <- as.factor(dataset$Reason)
dataset$label <- as.factor(dataset$label)
dataset$Reason <- as.factor(dataset$Reason)
dataset$referrals <- as.factor(dataset$referrals)
dataset$Name <- as.character(dataset$Name)
dataset$Campaign <- as.character(dataset$Campaign)
dataset$No_Children <- as.factor(dataset$No_Children)
barplot(table(dataset$label), xlab = "Label", ylab = "Total of Customers", main = "Barplot of the Total Customers per Label")
png('Barplot-Total-Customer-Label.png')
#write.csv(test, file="case-of-study.csv")

str(dataset)
table(dataset$label)

ggplot(dataset, aes(x = Education_Status, fill = factor(label))) + facet_wrap(~Gender) +  stat_count(width = 0.5) + xlab("Education Status") + ylab("Total Count") + labs(fill="label")

ggplot(dataset, aes(x = Reason, fill = label)) + geom_bar() + facet_wrap(~Education_Status) + xlab("Reason") + ylab("Total Count") + labs(fill = "label")

ggplot(dataset, aes(x = No_Children, fill = label)) + geom_bar() + facet_wrap(~Education_Status) + xlab("Number of Children") + ylab("Total Count") + labs()

ggplot(dataset, aes(x = referrals, fill = label)) + geom_bar() + facet_wrap(~Education_Status) + xlab("Referrals") + ylab("Total Count") + labs(fill = "label")

males <- dataset[which(dataset$Gender == "male"),]

ggplot(males, aes(x = Education_Status, fill = factor(label))) +  stat_count(width = 0.5) + xlab("Education Status") + ylab("Total Count") + labs(fill="label")

females <- dataset[which(dataset$Gender == "female"),]

ggplot(males, aes(x = Education_Status, fill = factor(label))) +  stat_count(width = 0.5) + xlab("Education Status") + ylab("Total Count") + labs(fill="label")


ggplot(dataset, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Label Status by Gender") + facet_wrap(~Gender) + scale_y_continuous(limits = c(0,250)) 

males_label_1 <- dataset[which(dataset$Gender == "male" & dataset$label == 1),]
males_label_0 <- dataset[which(dataset$Gender == 'male' & dataset$label == 0),]
females_label_1 <- dataset[which(dataset$Gender == 'female' & dataset$label == 1), ]
females_label_0 <- dataset[which(dataset$Gender == 'female' & dataset$label == 0), ]
female_na_label1 <- dataset[which(dataset$Gender == 'female' & dataset$label == 1 & dataset$Education_Status == 1 & is.na(dataset$Reason)),]

ggplot(males_label_1, aes(x = ))

ggplot(males_label_1, aes(x = Education_Status)) +  stat_count(width = 0.5) + xlab("Education Status") + ylab("Total of Males with Label 1") + labs(fill="label")  + ggtitle("Barplot of the Total Male with Label equals '1' per Education Status by Reason") + facet_wrap(~Reason)

ggplot(males_label_0, aes(x = Education_Status)) +  stat_count(width = 0.5) + xlab("Education Status") + ylab("Total of Males with Label 0")  + ggtitle("Barplot of the Total Male with Label equals '0' per Education Status by Reason") + facet_wrap(~Reason)

ggplot(females_label_1, aes(x = Education_Status)) +  stat_count(width = 0.5) + xlab("Education Status") + ylab("Total of Females with Label 1") + labs(fill="label")  + ggtitle("Barplot of the Total Females with Label equals '1' per Education Status by Reason") + facet_wrap(~Reason)


ggplot(females_label_0, aes(x = Education_Status)) +  stat_count(width = 0.5) + xlab("Education Status") + ylab("Total of Females with Label 0")  + ggtitle("Barplot of the Total Females with Label equals '0' per Education Status by Reason") + facet_wrap(~Reason)

ggplot(males_label_1, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Males with Label '1' by Education Status") + scale_y_continuous(limits = c(0,50)) + facet_wrap(~Education_Status)

ggplot(males_label_0, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Males with Label '0' by Education Status") + scale_y_continuous(limits = c(0,100)) + facet_wrap(~Education_Status)

ggplot(males_label_1, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Males with Label '1' by No of Children") + scale_y_continuous(limits = c(0,50)) + facet_wrap(~No_Children)

ggplot(males_label_0, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Males with Label '0' by No of Children") + scale_y_continuous(limits = c(0,75)) + facet_wrap(~No_Children)

ggplot(males_label_1, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Males with Label '1' by Reason") + scale_y_continuous(limits = c(0,50)) + facet_wrap(~Reason)

ggplot(males_label_0, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Males with Label '0' by Reason") + scale_y_continuous(limits = c(0,50)) + facet_wrap(~Reason)


ggplot(females_label_1, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Females with Label equal '1' by Education Status") + scale_y_continuous(limits = c(0,50)) + facet_wrap(~Education_Status)

ggplot(females_label_0, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Females with Label equal '0' by Education Status") + scale_y_continuous(limits = c(0,100)) + facet_wrap(~Education_Status)

ggplot(females_label_1, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Females with Label '1' by No of Children") + scale_y_continuous(limits = c(0,50)) + facet_wrap(~No_Children)

ggplot(females_label_0, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Females with Label '0' by No of Children") + scale_y_continuous(limits = c(0,75)) + facet_wrap(~No_Children) 

ggplot(females_label_1, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Females with Label '1' by Reason") + scale_y_continuous(limits = c(0,40)) + facet_wrap(~Reason)

ggplot(females_label_0, aes(x = label, y = Amount)) + geom_boxplot() + ggtitle("Boxplot of the Amount per Females with Label '0' by Reason") + scale_y_continuous(limits = c(0,75)) + facet_wrap(~Reason) 

## Prediction Model 

library(randomForest)
library(caTools)

str(dataset)
dim(dataset)
apply(dataset, 2, function(x) length(unique(x)))

##set.seed(123)
ind <- sample.split( Y = dataset$label, SplitRatio = 0.8)
train_dataset <- dataset[ind,]
test_dataset <- dataset[!ind,]
train_dataset$Age <- na.roughfix(train_dataset$Age)
train_dataset$Name <- NULL
train_dataset$loan_ident <-NULL
train_dataset <- na.omit(train_dataset)
cleandata <- dataset[complete.cases(dataset),]


train_dataset$Campaign <- NULL


## Fitting the model 
colSums(is.na(train_dataset))
modelRandom <- randomForest(label~., data = train_dataset, importance=TRUE,proximity=TRUE, mtry = 3)
modelRandom
importance(modelRandom)
varImpPlot(modelRandom, main = "Variables Importance")
library(pROC)


## Prediction

PredictionsWithProbs <- predict(modelRandom, test_dataset, type = 'prob')
auc <- auc(test_dataset$label, PredictionsWithProbs[,2])
plot(roc(test_dataset$label, PredictionsWithProbs[,2]), main = 'ROC Curve',ylab ='True positive rate' ,xlab = 'False positive rate')
PredictionsWithClass <- predict(modelRandom, test_dataset, type = 'class')
t <- table(predictions = PredictionsWithClass, actual = test_dataset$label)
t

## Accuracy metric

sum(diag(t)/sum(t))