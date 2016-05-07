library(plyr)
library(Cubist)
require(mlbench)
library(rpart)
library(ggplot2)
library(car)
library(caret)


f <-file("https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data", open="r")
momentum <- read.table(f, dec=".", header=F)
close(f)
colnames(momentum) <- c("mpg", "cylinders","displacement", "horsepower", "weight", "acceleration", 
                        "model year", "origin","car name")
head(momentum)
summary(momentum)
momentum$horsepower <- as.numeric(as.character(momentum$horsepower))
fac <- c("cylinders", "model year", "origin") 
momentum[fac] <- lapply(momentum[fac], as.factor) 

set.seed(123)
fit.hp<-rpart(horsepower[!is.na(horsepower)]~cylinders+displacement+weight+acceleration
              +`car name`,data=momentum[!is.na(momentum$horsepower),],method='anova')
momentum$horsepower[is.na(momentum$horsepower)]<-predict(fit.hp,momentum[is.na(momentum$horsepower),])
momentum$horsepower <- round(momentum$horsepower, digits = 1)
target <- as.data.frame(momentum$mpg)

pairs(~mpg + displacement + horsepower + weight + acceleration, data = momentum, col="blue")
ggplot(data= momentum, aes(x= displacement, y= mpg, color= displacement))+ geom_point()
ggplot(data= momentum, aes(x= cylinders, y= mpg, color= cylinders))+ geom_boxplot()
ggplot(data= momentum, aes(x= cylinders, y= displacement, fill= cylinders))+ geom_boxplot()


momentum$`car name`[1]
strsplit(as.character(momentum$`car name`)[1], " +")[[1]][1]
momentum$brand <- sapply(as.character(momentum$`car name`), FUN=function(x) {strsplit(x, " +")[[1]][1]})


momentum$brand <- as.factor(momentum$brand)
momentum$brand <-recode(momentum$brand, "c('maxda', 'mazda')='mazda'; c('chevroelt', 'chevrolet', 'chevy')='chevrolet';
                        c('toyouta', 'toyota')='toyota'; c('mercedes', 'mercedes-benz')='mercedes';
                        c('volkswagen', 'vw', 'vokswagen')='volkswagen';c('mercury', 'capri')='mercury';
                        c('opel','buick')='buick'")

tapply(momentum$horsepower, momentum$cylinders, FUN=mean)

momentum$efficiency[momentum$horsepower < 80 & momentum$weight <2500] <-"Best"
momentum$efficiency[momentum$horsepower >= 160 & momentum$weight >=4000] <-"Worst"
momentum$efficiency[!momentum$efficiency %in% c("Best", "Worst")] <-"Acceptable"
momentum$efficiency <- as.factor(momentum$efficiency)
#now validating what we just did with our assumption.
tapply(momentum$mpg, momentum$efficiency, FUN=mean)

momentum<- subset(momentum, brand!="hi")

full <- momentum[,-9]

aggregate(data.frame(full$horsepower, full$displacement, full$mpg, full$weight, full$mpg), by=list(full$cylinders,
                                                                                                   full$efficiency),FUN="mean")


mpg_mean <- ddply(full, .(cylinders, efficiency), summarize, Mpg_Mean=mean(mpg))
X_full <- merge(full, mpg_mean, by=c("cylinders", "efficiency"))


X_full$flag_high <- ifelse(X_full$mpg > X_full$Mpg_Mean,1,0)
eff_high <- ddply(X_full, .(cylinders, efficiency), summarize, Eff_High=mean(flag_high))
X_prefinal <- merge(X_full, eff_high, by=c("cylinders", "efficiency"))

X_prefinal <- subset(X_prefinal, select=-c(flag_high))

colnames(X_prefinal)[8] <- "model_year"

## 75% of the sample size
smp_size <- floor(0.75 * nrow(X_prefinal))

## set the seed to make partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(X_prefinal)), size = smp_size)

train <- X_prefinal[train_ind, ]
test <- X_prefinal[-train_ind, ]

target <- train$mpg
train$mpg <- NULL
cube <- train(x = train, y = target, "cubist", 
              tuneGrid = expand.grid(.committees = c(1, 10, 50, 100),.neighbors =c(0, 1, 5, 9)),
              trControl = trainControl(method = 'cv'))
print(cube)
mtPred<-predict(cube,test)
#rmse
sqrt(mean((mtPred - test$mpg)^2))
#r-square
(cor(mtPred,test$mpg)^2)*100
