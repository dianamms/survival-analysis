
library(glmnet)
library(survival)
library(Hmisc)
library(readxl)
library(pec)
library(randomForestSRC)
library(ggRandomForests)
library(caret)
library(pec)


# The dataset must have a Status column with values of 0 or 1 that tells if the corresponding patient y is indicative of a 
# failure time or right censoring time and a Survival Time column (OS).

train <- data.frame(read_excel("Data_clean.agegr.trn.xlsx"))
xtrn <- as.matrix(train[,5:1031],use.names=FALSE)
ytrn <- Surv(train$OS, train$Status.OS)

test <- data.frame(read_excel("Data_clean.agegr.tst.xlsx"))
xtst<- as.matrix(test[,5:1031], use.names=FALSE)
ytst <- Surv(test$OS, test$Status.OS)

head(train)
dim(train)

par(mfrow=c(2,2))
hist(train$Grade)
hist(train$Status.OS)
hist(test$Grade)
hist(test$Status.OS)

cox <- glmnet(xtrn, ytrn, family="cox", alpha=1)

plot(cox)

cv.cox <- cv.glmnet(xtrn, ytrn, family="cox", alpha=1)
plot(cv.cox)

pred.cox<- predict(cox, newx = xtst, type = "response", s = cv.cox$lambda.min)
cindex<- rcorr.cens(pred.cox*-1, ytst)
cindex

Coefficients <- coef(cox, s = cv.cox$lambda.min)  
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index
names(train[,Active.Index+4])
Active.Coefficients

df<-train[,Active.Index+4]
y<-train[,2:3]
dff<-cbind(y,df)

head(dff)

model <- rfsrc(Surv(OS,Status.OS) ~ ., dff,
                    nsplit = 10, na.action = "na.omit",
                    importance = FALSE,ntree = 120, nodesize = 3, block.size = 2,
                    splitrule="random")
print(model)



pred<- predict(model, newdata = test,
                     na.action = "na.omit",importance=TRUE)

print(pred)

imp.rsf <- sort(model$importance, decreasing = T)
plot(gg_vimp(model))
