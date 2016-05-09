#Santander Customer Satisfaction

san <- read.csv("santander.csv", header = TRUE) #Training data
santest <- read.csv("test.csv", header = TRUE) #Testing data
dim(san); dim(santest)
alldata <- rbind(san[, -371], santest) #All data
names(alldata)
table(san$TARGET)

#資料清理================================================================================

#重新命名欄位

var <- sort(c(15, 16, 3, 39,41, 1, 2, 5, 6, 8, 12, 13, 14, 17, 18, 19, 20, 24, 
         25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 37, 39, 40, 41, 44, 46))
varlabel <- names(alldata)
varlabel <- gsub("efect", "Cash", varlabel)
varlabel <- gsub("meses", "Months", varlabel)
varlabel <- gsub("hace", "Ago", varlabel)
varlabel <- gsub("delta", "Differ", varlabel)
varlabel <- gsub("imp", "Amount", varlabel)
varlabel <- gsub("op", "Transaction", varlabel)
varlabel <- gsub("saldo", "Balance", varlabel)
varlabel <- gsub("num", "Num", varlabel)
varlabel <- gsub("corto", "ShortT", varlabel)
varlabel <- gsub("medio", "MidT", varlabel)
varlabel <- gsub("largo", "LongT", varlabel)
varlabel <- gsub("var", "Var", varlabel)
varlabel <- gsub("reemb", "Refund", varlabel)
varlabel <- gsub("comer", "Comercial", varlabel)
varlabel <- gsub("ind", "Ind", varlabel)
varlabel <- gsub("trasp", "Transfer", varlabel)
varlabel <- gsub("venta", "Sale", varlabel)
varlabel <- gsub("aport", "Contribution", varlabel)
varlabel <- gsub("amort", "Amortization", varlabel)
varlabel <- gsub("med", "Mean", varlabel)
varlabel <- gsub("in", "In", varlabel)
varlabel <- gsub("out", "Out", varlabel)

names(alldata) <- varlabel
names(san)[1:370] <- varlabel
names(santest)[1:370] <- varlabel

##移除常數欄位
sdt <- 0
for(i in 1:370){
        sdt[i] <- sd(alldata[, i])
}
rm_col <- which(sdt == 0)
alldata <- alldata[, -rm_col]
dim(alldata)

##移除重複欄位（可以不做這項）
library(digest)
alldata <- alldata[, !duplicated(apply(alldata, 2, digest))]
dim(alldata)
names(alldata)

##移除接近0的欄位
library(caret)
nsv <- nearZeroVar(alldata, saveMetrics = TRUE)
nsv
alldata <- alldata[, -(which(nsv$nzv == TRUE))]
dim(alldata)

##檢查多元共線性
cor <- round(cor(alldata), 2)
corM <- as.matrix(cor) #cor()求相關矩陣
corM[lower.tri(corM, diag = TRUE)] <- NA
library(reshape2)
highName <- subset(melt(corM, na.rm = TRUE), value > .900) 
dim(highName)

colNum <- matrix(rep(0, 40), c(20, 2))
for(i in 1:20){
        for(j in 1:2){
                colNum[i, j] <- which(names(alldata) == highName[i, j])        
        }
}
colNum

alldata <- alldata[, -c(4, 8, 11, 12, 14, 16, 17, 20, 22, 30, 31, 32, 39, 41, 44, 47)]
dim(alldata)

names(alldata)
alldata <- alldata[, -1]

hist(alldata$Var15) #Var15應為年齡

names(alldata)[1] <- "Age"

training <- alldata[c(1:76020), ]
training$TARGET <- san$TARGET #TARGET塞回來
testing <- alldata[c(76021:dim(alldata)[1]), ]
testing$ID <- santest[, 1] #ID塞回來

dim(training); dim(testing)
names(testing)

lambdaf <- function(x) {
        n <- sum(x)
        x <- matrix(as.numeric(x), dim(x))
        SumR <- sum(apply(x, 1, max))
        SumC <- sum(apply(x, 2, max))
        MaxC <- max(colSums(x))
        MaxR <- max(rowSums(x))
        
        RClambda <- (SumC - max(rowSums(x))) / (n - MaxR)
        CRlambda <- (SumR - MaxC) / (n - MaxC)
        Symmetrylambda <- (SumR + SumC - MaxC - MaxR) / ((2 * n) - MaxC - MaxR)
        
        Lambdalist <- list(RClambda, CRlambda, Symmetrylambda)
        names(Lambdalist) <- c("RClambda", "CRlambda", "Symmetrylambda")
        
        Lambdalist
}

names(training)
PRElist <- 0
for(i in 3:8){
        PRElist[i - 2] <- lambdaf(matrix(table(training[, i], training$TARGET), ncol = 2))[[1]]
}
PRElist
names(training)[3:8]

PreInd_Var10_ult1 <- lambdaf(matrix(table(training[, 19], training$TARGET), ncol = 2))[[1]]
PreInd_Var43_emit_ult1 <- lambdaf(matrix(table(training[, 20], training$TARGET), ncol = 2))[[1]]
PreInd_Var43_recib_ult1 <- lambdaf(matrix(table(training[, 21], training$TARGET), ncol = 2))[[1]]

Pre <- data.frame(Var = names(training)[c(3:8, 19:21)], #考慮只保留Ind_Var5和Ind_Var30
                  Pre = c(PRElist, PreInd_Var10_ult1, PreInd_Var43_emit_ult1, PreInd_Var43_recib_ult1))
Pre

training <- training[, -c(4, 5, 7, 8, 19, 20, 21)]
testing <- testing[, -c(4, 5, 7, 8, 19, 20, 21)]
dim(training); dim(testing)

names(training)

#進一步研究features
summary(training[which(training$Amount_Transaction_Var39_Comercial_ult3 != 0), 2])
sum(training$Amount_Transaction_Var39_Comercial_ult3 == 0)

      
#資料分組================================================================================

##訓練組、測試組

library(caret)
inTrain <- createDataPartition(y = training$TARGET, p = .8, list = FALSE)
train <- training[inTrain, ]
test <- training[-inTrain, ]

dim(train)
dim(test)
table(train$TARGET)

##解決資料不平衡問題

library(ROSE)
set.seed(19931202)
baltrain <- ovun.sample(TARGET ~ ., data = train, method = "both")$data
dim(baltrain)
table(baltrain$TARGET)


#模型建立================================================================================

#gbm法================================================================================

install.packages("caret")

library(gbm)
library(caret)
library(pROC)

ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
m_gbm <- train(factor(baltrain$TARGET, labels = c("X0","X1")) ~ ., data = baltrain, method = "gbm", metric = "ROC", trControl = ctrl, preProc = c("center", "scale"))
table(baltrain$TARGET)
table(factor(baltrain$TARGET, labels = c("X0","X1")))

#m_gbm <- train(baltrain[, -28], factor(baltrain[, 28]), method = "gbm", trControl = ctrl, metric = "ROC", preProc = c("center", "scale"))

gbm.predict = predict(m_gbm, test[, -28])
gbm.predict <- ifelse(gbm.predict == "X0", 0, 1)
table(gbm.predict, test$TARGET)

library(ROCR)
auc.perf <- performance(prediction(gbm.predict, test$TARGET), measure = "auc")
auc.perf #計算AUC
roc <- performance(prediction(gbm.predict, test$TARGET), "tpr", "fpr")
plot(roc, main = "ROC chart")

result <- predict(m_gbm, testing2)
result <- ifelse(result == "X0", 0, 1)
table(result)

answer <- data.frame(ID = testing2[, "ID"], TARGET = result)
head(answer)
write.csv(answer, file = "result.csv", row.names= FALSE)

#================================================================================

#glmnet法================================================================================

##LASSO

library(useful)
library(glmnet)
sanX <- build.x(TARGET ~ . -1, data = baltrain, contrasts = FALSE) #建立預測函數矩陣
head(sanX)
sanY <- build.y(TARGET ~ . -1, data = baltrain) #建立反應變數
head(sanY)

set.seed(41213)
sanCV1 <- cv.glmnet(x = sanX, y = sanY, family = "binomial", type.measure = "auc", nfold = 10, alpha = 0) #cv.glmnet()可以自動交叉驗證的值，預設alpha為1（LASSO）
pred <- predict(sanCV1, data.matrix(test[, -28]), s = sanCV1$lambda.1se, type = "response")
pred2 <- ifelse(pred > .60, 1, 0)
table(pred2, test[, 28])

library(ROCR)
auc.perf <- performance(prediction(pred2, test[, "TARGET"]), measure = "auc")
auc.perf #計算AUC
roc <- performance(prediction(pred2, test[, "TARGET"]), "tpr", "fpr")
plot(roc, main = "ROC chart")


sanX <- build.x(TARGET ~ . -1, data = baltrain, contrasts = FALSE)
sanY <- build.y(TARGET ~ . -1, data = baltrain)
sanCV2 <- cv.glmnet(x = sanX, y = sanY, family = "binomial", type.measure = "auc", nfold = 10, alpha = 0)

result <- predict(sanCV1, data.matrix(testing2[, -28]), s = sanCV1$lambda.1se, type = "response")
result2 <- ifelse(result > .60, 1, 0)
table(result2)

answer <- data.frame(ID = testing2[, "ID"], TARGET = result2)
names(answer) <- c("ID", "TARGET")
head(answer)
write.csv(answer, file = "result.csv", row.names = FALSE)

sanCV1$lambda.min
sanCV1$lambda.1se #最佳化lambda值
plot(sanCV1)

coef(sanCV1, s = "lambda.1se") #點代表沒被選中的變數，LASSO會把高度相關的變數排除掉
plot(sanCV1$glmnet.fit, xvar = "lambda")
abline(v = log(c(sanCV1$lambda.min, sanCV1$lambda.1se)), lty = 2)


#NNet法================================================================================

library(nnet)
scale01 <- function(x) { #資料歸一化，NNet常見的前置處理，將所有資料轉化為[0, 1]之間的數，取消個維度數據間數量級的差別
        ncol = dim(x)[2] - 1
        nrow = dim(x)[1]
        new = matrix(0, nrow, ncol)
        for(i in 1:ncol) {
                max = max(x[, i])
                min = min(x[, i])
                for(j in 1:nrow) {
                        new[j, i] = (x[j, i] - min) / (max - min)
                }
        }
        new
}


set.seed(1202)
baltrain2 <- baltrain
baltrain2[, 1:27] <- scale01(baltrain[, ])
baltrain2[, 28] <- factor(baltrain2[, 28])

library(caret)
set.seed(1202)
folds <- createFolds(y = baltrain2$TARGET, k = 10, list = TRUE, returnTrain = TRUE)

acc <- {}
for(i in 1:10){ #10層交叉驗證
        r = 1/max(abs(baltrain2[folds[[i]], 1:27])) #參數rang的變化範圍
        set.seed(1202)

        model1 <- nnet(TARGET ~., data = baltrain2[folds[[i]], ], #rang為初始隨機權重的範圍
                       size = 20, rang = r, decay = 5e-4, maxit = 500)

        pred <- predict(model1, baltrain2[-folds[[i]], ], type = "class")
        acc <- sum(diag(table(pred, baltrain2[-folds[[i]], 28]))) / dim(baltrain2[-folds[[i]], ])[1]
}
mean(acc)

r = 1/max(abs(baltrain2[1:27]))
model12 <- nnet(TARGET ~., data = baltrain2, size = 10, rang = r, decay = 5e-4, maxit = 500)
pred <- predict(model12, test[, -28], type = "class") #用在test set試驗
table(pred, test$TARGET)
acc2 <- sum(diag(table(result, test$TARGET))) / dim(test)[1]
acc2

library(ROCR)

auc.perf <- performance(prediction(as.integer(pred), test[, "TARGET"]), measure = "auc")
auc.perf #計算AUC
roc <- performance(prediction(as.integer(pred), test[, "TARGET"]), "tpr", "fpr")
plot(roc, main = "ROC chart")


set.seed(1202)
r = 1/max(abs(baltrain2[1:35]))
model2 <- nnet(TARGET ~., data = baltrain2, #rang為初始隨機權重的範圍
               size = 5, rang = r, decay = 5e-4, maxit = 200)
result2 <- predict(model12, testing, type = "class")
table(result2)

answer <- data.frame(ID = testing[, 1], TARGET = result2)
write.csv(answer, file = "result.csv", row.names= FALSE)

#================================================================================

#Logistic Regression法================================================================================

library(caret)
set.seed(1202)
folds <- createFolds(y = baltrain$TARGET, k = 10, list = TRUE, returnTrain = TRUE)
dim(baltrain)

library(ROCR)
auc <- {}
for(i in 1:10) {
        LG <- glm(TARGET ~ ., data = baltrain[folds[[i]], ], family = binomial)
        pre_LG <- predict(LG, baltrain[-folds[[i]], 1:27], type = "response")
        auc[i] <- auc(roc(as.integer(ifelse(pre_LG >= 0.60, 1, 0)), as.integer(baltrain2[-folds[[i]], 28])))
}
mean(auc)

LG2 <- glm(TARGET ~., data = baltrain, family = binomial)
pre_LG2 <- predict(LG2, test[, -28], type = "response")
result <- ifelse(pre_LG2 >= 0.60, 1, 0)
auc(roc(as.integer(result), as.integer(test[, 28])))

result <- predict(LG2, testing, type = "response")
result <- ifelse(result >= 0.60, 1, 0)
table(result)
cbind(table(result)[2]/dim(testing)[1], table(san$TARGET)[2]/dim(san)[1])

disat <- cbind(table(result)[2], table(san$TARGET)[2])
tot <- cbind(dim(testing)[1], dim(san)[1])
prop.test(disat, tot)

answer <- data.frame(ID = testing[, 1], TARGET = result)
write.csv(answer, file = "result.csv", row.names= FALSE)

acc = {}
for(t in seq(.1, .95, .05)){
        pre = ifelse(pre_LG > t, 1, 0)
        tab = table(test$TARGET, pre)
        acc = c(acc, sum(diag(tab))/dim(test)[1])
}
acc

#================================================================================


#kkNN法================================================================================

library(caret)
set.seed(1202)
folds <- createFolds(y = baltrain$TARGET, k = 10, list = TRUE, returnTrain = TRUE)

baltrain2 <- baltrain
baltrain2$TARGET <- factor(baltrain2$TARGET)

str(baltrain2)

auc <- 0
library(kknn)
for(i in 1:10){
        fit_pre_kknn <- kknn(TARGET ~., baltrain2[folds[[1]], 1:28], baltrain2[-folds[[1]], -28], k = 1501)
        fit <- fitted(fit_pre_kknn)
        table(fit, baltrain2[-folds[[1]], "TARGET"])
        auc[i] <- auc(roc(as.integer(fit), as.integer(baltrain2[-folds[[1]], 28])))
}
mean(auc)

m_kknn <- kknn(TARGET ~., baltrain2, test[, -28], k = 1501)
pred <- fitted(m_kknn)
table(pred, test$TARGET)

library(AUC)
auc(roc(as.integer(pred), as.integer(test$TARGET)))
library(ROCR)
roc <- performance(prediction(as.integer(pred), test[, "TARGET"]), "tpr", "fpr")
plot(roc, main = "ROC chart")

pred <- kknn(TARGET ~., baltrain2, testing[, -28], k = 1501)
predfit <- fitted(pred)
table(predfit)

answer <- data.frame(ID = testing$ID, TARGET = predfit)
head(answer)
write.csv(answer, file = "result.csv", row.names= FALSE)


#================================================================================

#Random Forest================================================================================

library(randomForest)

m_rf <- randomForest(as.factor(TARGET) ~., data = training, ntree = 1000, importance = TRUE)
importance(m_rf)
pred <- predict(m_rf, testing)
table(pred)

#not finished


#================================================================================

#EM================================================================================

library(mclust)
fit_EM <- Mclust(baltrain[, ])
summary(fit_EM, parameters = TRUE)
countries_BIC <- mclustBIC(countries[, -1])
countries_BICsum <- summary(countries_BIC, data = countries[, -1])
countries_BICsum
countries_BIC
plot(countries_BIC, G = 1:7, col = "black")
names(countries_BICsum)
mclust2Dplot(countries[, -1], classification = countries_BICsum$classification, 
             parameters = countries_BICsum$parameters, col = "black")
countries_Dens <- densityMclust(countries[, -1])
plot(countries_Dens, countries[, -1], col = "grey", nlevels = 55)
plot(countries_Dens, type = "persp", col = grey(.8))

#================================================================================


#XGB================================================================================

baltrain2 <- sparse.model.matrix(TARGET ~ ., data = baltrain)
baltrain3 <- xgb.DMatrix(data = baltrain2, label = baltrain$TARGET)
watchlist <- list(train = baltrain3)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.02,
                max_depth           = 6,
                subsample           = 0.9,
                colsample_bytree    = 0.85
)

clf <- xgb.train(   params              = param, 
                    data                = baltrain3, 
                    nrounds             = 500, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

testing2 <- testing
testing2$TARGET <- -1
testing2 <- sparse.model.matrix(TARGET ~ ., data = testing2)

pred <- predict(clf, testing2)
preds <- ifelse(pred > 0.83, 1, 0)
table(preds)

submission <- data.frame(ID = testing$ID, TARGET = preds)
write.csv(submission, "result2.csv", row.names = F)


#================================================================================

model <- glm(TARGET ~ var15 + imp_op_var41_ult1 + ind_var12_0 + ind_var13
            + ind_var30 + num_var30 + num_var42_0 + saldo_var30 + var36
            + ind_var10cte_ult1 + num_var22_ult1 + num_var22_ult3
            + num_meses_var5_ult3 + num_var45_ult1 + var38, data = baltrain)

testset <- read.csv("test.csv", header = TRUE)
pred <- predict(model, baltrain[-folds[[1]], ])
pred[which(pred >= 0.5)] = 1
pred[which(pred < 0.5)] = 0
table(pred, baltrain[-folds[[1]], "TARGET"])
sum(pred == baltrain[-folds[[1]], "TARGET"], na.rm = TRUE) / dim(baltrain[-folds[[1]], ])[1]

pred <- predict(model, testset)
pred[which(pred >= 0.5)] = 1
pred[which(pred < 0.5)] = 0

answer <- pred

answer <- data.frame(ID = testset[, 1], TARGET = pred)
write.csv(answer, file = "result.csv", row.names= FALSE)


#建模型預測

accuracy <- 0
for(i in 1:10) {
        train <- baltrain[folds[[i]], ]
        test <- baltrain[-folds[[i]], ]
        model <- xgboost(train[, -54], train[, 54], kernel = "radial", gamma = if(is.vector(train[, -54])) 1 else 1/ncol(train[, -54]))
        prediction <- predict(model, test)
        accuracy[i] <- sum(prediction == test$TARGET) / nrow(test)
}
avgacc <- mean(accuracy)



library(Hmisc)
describe(san)



table(san[, dim(san)[2]])

cal <- 0
for(i in 1:dim(san)[1]) {
        cal[i] <- sum(san[i, ] == 0)
}


#不要的=====

summary(san[, c(3, 4)])
head(san[which(san[, c(3, 4)] != 0), c(3, 4)], 10)
t.test(san[, 3], san[, 4], mu = 0, alternative = "greater", var.equal = TRUE, paired = T, conf.level = .95)
all(san[, 3] >= san[, 4]) #變數4可能是變數3消費扣款之後的選項
san$ComExp <- san[, 3] - san[, 4] #新創一個變數代表「商業消費」


summary(san[, c(5, 6)]) #op可能是「交易」，imp可能是「金額」
t.test(san[, 5], san[, 6], mu = 0, alternative = "greater", var.equal = TRUE, paired = T, conf.level = .95)
head(san[which(san[, c(5, 6)] != 0), c(5, 6)], 10)
all(san[, 6] >= san[, 5]) #變數6大於等於變數5，差距可大可小，ult可能指「最終值」

head(san[which(san[, 3] != 0 & san[, 3] != san[, 4]), c(3:6)], 10)

summary(san[, c(7, 17, 42)]) #只淘汰17，都跟var5有關，ind指「有沒有接受」某項服務，meses指「月份」
unique(san[, 42]) #只有分0, 1, 2, 3，推測是var5發生後幾個月了
table(san$num_meses_var5_ult3, san$TARGET) #看似隨著時間消逝，不滿意的比例越來越少

unique(san$ind_var5) #只有0, 1
table(san$ind_var5, san$TARGET)
notsat <- c(1959, 1049) #不滿意人數
totpeo <- c(sum(san$ind_var5 == 0), sum(san$ind_var5 == 1)) #各組總人數
prop.test(notsat, totpeo, alternative = "greater") #沒發生var5事件的人明顯比較不滿

unique(san$num_var5) #只有0, 3, 5, 9, 15
table(san$num_var5) #6, 9, 15的人極少（考慮合併）
table(san$num_var5, san$TARGET)
notsat <- c(1959, 1042 + 7)
totpeo <- c(sum(san$num_var5 == 0), sum(san$num_var5 > 0))
prop.test(notsat, totpeo, alternative = "greater") #跟ind_var5的結果極為接近，兩變數擇一即可


summary(san[, c(9, 10)]) #淘汰9
unique(san$ind_var13_0); unique(san$ind_var13)
sum(san[, 9] == san[, 10]) / dim(san)[1] #9, 10號變數幾乎是一樣的，擇一


summary(san[, c(12, 13)]) #淘汰12
sum(san[, 12] == san[, 13]) / dim(san)[1] #12, 13號變數幾乎是一樣的，擇一


summary(san[, c(14, 15, 23)]) #淘汰14, 15
sum(san[, 14] == san[, 15]) / dim(san)[1] #14, 15號變數幾乎是一樣的，考慮留15
summary(san[, 23])
sd(san[, 23])
library(MASS)
truehist(san[, 23], prob = TRUE, ylab = "Density", main = "Histogram") #幾乎都是0或3
sum(san[, 23] == 0 | san[, 23] == 3) / dim(san)[1] #幾乎都是0或3，以3為最大宗
table(san[, 23], san$TARGET)
notsat <- c(528, 2422 + 56 + 2)
totpeo <- c(sum(san$num_var41_0 == 0), sum(san$num_var41_0 > 0))
prop.test(notsat, totpeo, alternative = "greater") #num_var41_0為0的人明顯較3的人更不滿
all(san[which(san[, 15] == 0), 23] == 0) #ind_var41_0為0的人num_var41_0亦為0，可以只留num_var_41_0


summary(san[, c(16, 21)]) #淘汰21
table(san[, 16]); table(san[, 21]) #兩組長得極像，擇一
table(san[, 16], san$TARGET)


summary(san[, c(18, 24)])
table(san$num_var12_0) #長得不太相似，再研究
table(san$num_var42_0)


summary(san[, c(18, 24)])
table(san[, 18]) #長得不太相似，再研究
table(san[, 24])


summary(san[, c(22, 23)]) #淘汰22，長得極像
table(san[, 22])
table(san[, 23])


summary(san[, c(30, 31, 32, 33)]) #淘汰31, 32, 33
all(san$ind_var_10_ult1 == san$ind_var10cte_ult1) #30, 31完全一樣
all(san$ind_var9cte_ult1 == san$ind_var9_ult1) #32, 33完全一樣
sum(san[, 31] == san[, 32]) / dim(san)[1] #有極大的比例30至33完全一樣，擇一就好

summary(san[, c(39, 40)]) #淘汰40，med表示「平均」
table(san$num_var22_ult3) #ult3為止的var22的值
table(san$num_med_var22_ult3) #ult3為止的平均var22
summary((san[, 39] / 3 - san[, 40])) #變數40是變數39除以3得到的，所以擇一就好

summary(san[, c(41, 45, 48)]) #淘汰41, 45
summary((san[, 48] / 3 - san[, 41])) #變數41是變數48除以3得到的，保留變數48
table(san$num_var45_hace2, san$TARGET) #var45是幾天前發生的了（單位間隔為3天），時間越近，不滿比例疑似越高
summary(san[, c(45, 48)])
table(san[, 45], san[, 48]) #上三角矩陣，懷疑變數48代表var45還有幾天「到期」？
all(san[, 48] >= san[, 45])


san <- san[, -c(9, 12, 14, 15, 17, 21, 22, 31, 32, 33, 41, 45)]
dim(san)
names(san)