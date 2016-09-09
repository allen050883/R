# R
# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
inTraining <- createDataPartition(data_1$Semana, p = 0.5, list = FALSE)
training <- data_1[ inTraining,]
testing  <- data_1[-inTraining,]
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 1)
#number做十次迭代-->10 fold
set.seed(825)
gbmFit1 <- train(Demanda_uni_equil~Semana+Agencia_ID+Canal_ID+Ruta_SAK+Cliente_ID+Producto_ID, data = training, method = "lm", trControl = fitControl, verbose = FALSE)
importance <- varImp(gbmFit1, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
