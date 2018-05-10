library('tree') # деревья
library('MASS')
library('randomForest') # случайный лес
library('gbm')
attach(Boston)
#непрерывный Y==============================
# обучающая выборка
set.seed(3)
train <- sample(1:nrow(Boston), nrow(Boston)/2) # обучающая выборка -- 50%

# обучаем модель
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)

# визуализация
plot(tree.boston)
text(tree.boston, pretty = 0)
yhat <- predict(tree.boston, newdata=Boston[-train])
mse.test <- mean((yhat - train)^2)
#случайный лес-------------------------
# обучаем модель
set.seed(3)
boston.test <- Boston[-train, "medv"]
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train,
                          mtry = 6, importance = TRUE)
# график результата
plot(rf.boston) 
# прогноз
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
plot(yhat.rf, boston.test)
# линия идеального прогноза
abline(0, 1)
# MSE на тестовой выборке
mse.test <- mean((yhat.rf - boston.test)^2)
#ошибка равна 15,904

#Категориальный Y==================================
# новая переменная
High <- ifelse(medv <= 25, '0', '1')
High
# присоединяем к таблице данных
Boston1 <- data.frame(Boston, High)
str(Boston1)
# модель бинарного дерева
tree.boston1 <- tree(High ~ .-medv, Boston1)
summary(tree.boston1)
# график результата
plot(tree.boston1) # ветви
text(tree.boston1) # подписи

# ядро генератора случайных чисел
set.seed(3)

# обучающая выборка
train1 <- sample(1:nrow(Boston1), nrow(Boston1)/2)

# тестовая выборка
boston1.test <- Boston1[-train1,]
High.test <- High[-train1]

# строим дерево на обучающей выборке
tree.boston.test <- tree(High ~ . -medv, Boston1, subset = train1)
summary(tree.boston.test)
# график результата
plot(tree.boston.test) # ветви
text(tree.boston.test) # подписи
# делаем прогноз
tree.pred <- predict(tree.boston.test, boston1.test, type = "class")



# обучаем модель
set.seed(3)
rf.boston1 <- randomForest(High ~. -medv , data = Boston1, subset = train,
                          mtry = 6, importance = TRUE)
# график результата
plot(rf.boston1) 

# прогноз
yhat.rf1 <- predict(rf.boston, newdata = Boston1[-train, ])
boston11.test <- Boston1[-train, "High"]
as.numeric(boston11.test)
# MSE на тестовой выборке
mse.test <- mean((yhat.rf1 - as.numeric(boston11.test)^2))

# важность предикторов
importance(rf.boston) # оценки

varImpPlot(rf.boston) # графики
