#Пользуясь примером из лекции файл (5.0.R) проанализируйте данные
#о возрасте и физ. характеристиках молюсков
#https://archive.ics.uci.edu/ml/datasets/abalone
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=TRUE, sep=",")
summary(data)
colnames(data)
colnames(data) <- c("sex", "length", "diameter", "height", 
                "whole_weight", "shucked_weight",
                "viscera_weight", "shell_weight", "rings")

colnames(data)
data$sex <- factor(c("Female", "Infant", "Male")[data$sex])
par(mfrow=c(1,3))
hist(data$diameter, main = "Диаметр, мм")
hist(data$height, main = "Высота, мм")
hist(data$whole_weight, main = "Полный вес, гр")
#Видим ассиметрию https://en.wikipedia.org/wiki/Skewness
#и выбросы (от них нужно избавиться)

#Визулизируем возможные зависимости
par(mfrow=c(1,2)) 
plot(data$diameter, data$whole_weight,'p',main = "Зависимость веса от диаметра")
plot(data$height, data$whole_weight,'p',main = "Зависимость веса от высоты")

#Хорошо видна зависимость, нужно её исследовать
#построить линейные модели при помощи функции lm, посмотреть их характеристики
#избавиться от выборосов, построить ещё модели и проверить их
#разделить массив данных на 2 случайные части
#подогнать модель по первой части
#спрогнозировать (функция predict) значения во второй части
#проверить качесвто прогноза

# Загрузка данных, обзывание, саммари
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=TRUE, sep=",")
colnames(data) <- c("sex", "length", "diameter", "height", 
                    "whole_weight", "shucked_weight",
                    "viscera_weight", "shell_weight", "rings")
data$sex <- factor(c("Female", "Infant", "Male")[data$sex])
summary(data)

par(mfrow=c(1,3))
hist(data$diameter, main = "Диаметр, мм")
hist(data$height, main = "Высота, мм")
hist(data$whole_weight, main = "Полный вес, гр")

# Чтобы красиво 4 графика
# par(mfrow=c(2,2))

# Линейные модели по изначальным данным
# Вес от диаметра
lm.d.1 <- lm(whole_weight ~ diameter, data=data)
summary(lm.d.1)
plot(lm.d.1)

# Вес от высоты
lm.h.1 <- lm(whole_weight ~ height, data=data)
summary(lm.h.1)
plot(lm.h.1)

# Вес от длины
lm.l.1 <- lm(whole_weight ~ length, data=data)
summary(lm.l.1)
plot(lm.l.1)

# Избавление от выбросов
data.noout <- data[complete.cases(data),]
data.noout <- data.noout[data.noout$diameter > .1 & data.noout$diameter < .6,]
data.noout <- data.noout[data.noout$height < .2 & data.noout$height > 0.05,]
data.noout <- data.noout[data.noout$whole_weight < 1.8,]
data.noout <- data.noout[data.noout$rings >= 5 & data.noout$rings < 20,]

par(mfrow=c(1,3))
hist(data.noout$diameter, main = "Диаметр, мм")
hist(data.noout$height, main = "Высота, мм")
hist(data.noout$whole_weight, main = "Полный вес, гр")

par(mfrow=c(1,2)) 
plot(data.noout$diameter, data.noout$whole_weight,'p',main = "Зависимость веса от диаметра")
plot(data.noout$height, data.noout$whole_weight,'p',main = "Зависимость веса от высоты")
plot(data.noout$length, data.noout$whole_weight,'p',main = "Зависимость веса от длины")
plot(data.noout$sex, data.noout$whole_weight,'p',main = "Зависимость веса от пола")
plot(data.noout$whole_weight, data.noout$rings,'p',main = "Зависимость кол-ва колец от веса")
plot(data.noout$diameter, data.noout$rings,'p',main = "Зависимость кол-ва колец от диаметра")
plot(data.noout$length, data.noout$rings,'p',main = "Зависимость кол-ва колец от длины")
plot(data.noout$height, data.noout$rings,'p',main = "Зависимость кол-ва колец от высоты")

# Чтобы красиво 4 графика
par(mfrow=c(2,2))

# Линейные модели по очищеным данным
# Вес от диаметра
lm.d.2 <- lm(whole_weight ~ diameter, data=data.noout)
summary(lm.d.2)
plot(lm.d.2)

# Вес от высоты
lm.h.2 <- lm(whole_weight ~ height, data=data.noout)
summary(lm.h.2)
plot(lm.h.2)

# Вес от длины
lm.l.2 <- lm(whole_weight ~ length, data=data.noout)
summary(lm.l.2)
plot(lm.l.2)

# Кол-во колец от веса
lm.r.2 <- lm(rings ~ whole_weight, data=data.noout)
summary(lm.r.2)
plot(lm.r.2)

# Делим пополам
odds <- seq(1, nrow(data.noout), by=2)
data.in <- data.noout[odds,]
data.out <- data.noout[-odds,]

# Предсказание веса по диаметру
lm.d.half <- lm(whole_weight ~ diameter, data=data.in)
summary(lm.d.half)
data.d.pred <- predict(lm.d.half)
cor(data.in$whole_weight, data.d.pred)
plot(data.in$whole_weight, data.d.pred)

data.d.pred.out <- predict(lm.d.half, data.out)
cor(data.out$whole_weight, data.d.pred.out)
plot(data.out$whole_weight, data.d.pred.out)

# Предсказание веса по высоте
lm.h.half <- lm(whole_weight ~ height, data=data.in)
summary(lm.h.half)
data.h.pred <- predict(lm.h.half)
cor(data.in$whole_weight, data.h.pred)
plot(data.in$whole_weight, data.h.pred)

data.h.pred.out <- predict(lm.h.half, data.out)
cor(data.out$whole_weight, data.h.pred.out)
plot(data.out$whole_weight, data.h.pred.out)

# Предсказание веса по длине
lm.l.half <- lm(whole_weight ~ length, data=data.in)
summary(lm.l.half)
data.l.pred <- predict(lm.l.half)
cor(data.in$whole_weight, data.l.pred)
plot(data.in$whole_weight, data.l.pred)

data.l.pred.out <- predict(lm.l.half, data.out)
cor(data.out$whole_weight, data.l.pred.out)
plot(data.out$whole_weight, data.l.pred.out)

# Предсказание кол-ва колец по весу
lm.r.half <- lm(rings ~ whole_weight, data=data.in)
summary(lm.r.half)
data.r.pred <- predict(lm.r.half)
cor(data.in$rings, data.r.pred)
plot(data.in$rings, data.r.pred)

data.r.pred.out <- predict(lm.r.half, data.out)
cor(data.out$rings, data.r.pred.out)
plot(data.out$rings, data.r.pred.out)
