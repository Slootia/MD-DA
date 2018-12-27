# Подключаем крутые библиотеки
library(dplyr)
library(ggplot2)

## Загрузка и очистка данных

lab2.data <- read.csv("https://raw.githubusercontent.com/SergeyMirvoda/da2016/master/data/calif_penn_2011.csv")
ncol(lab2.data)
nrow(lab2.data)

# Суммирует значения в столбцах
# apply вернёт матрицу (т. к. c(1,2)) из булевых значений, где TRUE значит значение в lab2.data is.na
# следовательно colSums просуммирует получившиеся логические значения как TRUE = 1, FALSE = 0
# следовательно мы получим количество NA в каждом столбце
# почему этот замечательный финт раньше не показали?
num.nas <- colSums(apply(lab2.data, c(1, 2), is.na))
# чистим от на
lab2.nona_data <- na.omit(lab2.data)
# сколько вычистили
nrow(lab2.data) - nrow(lab2.nona_data)
# сколько максимально на в колонке было
max(num.nas)
# Не совпадают, конечно, потому что в одной строке одни колонки НА, в других другие

# Зададим константы для читаемости
commifornia <- 6
pennysvalnia <- 42
butte_county <- 7
santa_clara <- 85
york_county <- 133

## Новые дома
lab2.nona_data %>%
  ggplot(aes(Built_2005_or_later, Median_house_value)) +
  geom_point() + 
  ggtitle('По всем штатам') +
  xlab('Процент домов, построенных не раньше 2005') +
  ylab('Медиана стоимости домовладения')

# В КАЛифорнии и пенисванальнии
lab2.nona_data %>%
  filter(STATEFP == commifornia) %>%
  ggplot(aes(Built_2005_or_later, Median_house_value)) +
  geom_point() +
  ggtitle('По Коммифорнии') +
  xlab('Процент домов, построенных не раньше 2005') +
  ylab('Медиана стоимости домовладения')

lab2.nona_data %>%
  filter(STATEFP == pennysvalnia) %>%
  ggplot(aes(Built_2005_or_later, Median_house_value)) +
  geom_point() +
  ggtitle('По Пенисльвании') +
  xlab('Процент домов, построенных не раньше 2005') +
  ylab('Медиана стоимости домовладения')

## Незанятые дома
# Добавление колонки
lab2.nona_data <- lab2.nona_data %>%
  mutate(vacancy_rate = Vacant_units / Total_units)

# Статистики
min(lab2.nona_data$vacancy_rate)
max(lab2.nona_data$vacancy_rate)
mean(lab2.nona_data$vacancy_rate)
median(lab2.nona_data$vacancy_rate)

# Диаграмма
lab2.nona_data %>%
  ggplot(aes(Median_house_value, vacancy_rate)) +
  geom_point() +
  xlab('Медиана стоимости домовладения') + 
  ylab('Уровень найма домовладений')

## Корреляция

# Код из задания
# Подсчитывает медиану медиан стоимости дома в 6 штате (Коммифорния), 1 регионе.
# Для этого сначала проходит по каждой строке и добавляет её номер в вектор acc,
# если номер штата и номер региона соответствуют нужным, причем каждый раз создается
# новый вектор, в который кладётся старый + новое значение, это вообще легально?
# Затем проходит по этому вектору, заполняя другой вектор, accmv, значениями из
# 10 колонки матрицы данных, "Median_house_value", из тех строчек, номера которых
# лежат в векторе acc, причём тут тоже как будто каждый раз создается новый вектор.
# Ну и в конце считает медиану этого безобразия.
acc <- c()
for (tract in 1:nrow(ca_pa)) {
  if (ca_pa$STATEFP[tract] == 6) {
    if (ca_pa$COUNTYFP[tract] == 1) {
      acc <- c(acc, tract)
    }
  }
}
accmv <- c()
for (tract in acc) {
  accmv <- c(accmv, ca_pa[tract,10])
}
median(accmv)

# Идентичный код
# dplyr'ом, потому что он мне очень нравится
lab2.nona_data %>%
  filter(STATEFP == commifornia & COUNTYFP == 1) %>%
  summarize(mdn = median(Median_house_value)) %>%
  .$mdn

# Средний процент построенных домовладений в округах
lab2.nona_data %>%
  filter(
    STATEFP == commifornia & (COUNTYFP == butte_county | COUNTYFP == santa_clara) |
    STATEFP == pennysvalnia & COUNTYFP == york_county
  ) %>%
  group_by(STATEFP) %>%
  summarize(avg = mean(Total_units))

# Корреляции

#   i. Для всего набора
lab2.nona_data %>%
  summarize(crl = cor(Median_house_value, Built_2005_or_later)) %>%
  .$crl
#  ii. Для Коммифорнии
lab2.nona_data %>%
  filter(STATEFP == commifornia) %>%
  summarize(crl = cor(Median_house_value, Built_2005_or_later)) %>%
  .$crl
# iii. Для Пенисльвании
lab2.nona_data %>%
  filter(STATEFP == pennysvalnia) %>%
  summarize(crl = cor(Median_house_value, Built_2005_or_later)) %>%
  .$crl
#  iv. Для Butte County
lab2.nona_data %>%
  filter(STATEFP == commifornia & COUNTYFP == butte_county) %>%
  summarize(crl = cor(Median_house_value, Built_2005_or_later)) %>%
  .$crl
#   v. Для Santa Clara
lab2.nona_data %>%
  filter(STATEFP == commifornia & COUNTYFP == santa_clara) %>%
  summarize(crl = cor(Median_house_value, Built_2005_or_later)) %>%
  .$crl
#  vi. Для York County
lab2.nona_data %>%
  filter(STATEFP == pennysvalnia & COUNTYFP == york_county) %>%
  summarize(crl = cor(Median_house_value, Built_2005_or_later)) %>%
  .$crl

# Диаграммы
lab2.nona_data %>%
  filter(
    STATEFP == commifornia & (COUNTYFP == butte_county | COUNTYFP == santa_clara) |
    STATEFP == pennysvalnia & COUNTYFP == york_county
  ) %>%
  ggplot(aes(Median_household_income, Median_house_value)) +
  geom_point() +
  facet_grid(rows = vars(COUNTYFP)) +
  xlab('Медиана дохода') +
  ylab('Медиана стоимости домовладения')
