file_path <- "/Users/mariamasenko/University/6 semestr/big data/lab2/chips.csv"
data <- read.csv(file_path, stringsAsFactors = FALSE)

surname <- data$`Фамилия`
salt <- data$`Соленые`
bekon <- data$`Бекон`
green_onion <- data$`Зеленый.лук`
cream_herbs <- data$`Сметана.и.зелень`
cream_onion <- data$`Сметана.и.лук`
crab <- data$`Краб`
cheese <- data$`Сыр`
cucumber <- data$`Малосольные.огурчики`
paprika <- data$`Рифленые.паприка`
lobster <- data$`Рифленые.лобстер`

chips <- data.frame(
  "Фамилия" = surname,
  "Соленые" = salt,
  "Бекон" = bekon,
  "Зеленый.лук" = green_onion,
  "Сметана.и.зелень" = cream_herbs,
  "Сметана.и.лук" = cream_onion,
  "Краб" = crab,
  "Сыр" = cheese,
  "Малосольные.огурчики" = cucumber,
  "Рифленые.паприка" = paprika,
  "Рифленые.лобстер" = lobster
)
print(chips)

min_values <- apply(chips[,-1], 2, min)
max_values <- apply(chips[,-1], 2, max)
mean_values <- apply(chips[,-1], 2, mean)

stats <- data.frame(
  "Минимум" = min_values,
  "Максимум" = max_values,
  "Среднее значение" = mean_values
)

more <- colSums(chips[,-1] > 7)
less <- colSums(chips[,-1] < 3)

preferences <- data.frame(
  "Более 7" = more,
  "Менее 3" = less
)

rating <- stats[order(-stats$Среднее),]
print(rating)

library(ggplot2)
library(reshape2)
library(lattice)

data <- melt(chips, id.vars = "Фамилия")

# Способ 1: ggplot2
ggplot(data, aes(x = variable, y = value)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#9a7cec") +
  labs(title = "Средние оценки вкусов (ggplot2)", x = "Вкус", y = "Оценка") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Способ 2: Base R
barplot(
  unlist(stats$`Среднее.значение`),
  names.arg = rownames(stats), 
  col = "skyblue",
  main = "Средние оценки вкусов (Base R)", 
  las = 3, 
  ylab = "Средняя оценка",
  cex.names = 0.8
)

# Способ 3: Lattice
barchart(
  Среднее.значение ~ rownames(stats), 
  data = stats, 
  col = "#4ccf6c",
  main = "Средние оценки вкусов (Lattice)",
  xlab = "Вкус", 
  ylab = "Средняя оценка", 
  scales = list(x = list(rot = 45, cex = 0.8)
))
