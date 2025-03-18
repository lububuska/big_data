install.packages("openxlsx")

library(openxlsx)
data_xlsx <- read.xlsx("/Users/mariamasenko/University/6_semestr/big data/lab2/chips.xlsx", sheet = 1)
data_csv <- read.csv("/Users/mariamasenko/University/6_semestr/big data/lab2/chips.csv", row.names = 3)
results <- data_csv[-(1:2)]

# дескриптивный анализ по каждому вкусу

# мода
find_mode <- function(values) {
  freq_table <- table(values)
  max_count <- max(freq_table)
  mode_values <- names(freq_table[freq_table == max_count])
  if (length(mode_values) == length(freq_table)) {
    return(NA)
  }
  return(mode_values)
}

modes <- lapply(results, find_mode)

mode_df <- data.frame(
  Вкус.чипсов = names(modes),
  Мода = I(modes),
  row.names = NULL
)

# медиана
medians <- sort(apply(results, 2, median), decreasing = FALSE)
median_df <- data.frame(
  Вкус.чипсов = names(medians),
  Медиана = medians,
  row.names = NULL
)

# среднее значение
means <- sort(colMeans(results), decreasing = FALSE)
mean_df <- data.frame(
  Вкус.чипсов = names(means),
  Среднее.значение = means,
  row.names = NULL
)

# минимальное значение
mins <- sort(apply(results, 2, min), decreasing = FALSE)
min_df <- data.frame(
  Вкус.чипсов = names(mins),
  Минимальное = mins,
  row.names = NULL
)

# максимальное значение
maxs <- sort(apply(results, 2, max), decreasing = FALSE)
max_df <- data.frame(
  Вкус.чипсов = names(maxs),
  Максимальное = maxs,
  row.names = NULL
)

# дисперсия
variances <- sort(apply(results, 2, var), decreasing = FALSE)
var_df <- data.frame(
  Вкус.чипсов = names(variances),
  Дисперсия = variances,
  row.names = NULL
)

# наборы данных и их анализ

subset1 <- results[results$Бекон > 5, ]
subset2 <- results[results$Рифленые.лобстер >= 7, ]

dimension1 <- dim(subset1)
dimension2 <- dim(subset2)

median_becon <- apply(subset1, 2, median)
moda_becon <- apply(subset1, 2, find_mode)
mean_becon <- sort(colMeans(subset1), decreasing = FALSE)

median_lobster <- apply(subset2, 2, median)
moda_lobster <- apply(subset2, 2, find_mode)
mean_lobster <- sort(colMeans(subset2), decreasing = FALSE)

# для бекона
boxplot(subset1,
  main = "Оценки набора Бекон > 5",
  xlab = "Оценка",
  las = 0,
  horizontal = TRUE,
  cex.axis = 0.7
)

barplot(mean_becon,
  main = "Столбчатая диаграмма для Бекона > 5",
  xlab = "Среднее значение",
  las = 0,
  horiz = TRUE,
  cex.names = 0.8,
  xlim = c(0, 10)
)
axis(1, at = seq(0, 10, by = 1))


hist(subset1$Соленые,
  main = "Оценки соленых чипсов для Бекона > 5",
  xlab = "Оценка",
  ylab = "Частота",
  breaks = seq(1, 10, by = 1)
)

# для лобстера
boxplot(subset2,
  main = "Оценки набора Лобстер >= 7",
  xlab = "Оценка",
  las = 0,
  horizontal = TRUE,
  cex.axis = 0.7
)
axis(1, at = seq(1, 10, by = 2))

barplot(mean_lobster,
  main = "Столбчатая диаграмма для Лобстер >= 7",
  xlab = "Среднее значение",
  las = 0,
  horiz = TRUE,
  cex.names = 0.8,
  xlim = c(0, 10)
)
axis(1, at = seq(0, 10, by = 1))

hist(subset2$Малосольные.огурчики,
  main = "Оценки чипсов малосольные огурчики для Лобстер >= 7",
  xlab = "Оценка",
  ylab = "Частота",
  breaks = seq(1, 10, by = 1)
)

merged_data <- merge(subset2, subset1, by.x = "row.names", by.y = "row.names", all.x = TRUE) # слияние таблиц
subset_example <- results[results$Рифленые.паприка == 8 & results$Сметана.и.лук > 4, ] # формирование части из целого
subset <- rbind(subset1, subset2) # добавление строк
subset_without <- subset1[!subset1$Сметана.и.зелень == 10,] # исключение переменных


# Межквартильный размах
IQR(results$Соленые)

summary(results)

par(mar = c(5, 9, 4, 2))
boxplot(results,
  main = "Оценки пицц",
  xlab = "Оценка",
  las = 1,
  horizontal = TRUE,
  cex.axis = 0.8
)

barplot(means,
  main = "Общая столбчатая диаграмма",
  xlab = "Среднее значение",
  las = 1,
  horiz = TRUE,
  cex.names = 0.8,
  xlim = c(0, 10)
)