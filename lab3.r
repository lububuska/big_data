library(openxlsx)

quartz(width = 10, height = 5)

men <- read.xlsx("/Users/mariamasenko/University/6_semestr/big data/lab3/Лыжные гонки.xlsx", sheet = 1)
women <- read.xlsx("/Users/mariamasenko/University/6_semestr/big data/lab3/Лыжные гонки.xlsx", sheet = 2)

# Столбчатые диаграммы
bc_men_count <- apply(men[,-1], 2, sum)
bc_women_count <- apply(women[,-1], 2, sum)

max_val <- max(bc_men_count)

par(mfrow = c(1, 2))
barplot(bc_men_count, 
        names.arg = colnames(men)[-1], 
        col = "#59ffbf", 
        main = "Мужчины", 
        xlab = "Места", 
        ylab = "Количество",
        ylim = c(0, max_val+1),
        )

barplot(bc_women_count, 
        names.arg = colnames(women)[-1], 
        col = "#ae43ff", 
        main = "Женщины", 
        xlab = "Места", 
        ylab = "Количество",
        ylim = c(0, max_val+1),
        )

# года, в которых были заняты первые места
years_men <- men$Год[men$`1` > 0]
years_women <- women$Год[women$`1` > 0]
# кол-во первых мест
first_place_men <- sum(men$`1` > 0)
first_place_women <- sum(women$`1` > 0)
# круговые диаграммы 
par(mfrow = c(1, 2))
legend_colors <- c("darkgreen", "blue", "red", "orange", "yellow", "green", "purple", "lightgreen")

# Для мужчин
if (length(years_men) == 0) {
  pie(c(1, 0), labels = "0", main = "Мужчины", col = "gray")
  legend("topright", legend = c("Нет первых мест"), fill = "gray")
} else {
  men_table <- table(years_men)
  pie(as.numeric(men_table), 
      labels = names(men_table),
      main = "Мужчины", 
      col = legend_colors[1:length(men_table)],
      cex = 0.6)
  legend("topright", 
         legend = names(men_table),
         fill = legend_colors[1:length(men_table)],
         cex = 0.6)
}

# Для женщин
if (length(years_women) == 0) {
  pie(c(1, 0), labels = "0", main = "Женщины", col = "gray")
  legend("topright", legend = c("Нет первых мест"), fill = "gray")
} else {
  women_table <- table(years_women)
  pie(as.numeric(women_table), 
      labels = names(women_table),  # Отображаем количество призовых мест на диаграмме
      main = "Женщины", 
      col = legend_colors[1:length(women_table)],
      cex = 0.6)
  legend("topright", 
         legend = names(women_table),  # Отображаем года в легенде
         fill = legend_colors[1:length(women_table)],
         cex = 0.6)
}

# функциональные графики
par(mfrow = c(1, 2))
plot_graph <- function(df, color, title) {
  total_places <- rowSums(df[, 2:4], na.rm = TRUE)
  matplot(df$Год, total_places,main = title,  type = "b", pch = 19, lty = 1, col = color,
          xlab = "Год", ylab = "Количество мест", xaxt = "n", 
          ylim = c(0, max(df[, 2]*4)))
  axis(1, at = df$Год, labels = df$Год)
}

plot_graph(men, "blue", "Тенденции изменения кол-ва призовых мест\n для мужчин за последние 30 лет")
plot_graph(women, "#ae43ff", "Тенденции изменения кол-ва призовых мест\n для женщин за последние 30 лет")



# график изменения спортивных достижений (золото)
gold <- read.xlsx("/Users/mariamasenko/University/6_semestr/big data/lab3/Лыжные гонки.xlsx", sheet = 3)

par(mar = c(6, 4, 4, 11)) 
matplot(gold$Год, 
        gold[, -1], 
        main = "Изменение кол-ва 1-х мест по 7-ми странам-призерам\n за последние 6 олимпиад", 
        type = "b", pch = 19, 
        lty = 1, 
        lwd = 2,
        col = legend_colors,
        xlab = "Год", ylab = "Количество мест (золото)", 
        xaxt = "n", 
        ylim = c(0, max(gold[, -1]+10)))
axis(1, at = gold$Год, labels = gold$Год)
legend("topright", legend = colnames(gold)[-1], xpd = TRUE, bty = "n", inset = -c(0.25, 0), fill = legend_colors)


# график изменения спортивных достижений (все призовые)
prize_places <- read.xlsx("/Users/mariamasenko/University/6_semestr/big data/lab3/Лыжные гонки.xlsx", sheet = 4)

par(mar = c(5, 4, 4, 11)) 
matplot(prize_places$Год, 
        prize_places[, -1], 
        main = "Изменение кол-ва призовых мест по 7-ми странам-призерам\n за последние 6 олимпиад", 
        type = "b", pch = 19, 
        lty = 1, 
        lwd = 2,
        col = legend_colors,
        xlab = "Год", ylab = "Количество мест (призовые)", 
        xaxt = "n", 
        ylim = c(0, max(prize_places[, -1]+10)))
axis(1, at = prize_places$Год, labels = prize_places$Год)
legend("topright", legend = colnames(prize_places)[-1], xpd = TRUE, bty = "n", inset = -c(0.25, 0), fill = legend_colors)


# динамика лыжных гонок по женщинам и мужчинам за последние 6 Олимпиад
all <- cbind(women[,1:4], men[,2:4])
total_places1 <- rowSums(women[, 2:4], na.rm = TRUE)
total_places2 <- rowSums(men[, 2:4], na.rm = TRUE)
total_places_all <- cbind(women[, 1], total_places1, total_places2)


year <- all[1:6, ]
par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
matplot(total_places_all[, 1], 
        total_places_all, 
        type = "b", pch = 19, 
        lty = c(1, 3),
        col = c("blue", "darkgreen"),
        xlab = "Год", ylab = "Количество мест (призовые)", 
        xaxt = "n", 
        lwd = 3,
        ylim = c(0, max(year[, -1])+5))
axis(1, at = year$Год, labels = year$Год)
legend('topleft', c('Мужчины', 'Женщины'), lty = c(1, 3), cex = 0.8, lwd=4, col = c("blue", "darkgreen"))

colnames(total_places_all) <- c("Год", "Женщины", "Мужчины")
total_places_all_t <- t(total_places_all)
total_places_all_t_sorted <- total_places_all_t[, order(total_places_all_t[1, ])]
barplot(as.matrix(total_places_all_t_sorted[-1,]), 
        beside = TRUE,
        col = c("darkgreen", "blue"), 
        xlab = "Год", 
        names.arg = total_places_all_t_sorted[1,],
        ylab = "Количество мест (призовые)", 
        ylim = c(0,8),
        cex.names = 0.8)
legend('topright', c('Женщины', 'Мужчины'), fill=c("darkgreen", "blue"))



total_places_pie <- apply(total_places_all[, -1], 2, sum)
pie(total_places_pie, 
    labels = c(total_places_pie[1], total_places_pie[2]),
    col=c("darkgreen", "blue"), 
    sub="Кол-во призовых мест за 6 олимпиад\n (женщины, мужчины)")
legend('topright', c('Женщины', 'Мужчины'), fill=c("darkgreen", "blue"))

mtext("Общее количество призовых мест на 6 олимпиадах по лыжным гонкам", 
      outer = TRUE, cex = 1, font = 2)
