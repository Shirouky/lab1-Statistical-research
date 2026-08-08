setwd("D:/МИФИ/5 семестр/Статистические исследования/Lab1")
load("Вариант_20_Б23-902_Деребас Любовь Игоревна.RData")

png("Graphics/Price2015 boxplot.png", width = 800, height = 600, res = 150)
boxplot(FinalData$Price2015,range = 1.5,col = "pink",border = "darkgray",names = names(df)[3],
        main ="Price2015 boxplot", ylab = names(df)[3] )
dev.off()

df_clean <- na.omit(FinalData)

remove_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  return(x >= lower & x <= upper)
}


cleanable_cols <- c("Year_Birth", "Income", "Price2014", "Quantity2015", "Price2015", "Quantity2015")

for (col in cleanable_cols) {
  mask <- remove_outliers_iqr(df_clean[[col]])
  df_clean <- df_clean[mask, ]
}
df_clean <- df_clean[df_clean$Marital_Status != "Absurd", ]
df_clean <- droplevels(df_clean)

png("Graphics/Clean price2015 boxplot.png", width = 800, height = 600, res = 150)
boxplot(df_clean$Price2015,range = 1.5,col = "pink",border = "darkgray",names = names(df)[3],
        main ="Clean price2015 boxplot", ylab = names(df)[3] )
dev.off()



current_year <- 2025
df_clean$Age <- current_year - df_clean$Year_Birth
age_breaks <- quantile(df_clean$Age, probs = seq(0, 1, length.out = 6))
df_clean$Age_Group <- cut(df_clean$Age, 
                             breaks = age_breaks, 
                             include.lowest = TRUE,
                             labels = c("Young", "Young adults", 
                                        "Middle age", "Adults", "Eldery"))
income_min <- min(df_clean$Income)
income_max <- max(df_clean$Income)
income_breaks <- seq(income_min, income_max, length.out = 11)
df_clean$Income_Group <- cut(df_clean$Income, 
                                breaks = income_breaks,
                                include.lowest = TRUE,
                                labels = paste("Income group", 1:10))


# Индивидуальные индексы цен и количества
df_clean$price_index_2015 <- df_clean$Price2015 / df_clean$Price2014
df_clean$quantity_index_2015 <- df_clean$Quantity2015 / df_clean$Quantity2014

# Индекс Пааше
Q_paasche <- sum(df_clean$Price2015 * df_clean$Quantity2015) / sum(df_clean$Price2015 * df_clean$Quantity2014)
# Индекс Ласпейреса
Q_laspeyres <- sum(df_clean$Price2014 * df_clean$Quantity2015) / sum(df_clean$Price2014 * df_clean$Quantity2014)
# Индекс Фишера
Q_fisher <- sqrt(Q_paasche * Q_laspeyres)
cat(sprintf("Индекс Пааше: %2.8f, Индекс Ласпейреса = %2.8f, Индекс Фишера = %2.8f\n", 
            Q_paasche, Q_laspeyres, Q_fisher))


# 5) ПОКАЗАТЕЛИ ЦЕНТРА РАСПРЕДЕЛЕНИЙ ОТКЛОНЕНИЙ
price_quant_cols <- c("Price2014", "Price2015", "Quantity2014", "Quantity2015")

# По гистограммам видно, что все показатели имеют нормальное распрделение
# Тогда будем считать медиану и среднее арифметическое
png("Graphics/Price2015 histogram.png", width = 800, height = 600, res = 150)
hist(df_clean$Price2015, xlab = "Цена 2015", ylab = "Частота",
     main = "Price2015 histogram",
     col = "lightblue")
dev.off()

for (col in price_quant_cols) {
  mean_val <- mean(df_clean[[col]])
  median_val <- median(df_clean[[col]])
  
  cat(sprintf("%-12s: Среднее = %8.2f, Медиана = %8.2f\n", 
              col, mean_val, median_val))
}


print_result <- function(title, metric, round_coef) {
  cat(title, round(metric, round_coef), "\n")
}

# 6) ПОКАЗАТЕЛИ ВАРИАЦИИ И РАЗМАХА
calculate_variation_metrics <- function(x) {
  list(
    range = diff(range(x)),
    variance = var(x),
    std_dev = sd(x),
    coef_variation = sd(x) / mean(x) * 100,
    iqr = IQR(x),
    mean_absolute_deviation = mean(abs(x - mean(x)))
  )
}

for (col in price_quant_cols) {
  cat("\n---", col, "---\n")
  metrics <- calculate_variation_metrics(df_clean[[col]])
  print_result("Размах:", metrics$range, 2)
  print_result("Дисперсия:", metrics$variance, 2)
  print_result("Стандартное отклонение:", metrics$std_dev, 2)
  print_result("Коэффициент вариации:", metrics$coef_variation, 2)
  print_result("Интерквартильный размах:", metrics$iqr, 2)
}

# 7) ПОКАЗАТЕЛИ ХАРАКТЕРА РАСПРЕДЕЛЕНИЙ
calculate_distribution_metrics <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)

  skewness <- sum((x - mean_x)^3) / (n * sd_x^3)
  kurtosis <- sum((x - mean_x)^4) / (n * sd_x^4) - 3
  deciles <- quantile(x, probs = seq(0, 1, 0.1))
  
  list(
    skewness = skewness,
    kurtosis = kurtosis,
    decile_ratio = deciles[9] / deciles[2]
  )
}

for (col in price_quant_cols) {
  cat("\n---", col, "---\n")
  
  metrics <- calculate_distribution_metrics(df_clean[[col]])
  print_result("Асимметрия:", metrics$skewness, 4)
  print_result("Эксцесс:", metrics$kurtosis, 4)
  print_result("Коэффициент дифференциации:", metrics$decile_ratio, 4)
}

save(df_clean, file = "Results.RData")
