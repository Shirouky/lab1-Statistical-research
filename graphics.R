setwd("D:/МИФИ/5 семестр/Статистические исследования/Lab1")
load("Results.RData")

# 8) СТОЛБЧАТЫЕ ДИАГРАММЫ ГРУПП
par(mfrow = c(2, 2), mar = c(7, 4, 4, 2))
make_barplot <- function(data, title) {
  png(paste("Graphics/", title, ".png"), width = 800, height = 600, res = 150)
  barplot(data, main = title, 
          ylab = "People quantity",
          col = terrain.colors(length(data)), las = 2, cex.names = 0.8)
  dev.off()
}

age_counts <- table(df_clean$Age_Group)
make_barplot(age_counts, "Distribution by age groups")

income_counts <- table(df_clean$Income_Group)
make_barplot(income_counts, "Distribution by income groups")

marital_counts <- table(df_clean$Marital_Status)
make_barplot(marital_counts, "Distribution by marital status")

education_counts <- table(df_clean$Education)
make_barplot(education_counts, "Distribution by education")

# 9) ДИАГРАММЫ РАЗМАХА ДЛЯ ДИНАМИКИ ПОКАЗАТЕЛЕЙ
make_boxplot <- function(data, title, ylab) {
  png(paste("Graphics/", title, ".png"), width = 800, height = 600, res = 150)
  boxplot(data, main = title,  col = c("lightblue", "lightgreen"),
          ylab = ylab)
  dev.off()
}

price_data <- data.frame(
  Price2014 = df_clean$Price2014,
  Price2015 = df_clean$Price2015
)
make_boxplot(price_data, "Price dynamics", "Price")

quantity_data <- data.frame(
  Quantity2014 = df_clean$Quantity2014,
  Quantity2015 = df_clean$Quantity2015
)
make_boxplot(quantity_data, "Quantity dynamics", "Quantity")

# 10) ДИАГРАММЫ РАССЕИВАНИЯ С GGPLOT2
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}


df_long <- data.frame(
  Количество = c(df_clean$Quantity2014, df_clean$Quantity2015),
  Цена = c(df_clean$Price2014, df_clean$Price2015),
  Год = factor(rep(c(2014, 2015), each = nrow(df_clean)))
)

p <- ggplot(df_long, aes(x = Количество, y = Цена, color = Год)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("2014" = "cyan", "2015" = "pink")) +
  labs(title = "Price-Quantity Relationship (2014 and 2015)",
       x = "Quantity",
       y = "Price",
       color = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "top")

print(p)
ggsave("Graphics/Price-Quantity Relationship.png", p, width = 10, height = 6, dpi = 300)

# 11) АНАЛИЗ 5 САМЫХ КРУПНЫХ ГРУПП ПОТРЕБИТЕЛЕЙ
library(dplyr)

df_clean <- mutate(df_clean, Consumer_Group = paste(Age_Group, Income_Group, Marital_Status, Education, sep = " | "))

top_groups <- head(arrange(count(df_clean, Consumer_Group), desc(n)), 5)

cat("5 САМЫХ КРУПНЫХ ГРУПП ПОТРЕБИТЕЛЕЙ:\n")
print(top_groups)

df_top_groups <- filter(df_clean, Consumer_Group %in% top_groups$Consumer_Group)
group_stats <- summarise(
  group_by(df_top_groups, Consumer_Group),
  Count = n(),
  Avg_Price2014 = mean(Price2014),
  Avg_Price2015 = mean(Price2015),
  Avg_Quantity2014 = mean(Quantity2014),
  Avg_Quantity2015 = mean(Quantity2015),
  Avg_Income = mean(Income)
)

print(group_stats)

group_sizes <- setNames(top_groups$n, top_groups$Consumer_Group)
make_barplot(group_sizes, "Size of the biggest groups")

prices_2014 <- setNames(group_stats$Avg_Price2014, group_stats$Consumer_Group)
make_barplot(prices_2014, "Price2014 for 5 groups")

prices_2015 <- setNames(group_stats$Avg_Price2015, group_stats$Consumer_Group)
make_barplot(prices_2015, "Price2015 for 5 groups")

quantity_2014 <- setNames(group_stats$Avg_Quantity2014, group_stats$Consumer_Group)
make_barplot(quantity_2014, "Quantity2014 for 5 groups")

quantity_2015 <- setNames(group_stats$Avg_Quantity2015, group_stats$Consumer_Group)
make_barplot(quantity_2015, "Quantity2015 for 5 groups")

write.csv(group_stats, "Tables/Group statistics.csv", row.names = FALSE, fileEncoding = "UTF-8")