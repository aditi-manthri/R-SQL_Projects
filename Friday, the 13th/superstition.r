library(dplyr)
friday_data <- read.csv("friday.csv")

t_test_result <- t.test(friday_data$sixth, friday_data$thirteenth, paired = TRUE)

wilcox_test_result <- wilcox.test(friday_data$sixth, friday_data$thirteenth, paired = TRUE)

type_analysis <- friday_data %>%
  group_by(type) %>%
  summarise(
    mean_sixth = mean(sixth),
    mean_thirteenth = mean(thirteenth),
    diff_mean = mean_thirteenth - mean_sixth,
    p_value = t.test(sixth, thirteenth, paired = TRUE)$p.value
  )

boxplot(friday_data$sixth, friday_data$thirteenth, 
        names = c("6th", "13th"),
        main = "Comparison of Counts on 6th and 13th",
        ylab = "Count")

cat("Paired T-Test Results:\n")
print(t_test_result)

cat("\nWilcoxon Signed-Rank Test Results:\n")
print(wilcox_test_result)

cat("\nAnalysis by Observation Type:\n")
print(type_analysis)
