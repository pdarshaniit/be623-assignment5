setwd("~/be623_assignment5")
library(dplyr)
library(ggplot2)
getwd()

#Question 1

circ_df <- data.frame(
  Sample = paste0("S", 1:12),
  Gene = c(rep("PER2", 6), rep("BMAL1", 6)),
  ZT = rep(c(0, 4, 8, 12, 16, 20), 2),
  Expression = c(
    1.8, 3.2, 6.5, 9.4, 7.1, 3.6,
    8.6, 5.9, 2.7, 1.5, 3.4, 6.8
  )
)

str(circ_df)

#Question 2

per2_rows <- circ_df[circ_df$Gene == "PER2", ]

mean(per2_rows$Expression)
median(per2_rows$Expression)
sd(per2_rows$Expression)

#Question 3

write.csv(circ_df, "circadian_expression.csv", row.names = FALSE)

circ_reloaded <- read.csv("circadian_expression.csv")

identical(circ_df$Expression, circ_reloaded$Expression)


#Question 4

per2_ranked <- circ_df %>%
  filter(Gene == "PER2") %>%
  arrange(desc(Expression))

bmal1_ranked <- circ_df %>%
  filter(Gene == "BMAL1") %>%
  arrange(desc(Expression))

per2_ranked
bmal1_ranked

per2_peak_ZT <- per2_ranked$ZT[1]
bmal1_peak_ZT <- bmal1_ranked$ZT[1]

per2_peak_ZT
bmal1_peak_ZT

abs(per2_peak_ZT - bmal1_peak_ZT)


#question 5
#Adding dark column

circ_df2 <- circ_df %>%
  mutate(dark = ZT >= 12)

circ_df2

#average expression of dark samples


dark_summary <- circ_df2 %>%
  filter(dark == TRUE) %>%
  group_by(Gene) %>%
  summarize(
    mean_expression = mean(Expression),
    sample_count = n()
  )

dark_summary

#average expression of light samples


light_summary <- circ_df2 %>%
  filter(dark == FALSE) %>%
  group_by(Gene) %>%
  summarize(
    mean_expression = mean(Expression),
    sample_count = n()
  )

light_summary

#Question 6

rhythm_plot <- ggplot(circ_df, aes(x = ZT, y = Expression, color = Gene)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("PER2" = "blue", "BMAL1" = "red")) +
  labs(
    title = "Circadian Gene Expression",
    x = "Zeitgeber time (h)",
    y = "Expression",
    color = "Gene"
  ) +
  theme_minimal()

rhythm_plot

ggsave("rhythm_plot.png", plot = rhythm_plot)

#Question 7

expression_boxplot <- ggplot(circ_df, aes(x = Gene, y = Expression, fill = Gene)) +
  geom_boxplot() +
  scale_fill_manual(values = c("PER2" = "blue", "BMAL1" = "red")) +
  labs(
    title = "Gene Expression Distribution",
    x = "Gene",
    y = "Expression"
  ) +
  theme_minimal()

expression_boxplot

ggsave("expression_boxplot.png", plot = expression_boxplot)
