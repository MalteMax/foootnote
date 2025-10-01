## packages ##
# STEP: install and load packages----------
for (pckg in c("tidyverse", "ggpubr", "grid", "gridExtra", "extrafont", "haven", "plotly", "scales")) {
  if(!require(pckg, character.only = TRUE)) {
    install.packages(pckg, character.only = TRUE)
    library(pckg, character.only = TRUE)
  }
}
rm(list = ls())
options(scipen = 5)

# FINANCIAL VS TEXTUAL LABELS
setwd("/Users/maltemax/ownCloud/SBE_ACC_Replacement_Surfdrive (Projectfolder)/SBE_ACC_Replacement_Surfdrive (Projectfolder)/projects/footnote/3_pipeline/1_intermediate")
df <- read_dta("sample_for_cohort_formation.dta")

df <- df %>%
  mutate(
    `1`         = if_else(dta_ratio == 1, 1L, 0L),
    `0.80-0.99` = if_else(dta_ratio >= 0.80 & dta_ratio < 1, 1L, 0L),
    `0.60-0.79` = if_else(dta_ratio >= 0.60 & dta_ratio < 0.80, 1L, 0L),
    `0.40-0.59` = if_else(dta_ratio >= 0.40 & dta_ratio < 0.60, 1L, 0L),
    `0.20-0.39` = if_else(dta_ratio >= 0.20 & dta_ratio < 0.40, 1L, 0L),
    `0.01-0.19` = if_else(dta_ratio > 0 & dta_ratio < 0.20, 1L, 0L),
    `0`         = if_else(dta_ratio == 0, 1L, 0L)
  )

grouped <- df %>%
  filter(fyear >= 2012) %>%
  mutate(ib_neg = if_else(ib_sum < 0, "yes", "no")) %>%
  group_by(fyear, ib_neg) %>%
  summarize(
    "100%" = mean(`1`),
    "80-99%" = mean(`0.80-0.99`),
    "60-79%" = mean(`0.60-0.79`),
    "40-59%" = mean(`0.40-0.59`),
    "20-39%" = mean(`0.20-0.39`),
    "1-19%" = mean(`0.01-0.19`),
    "0%" = mean(`0`)
    ) %>%
  mutate(ib_neg = factor(ib_neg, levels = c("yes", "no")))

plot_a <- ggplot(grouped, aes(x = fyear, linetype = ib_neg)) +
  geom_vline(xintercept = 2017, linetype = "solid", color = 'red', linewidth = 0.5) +
  geom_line(aes(y = `100%`)) +
  geom_label(data = subset(grouped, fyear %in% c(2017, 2023)),
             aes(y = `100%`, label = percent(`100%`, accuracy = 0.1)),
             fill = "black",           # background
             color = "white",          # text
             label.size = 0,           # no border stroke
             size = 2,
             vjust = 0.5,
             label.padding = unit(0.08, "lines"),
             show.legend = FALSE
  ) +
  scale_y_continuous(name = '% of observations', breaks = c(0.3, 0.2, 0.1, 0), limit = c(0, 0.3),
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  scale_x_continuous(name = '', breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_linetype_manual(name = 'Cumulative three-year loss', values = c("dashed", "solid")) + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 5),
        axis.title = element_text(colour = "black", size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        text = element_text(size = 9, family = "Times"),
        legend.position = "none",
        legend.key.size = unit(0.75, units = "cm")) +
  # annotate("text", x = 2016.6, y = 0.85, label = "Last year\npre-TCJA", size = 2.0, family = 'Times', color = 'red') +
  ggtitle("Panel A: DTA ratio of 100% (full VA)")
plot_a

plot_b <- ggplot(grouped, aes(x = fyear, linetype = ib_neg)) +
  geom_vline(xintercept = 2017, linetype = "solid", color = 'red', linewidth = 0.5) +
  geom_line(aes(y = `80-99%`)) +
  geom_label(data = subset(grouped, fyear %in% c(2017, 2023)),
             aes(y = `80-99%`, label = percent(`80-99%`, accuracy = 0.1)),
             fill = "black",           # background
             color = "white",          # text
             label.size = 0,           # no border stroke
             size = 2,
             vjust = 0.5,
             label.padding = unit(0.08, "lines"),
             show.legend = FALSE
  ) +
  scale_y_continuous(name = '% of observations', breaks = c(0, 0.2, 0.4, 0.6, 0.8), limit = c(0, 0.65),
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  scale_x_continuous(name = '', breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_linetype_manual(name = 'Cumulative three-year loss', values = c("dashed", "solid")) + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 5),
        axis.title = element_text(colour = "black", size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        text = element_text(size = 9, family = "Times"),
        legend.position = "none",
        legend.key.size = unit(0.4, units = "cm")) +
  # annotate("text", x = 2016.6, y = 0.85, label = "Last year\npre-TCJA", size = 2.0, family = 'Times', color = 'red') +
  ggtitle("Panel B: DTA ratio of 80-99%")
plot_b

plot_c <- ggplot(grouped, aes(x = fyear, linetype = ib_neg)) +
  geom_vline(xintercept = 2017, linetype = "solid", color = 'red', linewidth = 0.5) +
  geom_line(aes(y = `60-79%`)) +
  geom_label(data = subset(grouped, fyear %in% c(2017, 2023)),
             aes(y = `60-79%`, label = percent(`60-79%`, accuracy = 0.1)),
             fill = "black",           # background
             color = "white",          # text
             label.size = 0,           # no border stroke
             size = 2,
             vjust = 0.5,
             label.padding = unit(0.08, "lines"),
             show.legend = FALSE
  ) +
  scale_y_continuous(name = '% of observations', breaks = c(0.2, 0.1, 0), limit = c(0, 0.15),
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  scale_x_continuous(name = '', breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_linetype_manual(name = 'Cumulative three-year loss', values = c("dashed", "solid")) + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 5),
        axis.title = element_text(colour = "black", size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        text = element_text(size = 9, family = "Times"),
        legend.position = "none",
        legend.key.size = unit(0.4, units = "cm")) +
  # annotate("text", x = 2016.6, y = 0.85, label = "Last year\npre-TCJA", size = 2.0, family = 'Times', color = 'red') +
  ggtitle("Panel C: DTA ratio of 60-79%")
plot_c

plot_d <- ggplot(grouped, aes(x = fyear, linetype = ib_neg)) +
  geom_vline(xintercept = 2017, linetype = "solid", color = 'red', linewidth = 0.5) +
  geom_line(aes(y = `40-59%`)) +
  geom_label(data = subset(grouped, fyear %in% c(2017, 2023)),
             aes(y = `40-59%`, label = percent(`40-59%`, accuracy = 0.1)),
             fill = "black",           # background
             color = "white",          # text
             label.size = 0,           # no border stroke
             size = 2,
             vjust = 0.5,
             label.padding = unit(0.08, "lines"),
             show.legend = FALSE
  ) +
  scale_y_continuous(name = '% of observations', breaks = c(0.3, 0.2, 0.1, 0), limit = c(0, 0.20),
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  scale_x_continuous(name = '', breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_linetype_manual(name = 'Cumulative three-year loss', values = c("dashed", "solid")) + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 5),
        axis.title = element_text(colour = "black", size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        text = element_text(size = 9, family = "Times"),
        legend.position = "none",
        legend.key.size = unit(0.4, units = "cm")) +
  # annotate("text", x = 2016.6, y = 0.85, label = "Last year\npre-TCJA", size = 2.0, family = 'Times', color = 'red') +
  ggtitle("Panel D: DTA ratio of 40-59%")
plot_d

plot_e <- ggplot(grouped, aes(x = fyear, linetype = ib_neg)) +
  geom_vline(xintercept = 2017, linetype = "solid", color = 'red', linewidth = 0.5) +
  geom_line(aes(y = `20-39%`)) +
  geom_label(data = subset(grouped, fyear %in% c(2017, 2023)),
             aes(y = `20-39%`, label = percent(`20-39%`, accuracy = 0.1)),
             fill = "black",           # background
             color = "white",          # text
             label.size = 0,           # no border stroke
             size = 2,
             vjust = 0.5,
             label.padding = unit(0.08, "lines"),
             show.legend = FALSE
  ) +
  scale_y_continuous(name = '% of observations', breaks = c(0.3, 0.2, 0.1, 0), limit = c(0, 0.30),
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  scale_x_continuous(name = '', breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_linetype_manual(name = 'Cumulative three-year loss', values = c("dashed", "solid")) + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 5),
        axis.title = element_text(colour = "black", size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        text = element_text(size = 9, family = "Times"),
        legend.position = "none",
        legend.key.size = unit(0.4, units = "cm")) +
  # annotate("text", x = 2016.6, y = 0.85, label = "Last year\npre-TCJA", size = 2.0, family = 'Times', color = 'red') +
  ggtitle("Panel E: DTA ratio of 20-39%")
plot_e

plot_f <- ggplot(grouped, aes(x = fyear, linetype = ib_neg)) +
  geom_vline(xintercept = 2017, linetype = "solid", color = 'red', linewidth = 0.5) +
  geom_line(aes(y = `1-19%`)) +
  geom_label(data = subset(grouped, fyear %in% c(2017, 2023)),
             aes(y = `1-19%`, label = percent(`1-19%`, accuracy = 0.1)),
             fill = "black",           # background
             color = "white",          # text
             label.size = 0,           # no border stroke
             size = 2,
             vjust = 0.5,
             label.padding = unit(0.08, "lines"),
             show.legend = FALSE
  ) +
  scale_y_continuous(name = '% of observations', breaks = c(0.6, 0.4, 0.2, 0), limit = c(0, 0.60),
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  scale_x_continuous(name = '', breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_linetype_manual(name = 'Cumulative three-year loss', values = c("dashed", "solid")) + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 5),
        axis.title = element_text(colour = "black", size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        text = element_text(size = 9, family = "Times"),
        legend.position = "none",
        legend.key.size = unit(0.4, units = "cm")) +
  # annotate("text", x = 2016.6, y = 0.85, label = "Last year\npre-TCJA", size = 2.0, family = 'Times', color = 'red') +
  ggtitle("Panel F: DTA ratio of 1-19%")
plot_f

plot_g <- ggplot(grouped, aes(x = fyear, linetype = ib_neg)) +
  geom_vline(xintercept = 2017, linetype = "solid", color = 'red', linewidth = 0.5) +
  geom_line(aes(y = `0%`)) +
  geom_label(data = subset(grouped, fyear %in% c(2017, 2023)),
             aes(y = `0%`, label = percent(`0%`, accuracy = 0.1)),
             fill = "black",           # background
             color = "white",          # text
             label.size = 0,           # no border stroke
             size = 2,
             vjust = 0.5,
             label.padding = unit(0.08, "lines"),
             show.legend = FALSE
  ) +
  scale_y_continuous(name = '% of observations', breaks = c(0.2, 0.1, 0), limit = c(0, 0.1),
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  scale_x_continuous(name = '', breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_linetype_manual(name = 'Cumulative three-year loss', values = c("dashed", "solid")) + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 5),
        axis.title = element_text(colour = "black", size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        text = element_text(size = 9, family = "Times"),
        legend.position = "none",
        legend.key.size = unit(0.4, units = "cm")) +
  # annotate("text", x = 2016.6, y = 0.85, label = "Last year\npre-TCJA", size = 2.0, family = 'Times', color = 'red') +
  ggtitle("Panel G: DTA ratio of 0% (no VA)")
plot_g


legend_all <- get_legend(plot_a + theme(legend.position="bottom"))
g <- grid.arrange(plot_a, plot_b, plot_c, plot_d, plot_e, plot_f, plot_g, legend_all, nrow = 5, heights = c(1, 1, 1, 1, 0.15))

setwd("/Users/maltemax/ownCloud/SBE_ACC_Replacement_Surfdrive (Projectfolder)/SBE_ACC_Replacement_Surfdrive (Projectfolder)/projects/footnote/3_pipeline/2_final")
ggsave(file = "dta_ratio_by_year.pdf", width = 15, height = 20, units = "cm", g)

