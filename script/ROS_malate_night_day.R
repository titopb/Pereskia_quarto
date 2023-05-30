library(tidyverse)
library(here)

# treating data

data <- read.csv(here("data", "data3.csv"))
data <- janitor::clean_names(data)
data$treatment <- as.factor(data$treatment)
data$period <- as.factor(data$period)
data$period <- factor(data$period, levels = c("light", "dark"))

head(data)

glimpse(data)


# calculate basic statistics

summary_data <- data %>% 
  group_by(treatment, period) %>% 
  summarise(
    mean_TBAR = mean(tbars, na.rm = TRUE),
    sd_TBAR = sd(tbars, na.rm = TRUE),
    mean_H2o2 = mean(h2o2, na.rm = TRUE),
    sd_H2o2 = sd(h2o2, na.rm = TRUE),
    mean_cat = mean(cat, na.rm = TRUE),
    sd_cat = sd(cat, na.rm = TRUE),
    mean_sod = mean(sod, na.rm = TRUE),
    sd_sod = sd(sod, na.rm = TRUE),
    mean_apx = mean(apx, na.rm = TRUE),
    sd_apx = sd(apx, na.rm = TRUE),
    mean_malate = mean(malate, na.rm = TRUE),
    sd_malate = sd(malate, na.rm = TRUE)
  )

print(summary_data)


# Bar plot TBARS with SD.

tbars_plot <- ggplot(summary_data, aes(x = treatment, y = mean_TBAR, 
                                      fill = period)) +
  geom_bar(stat = "identity", position = "dodge",
           color = "black", width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_TBAR - sd_TBAR, ymax = mean_TBAR + sd_TBAR),
    position = position_dodge(0.7),
    width = 0.3
  ) + 
  labs(x = " ", y = expression(paste("TBARS nnmol g"^-1," FM"))) +
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(tbars_plot)

ggsave(here("figs", "tbars_plot.tiff"), tbars_plot, width = 8, height = 6, dpi = 300)

# bar plot H2O2 with sd.

h2o2_plot <- ggplot(summary_data, aes(x = treatment, y = mean_H2o2, 
                                       fill = period)) +
  geom_bar(stat = "identity", position = "dodge",
           color = "black", width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_H2o2 - sd_H2o2, ymax = mean_H2o2 + sd_H2o2),
    position = position_dodge(0.7),
    width = 0.3
  ) + 
  labs(x = " ", y = expression(paste("H"[2], "O"[2], " nmol g"^-1," FM"))) +
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(h2o2_plot)

ggsave(here("figs", "h2o2_plot.tiff"), tbars_plot, width = 8, height = 6, dpi = 300)

# bar plot CAT with sd.

cat_plot <- ggplot(summary_data, aes(x = treatment, y = mean_cat, 
                                      fill = period)) +
  geom_bar(stat = "identity", position = "dodge",
           color = "black", width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_cat - sd_cat, ymax = mean_cat + sd_cat),
    position = position_dodge(0.7),
    width = 0.3
  ) + 
  labs(x = " ", y = expression(paste("Catalase activity ", mu, "mol H"[2],"O"[2],  " mg"^-1," protein min"^-1))) +
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(cat_plot)

ggsave(here("figs", "cat_plot.tiff"), tbars_plot, width = 8, height = 6, dpi = 300)
