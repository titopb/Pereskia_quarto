library(tidyverse)
library(multcompView)
library(here)
library(ggrepel)

# Functions

# Calculate basic statistics
basic_statistics <- function(df, group_col, stat_cols) {
  df %>%
    group_by(!!sym(group_col)) %>%
    summarise(across(all_of(stat_cols), 
                     list(mean = mean, median = median, sd = sd),
                     .names = "{col}_{.fn}"))
}

# Perform ANOVA
perform_anova <- function(df, formula) {
  aov(as.formula(formula), data = df)
}

# Perform Tukey test
perform_tukey <- function(anova_result) {
  TukeyHSD(anova_result)
}

# Display Tukey letters
tukey_significance_letters <- function(tukey_result) {
  p_values <- tukey_result$treatment[, "p adj"]
  names(p_values) <- rownames(tukey_result$treatment)
  multcompLetters(p_values)
}

# Main script


# Load and clean data
dados <- read.csv(here("data", "data_with_wue.csv"))
dados$treatment <- as.factor(dados$treatment)
dados <- janitor::clean_names(dados)

head(dados)

# Calculate WUE and iWUE

dados <- dados %>% 
  mutate(wue = pn/e) %>% 
  mutate(iwue = pn/gs)

head (dados)

#save new file with WUE and iWUE

write.csv(dados, here("data", "data_with_wue.csv"), row.names = FALSE)

# Calculate basic statistics
data_summary <- basic_statistics(dados, "treatment", names(dados)[-1])
glimpse(data_summary)

# Perform multiple ANOVA and Tukey tests
measurements <- colnames(dados)[2:26]
anova_results <- lapply(measurements, 
                        function(m) perform_anova(dados, paste(m, "~ treatment")))
names(anova_results) <- measurements

tukey_results <- lapply(anova_results, perform_tukey)
names(tukey_results) <- measurements

# Print results and Tukey letters
for (measurement in measurements) {
  cat("\nANOVA results for", measurement, ":\n")
  print(summary(anova_results[[measurement]]))
  
  cat("\nTukey test results for", measurement, ":\n")
  print(tukey_results[[measurement]])
  
  tukey_letters <- tukey_significance_letters(tukey_results[[measurement]])
  cat("\nTukey letters for", measurement, ":\n")
  print(tukey_letters)
}

## Plot boxplots ##

# FvFM
ggplot(dados, aes(treatment, fvfm)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab("FvFm")+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,1)      

ggsave(here("figs","fvfm.tiff"), width = 90, height = 90, units = "mm", dpi = 300)


# Y(II)
ggplot(dados, aes(treatment, y_ii)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab("Y(II)")+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,0.8) 


ggsave(here("figs","yii.tiff"), width = 90, height = 90, units = "mm", dpi = 300)

# NPQ

ggplot(dados, aes(treatment, npq)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab("NPQ")+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,2) 


ggsave(here("figs","npq.tiff"), width = 90, height = 90, units = "mm", dpi = 300)

# 1-qP

ggplot(dados, aes(treatment, x1_q_p)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab("1-qP")+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,1.5) 


ggsave(here("figs","1qP.tiff"), width = 90, height = 90, units = "mm", dpi = 300)

# Y(NO)

ggplot(dados, aes(treatment, y_no)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab("Y(NO)")+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,1.5) 


ggsave(here("figs","yno.tiff"), width = 90, height = 90, units = "mm", dpi = 300)

# YNPQ

ggplot(dados, aes(treatment, y_npq)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab("Y(NPQ)")+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,1.5) 


ggsave(here("figs","ynpq.tiff"), width = 90, height = 90, units = "mm", dpi = 300)

# Pn

ggplot(dados, aes(treatment, pn)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab(expression(paste("P"[N], " (", mu, "mol m"^-2, " s"^-1, ")")))+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,10) 


ggsave(here("figs","pn.tiff"), width = 90, height = 90, units = "mm", dpi = 300)

# gs 

ggplot(dados, aes(treatment, gs)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab(expression(paste("g"[s], " (", "mol m"^-2, " s"^-1, ")")))+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,100) 


ggsave(here("figs","gs.tiff"), width = 90, height = 90, units = "mm", dpi = 300)

# E

ggplot(dados, aes(treatment, e)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab(expression(paste("E", " (mmol m"^-2, " s"^-1, ")")))+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,3) 


ggsave(here("figs","E.tiff"), width = 90, height = 90, units = "mm", dpi = 300)

# Ci

ggplot(dados, aes(treatment, ci)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab(expression(paste("C"[i], " (", mu, "mol . mol"^-1, ")")))+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,500) 


ggsave(here("figs","Ci.tiff"), width = 90, height = 90, units = "mm", dpi = 300)

# iWUE
ggplot(dados, aes(treatment, iwue)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab(expression(paste("WUE"[i], " (", mu, "mol mol"^-1, ")")))+  # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15)) + # Axis text size
  theme(axis.title = element_text(size = 18)) + #Axix title size
  scale_fill_manual(values = c("control" = "cyan3", "drought" = "coral1", "recovery" = "darkolivegreen2"))+
  ylim(0,0.4)                                                       # Fix scale 


  
  ggsave(here("figs","iWUE.tiff"), width = 90, height = 90, units = "mm", dpi = 300)
  

