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
dados <- read.csv(here("data", "data.csv"))
dados$treatment <- as.factor(dados$treatment)
dados <- janitor::clean_names(dados)

head(dados)

# Calculate basic statistics
data_summary <- basic_statistics(dados, "treatment", c("fm", "ci"))
glimpse(data_summary)

# Perform multiple ANOVA and Tukey tests
measurements <- colnames(dados)[2:22]
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

# Plot boxplots 

ggplot(dados, aes(treatment, fm)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  ylab(expression(Fresh~matter~g.plant^-1))+  # fix legend
  #ylab(expression(Psi[w]~(MPa)))+
  #ylab("FM") +
theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  ylim(0,800)                                                       # Fix scale 

ggsave(here("figs","fresh_matter.tiff"), width = 8, height = 6, dpi = 300)
  

