library(tidyverse)
library(psych)
library(multcompView)
library(here)

# treating data

dados <- read.csv(here("data", "data.csv"))
dados$treatment <- as.factor(dados$treatment)
dados <- janitor::clean_names(dados)

head(dados)

# calculate basic statistics

data_summary <- dados %>% 
  group_by(treatment) %>% 
  summarise(across(fm:ci, list(mean = mean, median = median, sd = sd), 
                   .names = "{col}_{.fn}"))

glimpse(data_summary)


# atributing variable
variable <- dados$water_potential


# descriptive statistics by group
# from package (psych)

##describeBy(variable, group = dados$treatment)

## ANOVA 
#anova <- aov(variable ~treatment, dados)
#summary(anova)

# Perform multiple ANOVA
data_long <- dados %>% pivot_longer(cols = fm:ci, 
                                   names_to = "measurement", values_to = "value")
anova_results <- list()
for (measurement in colnames(dados)[2:22]) {
  anova_results[[measurement]] <- aov(as.formula(paste(measurement, 
    " ~ treatment")), data = dados)
  print(summary(anova_results[[measurement]]))
}

# TukeyHSD

tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display

cld <- multcompLetters4(anova, tukey)
print(cld)


# boxplot 

ggplot(dados, aes(treatment, variable)) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot(aes(fill = treatment), show.legend = F) +
  xlab("") +
  #ylab(expression(Fresh~matter~g.plant^-1))+  # fix legend
  ylab(expression(Psi[w]~(MPa))) 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  ylim(0,-1)                                                       # Fix scale 

ggsave(here("figs","water_potential.tiff"), width = 4, height = 3, dpi = 300)

       