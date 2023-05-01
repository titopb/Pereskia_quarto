library(tidyverse)
library(psych)
library(multcompView)

# treating data

dados <- read.csv("data.csv", stringsAsFactors = T)
dados$treatment <- as.factor(dados$treatment)
dados <- janitor::clean_names(dados)

view(dados)
glimpse(dados)

# atributing variable
variable <- dados$water_potential


#descriptive statistics by group

describeBy(variable, group = dados$treatment)

# need to change the parameter caption for each one
#table_fm <- dados %>% 
 # group_by(treatment) %>% 
  #summarise(Mean = mean(fm),
   #         SD = sd(fm),
    #        median = median(fm))

#table_fm

#ANOVA 
anova <- aov(variable ~treatment, dados)
summary(anova)


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
  ylab(expression(Fresh~matter~g.plant^-1))+   # fix legend
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  ylim(0,1)                                                       # Fix scale 

ggsave("Fig3.tiff", width = 4, height = 3, dpi = 300)
