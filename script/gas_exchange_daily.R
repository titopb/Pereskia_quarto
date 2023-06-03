library(tidyverse)
library(here)

# load data

data <- read.csv(here("data", "dados2.csv"))
data <- janitor::clean_names(data)
head(data)

# calculate means and standart deviations

summary_data <- data %>% 
  group_by(time, treatment) %>% 
  summarise(mean_pn = mean(pn), 
            sd_pn = sd(pn),
            mean_gs = mean(gs),
            sd_gs = sd(gs),
            .groups = 'drop')

print(summary_data)

# Plot Pn with error bars

pn_plot <- ggplot(summary_data, aes(x = time, y = mean_pn, 
                                    color = treatment, 
                                    shape = treatment)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_pn - sd_pn, ymax = mean_pn + sd_pn), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Time (hours)", y = expression(paste("P"[N], " (", mu, "mol m"^-2," s"^-1, ")"))) +
  scale_x_continuous(breaks = seq(0, 24, by = 3)) +
  scale_color_manual(values = c("control" = "cyan3", "drought" = "coral1")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pn_plot

# save Pn plot as .tiff file

ggsave(here("figs", "pn_plot.tiff"), pn_plot, width = 8, height = 6, dpi = 300)

# Plot gs with error bars

gs_plot <- ggplot(summary_data, aes(x = time, y = mean_gs, color = treatment, shape = treatment)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_gs - sd_gs, ymax = mean_gs + sd_gs), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Time (hours)", y = expression(paste("g"[s], " (", "mol m"^-2," s"^-1, ")"))) +
  scale_x_continuous(breaks = seq(0, 24, by = 3)) +
  scale_color_manual(values = c("control" = "cyan3", "drought" = "coral1")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

gs_plot

# save gs plot as .tiff file

ggsave(here("figs", "gs_plot.tiff"), gs_plot, width = 8, height = 6, dpi = 300)
