
# Required packages
pacs <- c("tidyverse", "readxl")
sapply(pacs, require, character.only = TRUE)

# Data for mean plots
setwd("C:\\Users\\keiji\\Dropbox\\Nutrition\\Julie Jones")
df <- read_excel("./AJCN/Mac plot data by baseline values2.xlsx")
df <- df %>% 
  mutate(Level = factor(Level), 
         Variable = factor(Variable, levels = unique(df$Variable)))

p1 <- df %>% 
  slice(1:4) %>% 
  ggplot(aes(y = Level, x = MeanDiff, group = Level, color = Level, shape = Level)) +
  geom_point(size = 4) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2, size = 1) +
  xlim(-30, 30) +
  labs(y = expression(paste("BMI,  ", kg/m^2)), 
       x = "Estimated mean difference (Mac - Control), mg/dL", 
       color = "BMI", shape = "BMI", size = 3) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  facet_wrap(Variable~ ., strip.position = "top", ncol = 1) +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "bottom", strip.text.x = element_text(size = 12))

pval <- df %>% 
  slice(1:4) %>% 
  select(Variable, Pval) %>% 
  filter(!is.na(Pval)) %>% 
  mutate(label = paste("Intx p-val =", round(Pval, 3)))

p1 <- p1 + geom_text(data = pval,
                     aes(x = Inf, y = Inf, label = label, group = NULL, color = NULL, shape = NULL),
                     hjust = 1, vjust = 1.3, size = 4)

# WC plots -- not needed anymore
# p2 <- df %>% 
#   slice(7:10) %>% 
#   ggplot(aes(y = Level, x = MeanDiff, group = Level, color = Level, shape = Level)) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2, size = 1) +
#   xlim(-30, 30) +
#   labs(y = "Waist Circumference", 
#        x = "Estimated mean difference (Mac - Control), mg/dL", 
#        color = "WC", shape = "WC", size = 3) +
#   geom_vline(xintercept = 0, color = "black", linetype = 2) +
#   facet_wrap(Variable~ ., strip.position = "top", ncol = 1) +
#   scale_y_discrete(limits = rev) +
#   theme(legend.position = "bottom", strip.text.x = element_text(size = 12))
# 
# pval <- df %>% 
#   slice(7:10) %>% 
#   select(Variable, Pval) %>% 
#   filter(!is.na(Pval)) %>% 
#   mutate(label = paste("Intx p-val =", round(Pval, 3)))
# 
# p2 <- p2 + geom_text(data = pval,
#                      aes(x = Inf, y = Inf, label = label, group = NULL, color = NULL, shape = NULL),
#                      hjust = 1, vjust = 1.3, size = 4)

p3 <- df %>% 
  slice(13:16) %>% 
  ggplot(aes(y = Level, x = MeanDiff, group = Level, color = Level, shape = Level)) +
  geom_point(size = 4) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2, size = 1) +
  xlim(-30, 30) +
  labs(y = "% Body Fat", 
       x = "Estimated mean difference (Mac - Control), mg/dL", 
       color = "% Body Fat", shape = "% Body Fat", size = 3) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  facet_wrap(Variable~ ., strip.position = "top", ncol = 1) +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "bottom", strip.text.x = element_text(size = 12))

pval <- df %>% 
  slice(13:16) %>% 
  select(Variable, Pval) %>% 
  filter(!is.na(Pval)) %>% 
  mutate(label = paste("Intx p-val =", round(Pval, 3)))

p3 <- p3 + geom_text(data = pval,
                     aes(x = Inf, y = Inf, label = label, group = NULL, color = NULL, shape = NULL),
                     hjust = 1, vjust = 1.3, size = 4)

library(patchwork)
cairo_pdf("emmeans all subj paper2.pdf", width = 8.5, height = 6)
p1 + p3
# p1 + p2 + p3
dev.off()




# Female only figures
# Not needed anymore
#
# p4 <- df %>% 
#   slice(19:24) %>% 
#   ggplot(aes(y = Level, x = MeanDiff, group = Level, color = Level, shape = Level)) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2, size = 1) +
#   xlim(-50, 50) +
#   labs(y = expression(paste("BMI in Females,  ", kg/m^2)), 
#        x = "Estimated mean difference (Mac - Control), mg/dL", 
#        color = "BMI", shape = "BMI", size = 3) +
#   geom_vline(xintercept = 0, color = "black", linetype = 2) +
#   facet_wrap(Variable~ ., strip.position = "top", ncol = 1) +
#   scale_y_discrete(limits = rev) +
#   theme(legend.position = "bottom", strip.text.x = element_text(size = 12))
# 
# pval <- df %>% 
#   slice(19:24) %>% 
#   select(Variable, Pval) %>% 
#   filter(!is.na(Pval)) %>% 
#   mutate(label = paste("Intx p-val =", round(Pval, 3)))
# 
# p4 <- p4 + geom_text(data = pval,
#                      aes(x = Inf, y = Inf, label = label, group = NULL, color = NULL, shape = NULL),
#                      hjust = 1, vjust = 1.3, size = 4)
# 
# p5 <- df %>% 
#   slice(25:30) %>% 
#   ggplot(aes(y = Level, x = MeanDiff, group = Level, color = Level, shape = Level)) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2, size = 1) +
#   xlim(-50, 50) +
#   labs(y = "Waist Circumference in Females", 
#        x = "Estimated mean difference (Mac - Control), mg/dL", 
#        color = "WC", shape = "WC", size = 3) +
#   geom_vline(xintercept = 0, color = "black", linetype = 2) +
#   facet_wrap(Variable~ ., strip.position = "top", ncol = 1) +
#   scale_y_discrete(limits = rev) +
#   theme(legend.position = "bottom", strip.text.x = element_text(size = 12))
# 
# pval <- df %>% 
#   slice(25:30) %>% 
#   select(Variable, Pval) %>% 
#   filter(!is.na(Pval)) %>% 
#   mutate(label = paste("Intx p-val =", round(Pval, 3)))
# 
# p5 <- p5 + geom_text(data = pval,
#                      aes(x = Inf, y = Inf, label = label, group = NULL, color = NULL, shape = NULL),
#                      hjust = 1, vjust = 1.3, size = 4)
# 
# p6 <- df %>% 
#   slice(31:36) %>% 
#   ggplot(aes(y = Level, x = MeanDiff, group = Level, color = Level, shape = Level)) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2, size = 1) +
#   xlim(-50, 50) +
#   labs(y = "% Body Fat in Females", 
#        x = "Estimated mean difference (Mac - Control), mg/dL", 
#        color = "% Body Fat", shape = "% Body Fat", size = 3) +
#   geom_vline(xintercept = 0, color = "black", linetype = 2) +
#   facet_wrap(Variable~ ., strip.position = "top", ncol = 1) +
#   scale_y_discrete(limits = rev) +
#   theme(legend.position = "bottom", strip.text.x = element_text(size = 12))
# 
# pval <- df %>% 
#   slice(31:36) %>% 
#   select(Variable, Pval) %>% 
#   filter(!is.na(Pval)) %>% 
#   mutate(label = paste("Intx p-val =", round(Pval, 3)))
# 
# p6 <- p6 + geom_text(data = pval,
#                      aes(x = Inf, y = Inf, label = label, group = NULL, color = NULL, shape = NULL),
#                      hjust = 1, vjust = 1.3, size = 4)
# 
# cairo_pdf("emmeans females paper.pdf", width = 13, height = 9)
#  p4 + p5 + p6
# # p1 + p2 + p3 + plot_layout(design = layout)
# dev.off()