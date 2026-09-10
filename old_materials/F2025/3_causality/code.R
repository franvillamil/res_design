setwd("~/Documents/course_materials/res_design")
library(modelsummary)
library(dplyr)
library(ggplot2)
set.seed(7891234)

df = data.frame(
   intelligence = rnorm(1000, mean = 5, sd = 1.5),
   social_skills = rnorm(1000, mean = 5, sd = 1.5))

cor(df$intelligence, df$social_skills)

df = df %>%
 mutate(hired = 3 * intelligence + 2 * social_skills) %>%
 mutate(hired = hired / max(hired)) %>% 
 mutate(hired_binary = ifelse(hired > quantile(hired, 0.75), 1, 0))

ml = list(
  "Model 1" = lm(social_skills ~ intelligence, data = df),
  "Model 2" = lm(social_skills ~ intelligence + hired_binary, data = df))
modelsummary(ml, estimate = "{estimate}{stars}", output="html")

### PLOTS

pdf("slides/img/collider1.pdf", height = 5, width = 5)
ggplot(df, aes(x = intelligence, y = social_skills)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,10))
dev.off()

pdf("slides/img/collider2.pdf", height = 5, width = 5)
ggplot(df, aes(x = intelligence, y = social_skills)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,10))
dev.off()

pdf("slides/img/collider3.pdf", height = 5, width = 5)
ggplot(df, aes(x = intelligence, y = social_skills, color = hired)) +
  geom_point() +
  # geom_smooth(method="lm") +
  theme_bw() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,10)) +
  scale_color_gradient(low="red",high="green")
dev.off()

pdf("slides/img/collider4.pdf", height = 5, width = 5)
ggplot(df %>% filter(hired_binary == 1),
    aes(x = intelligence, y = social_skills)) +
  geom_point() +
  # geom_smooth(method="lm") +
  theme_bw() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,10))
dev.off()

pdf("slides/img/collider5.pdf", height = 5, width = 5)
ggplot(df %>% filter(hired_binary == 1),
    aes(x = intelligence, y = social_skills)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,10))
dev.off()