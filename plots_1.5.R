
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(rio))
suppressPackageStartupMessages(library(ggridges))

source("functions_1.5.R")
##################
# Fragestellung A
##################
files <- dir(here("Fragestellung_A"))[-2]
daten <- import_list(here("Fragestellung_A", files), setclass = "tbl")
names(daten) <- str_replace(files, ".csv", "")

desc <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 2000),
         method = fct_relevel(method, c("mcmc", "exact")),
         szenario = fct_relevel(szenario, c("10x4", "30x4", "60x4", "90x4", "120x4", "150x4"))) %>% 
  group_by(method, szenario) %>% 
  summarise(sd = sd(power)) %>% 
  mutate(pos = rep(0.3, 6))

a <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 2000),
         method = fct_relevel(method, c("mcmc", "exact")),
         szenario = fct_relevel(szenario, c("10x4", "30x4", "60x4", "90x4", "120x4", "150x4"))) %>% 
  ggplot(aes(x = szenario, y = power, linetype = method)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(0.9), width = 0.4) + 
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9)) +
  labs(x = "Matrix", y = "Power") +
  theme_pubr() + 
  ylim(c(0, 1)) + 
  theme(legend.position = c(.90, .10), 
        legend.text = element_text(size = 20), 
        legend.title = element_blank(),
        text = element_text(size = 19),
        axis.title = element_text(size = 21),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = desc, aes(x = szenario, y = pos, label = round(sd, 3)), 
            position = position_dodge(0.9), size = 5, show.legend = FALSE, vjust = -1)
print(a)
##################
# Fragestellung B
##################
files <- dir(here("Fragestellung_B"))
daten <- import_list(here("Fragestellung_B", files), setclass = "tbl")
names(daten) <- str_replace(files, "_0.6.csv", "")

desc <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, c("10x25", "30x25", "90x25", 
                                            "150x25", "250x25", "350x25", "500x25"))) %>% 
  select(-method) %>% 
  group_by(szenario) %>% 
  summarise(sd = sd(power)) %>% mutate(pos = rep(0.04, 7))

b <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, c("10x25", "30x25", "90x25", 
                                            "150x25", "250x25", "350x25", "500x25"))) %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 21)) + 
  ylab("Power") + 
  xlab("Matrix") +
  theme(text = element_text(size = 19),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = desc, aes(x = szenario, y = pos, 
                             label = round(sd, 3)), size = 5, show.legend = FALSE, vjust = -1)
print(b)
##################
# Fragestellung C
##################
files <- dir(here("Fragestellung_C"))
daten <- import_list(here("Fragestellung_C", files), setclass = "tbl")
names(daten) <- str_replace(files, "100x25_", "")
names(daten) <- str_replace(names(daten), ".csv", "")

desc <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)))) %>% 
  filter(szenario != "0") %>% 
  select(-method) %>% 
  group_by(szenario) %>%
  summarise(min = min(power), sd = sd(power))
  
c <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)))) %>% 
  filter(szenario != "0") %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 21)) + 
  ylab("Power") + 
  xlab("DIF-Parameter") + 
  theme(text = element_text(size = 19),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(data = desc, aes(x = szenario, y = min - 0.07, 
                             label = round(sd, 3)), size = 5, show.legend = FALSE, vjust = -1)
print(c)

plot3 <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)))) %>% 
  filter(szenario != "0") %>% 
  select(-method) %>% 
  ggplot(aes(x = power, y = szenario)) + 
  geom_density_ridges(aes(fill = paste(szenario)), 
                      alpha = 0.8, 
                      color = "white", 
                      scale = 3.5, from = 0, to = 1) +
  labs(x = "Power", y = "DIF-Parameter") +
  theme_ridges(font_size = 21) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_cyclical(values = c(rep("#8080ff", 12))) +
  theme(axis.title = element_text(size = 21))

plot3

#########################
# Fragestellung D leicht
#########################
files <- dir(here("Fragestellung_D", "leicht"))
daten <- import_list(here("Fragestellung_D", "leicht", files), setclass = "tbl")
names(daten) <- str_replace(files, "100x25_", "")
names(daten) <- str_replace(names(daten), ".csv", "")

desc <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8]))) %>% 
  select(-method) %>% 
  group_by(szenario) %>% 
  summarise(min = min(power), sd = sd(power))

d <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8]))) %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 21)) + 
  ylab("Power") + 
  xlab("DIF-Parameter") + 
  theme(text = element_text(size = 19),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(data = desc, aes(x = szenario, y = min - 0.08, 
                             label = round(sd, 3)), size = 5, show.legend = FALSE, vjust = -1)
print(d)

#########################
# Fragestellung D schwer
#########################
files <- dir(here("Fragestellung_D", "schwer"))
daten <- import_list(here("Fragestellung_D", "schwer", files), setclass = "tbl")
names(daten) <- str_replace(files, "100x25_", "")
names(daten) <- str_replace(names(daten), ".csv", "")

desc <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8]))) %>% 
  select(-method) %>% 
  group_by(szenario) %>%
  summarise(min = min(power),
            q25 = quantile(power, 0.25),
            mean = mean(power),
            median = median(power),
            q75 = quantile(power, 0.75),
            max = max(power),
            sd = sd(power)) 

d2 <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8]))) %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 21)) + 
  ylab("Power") + 
  xlab("DIF-Parameter") + 
  theme(text = element_text(size = 19),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = desc, aes(x = szenario, y = min - 0.1, 
                             label = round(sd, 3)), size = 5, show.legend = FALSE, vjust = -1)
print(d2)

# Mit Mittelwerten
d2means <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8]))) %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(data = desc, aes(x = szenario, y = mean, size = 1), 
             color = "red", alpha = 0.7, show.legend = FALSE) + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 21)) + 
  ylab("Power") + 
  xlab("DIF-Parameter") + 
  theme(text = element_text(size = 19),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = desc, aes(x = szenario, y = min - 0.1, 
                             label = round(sd, 3)), size = 5, show.legend = FALSE, vjust = -1)
print(d2means)

# Verteilungen 
plot2 <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8])),
         lage = rep(c("negativ", "positiv"), each = 21000)) %>% 
  select(-method) %>% 
  ggplot(aes(x = power, y = szenario)) + 
  geom_density_ridges(aes(fill = paste(szenario, lage)), 
                      alpha = 0.8, 
                      color = "white", 
                      scale = 3.5, from = 0, to = 1) +
  labs(x = "Power", y = "DIF-Parameter") +
  theme_ridges(font_size = 21) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_cyclical(values = c("#ff8080", "#ff8080", "#ff8080", "#ff8080", 
                                 "#ff8080", "#ff8080", "#ff8080", "#8080ff", 
                                 "#8080ff", "#8080ff", "#8080ff", "#8080ff", 
                                 "#8080ff", "#8080ff")) +
  theme(axis.title = element_text(size = 21))
        
plot2
##################
# Fragestellung E
##################
# files <- dir(here("Fragestellung_E"))
# daten <- import_list(here("Fragestellung_E", files), setclass = "tbl")
# names(daten) <- str_replace(names(daten), ".csv", "")
# 
# e <- daten %>%
#   map_df(rbind) %>%
#   mutate(szenario = rep(names(daten), each = 3000),
#          parameter = rep(rep(c("Step", "Burn-In"), 3), each = 3000),
#          szenario = fct_relevel(szenario, c("16", "32", "50", "300", "4000", "8000"))) %>% 
#   ggplot(aes(x = szenario, y = power)) + 
#   stat_boxplot(geom = "errorbar", width = 0.4) +
#   geom_boxplot() +  
#   theme_pubr() + 
#   ylim(c(0, 1)) +
#   theme(strip.text.x = element_text(size = 14),
#         axis.title = element_text(size = 18),
#         text = element_text(size = 14)) + 
#   facet_grid(. ~ parameter, scales = "free") + 
#   ylab("Power") + 
#   xlab("Parameter")
# print(e)

plots <- list(A = a, B = b, C = c, Dleicht = d, 
              Dschwer = d2, Dmeans = d2means, verteilung = plot2, verteilung_normal = plot3)

plots %>%
  walk2(.x = .,
        .y = names(.),
        ~ ggsave(plot = .x,
                 filename = paste0(here("images"), "/", .y, ".jpeg"),
                 width = 9,
                 height = 6,
                 dpi = 500))

ggsave(plot = plot2,
       filename = "images/verteilung.jpeg",
       width = 9,
       height = 6,
       dpi = 500)

ggsave(plot = b,
       filename = "images/B.jpeg",
       width = 8,
       height = 5,
       dpi = 500)

