
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(rio))

source("functions_1.5.R")
##################
# Fragestellung A
##################
files <- dir(here("Fragestellung_A"))
daten <- import_list(here("Fragestellung_A", files), setclass = "tbl")
names(daten) <- str_replace(files, ".csv", "")

a <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 2000),
         method = fct_relevel(method, c("mcmc", "exact")),
         szenario = fct_relevel(szenario, c("10x4", "30x4", "60x4", "90x4", "120x4", "150x4"))) %>% 
  ggplot(aes(x = szenario, y = power, linetype = method)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(0.9), width = 0.4) + 
  geom_boxplot(position = position_dodge(0.9)) +
  labs(x = "Matrix", y = "Power") +
  theme_pubr() + 
  ylim(c(0, 1)) + 
  theme(legend.position = c(.90, .10), 
        legend.text = element_text(size = 16), 
        legend.title = element_blank(),
        text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1)) 
print(a)
##################
# Fragestellung B
##################
files <- dir(here("Fragestellung_B"))
daten <- import_list(here("Fragestellung_B", files), setclass = "tbl")
names(daten) <- str_replace(files, "_0.6.csv", "")

b <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, c("10x25", "30x25", "90x25", 
                                            "150x25", "250x25", "350x25", "500x25"))) %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_boxplot() + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 18)) + 
  ylab("Power") + 
  xlab("Matrix") +
  theme(strip.text.x = element_text(size = 15),
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(b)
##################
# Fragestellung C
##################
files <- dir(here("Fragestellung_C"))
daten <- import_list(here("Fragestellung_C", files), setclass = "tbl")
names(daten) <- str_replace(files, "100x25_", "")
names(daten) <- str_replace(names(daten), ".csv", "")

c <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)))) %>% 
  filter(szenario != "0") %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_boxplot() + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 18)) + 
  ylab("Power") + 
  xlab("DIF-Parameter") + 
  theme(strip.text.x = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(c)
#########################
# Fragestellung D leicht
#########################
files <- dir(here("Fragestellung_D", "leicht"))
daten <- import_list(here("Fragestellung_D", "leicht", files), setclass = "tbl")
names(daten) <- str_replace(files, "100x25_", "")
names(daten) <- str_replace(names(daten), ".csv", "")

d <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8]))) %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_boxplot() + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 18)) + 
  ylab("Power") + 
  xlab("DIF-Parameter") + 
  theme(strip.text.x = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(d)

#########################
# Fragestellung D schwer
#########################
files <- dir(here("Fragestellung_D", "schwer"))
daten <- import_list(here("Fragestellung_D", "schwer", files), setclass = "tbl")
names(daten) <- str_replace(files, "100x25_", "")
names(daten) <- str_replace(names(daten), ".csv", "")

d2 <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8]))) %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_boxplot() + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 18)) + 
  ylab("Power") + 
  xlab("DIF-Parameter") + 
  theme(strip.text.x = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(d2)
##################
# Fragestellung E
##################
files <- dir(here("Fragestellung_E"))
daten <- import_list(here("Fragestellung_E", files), setclass = "tbl")
names(daten) <- str_replace(names(daten), ".csv", "")

e <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000),
         parameter = rep(rep(c("Step", "Burn-In"), 3), each = 3000),
         szenario = fct_relevel(szenario, c("16", "32", "50", "300", "4000", "8000"))) %>% 
  ggplot(aes(x = szenario, y = power)) + 
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +  
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(strip.text.x = element_text(size = 14),
        axis.title = element_text(size = 18),
        text = element_text(size = 14)) + 
  facet_grid(. ~ parameter, scales = "free") + 
  ylab("Power") + 
  xlab("Parameter")
print(e)

plots <- list(C = c, Dleicht = d, 
              Dschwer = d2, E = e)

plots %>%
  walk2(.x = .,
        .y = names(.),
        ~ ggsave(plot = .x,
                 filename = paste0(here("images"), "/", .y, ".jpeg"),
                 width = 5,
                 height = 5,
                 dpi = 500))

ggsave(plot = a,
       filename = "images/A.jpeg",
       width = 8,
       height = 5,
       dpi = 500)

ggsave(plot = b,
       filename = "images/B.jpeg",
       width = 8,
       height = 5,
       dpi = 500)

