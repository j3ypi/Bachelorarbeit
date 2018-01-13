
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(rio))

source(here("functions_1.3.R"))

# Fragestellung A
files <- list.files(here("Fragestellung_A"))
daten <- import_list(here("Fragestellung_A", files), setclass = "tbl")
names(daten) <- str_replace_all(files, "_0.6_moderat.csv", "")
names(daten) <- str_replace_all(names(daten), "_", "x")

dfA <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 6000)) 

dfA$method <- factor(dfA$method, levels = c("mcmc", "exact"), ordered = T)  

a <- ggplot(dfA, aes(x = szenario, y = power, linetype = method)) + 
  geom_boxplot(position = position_dodge(0.8)) + 
  stat_summary(fun.y = mean, geom = "point", 
               position = position_dodge(0.8), color = "black") + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(legend.position = c(.15, .15), 
        legend.text = element_text(size = 13), 
        legend.title = element_blank(),
        axis.title = element_text(size = 15)) + 
  ylab("Power") + 
  xlab("Matrix") 

# Fragestellung B
files <- list.files(here("Fragestellung_B"))
daten <- import_list(here("Fragestellung_B", files), setclass = "tbl")
names(daten) <- str_replace_all(files, "_0.6_moderat.csv", "")
names(daten) <- str_replace_all(names(daten), "_", "x")

dfB <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 6000)) 

dfB$method <- factor(dfB$method, levels = c("mcmc", "exact"), ordered = T)  
dfB$szenario <- factor(dfB$szenario, levels = c("30x20", names(daten)[-3]), ordered = T)  

b <- ggplot(dfB, aes(x = szenario, y = power, linetype = method)) + 
  geom_boxplot(position = position_dodge(0.8)) + 
  stat_summary(fun.y = mean, geom = "point", 
               position = position_dodge(0.8), color = "black") + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(legend.position = c(.15, .15), 
        legend.text = element_text(size = 13), 
        legend.title = element_blank(),
        axis.title = element_text(size = 15)) + 
  ylab("Power") + 
  xlab("Matrix") 

# Fragestellung C
files <- list.files(here("Fragestellung_C"))
daten <- import_list(here("Fragestellung_C", files), setclass = "tbl")
names(daten) <- str_replace_all(files, "_0.\\d_moderat.csv", "")
names(daten) <- str_replace_all(names(daten), "_", "x")

dfC <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 6000)) %>%
  mutate(dif = rep(c("0.3", "0.9", "0.3", "0.9"), each = 6000))

dfC$method <- factor(dfC$method, levels = c("mcmc", "exact"), ordered = T)  

append1 <- import(here("Fragestellung_A", "100_30_0.6_moderat.csv"), setclass = "tbl")
append2 <- import(here("Fragestellung_B", "350_20_0.6_moderat.csv"), setclass = "tbl")

dfC <- append1 %>%
  rbind(., append2) %>%
  mutate(szenario = rep(c("100x30", "350x20"), each = 6000)) %>%
  mutate(dif = rep("0.6", 12000)) %>%
  rbind(dfC, .)

c <- ggplot(dfC, aes(x = szenario, y = power, linetype = method)) + 
  geom_boxplot(position = position_dodge(0.8)) + 
  stat_summary(fun.y = mean, geom = "point", 
               position = position_dodge(0.8), color = "black") + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(legend.position = c(.15, .88), 
        legend.text = element_text(size = 13), 
        legend.title = element_blank(),
        axis.title = element_text(size = 15)) + 
  ylab("Power") + 
  xlab("Matrix") + 
  facet_grid(. ~ dif)

# Fragestellung D
files <- list.files(here("Fragestellung_D"))
daten <- import_list(here("Fragestellung_D", files), setclass = "tbl")
names(daten) <- str_replace_all(files, "_0.6_\\w+.csv", "")
names(daten) <- str_replace_all(names(daten), "_", "x")

dfD <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 6000)) %>%
  mutate(difficulty = rep(c("leicht", "schwer", "leicht", "schwer"), each = 6000))

dfD$method <- factor(dfD$method, levels = c("mcmc", "exact"), ordered = T)  

dfD <- append1 %>%
  rbind(., append2) %>%
  mutate(szenario = rep(c("100x30", "350x20"), each = 6000)) %>%
  mutate(difficulty = rep("moderat", 12000)) %>%
  rbind(dfD, .)

d <- ggplot(dfD, aes(x = szenario, y = power, linetype = method)) + 
  geom_boxplot(position = position_dodge(0.8)) + 
  stat_summary(fun.y = mean, geom = "point", 
               position = position_dodge(0.8), color = "black") + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(legend.position = c(.15, .15), 
        legend.text = element_text(size = 13), 
        legend.title = element_blank(),
        axis.title = element_text(size = 15)) + 
  ylab("Power") + 
  xlab("Matrix") + 
  facet_grid(. ~ difficulty)

# Fragestellung E
files <- list.files(here("Fragestellung_E"))
daten <- import_list(here("Fragestellung_E", files), setclass = "tbl")
names(daten) <- c("300", "4000", "8000")

dfE <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 3000)) 

e <- ggplot(dfE, aes(x = szenario, y = power)) + 
  geom_boxplot() +  
  stat_summary(fun.y = mean, geom = "point", 
               position = position_dodge(0.8), color = "black") + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 15)) + 
  ylab("Power") + 
  xlab("BurnIn Phase")

# Fragestellung F
files <- list.files(here("Fragestellung_F"))
daten <- import_list(here("Fragestellung_F", files), setclass = "tbl")
names(daten) <- c("16", "32", "64", "90")

dfF <- daten %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(daten), each = 6000)) %>% 
  filter(method == "mcmc")

f <- ggplot(dfF, aes(x = szenario, y = power)) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", 
               position = position_dodge(0.8), color = "black") + 
  theme_pubr() + 
  ylim(c(0, 1)) +
  theme(axis.title = element_text(size = 15)) + 
  ylab("Power") + 
  xlab("Step Parameter")

save_graphs <- function(plot){
  for (i in seq_along(plot)) {
    ggsave(filename = paste0(as.character(names(plot)[i]), 
                             ".jpeg"), 
           plot = plot[[i]], 
           device = "jpeg", 
           width = 5, 
           height = 5, 
           dpi = 1000)
  }  
}

plots <- list(A = a, B = b, C = c, D = d, E = e, Ff = f)

save_graphs(plots)

