
library(tidyverse)
library(here)
library(rio)
library(xtable)

source("functions_1.5.R")
##################
# Fragestellung A
##################
files <- dir(here("Fragestellung_A"))
datenA <- import_list(here("Fragestellung_A", files), setclass = "tbl")
names(datenA) <- str_replace(files, ".csv", "")

datenA %>% 
  auswertung(method, power) 
##################
# Fragestellung B
##################
files <- dir(here("Fragestellung_B"))
datenB <- import_list(here("Fragestellung_B", files), setclass = "tbl")
names(datenB) <- str_replace(files, "_0.6.csv", "")

datenB %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(datenB), each = 3000),
         szenario = fct_relevel(szenario, c("10x25", "30x25", "90x25", 
                                            "150x25", "250x25", "350x25", "500x25"))) %>% 
  auswertung_summary(quo(szenario), quo(power)) 

# %>% 
#   xtable() %>% 
#   print(include.rownames = FALSE)  
  
##################
# Fragestellung C
##################
files <- dir(here("Fragestellung_C"))
datenC <- import_list(here("Fragestellung_C", files), setclass = "tbl")
names(datenC) <- str_replace(files, "100x25_", "")
names(datenC) <- str_replace(names(datenC), ".csv", "")

datenC %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(datenC), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)))) %>% 
  auswertung_summary(quo(szenario), quo(power)) %>% 
  filter(szenario != 0)
#########################
# Fragestellung D leicht
#########################
files <- dir(here("Fragestellung_D", "leicht"))
datenD <- import_list(here("Fragestellung_D", "leicht", files), setclass = "tbl")
names(datenD) <- str_replace(files, "100x25_", "")
names(datenD) <- str_replace(names(datenD), ".csv", "")

datenD %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(datenD), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8]))) %>% 
  auswertung_summary(quo(szenario), quo(power)) %>% 
  mutate(diff = mean - median)
#########################
# Fragestellung D schwer
#########################
files <- dir(here("Fragestellung_D", "schwer"))
datenD2 <- import_list(here("Fragestellung_D", "schwer", files), setclass = "tbl")
names(datenD2) <- str_replace(files, "100x25_", "")
names(datenD2) <- str_replace(names(datenD2), ".csv", "")

datenD2 %>%
  map_df(rbind) %>%
  mutate(szenario = rep(names(datenD2), each = 3000),
         szenario = fct_relevel(szenario, as.character(seq(-1.75, 1.75, 0.25)[-8]))) %>% 
  auswertung_summary(quo(szenario), quo(power)) 


