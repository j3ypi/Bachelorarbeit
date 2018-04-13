
###########################
# Fragestellung B|C|D|E
###########################
library(tidyverse)
library(here)
library(eRm)
source("functions_1.5.R")
###########################
# Itempars (25)
###########################
itempars <- c(seq(from = -3, to = -1, by = 0.5),
              seq(from = -1, to = 1, by = 0.125)[-1],
              seq(from = 1, to = 3, by = 0.5)[-1])

############################
# Fragestellung B
############################
npers <- c(10, 30, 90, 150, 250, 350, 500)
npersList <- split(npers, seq_along(npers))

npersList %>%
  map(~ pwr_BCDE(itempars, n_pers = ., folder = "Fragestellung_B"))

############################
# Fragestellung C|D
############################
dif <- seq(from = -1.75, to = 1.75, by = .25)[-8]
difList <- split(dif, seq_along(dif))

difList %>%
  map(~ pwr_BCDE(itempars, dev = ., 
                  difficulty = "moderat", folder = "Fragestellung_C"))

difList %>% 
  map(~ pwr_BCDE(itempars, dev = ., n_repeats = 3000,
                  difficulty = "leicht", folder = "Fragestellung_D/leicht"))

difList %>% 
  map(~ pwr_BCDE(itempars, dev = ., n_repeats = 3000,
                  difficulty = "schwer", folder = "Fragestellung_D/schwer"))

############################
# Fragestellung E
############################
pwr_BCDE(itempars, burnIn = 300, folder = "Fragestellung_E/300")
pwr_BCDE(itempars, burnIn = 4000, folder = "Fragestellung_E/4000")
pwr_BCDE(itempars, burnIn = 8000, folder = "Fragestellung_E/8000")

pwr_BCDE(itempars, step = 16, folder = "Fragestellung_E/16")
pwr_BCDE(itempars, step = 32, folder = "Fragestellung_E/32")
pwr_BCDE(itempars, step = 50, folder = "Fragestellung_E/50")
