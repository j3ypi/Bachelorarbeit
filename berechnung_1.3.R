
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(eRm))
suppressPackageStartupMessages(library(tidyverse))

setwd("C:/users/j3Ypi/Desktop/Bachelorarbeit/Berechnung_1.3")
source("functions_1.3.R")
setwd("~/Dropbox/Berechnung_1.3")
if (Sys.info()[1] == "Windows") {
  cl <- makeCluster(4)
  registerDoParallel(cl)
}

##################################
# Fragestellung A:
# Einfluss der Itemanzahl auf 
# Genauigkeit der Powerberechnung
# der drei Tests
##################################
calc_pwr(n_items = 10, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_A/")
calc_pwr(n_items = 20, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_A/")
calc_pwr(n_items = 30, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_A/")
calc_pwr(n_items = 40, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_A/")
calc_pwr(n_items = 50, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_A/")

##################################
# Fragestellung B:
# Einfluss der Personenanzahl auf 
# Genauigkeit der Powerberechnung
# der drei Tests
##################################

calc_pwr(n_pers = 30, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_B/")
calc_pwr(n_pers = 150, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_B/")
calc_pwr(n_pers = 250, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_B/")
calc_pwr(n_pers = 350, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_B/")
calc_pwr(n_pers = 500, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_B/")

#####################################
# Fragestellung C:
# Einfluss der Abweichung der 
# Itemparameter auf Genauigkeit 
# der Powerberechnung der drei Tests
####################################

calc_pwr(n_pers = 100, n_items = 30, dev = .3, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_C/")
calc_pwr(n_pers = 100, n_items = 30, dev = .9, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_C/")
calc_pwr(n_pers = 350, n_items = 20, dev = .3, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_C/")
calc_pwr(n_pers = 350, n_items = 20, dev = .9, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_C/")

#####################################
# Fragestellung D:
# Einfluss der Itemschwierigkeit
# auf Genauigkeit der Powerberechnung
####################################

calc_pwr(n_pers = 100, n_items = 30, difficulty = "leicht", folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_D/")
calc_pwr(n_pers = 100, n_items = 30, difficulty = "schwer", folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_D/")
calc_pwr(n_pers = 350, n_items = 20, difficulty = "leicht", folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_D/")
calc_pwr(n_pers = 350, n_items = 20, difficulty = "schwer", folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_D/")

##################################
# Fragestellung E:
# Einfluss der burnIn-Phase auf 
# Genauigkeit der Powerberechnung
# durch MCMC
##################################

calc_pwr(test = "mcmc", burnIn = 300, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_E/one/")
calc_pwr(test = "mcmc", burnIn = 4000, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_E/two/")
calc_pwr(test = "mcmc", burnIn = 8000, folder = "C:/users/j3ypi/Dropbox/Daten/Fragestellung_E/three/")

#####################################
# Fragestellung F:
# Einfluss des step Parameters für
# Genauigkeit der Markov Chain
####################################

calc_pwr(step = 16, folder = "eins/")
calc_pwr(step = 32, folder = "zwei/")
calc_pwr(step = 64, folder = "drei/")
calc_pwr(step = 90, folder = "drei/")



