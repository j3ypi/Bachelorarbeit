
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(stringr))

setwd("C:/users/j3ypi/Desktop/Bachelorarbeit/Berechnung_1.3")
setwd("~/Dropbox/Berechnung_1.3")
source("functions_1.3.R")

mcmc_.1 <- read_files(toDesktop("Bachelorarbeit/Daten/Mcmc/SD_.1"))
exact_.1 <- read_files(toDesktop("Bachelorarbeit/Daten/Exact/SD_.1"))
sd_.1 <- bind_Lists(mcmc_.1, exact_.1)

mcmc_.3 <- read_files(toDesktop("Bachelorarbeit/Daten/Mcmc/SD_.3"))
exact_.3 <- read_files(toDesktop("Bachelorarbeit/Daten/Exact/SD_.3"))
sd_.3 <- bind_Lists(mcmc_.3, exact_.3)

steps <- read_files("eins/")

auswertung_allgemein(steps, "method", "power")

##########################
# Fragestellung A / B / C
# SD = 0.1
##########################
auswertung_allgemein(sd_.1, "method", "power")

#########################
# Fragestellung A / B / C
# SD = 0.3
#########################
auswertung_allgemein(sd_.3, "method", "power")

###############################
# Fragestellung D
# Vergleich der Burn In Phasen
###############################
burn <- read_csv(toDesktop("Bachelorarbeit/Daten/BurnIn/C_BurnIn.csv"))
auswertung_summary(burn, "method", "power")

step <- read_files("~/Dropbox/Daten/Fragestellung_F")
auswertung_allgemein(step, "method", "power")

test <- read_files("~/Dropbox/Daten/Fragestellung_B")
auswertung_allgemein(test, "method", "power")
# #############################################
# # Graphiken
# #############################################
# make_graph <- function(df){
# 
#   g <- ggboxplot(data = df, x = "method", y = "power", width = .8)
#   g <- ggpar(g, xlab = "Methode", ylab = "Power")
#   
#   return(g)
# }
# 
# ggplot(data = daten[[1]], mapping = aes(x = method, y = power)) + geom_boxplot()
# ggboxplot(data = daten[[1]], x = "method", y = "power", width = .8)

# Beispiel ausprobieren
# moeglicherwiese keine gleichverteilung aus der beim raschsampler gezogen wird
# testen ob Unterschied bei mehr als 8000 matrizen weg
cols <- c(1, 1, 1)
rows <- c(2, 2, 2)





