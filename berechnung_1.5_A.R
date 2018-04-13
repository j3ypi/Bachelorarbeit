
###########################
# Fragestellung A
###########################
library(here)
library(eRm)
source("functions_1.5.R")
###########################
# 4 x 10
###########################
persSums <- c(rep(1, 3), rep(2, 5), rep(3, 2))
itemSums <- c(8, 6, 3, 2)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")
###########################
# 4 x 30
###########################
persSums <- c(rep(1, 8), rep(2, 15), rep(3, 7))
itemSums <- c(23, 17, 12, 7)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")
###########################
# 4 x 60
###########################
persSums <- c(rep(1, 17), rep(2, 30), rep(3, 13))
itemSums <- c(49, 34, 21, 12)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")
###########################
# 4 x 90
###########################
persSums <- c(rep(1, 26), rep(2, 45), rep(3, 19))
itemSums <- c(71, 50, 34, 18)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")
###########################
# 4 x 120
###########################
persSums <- c(rep(1, 38), rep(2, 60), rep(3, 22))
itemSums <- c(92, 69, 41, 22)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")
###########################
# 4 x 150
###########################
persSums <- c(rep(1, 46), rep(2, 75), rep(3, 29))
itemSums <- c(117, 89, 53, 24)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")

