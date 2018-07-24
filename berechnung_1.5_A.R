
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

a <- count(persSums, itemSums)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")

# Neuer Versuch weil Power zu hoch
persSums <- c(rep(1, 2), rep(2, 5), rep(3, 3))
itemSums <- c(8, 6, 4, 3)
count(persSums, itemSums)

pwr_A(persSums, itemSums, folder = "Test")
###########################
# 4 x 30
###########################
persSums <- c(rep(1, 8), rep(2, 15), rep(3, 7))
itemSums <- c(23, 17, 12, 7)

b <- count(persSums, itemSums)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")
###########################
# 4 x 60
###########################
persSums <- c(rep(1, 17), rep(2, 30), rep(3, 13))
itemSums <- c(49, 34, 21, 12)

c <- count(persSums, itemSums)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")
###########################
# 4 x 90
###########################
persSums <- c(rep(1, 26), rep(2, 45), rep(3, 19))
itemSums <- c(71, 50, 34, 18)

d <- count(persSums, itemSums)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")
###########################
# 4 x 120
###########################
persSums <- c(rep(1, 38), rep(2, 60), rep(3, 22))
itemSums <- c(92, 69, 41, 22)

e <- count(persSums, itemSums)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")
###########################
# 4 x 150
###########################
persSums <- c(rep(1, 46), rep(2, 75), rep(3, 29))
itemSums <- c(117, 89, 53, 24)

f <- count(persSums, itemSums)

pwr_A(persSums, itemSums, folder = "Fragestellung_A")

library(xtable)

tibble(
  `Matrix size` = c("10 x 4", "30 x 4", "60 x 4", "90 x 4", "120 x 4", "150 x 4"),
  `N matrices` = c(a, b, c, d, e, f)
) %>% 
  xtable() %>% 
  print(include.rownames = FALSE)  
  

