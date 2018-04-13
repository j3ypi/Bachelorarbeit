
library(eRm)
library(here)
source("functions_1.5.R")

itempars <- c(seq(from = -3, to = -1, by = 0.5),
              seq(from = -1, to = 1, by = 0.125)[-1],
              seq(from = 1, to = 3, by = 0.5)[-1])

set.seed(123)
personenpars <- rnorm(n = 100, mean = 0, sd = 2)
half_length <- length(personenpars) / 2
groups <- c(rep(1, half_length), rep(0, half_length)) 
model <- sim.rasch(persons = personenpars, 
                   items = itempars,
                   seed = 123)
cols_sums <- colSums(model)
rows_sums <- rowSums(model) 

# Welches Item abweicht
# welche Spaltenrandsumme
which(cols_sums == max(cols_sums[-length(itempars)]))[1]
max(cols_sums[-length(itempars)])

which(cols_sums == getMiddle(cols_sums[-length(itempars)]))[1]
getMiddle(cols_sums[-length(itempars)])

which(cols_sums == min(cols_sums[-length(itempars)]))[1]
min(cols_sums[-length(itempars)])













