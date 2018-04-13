 
######################################
# Power Berechnung: Fragestellung A
######################################
pwr_A <- function(rows_sums, cols_sums,  
                  n_repeats = 1000, n_matrices = 3000, 
                  alpha = .05, dev = .6, 
                  item_pos = 2, burnIn = 300, 
                  step = 16, folder = ""){
  
  count(rows_sums, cols_sums) 
  model <- sample(rows_sums, cols_sums, 1)
  half_length <- length(cols_sums) / 2
  groups <- c(rep(1, half_length), rep(0, half_length)) 
  dif <- rep(0, length(cols_sums))
  dif[item_pos] <- dev
  
  path <- paste0(folder, "/",
                 as.character(length(rows_sums)), "x",
                 as.character(length(cols_sums)), ".csv")
  
  mcmc <- exact <- vector("numeric", n_repeats)
  
  mcmc <- replicate(n_repeats, 
                    pwr_mcmc(mat = model, 
                             group = groups,
                             dif = dif,
                             repetitions = n_matrices,
                             alpha = alpha, 
                             burn = burnIn,
                             steps = step))
  exact <- replicate(n_repeats, 
                     pwr_exact(rows = rows_sums, 
                               cols = cols_sums, 
                               group = groups, 
                               dif = dif, 
                               repetitions = n_matrices, 
                               alpha = alpha))
  rio::export(data.frame(power = c(mcmc, exact), 
                           method = rep(c("mcmc", "exact"),
                                        each = n_repeats)), path)
}

###############################################
# Power Berechnung: Fragestellung B, C, D, E
###############################################
pwr_BCDE <- function(itempars, n_repeats = 3000, 
                     n_matrices = 8000, alpha = .05, 
                     n_pers = 100, sd_pers = 2, 
                     dev = .6, burnIn = 300, difficulty = "moderat", 
                     step = 16, folder = ""){
  set.seed(123)
  personenpars <- rnorm(n = n_pers, mean = 0, sd = sd_pers)
  half_length <- length(personenpars) / 2
  groups <- c(rep(1, half_length), rep(0, half_length)) 
  model <- sim.rasch(persons = personenpars, 
                     items = itempars,
                     seed = 123)
  cols_sums <- colSums(model)
  rows_sums <- rowSums(model) 
  dif <- vector("numeric", length(cols_sums))
  mcmc <- vector("numeric", n_repeats)
  path <- paste0(folder, "/",
                 as.character(length(rows_sums)), "x",
                 as.character(length(cols_sums)), "_",
                 as.character(dev), ".csv")
  switch(
    difficulty,
    "leicht" = dif[which(cols_sums == max(cols_sums[-length(itempars)]))[1]] <- dev,
    "moderat" = dif[which(cols_sums == getMiddle(cols_sums[-length(itempars)]))[1]] <- dev,
    "schwer" =  dif[which(cols_sums == min(cols_sums[-length(itempars)]))[1]] <- dev
  )
  mcmc <- replicate(n_repeats, 
                    pwr_mcmc(mat = model, 
                             group = groups,
                             dif = dif,
                             repetitions = n_matrices,
                             alpha = alpha, 
                             burn = burnIn,
                             steps = step))
  rio::export(data.frame(power = mcmc,
                         method = rep(c("mcmc"), n_repeats)), path)
  
}

pwr_F <- function(itempars, n_repeats = 3000, 
                  n_matrices = 8000, alpha = .05, 
                  n_pers = 100, sd_pers = 2, item_pos = 2, 
                  dev = .6, burnIn = 300, difficulty = "moderat", 
                  step = 16, folder = ""){
  # Itempars und personenpars aendern
  half_length <- length(personenpars) / 2
  groups <- c(rep(1, half_length), rep(0, half_length)) 
  model <- sim.rasch(persons = personenpars, 
                     items = itempars,
                     seed = 123)
  cols_sums <- colSums(model)
  rows_sums <- rowSums(model) 
  dif <- vector("numeric", length(cols_sums))
  dif[item_pos] <- dev
  mcmc <- vector("numeric", n_repeats)
  path <- paste0(folder, "/",
                 as.character(length(rows_sums)), "x",
                 as.character(length(cols_sums)), "_",
                 as.character(dev), ".csv")
  mcmc <- replicate(n_repeats, 
                    pwr_mcmc(mat = model, 
                             group = groups,
                             dif = dif,
                             repetitions = n_matrices,
                             alpha = alpha, 
                             burn = burnIn,
                             steps = step))
  rio::export(data.frame(power = mcmc,
                         method = rep(c("mcmc"), n_repeats)), path)
  
}


######################################
# Exact: Konditionale Power Berechnung
######################################
pwr_exact <- function(rows, cols, group, 
                      dif, repetitions, alpha) {
  
  s <- sample(a = rows, b = cols, k = repetitions) 
  t <- colSums(s * group)
  e <- exp(colSums(t * dif))
  pwr <- sum(e[e >= quantile(e, 1 - alpha)]) / sum(e)
  
  return(pwr)
}

#####################################
# MCMC: Konditionale Power Berechnung
#####################################
pwr_mcmc <- function(mat, group, dif, repetitions, 
                     burn, steps, alpha) {
  
  s <- rsampler(mat, controls = rsctrl(n_eff = (repetitions - 1),
                                       burn_in = burn, 
                                       step = steps))
  t <- rstats(s, function(x) colSums(x * group))
  e <- exp(colSums(matrix(unlist(t), ncol = s$n_tot) * dif))
  pwr <- sum(e[e >= quantile(e, 1 - alpha)]) / sum(e)
  
  return(pwr)
}

##########################################################
# Auswertung mit Summary im tidy Format
# INPUTS:
# df: Datensatz als data.frame
# col1: Gruppierende Spalte als quosure
# col2: Auszuwertende Spalte als quosure
# z.B.: auswertung_summary(daten, quo(method), quo(power))
##########################################################
auswertung_summary <- function(df, col1, col2){
  df %>%
    group_by(!!col1) %>%
    summarise(min = min(!!col2, na.rm = T),
              Q.025 = quantile(!!col2, .025, na.rm = T),
              Q.25 = quantile(!!col2, .25, na.rm = T),
              median = median(!!col2, na.rm = T),
              mean = mean(!!col2, na.rm = T),
              Q.75 = quantile(!!col2, .75, na.rm = T),
              Q.975 = quantile(!!col2, .975, na.rm = T),
              max = max(!!col2, na.rm = T),
              sd = sd(!!col2, na.rm = T))
}

########################################
# Allgemeine Auswertungsfunktion
# INPUTS:
# df: Datensaetze als Liste
# col1: Gruppierende Spalte
# col2: Auszuwertende Spalte
# z.B.: auswertung(daten, method, power)
########################################
auswertung <- function(df, col1, col2){
  col1 <- enquo(col1)
  col2 <- enquo(col2)
  
  # Allgemeine Auswertung mit Summary im tidy Format
  a <- df %>%
    map_df(~ auswertung_summary(df = .x, col1 = col1, col2 = col2)) %>%
    mutate(szenario = rep(names(df), each = 2))
  # Vergleich der Standardabweichungen
  b <- a %>% 
    select(!!col1, sd, szenario) %>%
    spread(!!col1, sd) %>%
    mutate(mcmc_smaller = mcmc < exact)

  return(list(Allgemeine_Auswertung = a, 
              Vergleich_Standardabweichungen = b))
}

########################################
# Mittlere Zahl zurueckgeben
# bei ungeraden Itemanzahlen rundet R ab
# (z.B. bei 10.5 nimmt es den 10. Index)
########################################
getMiddle <- function(x){
  sorted <- sort(x)
  middle <- sorted[length(x) / 2]
  return(middle)
}

#############################
# Exact counting
#############################
count <- function(a, b, matrix_type = 0, 
                  input_filename = '._____input_____.dat',
                  table_filename = '._____table_____.bin') {
  # Count the number of matrices with row sums a and column sums b.
  #
  # INPUTS:
  # a = vector of m nonnegative numbers
  # b = vector of n nonnegative numbers, such that sum(a)==sum(b)
  # matrix_type = 0: binary matrices, 1: nonnegative integer matrices
  # input_filename = string that will be used 
  # as a filename for input data.
  # table_filename = string that will be used 
  # as a filename for saving binary data.
  # (This data will be used if you want sample, otherwise you can delete it.)
  #
  # OUTPUT:
  # number = the number of matrices with row sums a and column sums b.
  #          (This is a string, since it may be too large for normal R types.)
  #
  # (Note: This is a wrapper for the executable count.exe.)
  
  m <- length(a)
  n <- length(b)
  
  # generate input file
  sink(input_filename)
  cat(m,n,matrix_type,'\n')
  cat(as.character(a),'\n')
  cat(as.character(b),'\n')
  sink()
  
  # run count.exe
  arguments <- paste(input_filename, table_filename, '1')
  output <- system2('count.exe', arguments, stdout = TRUE, stderr = TRUE)
  status <- attr(output,"status")
  if (!is.null(status)) stop(output)
  
  return(output)
}

#############################
# Exact sampling
#############################
sample <- function(a, b, k, input_filename = '._____input_____.dat',
                   table_filename = '._____table_____.bin',
                   output_filename = '._____output_____.dat') {
  # Draw samples from the uniform distribution 
  # on matrices with specified margins.
  #
  # INPUTS:
  # a = vector of m nonnegative numbers
  # b = vector of n nonnegative numbers, such that sum(a) == sum(b)
  # k = number of samples to draw
  # input_filename = filename of input data 
  # (this must be the same one used in count()).
  # table_filename = filename of saved binary data (same one used in count()).
  # output_filename = string that will be used as a filename for output data.
  #
  # OUTPUT:
  # samples = (m x n x k) array of sampled (m x n) matrices
  #
  # (Note: This is a wrapper for the executable sample.exe.)
  
  if (!file.exists(input_filename))
    stop(sprintf('Input file %s does not exist. 
                 You must call count() first.',input_filename))
  end
  if (!file.exists(table_filename))
    stop(sprintf('Table file %s does not exist. 
                 You must call count() first.',table_filename))
  end
  
  m <- length(a)
  n <- length(b)
  
  # run sample.exe
  arguments <- paste(input_filename, table_filename, output_filename, k, '1')
  output <- system2('sample.exe', arguments, stdout = TRUE, stderr = TRUE)
  status <- attr(output,"status")
  if (!is.null(status)) stop(output)
  
  # read samples from file
  values = scan(output_filename, n = m*n*k, quiet = TRUE)
  
  # rearrange samples into 3-D array
  samples <- aperm(array(values, c(n,m,k)), c(2,1,3))
  
  return(samples)
}
