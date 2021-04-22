###========================================
### Code for Week 13 PVA tutorial
### Part 2: Thursday
###========================================

###========================================
### 1: Modeling st/age structured 
### populations with projection matrices
###========================================

### Defining the projection matrix and initial population vector
A <- matrix(c(1,4,0.5,0),byrow=TRUE,nrow=2) # projection matrix
A # display the projection matrix
n0 <- matrix(c(6,0),nrow=2) # initial population vector
n0 # display the initial population vector

### Running through 9 years in our model and seeing how 
### the population changes over time
n0 <- matrix(c(6,0),nrow=2) # initial population vector ; redefine here to make sure we get consistent results below.
for (i in 1:9) {
  nt <- A%*%n0
  rownames(nt) <- c("Seedling","Adult")
  colnames(nt) <- paste("Pop'n @ time:",i,collapse=" ")
  print(nt)
  print(paste("Seedling to adult ratio:",signif(nt[1,1]/nt[2,1],2),collapse=" "))
  print(paste("Growth rate:",signif(sum(nt)/sum(n0), 3), collapse=""))
  n0 <- nt # update n0 to take on the previous step's nt value
}

###========================================
### 2: Khaya senegalensis example
###========================================

### Loading packages
library(dplyr)    # data wrangling package
library(magrittr) # pipe %>% function
library(ggplot2)  # plotting library
library(readr)    # interacting with data tables
library(popbio) # load popbio package

### Reading in the data
KhayaDF <- readr::read_tsv("https://raw.githubusercontent.com/charlottehchang/BIOL104PO/master/data/KhayaMatrices.tsv")

### Vector storing the different names for the conditions
khaya_conditions <- c("High S","High B","Low S","Low B") # high harvest or low harvest in the Sakarou (S) or Boukoussera (B) populations in Central Benin

### Processing the data to extract the matrices
khaya_matrices <- list()
khaya_stages <- c("Seed","Sap","Juv","SmAd","LgAd") # Seedling, Sapling, Juvenile, Small adult tree, and Large adult tree
for (i in 1:length(khaya_conditions)) { # iterate through the 4 matrix conditions
  condition <- khaya_conditions[i] # select condition number i (element number i) from the khaya_conditions vector
  # Below, we walk through the spreadsheet and programmatically extract each 
  khaya_mat_i <- KhayaDF %>% 
    dplyr::filter(Condition==condition) %>% # filter the rows in the spreadsheet corresponding to this condition
    dplyr::select(A1:A5) # select the columns that store the matrix data
  # Now we store that corresponding matrix
  khaya_matrices[[i]] <- as.matrix(khaya_mat_i) # convert the data to a matrix format
  colnames(khaya_matrices[[i]]) <- khaya_stages # store the stage names
  rownames(khaya_matrices[[i]]) <- khaya_stages
}

names(khaya_matrices) <- gsub(" ","",khaya_conditions) # assigning names to the projection matrices

### Here we will calculate the population growth rate, lambda
### for each of the four projection matrices.
khaya_popbio <- tibble::tibble(Popn = khaya_conditions,
                               lambdas = sapply( khaya_matrices, lambda)) # initiate a data table

khaya_popbio # print our data table

### Q: Which populations are growing or declining? Are any staying stable?

### Here we display a heat map of one of the projection matrices
### Specifically, we will pick the second matrix (High harvest in Boukoussera),
### which has lambda = 0.98
image2(khaya_matrices$HighB) # equivalently, image2(khaya_matrices[[2]])

### What is the survivorship rate for large adult trees?
### How many offspring do large adult trees produce?

###========================================
### 2.1: Showing trajectories for 
### K. senegalensis populations
###========================================

### First, we will specify a starting population vector
n0 <- c(500, 200, 100, 80, 50) # 500 seedlings, 200 saplings, 100 juveniles, 80 small adult, and 50 large adult trees
  # You can arbitrarily change these 5 values to any (>= 0) number you like
number_years <- 100

### Use the popbio package to calculate projections for our populations
highB_Nt <- pop.projection(A=khaya_matrices$HighB, n=n0, iterations = number_years) 
  # We project the population forward using the projection matrix A, 
  # the starting population vector n0, and for number_years time duration.
lowB_Nt <- pop.projection(A=khaya_matrices$LowB, n=n0, iterations = number_years)

### Creating a data table to store the outputs
B_Nt_DF <- tibble::tibble(Time = rep(1:number_years, 2), # time steps
                          Nt = c(highB_Nt$pop.sizes, lowB_Nt$pop.sizes),
                          Harvest = c(rep("High",number_years),rep("Low",number_years)))

### Plotting the population model for the two Boukoussera populations
p <- ggplot(B_Nt_DF, aes(x=Time,y=Nt,color=Harvest))
p <- p + geom_point() + geom_line()
p <- p + scale_color_manual(values=c("purple","orange"))
p <- p + theme_classic() + theme(legend.direction = "horizontal",legend.position="top")
p <- p + labs(x="Years",y="Population size")
p
  ### What do we see?

### What does the distribution of stage classes look like?
stage_colors <- c("#fc8d62","#8da0cb","#e78ac3","#a6d854","#66c2a5") # from https://colorbrewer2.org/#type=qualitative&scheme=Set2&n=5
stage.vector.plot(lowB_Nt$stage.vectors, col=stage_colors, main="Low harvest population stage plot") 
stage.vector.plot(highB_Nt$stage.vectors, col=stage_colors, main="High harvest population stage plot",ylim=c(0,0.6)) 

###========================================
### 3: Population viability analysis
###========================================

### Let's take a look again at those values 
### of lambda associated with each population's
### projection matrix
khaya_popbio # print our data table

### Setting up our parameters for our stochastic projection
popn_probabilities <- c(0.3, 0.3, 0.2, 0.2) # this is the most critical parameter.
  # This specifies that the high harvest conditions occur 30% of the time 
  # and the low harvest conditions 20% of the time
n0 <- c(500, 200, 100, 80, 50) # 500 seedlings, 200 saplings, 100 juveniles, 80 small adult, and 50 large adult trees
  # You can arbitrarily change these 5 values to any (>= 0) number you like
number_years <- 100 # number of time steps to run each simulation
n_sims <- 1000 # number of simulations to run

### Running the stochastic population simulation
khaya_stoch <- stoch.projection(khaya_matrices, n0 = n0, tmax = number_years, nreps=n_sims)
khaya_stoch_sum <- rowSums(khaya_stoch) # final population size for each of the n_sim number of simulations 

### Data frame to store the simulated outputs
khaya_stoch_DF <-  tibble::tibble(simRun = 1:n_sims, # simulated run index
                                  Nend = khaya_stoch_sum) # final population size for each simulation                  
head(khaya_stoch_DF) # view first few entries

### Visualize our distribution of final population sizes
p <- ggplot(khaya_stoch_DF, aes(x=Nend))
p <- p + geom_histogram()
p <- p + labs(x="Final population size",y="Frequency")
p <- p + geom_vline(xintercept = 500, color="red", linetype=2, lwd=2)
p <- p + theme_bw()
p
  # The vertical red dashed line corresponds to one threshold for 
  # "minimum viable population" size--500 individual trees.
  # What do we observe below? 
  # (And is there some way to see when we fall below that extinction threshold?)

### Assess extinction probability through time
nExt <- 500 # extinction threshold; otherwise we use the same parameters as above
khaya_stoch_ext <- stoch.quasi.ext(matrices = khaya_matrices, n0 = n0, Nx = nExt, 
                                   tmax = number_years, nreps = n_sims)

### Plotting the cumulative probability of extinction across
### 10 model runs where each simulation had n_sims number of 
### simulated populations for number_years time.
ext_title <- paste("Time to reach a quasi-extinction threshold of",nExt,"individuals",collapse="")
matplot(khaya_stoch_ext, xlab="Years", ylab="Quasi-extinction probability",
        type='l', lty=1, col=rainbow(10), las=1,
        main=ext_title)

###========================================
### 4: Performing thought experiments in conservation
###========================================

expMat <- khaya_matrices$HighB # experimental matrix storing the high harvest Boukoussera matrix
image2(khaya_matrices$HighB)
## Here we will re-assign the value for juvenile survivorship
  # Note that survivorship should not exceed 1 in total!
expMat[3,3] <- 0.95 # increasing juvenile survivorship
lambda(expMat)
expMat <- khaya_matrices$HighB # You can comment this out to examine layering changes together
  # But we are re-setting those values here.

## Re-assigning adult survivorship
expMat[4,4] <- 0.93 # increasing small adult tree survivorship
  # You can toggle this value differently
lambda(expMat)
expMat <- khaya_matrices$HighB

## Re-assigning adult seedling production / fecundity
expMat[1,5] <- 4 # increasing large tree seedling production
lambda(expMat)

### It turns out that we can see which values have 
### the largest proportional contribution to changing lambda
elasticity(khaya_matrices$HighB) %>%
  image2()
  # What does this show?

###========================================
### 5: Exploring these concepts further
###========================================

### Setting up our parameters for our stochastic projection
popn_probabilities <- c(P1, P2, P3, P4) # this is the most critical parameter.
  # Change these values yourself to probabilities that sum to 1.
n0 <- c(500, 200, 100, 80, 50) # 500 seedlings, 200 saplings, 100 juveniles, 80 small adult, and 50 large adult trees
# You can arbitrarily change these 5 values to any (>= 0) number you like
nExt <- 500 # extinction threshold; you could change this
# Past conservation papers have recommended a 50/100/500 threshold

### Running the stochastic population simulation
khaya_stoch <- stoch.projection(khaya_matrices, n0 = n0, tmax = number_years, nreps=n_sims)
khaya_stoch_sum <- rowSums(khaya_stoch) # final population size for each of the n_sim number of simulations 

### Data frame to store the simulated outputs
khaya_stoch_DF <-  tibble::tibble(simRun = 1:n_sims, # simulated run index
                                  Nend = khaya_stoch_sum) # final population size for each simulation                  
# head(khaya_stoch_DF) # view first few entries; uncomment by deleting leading # to run

### Visualize our distribution of final population sizes
p <- ggplot(khaya_stoch_DF, aes(x=Nend))
p <- p + geom_histogram()
p <- p + labs(x="Final population size",y="Frequency")
p <- p + geom_vline(xintercept = 500, color="red", linetype=2, lwd=2)
p <- p + theme_bw()
p
  # The vertical red dashed line corresponds to one threshold for 
  # "minimum viable population" size--500 individual trees.
  # What do we observe below?

### Calculating cumulative probabilities over time of falling below the quasi-extinction threshold
khaya_stoch_ext <- stoch.quasi.ext(matrices = khaya_matrices, n0 = n0, Nx = nExt, 
                                   tmax = number_years, nreps = n_sims)

### Plotting the cumulative probability of extinction across
### 10 model runs where each simulation had n_sims number of 
### simulated populations for number_years time.
ext_title <- paste("Time to reach a quasi-extinction threshold of",nExt,"individuals",collapse="")
matplot(khaya_stoch_ext, xlab="Years", ylab="Quasi-extinction probability",
        type='l', lty=1, col=rainbow(10), las=1,
        main=ext_title)

### Based on what you've observed today,
### what values might you be especially concerned
### about, prioritize for additional surveying
### to assess accuracy, or prioritize for
### conservation interventions?
expMat <- khaya_matrices$HighS # experimental matrix storing the high harvest Sakarou matrix
  # this population is in a very bad way - lambda ~ 0.85
image2(khaya_matrices$HighS) # use a heatmap to display the values of projection matrix HighS

### Which vital rates have the largest 
### proportional contribution to changing lambda?
elasticity(khaya_matrices$HighS) %>%
  image2()

## Here we will re-assign the value for juvenile survivorship
# Note that survivorship should not exceed 1 in total!
expMat[X,Y] <- VALUE # change X and Y to the row and column you seek to change
# change VALUE to the VALUE you'd want to change that matrix entry to.
# e.g. maybe I'd write:
# expMat[5,5] <- 0.7 # to increase adult survivorship from 0.625 to 0.7
lambda(expMat)

# expMat <- khaya_matrices$HighS # uncomment to restore the values of HighS to exp(erimental) Mat(rix)
# Changing another value
expMat[X,Y] <- VALUE # change X and Y to the row and column you seek to change
# change VALUE to the VALUE you'd want to change that matrix entry to.
lambda(expMat)

