###========================================
### Code for Week 13 PVA tutorial
###========================================
### Calling in packages
library(dplyr)
library(ggplot2)
library(popbio)

###========================================
### Section 1: Calculating𝜆from census data
###========================================

### Initiating data
gerber_graywhale_data <- tibble::tibble(Year=c(1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1984,1985,1987,1992,1993,1995),
                                        Whales=c(13041,12289,12816,11182,9813,16958,14839,13160,14835,15936,17170,13330,16593,21922,20465,21123,17673,23098,22565)) # from: https://apps.automeris.io/wpd/ picking the points out of the Gerber et al. 1999 figure 1

### Display data
head(gerber_graywhale_data) # only view the first 6 rows

### Log-transforming whale counts
gerber_graywhale_data <-  gerber_graywhale_data %>% # adding on log-transformed columns
  mutate(logWhales=log(Whales))

### Inspect/display data
head(gerber_graywhale_data)

### Calculating the population growth rate
# Step 1: Run the linear model
whale_model <- lm(logWhales~Year, data=gerber_graywhale_data)
# Step 2: Extract the coefficient corresponding to ln(lambda)
# In R, log = natural log
whale_lambda <- coef(whale_model)[2]
# Step 3: Exponentiate the ln(lambda) estimate to return our estimate of lambda
exp(whale_lambda)

###========================================
### Section 2: Matrix algebra refresher
###========================================

### Matrix addition
A <- matrix(c(2,0,4,6),byrow=T,nrow=2)
B <- matrix(c(1,5,2,7),byrow=T,nrow=2) # note that in R, byrow=T is the same as byrow=TRUE --> 
#this is just telling R to take these values and fill them in by rows not columns
C <- A + B # matrix addition
C # display C 

### Matrix subtraction
C <- A - B
C

### Matrix multiplication
A <- matrix(c(1,0,5,0,4,3,2,6,0),byrow = T,nrow=3)
B <- matrix(c(3,2,1),byrow=FALSE,nrow=3)
C <- A%*%B # %*% is R syntax for matrix multiplication
C # view the matrix product.

###========================================
### Section 3: Modeling st/age structured 
### populations with projection matrices
###========================================

### Defining a projection matrix
oystercatcher_stages <- c("juvenile", "subadult", "adult") # vector storing stage names for oystercatcher
oystercatcher_matrix <- matrix(c(0,0,0.054,0.665,0,0,0,0.724,0.95),byrow=T,
                               nrow=3,dimnames=list(oystercatcher_stages,oystercatcher_stages)) # tack on stage names to vector

oystercatcher_matrix # Box 4.1 in the Morris PVA Handbook

### Growth rate
lambda(oystercatcher_matrix) # What do we observe?
  # Are the oystercatchers increasing, staying stable, 
  # or decreasing over time?

### Projecting the population through time
oystercatcher_n0 <- c(10, 50, 100) # 10 juveniles, 50 subadults, and 100 adults at time 0
# oystercatcher_n0 <- c(1000, 500, 47) # 1000 juveniles, 500 subadults, and 47 adults at time 0 - uncomment to run
  # you can modify this to see what happens and what does or doesn't change
oystercatcher_Nt <- pop.projection(A=oystercatcher_matrix, n=oystercatcher_n0, iterations = 20) # A is the projection matrix
    # n is the starting population vector and iterations is the number of time steps (years in this case)
    # You could change iterations to a larger or smaller number (e.g. iterations = 50 or iterations = 100)

### Creating a data table to store the outputs
oystercatcher_Nt_DF <- tibble::tibble(Time = 1:length(oystercatcher_Nt$pop.sizes), # time steps
                                      Nt = oystercatcher_Nt$pop.sizes)

### Plotting our oystercatcher population model
p <- ggplot(oystercatcher_Nt_DF, aes(x=Time,y=Nt))
p <- p + geom_point() + geom_line()
p <- p + theme_classic()
p <- p + labs(x="Years",y="Oystercatcher population size")
p
  # Does oystercatcher population size change in response to different values of oystercatcher_n0?

### Plotting our oystercatcher population model - distribution by stages
  # y-axis in the plot below is the proportion of the population in each category
stage.vector.plot(oystercatcher_Nt$stage.vectors, col=2:4) 
  # if we change oystercatcher_n0 above, does that change the long-run stage class distribution?

### Calculating elasticities
elasticity(oystercatcher_matrix) %>%
  signif(3) # round to 3 sig-figs

### Visualizing elasticities using nifty function, image2
image2( elasticity(oystercatcher_matrix) )

### Food for thought:
  # What does elasticity tell us about toggling lambda up or down for oystercatchers?
  # Which vital rates are most important to target as a manager?

# You can try this by manipulating:
oystercatcher_stages <- c("juvenile", "subadult", "adult") # vector storing stage names for oystercatcher
oystercatcher_matrix <- matrix(c(0,0.01,0.0,0.665,0,0,0,0.724,0.95), # change the values here - see what happens with lambda
                               byrow=TRUE,nrow=3,
                               dimnames=list(oystercatcher_stages,oystercatcher_stages)) # tack on stage names to vector

# image2(oystercatcher_matrix) # Inspect your modified oystercatcher matrix - uncomment to run
lambda(oystercatcher_matrix) # What do we observe?


