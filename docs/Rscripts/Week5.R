###========================================
### Code for Week 5 R tutorial
###========================================

### Loading packages with helpful functions
library(ggplot2)
library(dplyr)
POMblue <- "#0057B7" # Pomona blue hex code

###========================================
### Code to perform species-area calculations
### in tutorial (Section 1 of document)
###========================================

### Code to create a data table storing the Southeast Asian bird data
seAsiaBirds <- tibble::tibble(areaMi2=c(312000,290000,144000,70000,48000,25000,4500,8870,18000,4600),
                              birds=c(540,420,368,220,337,232,111,143,137,108))

### Display first few rows of the Southeast Asia bird dataset
seAsiaBirds

### Create a plot of bird richness versus island area
p <- ggplot(seAsiaBirds,aes(x=areaMi2, y=birds))
p <- p + geom_point()
p <- p + labs(x="Area (square miles)",y="Bird species richness")
p <- p + theme_classic()
p

###========================================
### Code to log-log transform species-area
### data for the birds of Southeast Asia
###========================================

### Below, we first apply log10 to the columns of our data on SE Asian birds
seAsiaBirds <- seAsiaBirds %>%
  mutate(log10Area = log10(areaMi2), log10Birds = log10(birds))

# Display seAsiaBirds in the console
seAsiaBirds

# Confirm that the log-log transformation linearized our data
p <- ggplot(seAsiaBirds,aes(x = log10Area, y = log10Birds))
p <- p + geom_point()
p <- p + labs(x="Area (log-transformed)",y="Birds (log-transformed)")
p <- p + theme_classic()
p

### Use lm to calculate the values of d and z and store the model in the object speciesAreaModel
speciesAreaModel <- lm(log10Birds ~ log10Area, data=seAsiaBirds)
## See the values estimates for d and z
print("The cofficient names (Intercept) corresponds to d and log10Area to z")
coef(speciesAreaModel)
  ## Store the estimates in the objects d and z
d <- coef(speciesAreaModel)[1] # first item in this two-item vector is the intercept of the straight line fit to our log-log transformed Southeast Asian birds dataset
z <- coef(speciesAreaModel)[2] # second item in the two-item vector is the slope

###========================================
### Estimating species losses from habitat
### loss
###========================================

### We can take the value of z that we calculated in the Southeast Asia
### bird dataset and use that to calculate 1) what proportion of species
### would remain after habitat loss and 2) what number of species would be
### lost after habitat loss.
S_0 = 111 # initially we have 111 species in this example (Palawan)
R = 0.9 # We lose 10% of habitat in this example: (1-R)*100% = 10%
species_proportion_surviving <- R^z
species_lost <- S_0 * (1-R^z)

###========================================
### Refresher on logarithms - what does
### log-transforming data do? (Section 1.1.1)
###========================================

### Recall the log-log species-area relationship:
### log S = log(c A^z) (Equation 1)
### we can confirm that the previous equation is 
### mathematically equivalent to the following one:
### log S = log c + log(A^z) (Eqn. 2)
### which is in turn equal to:
### log S = log c + z log A (Eqn. 3)
### For convenience, we'll call c and z something different:
### c1 and z1
### That way, we don't overwrite the previously calculated values
### for c and z from the Southeast Asia bird dataset

### Let c1 = 5, z1 = 0.28, A = 1000
### We'll calculate both expressions
### and confirm that they are identical
c1 <- 5
z1 <- 0.28
A <- 1000
eqn1 <- log10(c1*A^z1)
eqn1 # value of the first equation
eqn2 <- log10(c1) + log10(A^z1)
eqn2 # value of second equation
eqn3 <- log10(c1) + z1*log10(A)
eqn3 # value of third equation

###========================================
### Prioritizing decisions in conservation
### Section 2 of the tutorial
###========================================

### Creating a simulated conservation dataset
### This simulated data has 20 sites with different
### costs and benefits. These could be 20 habitat patches,
### or some other set of 20 locations where we have to
### decide which sites we would prioritize for
### conservation actions.
set.seed(47) # instantiating random seed to ensure reproducibility
conservationDF <- tibble::tibble(
  site_id = c(1:20), # site ID
  costs = runif(20,min=200,max=1000), # cost of enacting conservation
  value = 1/(1 + exp(-costs/max(costs))) * abs(rlnorm(20,2,0.5)) * 100 # ecosystem benefit of each site
)

conservationDF # display the conservation dataset

### Generate scatterplot for value versus cost
p <- ggplot(conservationDF, aes(x=costs, y=value, label=site_id))
p <- p + geom_label(position=position_dodge2(width=0.8),fill=POMblue,alpha=0.4,label.padding=unit(0.2,"lines"),label.size=0) # plot site labels with commands to avoid overplotting, remove label border
p <- p + labs(x="Cost per site", y="Ecosystem service benefit") # change x- and y-axis labels
p <- p + theme_light() # change background appearance of plot
p

dplyr::arrange(conservationDF, desc(value))[1:4,]
# Alternative:
# View(conservationDF) # then in the viewer pane, you can use the interactive column toggling feature - delete leading pound sign (#) to uncomment and run this code

### Creating a column that stores value per cost for the 20 sites
conservationDF <- conservationDF %>%
  mutate(vpw = value/costs) # value per cost for each site

### View sites ordered by the vpw column
dplyr::arrange(conservationDF, desc(vpw))[1:6,]

### Generate scatterplot
p <- ggplot(conservationDF, aes(x=value, y=vpw, label=site_id)) # conservation value on the x-axis, value per cost on y-axis
p <- p + geom_label(position=position_dodge2(width=0.8),fill=POMblue,alpha=0.4,label.padding=unit(0.2,"lines"),label.size=0) # plot site labels with commands to avoid overplotting, remove border
p <- p + labs(x="Benefit", y="Benefit/Cost") # change x- and y-axis labels
p <- p + theme_minimal() # change background theme
p

## Function to solve for "nearly-optimal" selection of sites
## to maximize biodiversity conservation
  # Don't worry too much about the specifics of this function - just copy it into your R console
  # It will use different rules of thumb to find which sites are (approximately) optimal for conserving
conservation_site_selection <- function(conservationDF, maxcost) {
  knapsack <- data.frame(conservationDF)
  capacity <- maxcost
  
  # 1) Greedy algorithm
  # sort by value per weight
  knapsack <- knapsack[order(knapsack$vpw, decreasing = TRUE),]
  # fill knapsack with all items taken in this sequence
  # and still fitting into the knapsack
  packed_items <- c()
  packed_weight <- 0
  packed_value <- 0
  for (id in 1:nrow(knapsack)) {
    item <- knapsack[id, ]
    left_capacity <- capacity - packed_weight
    # check if next item in the sequence still fits into knapsack
    if (left_capacity > 0 && item$costs <= left_capacity) {
      packed_items <- c(packed_items, item$site_id)
      packed_weight <- packed_weight + item$costs
      packed_value <- packed_value + item$value
    } 
  }
  
  # 2) Addition to "modified greedy"
  # determine most valuable single item fitting into knapsack
  knapsack <- knapsack[order(knapsack$value, decreasing = TRUE),]
  best_single_item <- 0
  best_single_value <- 0
  best_single_weight <- 0
  for (id in 1:nrow(knapsack)) {
    item <- knapsack[id, ]
    # check if next item in the sequence still fits into knapsack
    if (item$costs <= capacity) {
      best_single_item <- item$site_id
      best_single_value <- item$value
      best_single_weight <- item$costs
      break
    } 
  }
  
  # now return best configuration
  if (best_single_value < packed_value) {
    ret_val <- tibble::tibble(Sites=paste(packed_items,collapse=", "),
                              TotalBiodiv=packed_value,
                              TotalCost=packed_weight)
  } else {
    ret_val <- tibble::tibble(Sites=best_single_item,
                              TotalBiodiv=best_single_value,
                              TotalCost=best_single_weight)
  }
  return(ret_val)
}

### Solving for the most optimal set of sites
result <- conservation_site_selection(conservationDF = conservationDF, maxcost=1500)
  # See which sites are selected:
result

###========================================
### Additional exercises 
### Section 3 of the tutorial
###========================================

# Copy the code over from Section 3 of https://charlottehchang.github.io/BIOL104PO/Week5.html
# To run the code in your console
