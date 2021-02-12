###========================================
### Code in Section 5
###========================================

### Loading packages with helpful functions
library(dplyr) 
# R will let you know if loading in this package over-rides any existing function names; don't worry about this message for now.

### Reading in data
USdata <- readr::read_csv("https://raw.githubusercontent.com/charlottehchang/BIOL104PO/master/data/StateSpeciesDatasetS21.csv") # Here, we are using the function read_csv from the package readr
# R will print out a message saying how it has parsed the data table into different columns

### Previewing the data
USdata

### Subsetting our data by rows
filter(USdata, ThreatMammals >= 50) # select all rows (each row is a state) where there are 10 or more threatened mammals
filter(USdata, State=="CA" | State=="AZ") # select all rows corresponding to the states of California OR Arizona. The vertical bar (|) denotes an "OR" operation
filter(USdata, TotMammals > 50 & TotBirds > 400) # select all rows with more than 50 mammals and more than 400 birds

### Selecting columns from the data table
select(USdata,EndVascPlants) # Here, we are selecting the column of endemic vascular plant richness counts
select(USdata, FarmAcres2018) # Here, we are selecting the column storing the number of acres of land (in units of 1000 acres)

### Creating new variables and storing them in new columns
thousand_acres_to_miles <- 1.56 # multiplier to convert 1000 acres to square miles
USdata <- mutate(USdata, FarmAcres2018Mi2 = FarmAcres2018*thousand_acres_to_miles) # taking the conversion rate for 1000 acres to units of square miles and creating a new variable that stores farm area in each state in square miles
# Ensure that we store this output in the variable USdata, so that way the new column will show up when we call or refer to USdata
USdata <- mutate(USdata, FarmProportion = FarmAcres2018Mi2/StateLandAreaMi2)
## A few additional operations
summary( select(USdata, FarmProportion) )  # Summarize the proportion of land that is farm area across the 50 states.

### Creating a summary across a subset of data
summarize(USdata, min_number_threatened_birds = min(ThreatBirds), 
          mean_number_threatened_birds = mean(ThreatBirds),
          max_number_threatened_birds = max(ThreatBirds)) # Creating a small table that summarizes the minimum and maximum species richness of threatened bird species across the 50 states

###========================================
### Section 7: Combining operations using pipes
###========================================

acres_to_miles <- 1.56/1000 # multiplier to convert 1 acre to square miles 
# You don't need to re-copy and re-run this line if you already defined this object before in your R session

comparisonDF <- USdata %>% # passing the object USdata to the next command
  mutate(proportion_land_protected = (TotalProtectedAreasAcres * acres_to_miles)/StateLandAreaMi2 ) %>% # I don't need to refer to the USdata object! mutate() knows to use that object. Instead, I can directly refer to the new column I want to create from existing columns.
  mutate(State_category=case_when(proportion_land_protected <= 0.2 ~ "Low",
                                  proportion_land_protected <= 0.5 ~ "Medium",
                                  proportion_land_protected > 0.5 ~ "High")) %>%
  group_by(State_category) %>%
  summarize(EndemicInverts = mean(EndInverts),
            EndemicPlants = mean(EndVascPlants + EndNonVascPlants))

comparisonDF

# Note that in this code chunk, we didn't define the USdata_with_land_protection object. We didn't need to have some intermediate variable to store the intermediate outputs!
# Note also that if you call USdata again, you wouldn't see these new columns (e.g. proportion_land_protected) in that data table. That's because we never assigned the output of mutate back to the USdata object (this can be a good thing when you don't care or don't want intermediate data outputs to muck up your original data)