###========================================
### Code in Sections 1 & 2 of document
###========================================

### Load dplyr package into R workspace
library(dplyr)

### Read in data
CA_county_data <- readr::read_tsv("https://raw.githubusercontent.com/charlottehchang/BIOL104PO/master/data/CA_protectedareas_datasheet.tsv") # this is a link to the Provided Datasheet for the class project

### Take a look at the first few rows of the data table
CA_county_data
  # Open up a spreadsheet viewer
View(CA_county_data)

### Using pipes to link up commands to 
  ### 1) subset CA_county_data based on the values in the column Birds
  ### and 2) select a subset of columns from the data table
  ### We will store the output in a new object called birdsTreesIUCN
birdsTreesIUCN <- CA_county_data %>% # -->
  filter(Birds >= 210) %>%
  select(County,iucn_threat_count_species,Trees,Birds)

birdsTreesIUCN # display our subset of the CA_county_data data table

### Using pipes to 1) calculate proportion farmland in each county
### and 2) summarize proportion farmland across the 5 regions in CA.
### We will store the output in a new object called CA_farm_summary
CA_farm_summary <- CA_county_data %>% # Starting my linked functions with the data object CA_county_data
  mutate(farmland_proportion = farmland_area_HAs/area_of_county_ha) %>% # Running the mutate command to create a new column of data
  group_by(Region) %>%
  summarize(mean_farmland_proportion = mean(farmland_proportion))

### Uncomment the line below (delete the leading # pound sign)
### to print the CA_farm_summary at the console
# CA_farm_summary