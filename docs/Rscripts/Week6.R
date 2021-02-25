###========================================
### Code for Week 6 R tutorial
###========================================

### Load dplyr and stringr packages into R workspace
library(dplyr)
library(stringr)

###========================================
### Code to create simple tables to 
### illustrate join operations
###========================================

### Table storing colors of 5 fruits
dt1 <- tibble::tibble(fruit=c("apple","pear","orange","kiwi","mangosteen"),
                      color=c("red","green","orange","brown","purple"))
dt1 # display table

### Table storing prices of 3 fruits
dt2 <- tibble::tibble(fruit=c("pear","orange","mangosteen","cherimoya"),
                      price=c(1.25,1,5,4.7)) # price per pound
dt2 # display table

###========================================
### Code to perform different 
### joining operations
###========================================

### Left join (look for matches for dt1 from dt2)
left_join(dt1, dt2) # Note that the left_join function detects that both data tables both have a column named "fruit". What would happen if they didn't have a column with the same name?

### Right join
right_join(dt1, dt2)

### Inner join
inner_join(dt1, dt2)

### Full join
full_join(dt1, dt2)

###========================================
### Joining data tables relies on
### consistent names between the
### data tables
###========================================

### Modifying dt2
dt2 <- tibble::tibble(fruit=c("Pear","Orange","Mangosteen","Cherimoya"),
                      price=c(1.25,1,5,4.7)) # price per pound
dt2 # display table

### Compare to:
dt1

### See what happens - R can't join the fruits up - the words are not identical because R is case sensitive
full_join(dt1,dt2) 

### Let's fix this problem and make the fruit names consistent
dt2new <- dt2 %>% 
  mutate(fruit=tolower(fruit)) # a function that takes character/strings (words) and converts them to lower case
dt2new # confirm that now the fruit names are lower case

### Re-run full_join
full_join(dt1,dt2new)

###========================================
### Gaining understanding about data joins
###========================================

### Compare the outputs of:
left_join(dt1, dt2)
left_join(dt1, dt2new)

### Compare the outputs of:
right_join(dt1, dt2)
right_join(dt1, dt2new)

### Compare the outputs of:
inner_join(dt1, dt2)
inner_join(dt1, dt2new)

###========================================
### Joining socioeconomic data
### to the Provided Datasheet
### of biodiversity data in California
###========================================

### Read in data
CA_county_data <- readr::read_tsv("https://raw.githubusercontent.com/charlottehchang/BIOL104PO/master/data/CA_protectedareas_datasheet.tsv") # this is a link to the Provided Datasheet for the class project
### Take a look at the first few rows of the data table
CA_county_data

### Data on socio-economic statuses in California
CA_county_SES <- readr::read_tsv("https://raw.githubusercontent.com/charlottehchang/BIOL104PO/master/data/CA_ses.tsv") # URL for spreadsheet storing socioeconomic data by county in California
### Take a look at the first few rows of the data table
CA_county_SES

### Data on racial composition in California
CA_county_demog <- readr::read_tsv("https://raw.githubusercontent.com/charlottehchang/BIOL104PO/master/data/CA_census.tsv")
### Take a look at the first few rows of the data table
CA_county_demog

### Cleaning up county names in CA_county_SES and CA_county_demog
CA_county_SES$County <- str_replace_all(CA_county_SES$County," County","") # replace " County" at the end of the county names with nothing - basically like find and replace deleting the " County" string.
CA_county_demog$CTYNAME <- str_replace_all(CA_county_demog$CTYNAME," County","")

### Merge the data table that has socioeconomic data to CA_county_data
CA_merged <- inner_join(CA_county_data, CA_county_SES)
  # Compare the dimensions of:
dim(CA_county_data)
  # to the dimensions of the merged data table:
dim(CA_merged)

### Merge the demographic data table to the merged data table above, which was stored in CA_merged
CA_merged <- inner_join(CA_merged, CA_county_demog, by=c("County"="CTYNAME")) # Remember my question earlier about doing matches across data tables where the column names aren't the same? Specifying by=c(columnNameForMatchingInDataTable1 = column_name_for_matching_in_data_table_2) is how we can tell R which columns to merge the data tables on.
  # Compare the dimensions of:
dim(CA_county_data)
  # to the dimensions of the merged data table:
dim(CA_merged)
