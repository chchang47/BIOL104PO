###========================================
### Code in the Week 4 activities
###========================================

###========================================
### Code in Section 2
###========================================

### Loading packages into R workspace
library(dplyr)
library(ggplot2)

### Read in data
CA_county_data <- readr::read_tsv("https://raw.githubusercontent.com/charlottehchang/BIOL104PO/master/data/CA_protectedareas_datasheet.tsv") # this is a link to the Provided Datasheet for the class project

### Take a look at the first few rows of the data table
CA_county_data

### Creating a scatterplot
# Note that we will store the scatterplot in an object named "p" for plot
p <- ggplot(CA_county_data, aes(x=PAs_gapstatus1thru4_Cts, y=PAs_gapstatus1thru4_HAs)) # specifying an aesthetic where x is the count of protected areas and y is the area in hectares of all PAs in each county
p <- p + geom_point() # adding on points to create a scatterplot
p <- p + labs(x="Protected area count", y="Protected area extent (ha)") # changing the default x and y axis labels to more informative labels
p # calling p which will be displayed in the plot viewer

###========================================
### Code in Section 3
###========================================

### Creating a boxplot
p <- ggplot(CA_county_data, aes(x=Region, y=iucn_threat_count_species))
p <- p + geom_boxplot()
p <- p + labs(x="Region",y="IUCN Red-Listed species")
# p <- p + theme_classic() # compare and contrast what happens when you uncomment this line
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # here, we are specifying that R should rotate the x-axis text by 90 degrees to avoid the long regional names getting written over each other. No need to worry about this line of code too much.
p

###========================================
### Code in Section 4
###========================================

### Creating a histogram
p <- ggplot(CA_county_data, aes(x=area_of_county_ha))
p <- p + geom_histogram()
p <- p + labs(x="County area (ha)")
# p <- p + theme_minimal() # compare and contrast what happens when you uncomment this line
p


