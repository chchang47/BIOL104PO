###========================================
### Code for Week 10 R tutorial
###========================================

### Loading in packages
library(ggplot2) # package for plotting
library(dplyr) # package for manipulating data

### Reading in the data
EPAdf <- readr::read_tsv("https://raw.githubusercontent.com/charlottehchang/BIOL104PO/master/data/EPAstreams.tsv")

### Display the first few rows of the data
EPAdf

###========================================
### Section 1.1
###========================================

### Creating a data table to store maximum nitrogen levels by region
# Compiled from https://tinyurl.com/nb9zxtj6 and https://www.epa.gov/nutrient-policy-data/ecoregional-nutrient-criteria-rivers-and-streams
maxN <- tibble::tibble(Region=c(1:10),
                       RegionName=c("New England","NY and NJ","Mid-Atlantic","Southeast",
                                    "Upper Midwest","South","Midwest","Intermountain West","Pacific SW","Pacific NW"),
                       MaxNitrogen=c(380,540,710,900,900,690,880,880,380,380))

### Display the first few rows of the data
maxN

### Add on a column that takes all of the new columns from maxN 
### (MaxNitrogen and RegionName) and joins them to the EPAdf data table.
### This way, we can see for each stream in each region if 
### its N level is too high.
EPAdf <- EPAdf %>%
  left_join(maxN,by=c("Region"="Region"))

### View the columns Region, Nitrogen, and MaxNitrogen
EPAdf[,c("Region","Nitrogen","MaxNitrogen")] # equivalent to:
# dplyr::select(EPAdf, Region, Nitrogen, MaxNitrogen) # uncomment to run to see that it produces the same output

### Create a new column that tracks if each stream's N levels are too high
EPAdf <- EPAdf %>%
  mutate(HighN = ifelse(Nitrogen>MaxNitrogen,"High","Low")) # the ifelse function (?ifelse) assigns the value 1 if the stream's N level is too high (>MaxNitrogen). Otherwise, if the stream's N level is less than or equal to MaxNitrogen, that stream gets a 0.

### View the columns Region, Nitrogen, Max nitrogen in that region, and High nitrogen
# A value of 1 for High nitrogen indicates that this stream has too much nitrogen
EPAdf %>%
  dplyr::select(Region,Nitrogen,MaxNitrogen,HighN)

###========================================
### Section 2
###========================================

### Below, we are going to use the arrange function from dplyr
### to re-order the rows in EPAdf based on the levels of N
### in each stream
EPAdf <- EPAdf %>% 
  dplyr::arrange(desc(Nitrogen)) # order in terms of decreasing values of Nitrogen, from most to least

### Generate boxplots for stream nitrogen levels across regions
p <- ggplot(EPAdf, aes(x=RegionName,y=Nitrogen)) # Initialize plot; x-axis (grouping variable) is region, y-axis is stream N levels
p <- p + geom_boxplot(outlier.shape = NA) # suppress outliers 
p <- p + ylim(0,20000) # limit the y-axis range
p <- p + labs(x="EPA Region",y="Nitrogen (μg per L)") # add more informative x- and y-axis labels
p <- p + coord_flip() # reverse the x- and y-axes because the region names are long
p <- p + theme_linedraw() # change the theme of the plot
p

### Generate boxplots where each boxplot is ordered by its relative nitrogen levels
  # I recommend using this boxplot to guide your answer to the Gradescope assignment
p <- ggplot(EPAdf, aes(x=reorder(RegionName,-Nitrogen),y=Nitrogen)) # Initialize plot; x-axis will now be ordered based on the values of nitrogen, y-axis is stream N levels
p <- p + geom_boxplot(outlier.shape = NA) # suppress outliers 
p <- p + ylim(0,20000) # limit the y-axis range
p <- p + labs(x="EPA Region",y="Nitrogen (μg per L)") # add more informative x- and y-axis labels
p <- p + coord_flip() # reverse the x- and y-axes because the region names are long
p <- p + theme_classic() # change the theme
p

###========================================
### Section 2.1
###========================================

### Create a summary table to store the proportion of
### streams in each region that have excessive 
### levels of nitrogen
N_summary_table <- EPAdf %>%
  group_by(RegionName) %>%
  summarize(ProportionHigh = signif( length( which( HighN=="High") ) /n(), 2)) 
# this is a bit convoluted but we are seeing which 
# streams have "High" levels of nitrogen (in excess of 
# threshold values for each region), then we see how 
# many there are (using length), and finally, 
# we reduce the number of digits reported 
# to 2 "significant figures"

### Clean up summary table by displaying 
### the results in ascending order of 
### the proportion of streams that have
### excessively high N levels
N_summary_table <- N_summary_table %>%
  dplyr::arrange(ProportionHigh) 
# arrange in ascending order of proportion of streams exceeding N limit

### Display the data table using 
### htmlTable from the package htmlTable
### and change the column names
names(N_summary_table) <- c("Region","Proportion streams with excess N")
htmlTable::htmlTable(N_summary_table,rname=FALSE)

###========================================
### Section 3
###========================================

### Control the order of plotting High versus Low for the HighN column
EPAdf$HighN <- factor(EPAdf$HighN,levels = c("Low","High")) # make Low come before High, rather than the default which is alphabetical order

### Create a boxplot that visualizes stream biodiversity 
### across nitrogen polluted and non-polluted streams
p <- ggplot(EPAdf, aes(x=HighN, y=MMI)) # initiate plot with HighN on x-axis; MMI on y-axis
p <- p + geom_boxplot(notch = TRUE) # add boxplots with notches
p <- p + theme_bw() # change the plotting theme
p <- p + labs(x="Nitrogen level in stream",y="Benthic macroinvertebrate diversity score") # change x- and y-axis labels
p