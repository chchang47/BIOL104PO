###========================================
### Code for Week 9 R tutorial
###========================================

### Load the ggplot2 package into the R workspace
library(ggplot2)

###========================================
### Code for Sections 1 and 2
###========================================
### Reading in the data
phenoDF <- readr::read_tsv("https://raw.githubusercontent.com/charlottehchang/BIOL104PO/master/data/ButterflyPhenology.tsv")

### Display the first few rows of the data
phenoDF

### Building on the ggplot tutorial from Week 4
  # Specifically, the scatterplot exercise from Section 2
  # Link: https://charlottehchang.github.io/BIOL104PO/Week4.html
p <- ggplot(phenoDF, aes(x=SpringTempC, y=DayOfYear))
p <- p + geom_point() # adding on points plotted at the locations given by x and y above
p <- p + labs(x="Mean spring temperature (°C)", y="Flight timing (day of year)") # changing the x and y-axis labels to be informative
p <- p + theme_classic() # modify the style of the plot
p # display the plot, p

### Running the lm command
butterflyModel <- lm(DayOfYear ~ SpringTempC, data=phenoDF)

### Showing the coefficient estimates
summary(butterflyModel)

### Generating plot with line
p <- ggplot(phenoDF, aes(x=SpringTempC, y=DayOfYear))
p <- p + geom_point() # adding on points plotted at the locations given by x and y above
p <- p + geom_smooth(method="lm", fill=NA)
p <- p + labs(x="Mean spring temperature (°C)", y="Flight timing (day of year)") # changing the x and y-axis labels to be informative
p <- p + theme_bw() # modify the style of the plot
p # display the plot, p

###========================================
### Code for Section 3 (Gradescope Homework)
###========================================

### Running the lm command
summerButterflyModel <- lm(DayOfYear ~ ..., data=phenoDF) 
  # note that you need to replace the ...'s in this document

### Showing the coefficient estimates
summary(summerButterflyModel)

### Generating plot with line
p <- ggplot(phenoDF, aes(x=..., y=DayOfYear))
p <- p + geom_point() # adding on points plotted at the locations given by x and y above
p <- p + geom_smooth(method="lm", fill=NA, color="purple") # add on the regression line; note that you can change the color to whatever you prefer.
p <- p + labs(x="... (°C)", y="...") # changing the x and y-axis labels to be informative
# p <- p + theme_bw() # modify the style of the plot; uncomment by deleting leading # hashtag sign to run
p # display the plot, p