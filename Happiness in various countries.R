# Importing the excel file with library readxl
library(readxl)

# Choosing the dataset
# #select the happiness data set with year 2016
x2016_report <- read_excel( file.choose())

#select the happiness data set with year 2020
x2020_report <- read_excel( file.choose())

# viewing the data set
View(X2020_report)
View(X2016_report)

# Structure of data set
str(X2020_report)
str(X2016_report)

#Heading the data
head(X2020_report)
head(X2016_report)

# Installing libraries
library(dplyr)
library(ggplot2)
library(ggmap)
library(data.table)
library(treemapify)
library(RColorBrewer)
library(maps)
library(mapproj)
library(googleVis)

# Checking for NA values as we are reading the excel sheet for 2016 and 2020
sum(is.na(x2016_report))
sum(is.na(x2020_report))

################################################################################

#plot-1
# Sorting to get top 5 country w.r.t happiness score for both the year of 2016 & 2020

# Creating a new data set for top five country separately for year 2016 & 2020
whd2016<-X2016_report %>% arrange(desc(happiness_score))%>%  slice(1:5)

whd2020<-X2020_report %>% arrange(desc(happiness_score))%>%  slice(1:5)


#Adding a column in whd2016 and whd2020 data set

year<- c(2016,2016,2016,2016,2016)
whd2016$year<- year

year<- c(2020,2020,2020,2020,2020)
whd2020$year<- year

#removing a column from whd2016 & whd2020 data set
new_whd2016 <- subset(whd2016, select = -c(4))
new_whd2020 <- subset(whd2020, select = -c(4))

#ordering the column of new_whd2016 as per 2020, for using the rbind function
happy2016<- new_whd2016[,c(1,2,3,4,5,7,6,8,9,10)]

#combining the 2016 & 2020 data frames into whd
whd<- rbind(happy2016,new_whd2020)
whd$Happiness_score<- round(whd$happiness_score)

# Plot-1( Country not present in top five happiest country in the world )
myplot <- ggplot(data=whd, aes(country, Happiness_score, fill= happiness_score)) +geom_bar(stat = "identity")+facet_wrap(~year, scales = "free_y")
myplot

myplot <- myplot + theme(legend.title = element_text(colour = "red", size = 15, face 
                                                     = "bold"),legend.text = element_text(color = "grey")) 

# changing the facet title
myplot <- myplot + theme(strip.text.x = element_text(face="bold"),
                         strip.background = element_rect(colour="green",size = 2, fill= "yellow"),
                         strip.text = element_text(color = "black")) + xlab("Country") + ylab("Happiness Score")
myplot

# Changing the labels
myplot <- myplot + scale_colour_discrete(name="Happiness Score")
myplot

################################################################################

# Plot-2( Which continent is more happy)
#data for scatter plot( Deleting the column with different value to combine data with rbind)
new2016 <- subset(X2016_report, select = -c(4))
new2020<- subset(X2020_report, select = -c(4))

# Ordering the column in 2016 for rbind function
newnew2016<- new2016[,c(1,2,3,4,5,7,6,8,9)]

# Combining the data set with rbind( naming the data set as Datascat)
datascat<- rbind(newnew2016,new2020)

# Changing the column name
colnames(datascat)[9] <- "Continent"

# Plotting the scatter plot
myplot1 <- ggplot(data =datascat , aes(x=Continent, y=happiness_score)) + geom_jitter(aes(colour = Continent), alpha=1) +  theme_bw() + xlab("Continent") + ylab("Happiness score")
myplot1

################################################################################

# Plot-3 Tree Map()
library(treemap)
#Creating treemap with showing most happy countries in a contitnent(counting the frequency)
# Changing column name to change the Legend title 
df<- data.frame(datascat %>% count(Continent))
View(df)

# Creating a tree map
ggplot(df, aes(fill=Continent, area= n)) + geom_treemap()+
  labs(title = "Countinents with happy countries")

################################################################################

# Plot-4 Does GDP plays role in happiness(HW-5)
# Top five countries in GDP in the year 2020

# Creating a data set with top five countries
GDP2020<-X2020_report %>% arrange(desc(gdp_per_capita))%>%  slice(1:5)

# Keeping only the country and gdp column, Creating a new data set with this change
GDP2020new<- subset(GDP2020, select = -c(2,4,5,6,7,8,9,10))

# Creating the line chart
line <- gvisLineChart(GDP2020new, options=list(Slices="{1:2}", title="GDP per country", 
                                               titleTextStyle="{ color: 'Red',fontname:'Roman', fontsize:20 }" ,
                                               legend="bottom", width=500, 
                                       height=500)) 
plot(line)

#Happy country in 2020

# Sub-setting the data set to keep only country and their happiness score
hp2020new<- subset(whd2020, select = -c(3,4,5,6,7,8,9,10,11))

column <- gvisColumnChart(hp2020new, options=list(legend='bottom', title="Happiness per country", 
                                                  titleTextStyle="{ color: 'Blue',fontname:'Roman', fontsize:20 }" ,width=500, 
                                           height=500)) 
plot(column) 

# Merging the above two plots
plot(gvisMerge(column, line, 
               tableOptions = "cellspacing=20",
               horizontal=TRUE))

################################################################################


