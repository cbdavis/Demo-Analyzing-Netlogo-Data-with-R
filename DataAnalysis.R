############ !!!!!!!!!!!!!!!!!!!!!!!!!! ##########################
#Put this at the top of every file, unless you really really want to work with factors
#this will save you a lot of confusion
options(stringsAsFactors = FALSE)

#The main wiki page around these examples is at:
#http://wiki.tudelft.nl/bin/view/Education/SPM955xABMofCAS/LectureDataAnalysis
#code + data we will be using is at https://github.com/cbdavis/Demo-Analyzing-Netlogo-Data-with-R

# The contents are:
# * R code
# * the nlogo file used to generate the data. 
#   This also includes methods that can write the network structure to a file (included as network.txt)
# * TeamAssemblyModelData.csv - the data from last year's exam

# Put all these into the same directory, open up RStudio
# and set your working directory to this directory containing the files. 
# Click on "Tools -> Set Working Directory -> Choose Directory" to do this. 
# On a mac, this may also be:
#     "Session -> Set Working Directory -> Choose Directory" to do this.

#Then install the necessary packages for R
# * In the bottom right quadrant, there is a "Packages" tab, with an option to "Install Packages". 
#   Click on this to install the following libraries: 
#   * ggplot2 - this will be used to do most of the plotting.
#   * reshape
#   * sqldf - used for querying data, performing aggregations, filtering, etc.

#####Basic R examples
#See http://www.statmethods.net/index.html for a good overview of R

#Main components of what you're seeing:
# RStudio has four panels, and in the default config these are:
### Top Left - Code - here you can open and work on different script files

### Top Right - Workspace/History
# This shows all the variables and functions that are currently loaded in the workspace
# You can click on the variables to see their values, which can be useful to inspect
# how the code is operating.
# The history shows all of the commands that you have run.

### Bottom Left - Console
# commands can be run here
# You can also find documentation for commands by typing in ?commandName to get help, i.e. ?sum

### Bottom Right - Files/Plots/Packages/Help
# Files - This shows everything in your current working directory
# Plots - Once you start plotting, multiple plots will be stored here.  
#         There are arrows in this view that allow you to navigate between multiple plots
# Packages - Shows all the packages installed and currently loaded
# Help - Shows documentation for varous functions.  

#You can run lines of code by highlighting them, and then clicking on "Run" above.
#You can run all the code at once by doing Code -> Run Region -> Run All

### Introduction to operations:

#Add two numbers, this will only show the value in the console output
1 + 1

#Assign a variable, this way the answer is stored
a = 1 + 1

#Same as above, just using "<-" instead of "="
a <- 1 + 1

#now add up two different variables
b = 3
c = a + b

#make a vector, c() is the function for putting elements into a vector
d = c(3,2,1,4)
#find the length
length(d)

#calculate the average and standard deviation
mean(d)
sd(d)

#do a simple plot
plot(d)

#make another vector e, which is filled with random numbers ranging from 0 to 1, and contains the same number of elements as the d vector
e = runif(length(d), 0, 1)

#combine these two vectors into a matrix, where d is the left column and e is the right column
f = cbind(d, e)

#combine these two vectors into a matrix, where d is the top row and e is the bottom row
g = rbind(d, e)

#See http://www.statmethods.net/advstats/matrix.html for more information about working with matrices

#transpose the matrix that you just made above
t(g)

#element-wise multiplication of two vectors
h = d * e

#matrix multiplication
d %*% t(e)

#Also, when you save R, it will request if you want to save the workspace
#This means that it will save all the variables currently loaded in the workspace

#use rm(variableName) to remove variables from the workspace

######Load in libraries needed for plotting
#TODO: Make sure that you have these packages installed where you see "library(something)"

#Load the ggplot2 library which is used for most of the visualizations here
#to understand why this library is cool, just do a google image search: https://www.google.com/search?q=ggplot2&tbm=isch
#See http://docs.ggplot2.org/current/ for documentation
#Also http://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
#and http://had.co.nz/ggplot2/book.pdf
#Note that plot() is not the same as ggplot()
#these are from two separate packages
library(ggplot2)

#used for querying data, performing aggregations, filtering, etc.
library(sqldf)

############### MAKE SURE THAT THE WORKING DIRECTORY IS SET ###############
#this line below sets the current working directory 
#setwd("/home/cbdavis/Demo-Analyzing-Netlogo-Data-with-R")

#### Make sure to specify the "Table" output for Netlogo
#read in the data. skip the first 6 lines, the line after that is the header, and the columns are separated by commas
#You can either specify the full path to the file, or make sure that the working directory for R points to the directory containing the file
myDataFrame = read.table("/home/cbdavis/Desktop/WolvesSheepRedux/Wolf Sheep Predation experiment-table.csv", skip = 6, sep = ",", head=TRUE)

#This gives you a quick summary of what's in the data
#This is especially important since it tells you what the column names are.
#For example, if you see "newcomer.incumbent" then you can access this column using myDataFrame$newcomer.incumbent
#Note: characters that are not letters or numbers (A-Z, a-z, 0-9) may be encoded as periods, 
#So a header that looks like "[run number]" will be accessible using myDataFrame$X.run.number.
#What you'll see here is that you're working with a data frame.
#It isn't a matrix, but it's an object containing several columns along with some
#additional properties.
summary(myDataFrame)

#this will also show you the names of the column names
colnames = colnames(myDataFrame)

##### Don't worry about what this means, it cleans up the column names #####
# Some colnames start with "X.", get rid of this 
colnames(myDataFrame) = gsub("X\\.", "", colnames(myDataFrame))
# Get rid of periods at the start and end of the names
colnames(myDataFrame) = gsub("^\\.|\\.$", "", colnames(myDataFrame))
# Convert all periods into underscores
colnames(myDataFrame) = gsub("\\.", "_", colnames(myDataFrame))
############################################################################

# you can also just rename the columns yourself
colnames(myDataFrame)[1] = "runNumber"  # change "run_number" to "runNumber"
colnames(myDataFrame)[2] = "grass_is_on"
colnames(myDataFrame)[11] = "tick"  # change "step" to "tick"
colnames(myDataFrame)[14] = "count_grass"

#These are now the data columns that I can work with
#   myDataFrame$runNumber
#   myDataFrame$grass_is_on
#   myDataFrame$sheep_reproduce
#   myDataFrame$initial_number_sheep
#   myDataFrame$grass_regrowth_time
#   myDataFrame$sheep_gain_from_food
#   myDataFrame$show_energy
#   myDataFrame$initial_number_wolves
#   myDataFrame$wolf_reproduce
#   myDataFrame$wolf_gain_from_food
#   myDataFrame$tick
#   myDataFrame$count_sheep
#   myDataFrame$count_wolves
#   myDataFrame$count_grass

# In the top right panel, you can now click on the "Environment" tab, 
# then click on "myDataFrame" to bring up a table view of it.

#find the maximum number of sheep encountered
max(myDataFrame$count_sheep)

#which index has the maximum number of sheep
indexWithMaxNumSheep = which.max(myDataFrame$count_sheep)
myDataFrame$count_sheep[indexWithMaxNumSheep]

#plot a sorted vector of the number of turtles over all time for all the simulations
#this gives you an idea of how often you encounter high, low, medium values
plot(sort(myDataFrame$count_sheep))

#just give me a quick scatterplot
scatterplot = ggplot(data=myDataFrame, aes(x=tick, y=count_sheep)) + #use myDataFrame for the data, columns for x and y
  geom_point() + #we want to use points
  xlab("tick") +  #specify x and y labels
  ylab("number of sheep") + 
  ggtitle("Number of sheep over time") #give the plot a title

print(scatterplot) #display the scatterplot

# Something's going on, but it's useful to distinguish between the different runs, 
# So color the dots by runNumber
scatterplot = ggplot(data=myDataFrame, aes(x=tick, y=count_sheep)) + #use myDataFrame for the data, columns for x and y
  geom_point(aes(colour = runNumber)) + #we want to use points, colored by runNumber
  xlab("tick") +  #specify x and y labels
  ylab("number of sheep") + 
  ggtitle("Number of sheep over time") #give the plot a title
print(scatterplot) #display the scatterplot

#Note: if you just do "ggplot(...)" instead of "something = ggplot(...)" then the image will be drawn automatically, 
#but you won't have a way to save it, except by clicking on the GUI for the image.
#Now save the plot
ggsave(scatterplot, file="scatter.png") 

#do the same with lines.  The only change from above is the addition of "group=runNumber" 
# and geom_line is used instead of geom_point
ggplot(data=myDataFrame, aes(x=tick, y=count_sheep, group=runNumber)) + #use myDataFrame for the data, columns for x and y
  geom_line(aes(colour = runNumber)) + #we want to use points, colored by runNumber
  xlab("tick") +  #specify x and y labels
  ylab("number of sheep") + 
  ggtitle("Number of sheep over time") #give the plot a title

#You can navigate back and forth between different graphs by using the left/right arrows in the "Plots" window
#To have multiple graph windows open, you need to tell R specifically to open a new window
#If you're using Windows, this will looks something like this
#
#  windows()
#	put your code for plot 1 here
#
#	windows()
#	put your code for plot 2 here
#
#For mac, you would use macintosh() instead of windows.  For Unix/Linux, you would use X11()
#See http://www.statmethods.net/graphs/creating.html for more info


#Give me a heatmap, without the scatter plot
# This can be useful if you have a HUGE number of points that are all in a sort of cloud
simpleHeatMapOfScatterPlot = ggplot(data=myDataFrame, aes(x=tick, y=count_sheep)) + 
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) 

print(simpleHeatMapOfScatterPlot)
ggsave(simpleHeatMapOfScatterPlot, file="simpleHeatMapOfScatterPlot.png") 

# make a scatter plot with density contours drawn on it
ggplot(data=myDataFrame, aes(x=count_grass, y=count_sheep)) + geom_point() + geom_density2d()

# instead of contours, show dots that are proportionally sized to the density
ggplot(data=myDataFrame, aes(x=count_grass, y=count_sheep)) + stat_density2d(geom="point", aes(size = ..density..), contour = FALSE)

# This is how to create filenames automatically
# You can use these in loops to create lots of graphs
fileprefix="histogram"
val = 3
# filename is now "histogram3.png"
filename = paste(fileprefix, val, ".png", sep="")

#actually, I'm in the mood for a histogram
simpleHistogram = ggplot(data=myDataFrame, aes(x=count_sheep)) + geom_histogram() 
print(simpleHistogram)
# save the image with the file name we just created
ggsave(simpleHistogram, file=filename) 

#now just give me a boxplot
# "group=round(tick/25)" means that we group all the data into boxes 25 steps wide
# If we just said "group=tick", then we would have 500 boxes, which fills up the whole plot
# and is hard to read
ggplot(data=myDataFrame, aes(x=tick, y=count_sheep, group=tick)) + 
  geom_boxplot()

boxplot = ggplot(data=myDataFrame, aes(x=tick, y=count_sheep, group=round(tick/25))) + 
  geom_boxplot()

print(boxplot)
ggsave(boxplot, file="boxplot.png") 

#show a matrix (i.e. a "facet grid") of individual graphs where every single
#graph show the values encountered for a single permutation of grass_regrowth_time & initial_number_sheep values
ggplot(data=myDataFrame, aes(x=tick, y=count_sheep, group=round(tick/25))) + 
  geom_boxplot() + 
  facet_grid(grass_regrowth_time ~ initial_number_sheep)

# Same, but make the y scales independent per row to stretch things out a bit
boxplot = ggplot(data=myDataFrame, aes(x=tick, y=count_sheep, group=round(tick/25))) + 
  geom_boxplot() + 
  facet_grid(grass_regrowth_time ~ initial_number_sheep, scales="free_y")
print(boxplot)

#do the same, now just with lines
ggplot(data=myDataFrame, aes(x=tick, y=count_sheep)) + 
  geom_line() + 
  facet_grid(grass_regrowth_time ~ initial_number_sheep, scales="free_y")

# This looks weird since there are two repetitions, we should separate these out
facetGridWithLines = ggplot(data=myDataFrame, aes(x=tick, y=count_sheep, group=runNumber)) + 
  geom_line(aes(colour = runNumber)) + 
  facet_grid(grass_regrowth_time ~ initial_number_sheep, scales="free_y")

print(facetGridWithLines)

ggsave(facetGridWithLines, file="facetGridWithLines.png") 

# just show single graphs per runNumber
ggplot(data=myDataFrame, aes(x=tick, y=count_sheep)) + 
  geom_point() + 
  facet_wrap(~runNumber)




#Now need to create a stacked area chart based on the columns below
#   myDataFrame$count_sheep
#   myDataFrame$count_wolves
#   myDataFrame$count_grass

# To do this, we need to stack up those columns into a single column.  
# We will use a column beside it to indicate what is being counted
# So instead of:
# count_sheep     count_wolves     count_grass
#     50                10             100
#
# We'll have:
# count_sheep     50
# count_wolves    10
# count_grass    100

#See http://www.statmethods.net/management/reshape.html for what's happening here
# Note that "count_sheep", "count_wolves", "count_grass" are not in the list
data2 = melt(myDataFrame, id=c("runNumber", "grass_is_on", "sheep_reproduce", "initial_number_sheep", "grass_regrowth_time", "sheep_gain_from_food", "show_energy", "initial_number_wolves", "wolf_reproduce", "wolf_gain_from_food", "tick"))
# Two new columns are introduced- "variable" and "value":
# variable     value
# count_sheep     50
# count_wolves    10
# count_grass    100

areaplot = ggplot(data=data2, aes(x=tick, y=value)) + 
  geom_area(aes(fill=variable)) + 
  facet_grid(grass_regrowth_time ~ initial_number_sheep, scales="free")

print(areaplot)
ggsave(areaplot, file="areaplot.png") 

##### Querying data using SQLDF package #####

# With this part of the tutorial, you're using SQL (http://en.wikipedia.org/wiki/SQL) to run queries over your data

# instructions for the package - http://code.google.com/p/sqldf/

# The best place to start is via tutorials online.  Just search for something like "sqlite tutorial queries"
# i.e. http://sqlite.awardspace.info/syntax/sqlitepg03.htm

# Official documentation: http://www.sqlite.org/lang.html
# This is way more than you need, and tutorials are easier to understand,
# but this shows you everything that you can do with the query language.

# The uppercase terms below are some of the more popular commands that you may find useful.  
# You can also use lowercase in the queries.  Examples of their use are further below.

# SELECT
# FROM 
# WHERE
# AS
# DISTINCT
# COUNT
# ORDER BY
# DESC
# GROUP BY
# BETWEEN
# AND
# OR
# MAX
# MIN

#### NOTE - column names should not contain any punctionation, otherwise the queries may not work.
# You need to change column names like "team.size" to something like "teamsize"

# just get me one row
x = sqldf("SELECT * FROM myDataFrame LIMIT 1")
# count the number of rows where the value for runNumber is equal to 1
sqldf("SELECT COUNT(*) FROM myDataFrame WHERE runNumber=1")
# the same, but where runNumber < 10
sqldf("SELECT COUNT(*) FROM myDataFrame WHERE runNumber<10")
# find the average count_sheep for each runNumber (averaged over all ticks)
sqldf("SELECT AVG(count_sheep) AS avgSheep FROM myDataFrame GROUP BY runNumber")
# same, but order the values for avgSheep descending
sqldf("SELECT AVG(count_sheep) AS avgSheep FROM myDataFrame GROUP BY runNumber ORDER BY avgSheep DESC")
# same, but also give me the runNumber that corresponds to each value
x = sqldf("SELECT runNumber, AVG(count_sheep) AS avgSheep FROM myDataFrame GROUP BY runNumber ORDER BY avgSheep DESC")
# plot stuff
plot(x$runNumber, x$count_sheep)
# find the distinct values for initial_number_sheep
sqldf("SELECT DISTINCT initial_number_sheep FROM myDataFrame")
# get me the distinct combinations of initial_number_sheep and grass_regrowth_time that were used
sqldf("SELECT DISTINCT initial_number_sheep, grass_regrowth_time FROM myDataFrame")
# select a subset of the original data, and then run a query on that subset
dataSubSet = sqldf("SELECT * FROM myDataFrame WHERE runNumber<10")
sqldf("SELECT count(*) FROM dataSubSet")
# get all rows where 20 <= p <= 60
sqldf("SELECT * FROM myDataFrame WHERE count_sheep BETWEEN 20 AND 60")
# get all data where two conditions are met
sqldf("SELECT * FROM myDataFrame WHERE count_wolves > count_sheep")
# get me the row with the maximum value for the maximum average component size
sqldf("SELECT *, MAX(count_sheep) FROM myDataFrame")

##### Working with network data #####

#Complete documentation: http://igraph.sourceforge.net/doc/R/igraph.pdf and at http://igraph.sourceforge.net/doc/R/00Index.html
#Examples: http://igraph.sourceforge.net/screenshots2.html
#     you can copy some of the example code from the igraph site, 
#     but make sure that you're looking at the R examples, and not the Python ones

# A few examples of igraph documented on the TU Delft Wiki:
# http://wiki.tudelft.nl/bin/view/Research/Igraph
# http://wiki.tudelft.nl/bin/view/Research/IgraphAnalysisKauffman

networkDataFrame = read.table("network.txt", sep = "\t",head=TRUE)
#these are the columns:
#networkDataFrame$tick
#networkDataFrame$FromNode
#networkDataFrame$ToNode

#For igraph, we need the first two columns in the data frame to contain information about the edges
#the first column is the from node & the second is the to node
#To do this we have to create a new data frame with rearranged columns
network_data_frame = data.frame(from = networkDataFrame$FromNode,
                                to = networkDataFrame$ToNode, 
                                tick = networkDataFrame$tick)

#create a new graph based on the data frame we just created 
g = graph.data.frame(network_data_frame, directed=TRUE)

#example of calculating the degree distribtuion for a graph
dd = degree.distribution(g, mode="in", cumulative=TRUE)
plot(dd, log="xy", xlab="degree", ylab="cumulative frequency", col=1, main="Nonlinear preferential attachment")

#draw a histogram of the shortest paths that occur for edges present at tick = 100
#here we get the edges in graph g, when get the edges that have a tick value of 100
edgeInfo = get.edges(g, which(E(g)$tick == 100))
#create a temporary graph based on the set of edges we just found above
tempGraph = graph(edgeInfo)
#find the shortest paths between all the vertices in this graph composed of all edges at tick 100
shortest_paths = shortest.paths(tempGraph)
#create a simple histogram of how many paths there are of each length
hist(shortest_paths)

#make a giant plot that looks cool, but isn't necessarily very useful
#the first three lines below help with the graph layout.  The only thing you have to check is that "g" is the graph you want to plot.  You don't have to worry about the rest
g <- simplify(g)
l <- layout.fruchterman.reingold(g)
l <- layout.norm(l, -1,1, -1,1) 
#plot the actual graph, using the layout ("l") that we've created above
#fruchterman.reingold is a force directed layout (like the wiki movie)
#don't show vertex labels, vertex size is 1, edge arrow size is 0.3
plot(g, layout=l, vertex.size=1, vertex.label=NA, edge.arrow.size=0.3, xlim=range(l[,1]), ylim=range(l[,2]))