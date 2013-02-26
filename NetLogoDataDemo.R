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
#   * hexbin - used for one of the histogram examples
#   * igraph - network analysis
#   * reshape
#   * sqldf - used for querying data, performing aggregations, filtering, etc.
#   * rgl - makes 3d graphs 
#   * akima


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

#used for hexagonal binning for one of the plots
library(hexbin)

#used for reshaping the data, i.e. data in columns may need to be moved to individual rows, etc.
library(reshape)

#used for querying data, performing aggregations, filtering, etc.
library(sqldf)

#working with network data
library(igraph)

############### MAKE SURE THAT THE WORKING DIRECTORY IS SET ###############
#this line below sets the current working directory 
#setwd("/home/cbdavis/Demo-Analyzing-Netlogo-Data-with-R")

#read in the data. skip the first 6 lines, the line after that is the header, and the columns are separated by commas
#You can either specify the full path to the file, or make sure that the working directory for R points to the directory containing the file
myDataFrame = read.table("TeamAssemblyModelData.csv", skip = 6, sep = ",", head=TRUE)

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
#Here I just clean up some of the names to get rid of weird characters.
#Re-map these to whatever names you find useful.
#You could also just do this by changing the header names in your data file.
colnames[1] = "runnumber"
colnames[2] = "maxdowntime"
colnames[3] = "layout"
colnames[4] = "plot"
colnames[5] = "teamsize"
colnames[8] = "step"
#write the fixed values back to the data frame
colnames(myDataFrame) = colnames

#These are now the data columns that I can work with
#   myDataFrame$runnumber                      
#   myDataFrame$max.downtime                  
#   myDataFrame$layout
#   myDataFrame$plot                          
#   myDataFrame$team.size
#   myDataFrame$p                             
#   myDataFrame$q
#   myDataFrame$step                          
#   myDataFrame$count.turtles
#   myDataFrame$fractionAgentsInGiantComponent
#   myDataFrame$averageComponentSize
#   myDataFrame$previous                      
#   myDataFrame$incumbent.incumbent
#   myDataFrame$newcomer.incumbent            
#   myDataFrame$newcomer.newcomer

#find the maximum number of turtles encountered
max(myDataFrame$count.turtles)

#which index has the maximum number of turtles
indexWithMaxNumTurtles = which.max(myDataFrame$count.turtles)
myDataFrame$count.turtles[indexWithMaxNumTurtles]

#plot a sorted vector of the number of turtles over all time for all the simulations
#this gives you an idea of how often you encounter high, low, medium values
plot(sort(myDataFrame$count.turtles))

#just give me a quick scatterplot
scatterplot = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize)) + #use myDataFrame for the data, columns for x and y
                    geom_point(aes(colour = runnumber)) + #we want to use points, colored by runnumber
                    xlab("step") +  #specify x and y labels
                    ylab("average component size") + 
                    ggtitle("Average component size over time") #give the plot a title

print(scatterplot) #display the scatterplot
#Note: if you just do "ggplot(...)" instead of "something = ggplot(...)" then the image will be drawn automatically, 
#but you won't have a way to save it, except by clicking on the GUI for the image.
#Now save the plot
ggsave(scatterplot, file="scatter.png") 

#do the same with lines.  The only change from above is the addition of "group=runnumber" 
# and geom_line is used instead of geom_point
ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize, group=runnumber)) + #use myDataFrame for the data, columns for x and y
  geom_line(aes(colour = runnumber)) + #we want to use points, colored by runnumber
  xlab("step") +  #specify x and y labels
  ylab("average component size") + 
  ggtitle("Average component size over time") #give the plot a title

#You can navigate back and forth between different graphs by using the left/right arrows in the "Plots" window
#To have multiple graph windows open, you need to tell R specifically to open a new window
#If you're using Windows, this will looks something like this
#
#	windows()
#	put your code for plot 1 here
#
#	windows()
#	put your code for plot 2 here
#
#For mac, you would use macintosh() instead of windows.  For Unix/Linux, you would use X11()
#See http://www.statmethods.net/graphs/creating.html for more info

#create a 2d histogram with hexagonal bins
hexBinExample = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize, fill=..density..)) + 
                    stat_binhex(bins=50) 
print(hexBinExample)
ggsave(hexBinExample, file="hexBinExample.png") 

#similar example, but just give me a heatmap, without the scatter plot
simpleHeatMapOfScatterPlot = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize)) + 
                                    stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) 

print(simpleHeatMapOfScatterPlot)
ggsave(simpleHeatMapOfScatterPlot, file="simpleHeatMapOfScatterPlot.png") 

fileprefix="histogram"
val = 3
filename = paste(fileprefix, val, ".png", sep="")

#actually, I'm in the mood for a histogram
simpleHistogram = ggplot(data=myDataFrame, aes(x=averageComponentSize)) + geom_histogram() 
print(simpleHistogram)
ggsave(simpleHistogram, file=filename) 

#now just give me a boxplot
# "group=round(step/25)" means that we group all the data into boxes 25 steps wide
# If we just said "group=step", then we would have 500 boxes, which fills up the whole plot
# and is hard to read
ggplot(data=myDataFrame, aes(x=step, y=count.turtles, group=step)) + 
  geom_boxplot()
       
boxplot = ggplot(data=myDataFrame, aes(x=step, y=count.turtles, group=round(step/25))) + 
                geom_boxplot()

print(boxplot)
ggsave(boxplot, file="boxplot.png") 

#show a matrix (i.e. a "facet grid") of individual graphs where every single
#graph show the values encountered for a single permutation of p & q values
boxplot = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize, group=round(step/25))) + 
                  geom_boxplot() + 
                  facet_grid(p ~ q, scales="free")
print(boxplot)

#do the same, now just with lines
facetGridWithLines = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize)) + 
                            geom_line() + 
                            facet_grid(p ~ q, scales="free")

print(facetGridWithLines)
ggsave(facetGridWithLines, file="facetGridWithLines.png") 

#same as above, but look at the fraction of agents in the giant component
facetGridWithPoints = ggplot(data=myDataFrame, aes(x=step, y=fractionAgentsInGiantComponent)) + 
                              geom_point() + 
                              facet_grid(p ~ q, scales="free")

print(facetGridWithPoints)
ggsave(facetGridWithPoints, file="facetGridWithPoints.png") 

#create a heat map showing the fractionAgentsInGiantComponent for combinations of p and q at time step = 50
indices = which(myDataFrame$step == 50)
#first element is rows, second is columns
df = myDataFrame[indices,]
heatMapStep50 = ggplot(data=df, aes(x=p, y=q)) + geom_tile(aes(fill=fractionAgentsInGiantComponent))
print(heatMapStep50)
ggsave(heatMapStep50, file="heatMapStep50.png") 

#same as above, except in the "which" statement we use the modulo operator to find which indices cover steps in increments of 50
#In other words, this will match everything with step number 0, 50, 100, ... 500
#Then it will perform a facet grid like some of the examples above so you will have heat maps for a series of time steps
indices = which((myDataFrame$step %% 50) == 0)
df = myDataFrame[indices, colnames(myDataFrame)]
#WARNING - make sure that you include the smaller dataframe, otherwise this will try to plot the whole set of data, and your computer will explode
heatMapFacetWrap = ggplot(data=df, aes(x=p, y=q)) + 
                          geom_tile(aes(fill=fractionAgentsInGiantComponent)) + 
                          scale_fill_continuous("fraction\nof\nagents") + 
                          facet_wrap(~ step) + 
                          xlab("p") + 
                          ylab("q") + 
                          ggtitle("Heatmap of fraction of agents\nin giant component at steps")

print(heatMapFacetWrap)
ggsave(heatMapFacetWrap, file="heatMapFacetWrap.png") 

#Now need to create a stacked area chart based on the columns below
#   myDataFrame$previous
#   myDataFrame$incumbent.incumbent
#   myDataFrame$newcomer.incumbent            
#   myDataFrame$newcomer.newcomer

#TODO there may be a better way of doing this, although all the documentation I've found so far
#indicates that you pass in x and y vectors and then just specify which column is used to color them.
#It would be nice to just pass it a bunch of columns and not have to merge them.

#See http://www.statmethods.net/management/reshape.html for what's happening here
#The main idea is that if we want to create a stacked graph where we can compare the values contained in the "previous", "incumbent.incumbent", "newcomer.incumbent", and "newcomer.newcomer" columns
#The issue is that in order to plot this, we can only pass in an x and y vector, along with another vector that indicates how to color the points.
#To do this, we need to "melt" the data.  What you see below is a list of all the data column names, except for the four that we want to combine.
#When we "melt" the data, it will still use this list of column names, although the four combined columns will now be accessible through 
#data2$variable (a string indicating the original column name) and data2$value (the actual value from the original column).
#It's a good idea to run summary(data2) over the melted data to make sure that the values for "variable" are the columns that you think they should be.  
#It may merge in additional columns if you're not careful.
data2 = melt(myDataFrame, id=c("runnumber", "maxdowntime", "layout", "plot", "teamsize", "p", "q", "step", "count.turtles", "fractionAgentsInGiantComponent", "averageComponentSize"))
areaplot = ggplot(data=data2, aes(x=step, y=value)) + 
                  geom_area(aes(fill=variable)) + 
                  facet_grid(p ~ q, scales="free")

print(areaplot)
ggsave(areaplot, file="areaplot.png") 

#Now plot the number of incumbent.incumbent and newcomer.newcomer links for each value of p and q
#This can be useful if there is a phase delay in the data, similar to what is seen in the wolf-sheep model
#What you often see with this type of visualization is that there is a trail of values from the beginning of the simulation
#that then leads to a large cluster (i.e. attractor) of values.  
facetGridScatterPlot = ggplot(data=myDataFrame, aes(x=incumbent.incumbent, y=newcomer.newcomer)) + 
                              geom_point() + 
                              facet_grid(p ~ q, scales="free")

print(facetGridScatterPlot)
ggsave(facetGridScatterPlot, file="facetGridScatterPlot.png") 

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
# count the number of rows where the value for runnumber is equal to 1
x = sqldf("SELECT COUNT(*) FROM myDataFrame WHERE runnumber=1")
# the same, but where runnumber < 10
x = sqldf("SELECT COUNT(*) FROM myDataFrame WHERE runnumber<10")
# find the average fractionAgentsInGiantComponent for each runnumber (averaged over all ticks)
x = sqldf("SELECT AVG(fractionAgentsInGiantComponent) AS avgFrac FROM myDataFrame GROUP BY runnumber")
# same, but order the values for avgFrac descending
x = sqldf("SELECT AVG(fractionAgentsInGiantComponent) AS avgFrac FROM myDataFrame GROUP BY runnumber ORDER BY avgFrac DESC")
# same, but also give me the runnumber that corresponds to each value
x = sqldf("SELECT runnumber, AVG(fractionAgentsInGiantComponent) AS avgFrac FROM myDataFrame GROUP BY runnumber ORDER BY avgFrac DESC")
# plot stuff
plot(x$runnumber, x$avgFrac)
# find the distinct values for team.size
x = sqldf("SELECT DISTINCT teamsize FROM myDataFrame")
# get me the distinct combinations of p and q that were used
x = sqldf("SELECT DISTINCT p, q FROM myDataFrame")
# select a subset of the original data, and then run a query on that subset
dataSubSet = sqldf("SELECT * FROM myDataFrame WHERE runnumber<10")
x = sqldf("SELECT count(*) FROM dataSubSet")
# get all rows where 20 <= p <= 60
x = sqldf("SELECT * FROM myDataFrame WHERE p BETWEEN 20 AND 60")
# get all data where two conditions are met
x = sqldf("SELECT * FROM myDataFrame WHERE fractionAgentsInGiantComponent > 0.5 AND averageComponentSize > 10")
# get me the row with the maximum value for the maximum average component size
x = sqldf("SELECT *, MAX(averageComponentSize) FROM myDataFrame")

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