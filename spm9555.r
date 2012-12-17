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

#Then install the necessary packages for R
# * In the bottom right quadrant, there is a "Packages" tab, with an option to "Install Packages". 
#   Click on this to install the following libraries: 
#   * ggplot2 - this will be used to do most of the plotting.
#   * hexbin - used for one of the histogram examples
#   * igraph - network analysis
#   * rgl - makes 3d graphs 

#####Basic R examples
#See http://www.statmethods.net/index.html for a good overview of R

#Main components of what you're seeing
#engine - this is part that does all the calculations, processing.  You can access it directly from the command line
#gui - this is a user-friendly interfact that talks directly to the R engine.  Whenever you click on something, the gui is basically sending the corresponding command to the R engine
#script - the file you're reading right now that tells the commands to be executed
#data file - many different formats can be used, although csv is one of the easiest to work with.

#You can run R in an interactive mode, where you only run selected statements.  
#In RStudio, you will see the options like "Run current line" and "Run selection"
#You can also run an R script all at once, which may be useful if you get a new data set, 
#but want to rerun all the graphs.

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

#in RKWard you can see the "Workspace" tab on the left side of the GUI
#this will show all the packages that you have loaded.  
#It will also list all of the variables that you have currently set in the workspace
#It also allows you to get a summary of these and inspect their values
#This can be quite useful for exploring what's happening.

#Also, when you save R, it will request if you want to save the workspace
#This means that it will save all the variables currently loaded in the workspace

######Load in libraries needed for plotting
#TODO: Make sure that you have these packages installed where you see "library(something)"

#See http://had.co.nz/ggplot2/ for documentation
#Also http://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
#and http://had.co.nz/ggplot2/book.pdf
#ggplot2 is used for most of the visualizations here
#Note that plot() is not the same as ggplot()
#these are from two separate packages
library(ggplot2)

#used for hexagonal binning for one of the plots
library(hexbin)

#RGL is an OpenGL library that allows you to create 3D graphics
#Package website: http://cran.r-project.org/web/packages/rgl/
#Manual: http://cran.r-project.org/web/packages/rgl/rgl.pdf
library(rgl)

#this line below sets the current working directory 
#setwd("/home/cbdavis/Desktop/svn/ChrisDavis/PhD/R/NetlogoDemo")
#read in the data. skip the first 6 lines, the line after that is the header, and the columns are separated by commas
#You can either specify the full path to the file, or make sure that the working directory for R points to the directory containing the file
myDataFrame = read.table("TeamAssemblyModelData.csv", skip = 6, sep = ",",head=TRUE)

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
colnames[3] = "layout"
colnames[4] = "plot"
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
scatterplot = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize)) + geom_point() + xlab("step") + ylab("average component size") + opts(title="Average component size over time")
print(scatterplot) #display the scatterplot. 
#Note: if you just do "ggplot(...)" instead of "something = ggplot(...)" then the image will be drawn automatically, 
#but you won't have a way to save it, except by clicking on the GUI for the image.
#Now save the plot
ggsave(scatterplot, file="scatter.png") 


#By default, only one graph window will be open, and new graphs will overwrite the old ones
#To have multiple graph windows open, you need to tell R specifically to open a new window
#If you're using Windows, this will looks something like this
#
#	windows()
#	put your code for plot 1 here
#
#	windows()
#	put your code for plot 2 here
#
#For mac, you would use macintosh() instead of windows.  For Unix, you would use X11()
#See http://www.statmethods.net/graphs/creating.html for more info

#create a 2d histogram with hexagonal bins
hexBinExample = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize, fill=..density..)) + stat_binhex(bins=50)
print(hexBinExample)
ggsave(hexBinExample, file="hexBinExample.png") 

#do a scatter plot and then also plot some polygons to show the density of points
scatterPlotWithPolygonsIndicatingDensity = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize)) + geom_point()+ stat_density2d(aes(fill = ..level..), geom="polygon") 
print(scatterPlotWithPolygonsIndicatingDensity)
ggsave(scatterPlotWithPolygonsIndicatingDensity, file="scatterPlotWithPolygonsIndicatingDensity.png") 

#similar example, but just give me a heatmap, without the scatter plot
simpleHeatMapOfScatterPlot = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize)) + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) 
print(simpleHeatMapOfScatterPlot)
ggsave(simpleHeatMapOfScatterPlot, file="simpleHeatMapOfScatterPlot.png") 

#actually, I'm in the mood for a histogram
simpleHistogram = ggplot(data=myDataFrame, aes(x=averageComponentSize)) + geom_histogram() 
print(simpleHistogram)
ggsave(simpleHistogram, file="simpleHistogram.png") 

#instead of a histogram, draw lines, where each line represents a distinct runnumber
ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize)) + geom_line()
ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize, group=step)) + geom_line()
linegraph = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize, group=runnumber)) + geom_line()
print(linegraph)
ggsave(linegraph, file="linegraph.png") 

#now just give me a boxplot
boxplot = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize, group=runnumber)) + geom_boxplot()
print(boxplot)
ggsave(boxplot, file="boxplot.png") 

#show a matrix (i.e. a "facet grid") of individual graphs where every single
#graph show the values encountered for a single permutation of p & q values
facetGridWithLines = ggplot(data=myDataFrame, aes(x=step, y=averageComponentSize)) + geom_line() + facet_grid(p ~ q, scales="free")
print(facetGridWithLines)
ggsave(facetGridWithLines, file="facetGridWithLines.png") 

#same as above, but look at the fraction of agents in the giant component
facetGridWithPoints = ggplot(data=myDataFrame, aes(x=step, y=fractionAgentsInGiantComponent)) + geom_point() + facet_grid(p ~ q, scales="free")
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
heatMapFacetWrap = ggplot(data=df, aes(x=p, y=q)) + geom_tile(aes(fill=fractionAgentsInGiantComponent)) + scale_fill_continuous("fraction\nof\nagents") + facet_wrap(~ step) + xlab("p") + ylab("q") + opts(title="Heatmap of fraction of agents\nin giant component at steps")
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
data2 = melt(myDataFrame, id=c("runnumber", "max.downtime", "layout", "plot", "team.size", "p", "q", "step", "count.turtles", "fractionAgentsInGiantComponent", "averageComponentSize"))
areaplot = ggplot(data=data2, aes(x=step, y=value)) + geom_area(aes(fill=variable)) + facet_grid(p ~ q, scales="free")
print(areaplot)
ggsave(areaplot, file="areaplot.png") 

#Now plot the number of incumbent.incumbent and newcomer.newcomer links for each value of p and q
#This can be useful if there is a phase delay in the data, similar to what is seen in the wolf-sheep model
#What you often see with this type of visualization is that there is a trail of values from the beginning of the simulation
#that then leads to a large cluster (i.e. attractor) of values.  
facetGridScatterPlot = ggplot(data=myDataFrame, aes(x=incumbent.incumbent, y=newcomer.newcomer)) + geom_point() + facet_grid(p ~ q, scales="free")
print(facetGridScatterPlot)
ggsave(facetGridScatterPlot, file="facetGridScatterPlot.png") 

##### Show attractors for each run in 3D #####
#here we create a scatterplot of the number of previous links vs. incumbent-incumbent vs. the number of necomer-incumbent links
#each combination of x,y,z is from a single step from a single simulation run.
x = myDataFrame$previous
y = myDataFrame$incumbent.incumbent
z = myDataFrame$newcomer.incumbent

#find the number of runs
numberOfRuns = length(unique(myDataFrame$runnumber))
#generate a random list of colors that will correspond to each run number
#See http://research.stowers-institute.org/efg/R/Color/Chart/ for more info.  R has a set of 657 defined colors... or you could just work directly with RGB values
colorList = round(runif(numberOfRuns, 1, 657))
#make a vector of the colors that correspond to each data point
colors = factor(myDataFrame$runnumber, labels=colorList)
#draw lots of colored spheres
spheres3d(x,y,z, radius=0.5, color=colors)

#This connects each of the spheres with lines based on the step
#based on this, we can see for each runnumber the path followed
#This for loop doesn't have to be included, and many slow down the visualization
for (i in c(1:length(colorList))) {
  locs = which(myDataFrame$runnumber == i)
  lines3d(myDataFrame$previous[locs], myDataFrame$incumbent.incumbent[locs], myDataFrame$newcomer.incumbent[locs], color=colors[locs], lwd=5)
}

#add the axes, title, and grid to the plot
axes3d(labels=TRUE, tick=TRUE)
title3d(main="title is here", xlab="previous", ylab="incumbent.incumbent", zlab="newcomer.incumbent")
grid3d(c("x", "y+", "z"), n=50)
#take a picture of this awesome creation
#You may want to do this manually in order to get the correct rotation, magnification of the picture.  There should be a way within rgl to set this automatically also
rgl.snapshot("Awesome3dImage.png", fmt="png")


#### create a surface plot in rgl ####

#this library is needed for the interp function
library(akima)

#Get some sample data to plot
#make a new data frame at step = 50
indices = which(myDataFrame$step == 50)
#first element is rows, second is columns
df = myDataFrame[indices,]

#set the values of x, y, and z.  After this, most of the code is copy/paste
#here x, y, and z are assumed to be of the same length
x = df$p
y = df$q
z = df$fractionAgentsInGiantComponent

#interpolate the points into a matrix
#The code below works if the x and y vectors are composed of regular intervals, i.e. 1,2,3... or 10,20,30...
s = interp(x,y,z,xo=seq(min(x), max(x), length=length(unique(x))), yo=seq(min(y), max(y), length=length(unique(y))))
#the new variable s is a data frame where s$x and s$y represent the rows and columns, and s$z is a matrix of heights
#Alternatively you can use the code below.  By default, it will create a 40x40 grid
#See http://hosho.ees.hokudai.ac.jp/~kubo/Rdoc/library/akima/html/interp.html for more documentation
#s = interp(x,y,z)
#next few lines set the range of colors to be used for the surface
#make sure to remove NA values, otherwise the colors won't work (see na.rm=TRUE)
zlim <- range(s$z, na.rm=TRUE)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- terrain.colors(zlen) # height color lookup table
col <- colorlut[ s$z-zlim[1]+1 ] # assign colors to heights for each point
#here you can change the number of used to represent the different heights on the surface
numColors = 50
colorlut <- terrain.colors(numColors)
#make sure to remove NA values, otherwise the colors won't work (see na.rm=TRUE)
col <- colorlut [((max(s$z, na.rm=TRUE) - s$z)/(max(s$z, na.rm=TRUE) - min(s$z, na.rm=TRUE)) * (numColors-1)) + 1]
open3d()
#draw the surface
surface3d(s$x, s$y, s$z, color=col, back="lines")
#scale the length of all three axes to efficiently fit the display.  This means that the axes will use different scales
aspect3d(1,1,1)
axes3d(labels=TRUE, tick=TRUE)
title3d(main="title is here", xlab="x axis", ylab="y axis", zlab="z axis")
grid3d(c("x+", "y+", "z"), n=50)


##### Working with network data #####
library(igraph)

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
