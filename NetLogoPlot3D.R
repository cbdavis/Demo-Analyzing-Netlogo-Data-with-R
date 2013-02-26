############ !!!!!!!!!!!!!!!!!!!!!!!!!! ##########################
#Put this at the top of every file, unless you really really want to work with factors
#this will save you a lot of confusion
options(stringsAsFactors = FALSE)

############### MAKE SURE THAT THE WORKING DIRECTORY IS SET ###############
#this line below sets the current working directory 
#setwd("/home/cbdavis/Demo-Analyzing-Netlogo-Data-with-R")

#read in the data. skip the first 6 lines, the line after that is the header, and the columns are separated by commas
#You can either specify the full path to the file, or make sure that the working directory for R points to the directory containing the file
myDataFrame = read.table("TeamAssemblyModelData.csv", skip = 6, sep = ",", head=TRUE)

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

#RGL is an OpenGL library that allows you to create 3D graphics
#Package website: http://cran.r-project.org/web/packages/rgl/
#Manual: http://cran.r-project.org/web/packages/rgl/rgl.pdf
library(rgl)

#this library is needed for the interp function
library(akima)

##### Show attractors for each run in 3D #####
#here we create a scatterplot of the number of previous links vs. incumbent-incumbent vs. the number of necomer-incumbent links
#each combination of x,y,z is from a single step from a single simulation run.
x = myDataFrame$previous
y = myDataFrame$incumbent.incumbent
z = myDataFrame$newcomer.incumbent

#get all the unique run numbers
uniqueRunNumbers = unique(myDataFrame$runnumber)
numberOfRuns = length(uniqueRunNumbers)
allColors = colors() #there's about 657 colors in the R palette
#generate a random list of colors that will correspond to each run number
colorList = allColors[round(runif(numberOfRuns, 1, length(allColors)))]

#make a vector of the colors that correspond to each data point
#this will give a warning, but don't worry about it
colors = factor(myDataFrame$runnumber, labels=colorList)

#draw lots of colored spheres
spheres3d(x,y,z, radius=0.5, color=colors)

#This connects each of the spheres with lines based on the step
#based on this, we can see for each runnumber the path followed
#This for loop doesn't have to be included, and many slow down the visualization
for (i in c(1:length(colorList))) {
  locs = which(myDataFrame$runnumber == i)
  lines3d(myDataFrame$previous[locs], # x
          myDataFrame$incumbent.incumbent[locs], # y 
          myDataFrame$newcomer.incumbent[locs], # z
          color=colors[locs], #color to use
          lwd=5)   #line width
}

#add the axes, title, and grid to the plot
axes3d(labels=TRUE, tick=TRUE)
title3d(main="title is here", xlab="previous", ylab="incumbent.incumbent", zlab="newcomer.incumbent")
grid3d(c("x", "y+", "z"), n=50)

#set the size of the window to 750x750 pixels
par3d(windowRect = c(0,0,750,750))
#take a picture of this awesome creation
#You may want to do this manually in order to get the correct rotation, magnification of the picture.  There should be a way within rgl to set this automatically also
rgl.snapshot("Awesome3dImage.png", fmt="png", top=TRUE)


#### create a surface plot in rgl ####

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
