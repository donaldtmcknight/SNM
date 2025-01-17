# Stream Network Movement (SNM)

An R package for analyzing animal movements in stream networks.

## Description

This package provides basic functions for analyzing animal movement patterns in a stream network including calculating: space use over time, cumulative distance moved over time, distances between pairs of points, directionality of movement, and distances moved between points over varying intervals of time.

## Installation

Copy and paste the code below to install the package

```
install.packages("devtools") #Installs devtools (if not already installed)
devtools::install_github("donaldtmcknight/SNM") #Installs SNM
```

Note that you may receive a warning about the retirement of packages such as sp and rgdal. The functions in SNM use current sf implementation and should not be an issue.

## Running the package

Detailed information about the package functions, what they do, and how to run them are provided in the vignette (this should install with the package in R, but a .pdf version is also available on Github). 

Briefly, use the *prep.data()* function to format the data for subsequent functions.
```
network.1 <- prep.data(l=stream.line,freq=1,nodes=nodes,lon.name="lon",lat.name="lat",node.name="id")
```

Use the *movements()* function to calculate space use, movement from previous points, cumulative distance moved, and directionality of movement.
```
movements(data=network.1,
          space.use=T,
          from.previous=T,
          cumulative=T,
          downstream.node=1,
          coords=animal.points,
          lon.name="lon.raw",
          lat.name="lat.raw",
          id.name="id",
          date.time.name="date.time")
```

Use the *dist.over.time()* function to calculate movement from previous points over varying time intervals
```
dist.over.time(data=network.1,
               coords=animal.points,
               lon.name="lon.raw",
               lat.name="lat.raw",
               id.name="id",
               date.time.name="date.time",
               units="days",
               time.diff=2,
               diff.max=NULL,
               sensitivity.min=0.1,
               sensitivity.max=0.2,
               sensitivity.change=0.04)
```
