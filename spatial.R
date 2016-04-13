setwd('~/Desktop/Data')

#spatial autocorrelation - same feature tends to be corrlated(have same values) around neighbours

data <- read.csv("Set1/obspts.csv")
class(data)
str(data)

library(sp) #to convert the simple dataframe to spatial points dataframe
coordinates(data) <- c("Easting", "Northing")
str(data)
proj4string(data) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
coordinates(data)

library(maptools)
data <- readShapePoly("set1/landcover.shp")
class(data1)   ##SpatialPolygonsDataFrame
str(data1) 

library(raster)
data <- raster("Set4/Set4.10396.tif")
class(data)
str(data)
###############################################################################
# This dataset consists of rice yield data for different fields for two years
# also it contains data about soil properties like SoilP, SoilK etc, and fertilizers
# The aim is to identify the important factors that affect yield across different fields
# Is it due to management factors, or due to climate or due to field properties?
data <- read.csv("Set3/Set3data.csv", header=TRUE)
class(data)
str(data)
plot(data$Longitude, data$Latitude)

#converting variables to factor
data$Season <- factor(data$Season)
data$RiceYear <- factor(data$RiceYear)
data$Field <- factor(data$Field)
data$Var <- factor(data$Var)

#for nominal data type(like gender) - convert it to factor in R
#for ordinal data type(rank) - convert it to ordered factor
#data$Weeds <- ordered(data$Weeds)
#data$Irrig <- ordered(data$Irrig)
#data$D50 <- ordered(data$D50)


#create a new variable region
data$Region <- 2
data$Region[data$Latitude < -33.50] <- 1
data$Region[data$Latitude > -33] <- 3
data$Region <- factor(data$Region)


library(ggplot2)
ggplot(data, aes(x=Region, y=Yield, colour=Season))+geom_boxplot()  #region 2 - less yield
#seasonal variation in region1

ggplot(data[data$Region==1,], aes(x=Field, y=Yield, colour=RiceYear))+geom_point()
#evaluate field 15 and 16
library(dplyr)
subset <- filter(data, Region==1, Field==c(15,16))

#soil properties
lapply(data[c("pH", "Corg", "SoilP", "SoilK", "Sand", "Silt", "Clay")], function(x) by(x, data$Region, mean))
#Sand is lowest in Region2, Silt is greatest in Region 2

#management practices
lapply(data[c("DPL", "Weeds", "Cont", "Irrig", "N", "P", "K")], function(x) by(x, data$Region, mean))
#weed level is highest in Region 2, weed control is least in Region2, Irrig effectiveness is also least in Region2,
#Fertilizer N, P is also least applied in Region2

#it seems like management pratices are playing an important role in yield 

#check the relationship between yield and predictors
library(ggplot2)
ggplot(data, aes(x=N, y=Yield))+geom_point()
ggplot(data, aes(x=pH, y=Yield))+geom_point()+geom_smooth()
ggplot(data, aes(x=Fert, y=Yield))+geom_point()+geom_smooth()
ggplot(data, aes(x=factor(DPL), y=Yield))+geom_point()+geom_smooth()
ggplot(data, aes(x=Clay, y=Yield))+geom_point()+geom_smooth()


#it seems there might be some effect based on season on yields for same farmers.
# farmer J was able to maintain same yield during diff seasons, but huge variation
#on yield for farmer L
#farmer and yield
boxplot(Yield~Farmer, data=data)  #farmer C has max yield
boxplot(Yield~Season, data=data)  #Season 3 has max yield
ggplot(data, aes(x=Farmer, y=Yield))+geom_boxplot()+facet_grid(.~Season)


#i think it violates two assumptions of linear models
#some of the obs are from same field, so errors cannot be independent
#looking at relationship between x and y, we see heteroscedascity
#linear models is definitely not a good choice
#the relationship between predictors and response is quite complex
ggplot(data, aes(x=Region, y=Yield, colour=Season))+geom_point()
# It looks like in Region 1, there is season effect on Yield, maybe due to weather, rainfall etc
ggplot(data, aes(x=Region, y=Yield, colour=Field))+geom_point()+facet_grid(.~Season)
#looks like lot of variation based on field properties on Yield, may be soil type etc

ggplot(data, aes(x=Region, y=Yield, colour=Field))+geom_point()+facet_grid(.~Farmer)
#there's definitely both field and farmer effect



mydata <- data
library(sp)
coordinates(mydata) <- c("Longitude", "Latitude")
proj4string(mydata) <- CRS("+proj=longlat")
plot(mydata)
plot(mydata@data$Northing, mydata@data$Easting, add=TRUE)




#based on this plot, it is clear this covers three regions
plot(y=data$Northing ,x=data$Easting)  
data$Location <- 2
data$Location[data$Northing < 6300000] <- 1
data$Location[data$Northing > 6340000] <- 3




#in diff regions, based on diff seasons, yield analysis
library(ggplot2)
ggplot(data, aes(x=Location, y=Yield))+geom_point()+facet_grid(.~Season)
#seems like region 2 within same season is not performing well
#is it due to soil properties, farmer practices, 

by(data$pH, data$Location, mean)
lapply(data[c(7:21)], function(x) by(x, data$Location, mean ))
#it seems like sand could be one of the reasons, silt, DPL, Weeds, Cont, Irrig, 
# it seems like it is a mixed of soil properties, and management - reason for low yield in
#region 2

ggplot(data, aes(x=Field, y=Yield, colour=factor(Location)))+geom_point()+facet_grid(.~Season)
ggplot(data, aes(x=Field, y=Yield, colour=Farmer))+geom_point()+facet_grid(.~Season)
#even though region is a factor, but farmer practices is also have some effect on yield
#what could be something of the region that can affect Yield? 
#rice is known to be flood irrigated in Uruguay, so if there is terrain, then may be 
#Yield is low

#let's check if terrain is affecting Yield (topography of region)
library(raster)
#dem.ras <- raster('Set3/srtm_26_19.tif')
dem.ras <- raster('Set3/Auxiliary/dem.asc')
projection(dem.ras) <- CRS("+proj=longlat +datum=WGS84")

range(data$Latitude)
range(data$Longitude)
crop.extent <- matrix(c(-54.52, -53.74,-33.76, -32.78), nrow=2, byrow=TRUE)
dem.set <- crop(dem.ras, extent(crop.extent))

slope.Set3 <- terrain(dem.set, type = "slope")

Set3.WGS <- data
coordinates(Set3.WGS) <- c("Longitude", "Latitude")
slopes <- extract(slope.Set3, Set3.WGS)
range(slopes, na.rm=TRUE)
cor(slopes, data$Yield, "pairwise.complete.obs")  #not correlated

#maybe a season effect?
#get the weather data
weather <- read.csv("Set3/Set3weather.csv")
plot(y=weather$Temp0203, x=weather$Measurement, type="p")
lines(weather$Temp0203, lty=3)

ggplot(weather, aes(x=Measurement,y=Temp0203))+geom_point()

ggplot(data, aes(x=Field, y=Yield, colour=factor(RiceYear)))+geom_point()
# it seems like there's an year affect on yield for field 12 and 5

#let's do a t-test to see if the affect is significant or not
t.test(data$Yield[(data$Field==5 & data$RiceYear==1)],data$Yield[(data$Field==5 & data$RiceYear==2)] )
# p-value is less, so significant

data$Yield[(data$Field==3 & data$RiceYear==1)]

#find summary of yield based on season
tapply(data$Yield, data$Season, mean)

#in a season, there are different regions
#in 1 season, there regions
#so may be summary of yield based on location-season combination
data$SeasonLoc <- with(data, Season+10*Location)
tapply(data$Yield, data$SeasonLoc, mean)


ggplot(data, aes(x=Season, y=Field, colour=factor(Location)))+geom_point()
ggplot(data, aes(x=Season, y=RiceYear))+geom_point()
# in 1 RiceYear you have 3 seasons, in each of the season you have 3 locations or regions


#on the same filed, yields are so different, may be some farmers were skilled(?)
ggplot(data, aes(x=Field, y=Yield, colour=Farmer))+geom_point()

#check summary based on two or more groups- use tapply
with(data, tapply(Yield, list(Farmer, Field, Season), mean)) #not an efficient way to do 

#create a new variable and then use tapply
data$Var <- with(data, paste(Season, Farmer, Field))
tapply(Yield, data$Var, mean)

ggplot(data, aes(x=Farmer, Yield, colour=factor(Location)))+geom_boxplot()+facet_grid(.~Season)
#there might be something in the region that is affecting Yield

#rpart,recursive partioning
#classification and regression trees (CART)
#there is a split for root node based on which predictor and value minimizes RSS(squares of teh diff
#between response and mean of the obs in that region)
#trees are prone to overfitting(tends to become large), so we need to prune the tree
#so we add a parameter that penalizes the tree for being more complex
#we identify that parameter by cross-validation(cost complexity parameter)

#decision tree to understand complex relationships of various predictors with yield
#let's dig into it deeper
cont.parms <- rpart.control(minsplit = 20,cp = 0.003)
model <- rpart("Yield ~ Season+ Field+ Region +DPL + Cont + Irrig + Emer +Weeds +Cont+Irrig+D50+
                      Fert + N + P + K + Variety + pH + Corg + SoilP + SoilK + Sand + Silt + Clay + Farmer", data = data)

min.xerror <- model$cptable[which.min(model$cptable[,"xerror"]),"CP"]
rt.pruned <- prune(model,cp = min.xerror) 
plot(rt.pruned)
text(rt.pruned)

#decision tree to understand complex relationships of various predictors with Farmer
Set3.rp2 <- rpart("Farmer ~ DPL + Cont + Irrig +
                    + N + P + K + Var + pH + Corg + SoilP + SoilK + Sand +
                  + Silt + Clay ", data = data, control = cont.parms)


plot(Set3.rp2)
text(Set3.rp2)
plotcp()

#bagging - bootstrap samples from original training dataset, and form trees, do this multiple
#times, and average the results - problem with this approach is that trees are not very
#independent(average will reduce the bias - sigma2/n)

#random forest- (modern form of bagging) (the idea is that we will try to minimize the correlation between trees) 
#random forest is ensemble of tress, and then you take the avergae of those trees
#for split candidates, instead of taking all the predictors p,
#we just take m =square-root of p predictors, forcing the trees to choose different
#predictors eveery time to reduce variance
#adding more trees won't hurt
notin <- c("ID", "RiceYear", "Season", "Field", "Latitude", "Longitude","Var", "Northing", "Easting", "Farmer", "Region")
data.new <- as.data.frame(lapply(data[!(names(data) %in% notin)], function(x) x/max(x)))

data <- cbind(data[c("RiceYear", "Season", "Field", "Var", "Farmer", "Region")], data.new)

library(randomForest)
model = randomForest(Yield ~., data=data)
VarImpPlot(model)


