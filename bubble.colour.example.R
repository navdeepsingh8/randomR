# LondonR Code Dojo 11/12/2013
# Code snippet: WDI/rworldmap - merging colours and bubbles in one chart

# load required packages
library(WDI)
library(rworldmap)
library(RColorBrewer)

# grab data from World Bank
contents <- WDI_data[[1]]
ind <- "NY.GDP.PCAP.CD"
name <- contents[which(contents[,"indicator"]==ind),"name"]
df <- WDI(indicator=ind,start=2011)

# create map data frame
mdf <- joinCountryData2Map(df,nameJoinColumn="iso2c",joinCode="ISO2")

# augment map data frame with area data
mdf$area <- joinCountryData2Map(WDI(indicator="AG.LND.TOTL.K2"),
                                nameJoinColumn="iso2c",joinCode="ISO2")$AG.LND.TOTL.K2
# create bubble/colour threshold and split data
thresh <- quantile(mdf$area,0.2,na.rm=TRUE)
mdf$dataToColour <- mdf[[ind]]
mdf$dataToColour[mdf$area<thresh] <- NA
mdf$dataToBubble <- mdf[[ind]]
mdf$dataToBubble[mdf$area>thresh] <- NA
mdf$bubbleSizes <- rep(0.5,length(mdf[[ind]]))

# set up colour palette for chart
pal <- brewer.pal(4,"YlOrRd")

# create coloured map without legend
mapParams <- mapCountryData(mdf,nameColumnToPlot="dataToColour",addLegend=FALSE,
                            colourPalette=pal,mapTitle=name)
# add custom legend
do.call(addMapLegend,  c(mapParams, legendWidth=0.5,
                         legendShrink=0.8, legendMar = 2,
                         legendLabels="all"))
# add bubbles for small land area countries
mapBubbles(mdf,nameZSize="bubbleSizes",nameZColour="dataToBubble",
           addColourLegend=FALSE,colourPalette=pal,fill=FALSE,addLegend=FALSE,
           add=TRUE,symbolSize=0.2,lwdSymbols=1.5)
