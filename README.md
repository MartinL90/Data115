# Data115
##Sort the data, create a new dataset contains data year from 2006 to 2011.
vgsales.year <- filter(vgsales, Year %in% c(2006, 2007, 2008, 2009, 2010, 2011))
##Create six histograms to show global sales of different genres.
ggplot(vgsales.year, aes(x=Global_Sales,color=Genre))+geom_histogram()+ggtitle("Video Games Sales From 2006 to 2011")+xlim(c(0,15))+ylim(c(0,350))+facet_wrap(~Year)
