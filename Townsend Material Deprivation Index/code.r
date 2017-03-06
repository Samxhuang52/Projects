###################################################
### gather variables  and combine dataframe ###
###################################################

x<-read.csv('ACS_13_5YR_DP03_with_ann.csv',header=TRUE)
y<-read.csv('ACS_13_5YR_DP04_with_ann.csv',header = TRUE)
x<-x[-1,]
y<-y[-1,]

df<-data.frame('geo'=x$GEO.display.label,
               'id'=x$GEO.id2,
               'uneply'= (x$HC03_VC07),
               'hosten'=(x$HC03_VC84),
               'noveh'=(y$HC03_VC84),
               'lowco'=(y$HC03_VC122))
#Replace '-' simbol with NA to represent missing value#
df[df=='-']<-NA


###histrogram###
#convert variable to numeric type#

df$uneply<-as.numeric(df$uneply)
df$hosten<-as.numeric(df$hosten)
df$noveh<-as.numeric(df$noveh)
df$lowco<-as.numeric(df$lowco)

#set up a two by twp graph#
par(mfrow=c(2,2))

hist(df$uneply,col="cadetblue", density=30, angle=50, border="black",
     main="unemployment rate", 
     xlab="unemployment rate", ylab="frequency")

hist(df$hosten,col="orange", density=30, angle=50, border="black",
     main="housing tenure", 
     xlab="housing tenure", ylab="frequency")

hist(df$noveh,col="red", density=30, angle=50, border="black",
     main="no vehicles", 
     xlab="no vehicles", ylab="frequency")

hist(df$lowco,col="yellow", density=30, angle=50, border="black",
     main="low occupancy", 
     xlab="low occupancy", ylab="frequency")

###summary###

apply(df[c('uneply','hosten','noveh','lowco')],2,summary)

##################################
### count the missing value###
##################################

df[!complete.cases(df),]
df.completed <-df[complete.cases(df),]

# missing value percentage of each valiables
sum(is.na(df$uneply))
sum(is.na(df$hosten))
sum(is.na(df$noveh))
sum(is.na(df$lowco))
#total percentage
1-nrow(df.completed)/nrow(df)

##percentage of missing value##
sum(is.na(df$uneply))/length(df$uneply)
sum(is.na(df$hosten))/length(df$hosten)
sum(is.na(df$noveh))/length(df$noveh)
sum(is.na(df$lowco))/length(df$lowco)



##########################
### scatter matrix ###
##########################
## no transform##
pairs(df[,c('uneply','hosten','noveh','lowco')])

##transformation##
dft <- data.frame('geo'=x$GEO.display.label,
                  'id'=x$GEO.id2,
                      'uneply'= log(as.numeric(x$HC03_VC07)+1),
                      'hosten'=log(as.numeric(x$HC03_VC84)+1),
                      'noveh'=log(as.numeric(y$HC03_VC84)+1),
                      'lowco'=sqrt(as.numeric(y$HC03_VC122)))
##sctter matrix## for transformed data##
pairs(dft[,c('uneply','hosten','noveh','lowco')])
##corelation matrix for transfromed data##
cor(data.frame(dft$uneply,dft$hosten,dft$noveh,dft$lowco))

################################
###Compute the Townsend index###
################################
library(matrixStats)
#get the mean for each variables#
tm<-data.frame(dft$geo,Means = rowMeans(dft[,-c(1,2)]))

#get the standard deviation for each variabels#
dfm <- data.matrix(dft[,-c(1,2)])
ts <- data.frame(dft$geo,Sd=rowSds(dfm))
#calculate towensed index
un_temp<-((dft$uneply-tm$Means)/ts$Sd)
hos_temp<-((dft$hosten-tm$Means)/ts$Sd)
nov_temp<-((dft$noveh-tm$Means)/ts$Sd)
low_temp<-((dft$lowco-tm$Means)/ts$Sd)
df_temp <-data.frame(un_temp,hos_temp,nov_temp,low_temp)

df_townsend <- data.frame(dft$geo,dft$id,Townsend=df_temp$un_temp+
                            df_temp$hos_temp+
                            df_temp$nov_temp+
                            df_temp$low_temp)
###########
########
###########
##Max Townsend Index Census Tract##
df_townsend[which.max(df_townsend$Townsend),]
##Min Townsend Index Census Tract##
df_townsend[which.min(df_townsend$Townsend),]



rm(df_temp,dfm,dft,tm,ts,x,y)
###############
###Map ###
###############
library(RColorBrewer)
library(rgdal) 

#import map data and remove census tract not in New York County#
map <- readOGR(dsn="tl_2013_36_tract", layer="tl_2013_36_tract")  
ny_map<- map[is.element(map$GEOID,df$id),]

#match the GEOID as row names of the map data and Townsend DataFrame#
row.names(ny_map)<-as.character(ny_map$GEOID)
row.names(df_townsend)<-df_townsend$dft.id
df_townsend<-df_townsend[row.names(ny_map),]

#Set up 9 levels of color, cut and break the Townsend Index#
breaks.factor <-cut(df_townsend$Townsend,
                   breaks=seq(from=min(df_townsend$Townsend),
                             to=max(df_townsend$Townsend),length=9))
#Set up color pallette to reverse order and set up color coding
color.palette <- rev(brewer.pal(n=length(levels(breaks.factor)),"Spectral"))
color.coding <- color.palette[as.numeric(breaks.factor)]

#Plot, add legend, text and point, arrow#
plot(ny_map, col=color.coding)
#-73.95,40.76
legend(-73.95,40.76565, legend=c(levels(breaks.factor),'Lowenstein'),
       fill=c(color.palette,'black'),cex=1.2, bty="n", y.intersp=1.2, ncol=1)
ny_map@bbox
text(-73.96,40.8848, cex=1.5,
     labels=c("New York County Townsend Index"))

subset(df_townsend, total==max(df_townsend, na.rm=TRUE))
points(coordinates(ny_map['36061014500',]), cex=1.2, pch=19)
arrows(x0=-74.017, y0=40.81, x1=-73.987, y1=40.775, length=0.1, lwd=2)
text(-74.04, 40.82, labels="Census Tract 145\n(Lowenstein)\nrank195")

###########################
##Lowenstein ranking ##
###########################
dfr = df_townsend[rev(order(df_townsend$Townsend)),]
dfr['rank']=seq(1,length(dfr$Townsend))
dfr[dfr$dft.id=='36061014500',]
