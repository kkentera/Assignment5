
#Load in the SkuMaster.csv dataset.
dat.master<-read.csv("https://raw.githubusercontent.com/kkentera/Assignment5/main/sku%20master.csv",header=TRUE,na.strings="0")
dat.master<-as.data.frame(dat.master)


#We would only like to consider those observations for which the Cubic Feet per 
#UOM is greater than zero and less than two, and for which the Weight per UOM is greater than zero and less than 50.
dat.master<-dat.master[dat.master$UomCube>0&dat.master$UomCube<2,]
dat.master<-dat.master[dat.master$UomWeight>0&dat.master$UomWeight<50,]



# Note also that only the factor levels of Case(CA), Each(EA), Pallet(PL), and Pound(LB) for the Units 
#of Measure (UoM) are admissible, all observations with other designations should be omitted
dat.master<-dat.master[dat.master$Uom=="CA"|dat.master$Uom=="EA"|dat.master$Uom=="PL"|dat.master$Uom=="LB",]

#In addition, all rows with NA should be dropped.
dat.master<-na.omit(dat.master)

#after filtering a dataframe, unused levels may be leftover
dat.master<-droplevels(dat.master)

#Create a boxplot on Weight per UOM. count outliers
boxplot(dat.master$UomWeight,
        col="cadetblue1",
        main="Boxplot of Weight Per Unit of Measure",
        ylab="Weight Per UOM (pounds)",
        cex.main=1.5,
        cex.label=1.25,
        font.main=2,
        border="blue",
        pch=20,
        cex=0.5)




outliers<-boxplot.stats(dat.master$UomWeight)$out
###############################################################################
#There are 75 outliers
###############################################################################


#Create a scatterplot on the Units per Case and the Weight per UOM
plot(dat.master$UomWeight,dat.master$UnitsPerCase,
     col="blue",
     main="Units Per Case by Weight Per Unit of Measure",
     xlab="Weight Per UOM (pounds)",
     ylab="Units Per Case",
     cex.main=1.5,
     cex.label=1.25,
     font.main=2,
     font.lab=2,
     border="blue",
     pch=20,
     cex=1)

#Create a plot showing the frequency/count with which the levels of the factor Commodity occur
dat.master$Commodity<-as.factor(dat.master$Commodity)
barplot(table(dat.master$Commodity),
        col="blue",
        main="Frequency of Factor Commodity",
        xlab="Factor Commodity",
        ylab="Frequency",
        cex.main=1.5,
        cex.label=1.25,
        font.main=2,
        font.lab=2,
        font.axis=2)



#Create a plot showing the frequency/count with which the levels of the Units of Measure occur
dat.master$Uom<-as.factor(dat.master$Uom)
barplot(table(dat.master$Uom))



#Create a side-by-side boxplot of Cubic Feet by UoM by the types of Flow.  
boxplot(dat.master$UomCube~dat.master$Flow) 



#Filter data to only have direct to store supply chain channel
dat.master<-dat.master[dat.master$Flow %in% c("DD"), ]


#Create a boxplot on the weight per UOM
boxplot(dat.master$UomWeight)
#########################################################
#The outliers appear to be where the UOM weight is 29.0, 17.8, and 20.0

#The highest outlier is 29.0, so we would keep these in the data frame. they are not outliers.
#########################################################


#Create a histogram on weight per UOM
hist(dat.master$UomWeight)


#Create a dotplot on Weight per Uom labeled with the SKU Number.  Which Sku has the highest Weight per Uom?
dotchart(dat.master$UomWeight,dat.master$SkuNbr)


#Create a stripchart for Weight per Uom by the Units of Measure (there should only be Each(EA) and Case(CA)).  
stripchart(dat.master$UomWeight~dat.master$Uom)



