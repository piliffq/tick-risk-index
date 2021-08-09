# Estimating Risk Index

This script uses 2 databases:

* Park.use: It's a long database in which every row represents a a time spent by one individual in a specific location within a park and site. One individual can be represented in one row if remained in the same location for the 30-min observation period; or multiple rows if moved to different locations within the site during the observation period. For each row we identified the habitat and time spent in that location.

* est.tick.sum: Includes the estimated tick density by park, site type and drag habitat type derived from a GLM.
 

# The proportion of time spent in each habitat/transect

For each person, estimate the total time

```{r}
    Park.use.ind <- Park.use %>% group_by(ind_id) %>% summarise(totelapsed = sum(elapsed))
    #merge with long database
    Park.use <- merge(Park.use, Park.use.ind, by.x = "ind_id", by.y = "ind_id", all.x = TRUE)    
```

For trails, since we don't have the exit times, we can generate a negative binomial distributions using the variance from the elapsed observations to have a measure of variability and a median of 15 min (half of estimated period).

```{r}
    variance <- var(Park.use$elapsed,na.rm = TRUE)
    mu <- 1.5
    variance <- 2
    size <- mu^2/(variance-mu)
    set.seed(1234)
    Park.use$trails.time <-rnbinom(9847, mu = mu, size = size)
    x_pnbinom <- seq(0, 100, by = 1) 
    a.1 <-pnbinom(x_pnbinom, mu = mu, size = size)
    plot(data.frame(a.1)) 
    Park.use$trails.time[Park.use$site.type != "Trail"] <- NA
    Park.use$trails.time[Park.use$trails.time > 30] <- 30
    
    we add the latter to elapsed time and total elapsed
    Park.use$totelapsed <- ifelse(Park.use$site.type == "Trail",Park.use$trails.time, Park.use$totelapsed)

    Park.use$elapsed <- ifelse(Park.use$site.type == "Trail",Park.use$trails.time, Park.use$elapsed)
```

Since the risk index is going to be estimated for the 30min observation period, we assume that if the total time is less than 30 min, the rest of the time was spent somewhere else and created another column to keep track of that:
```{r}
    Park.use$unknown.risk.time <- (30 - Park.use$totelapsed)/30
 ```   
 For each transect, estimated the proportion of time in a 30min obs period
```{r}
Park.use$known.risk.time <- (Park.use$elapsed)/30    
```

# Merge Park use database with tick density predicted database

```{r}
Park.use <- merge(Park.use,est.ticks.sum, by = "matchID", all.x = TRUE)

if they were in an impervious surface --> I set the predicted tick dens to 0
Park.use$median.AA[Park.use$Habitat_recode == "I"]<-0
Park.use$median.IS[Park.use$Habitat_recode == "I"]<-0
Park.use$median.HL[Park.use$Habitat_recode == "I"]<-0
Park.use$median.AA.1m[Park.use$Habitat_recode == "I"]<-0
Park.use$median.IS.1m[Park.use$Habitat_recode == "I"]<-0
Park.use$median.HL.1m[Park.use$Habitat_recode == "I"]<-0

```

# Risk index

Now you can calculate the risk index which is the probability of a tick encounter given the proportion of time spent in each area during 30min of observation and the density of ticks in each area per 1m2
    
    p=1-(1-t)^n
    t=Park.use$known.risk.time
    n=Park.use$dens.IS (and others)

If the person stayed in the same spot until the end of the observation period (30m), we turn it into 0.99 to avoid approaching the limit

```{r}
Park.use$known.risk.time[Park.use$known.risk.time == 1]<-0.99999

Park.use$Pb.IS <- (1-(1-Park.use$known.risk.time)^Park.use$median.IS.1m)
Park.use$Pb.AA <- (1-(1-Park.use$known.risk.time)^Park.use$median.AA.1m)
Park.use$Pb.HL <- (1-(1-Park.use$known.risk.time)^Park.use$median.HL.1m)
```

# Descriptive analysis 

```{r}
kruskal.test(Pb.IS ~ park , Park.use, site.type == "Open Space")
kruskal.test(Pb.IS ~ park , Park.use, site.type == "Trail")

kruskal.test(Pb.IS ~ site.type , Park.use, park == "Willowbrook")
kruskal.test(Pb.IS ~ park , Park.use, site.type == "Trail")

#IS in trails
      median(Park.use$Pb.IS[Park.use$site.type == "Trail"],na.rm = TRUE)
      quantile(Park.use$Pb.IS[Park.use$site.type == "Trail"], probs=0.25, na.rm=TRUE)
      quantile(Park.use$Pb.IS[Park.use$site.type == "Trail"], probs=0.75, na.rm=TRUE)
      
#IS in Open spaces
      median(Park.use$Pb.IS[Park.use$site.type == "Open Space"],na.rm = TRUE)
      quantile(Park.use$Pb.IS[Park.use$site.type == "Open Space"], probs=0.25, na.rm=TRUE)
      quantile(Park.use$Pb.IS[Park.use$site.type == "Open Space"], probs=0.75, na.rm=TRUE)
      quantile(Park.use$Pb.IS[Park.use$site.type == "Open Space"], probs=0.99, na.rm=TRUE)*100
      
#AA in trails
      median(Park.use$Pb.AA[Park.use$site.type == "Trail"],na.rm = TRUE)*100
      quantile(Park.use$Pb.AA[Park.use$site.type == "Trail"], probs=0.25, na.rm=TRUE)*100
      quantile(Park.use$Pb.AA[Park.use$site.type == "Trail"], probs=0.75, na.rm=TRUE)*100
      
      #AA in Open spaces
      median(Park.use$Pb.AA[Park.use$site.type == "Open Space"],na.rm = TRUE)
      quantile(Park.use$Pb.AA[Park.use$site.type == "Open Space"], probs=0.25, na.rm=TRUE)
      quantile(Park.use$Pb.AA[Park.use$site.type == "Open Space"], probs=0.75, na.rm=TRUE)
      quantile(Park.use$Pb.AA[Park.use$site.type == "Open Space"], probs=0.99, na.rm=TRUE)*100
      
#HL in trails
      median(Park.use$Pb.HL[Park.use$site.type == "Trail"& Park.use$park == "Conference House"],na.rm = TRUE)*100
      quantile(Park.use$Pb.HL[Park.use$site.type == "Trail" & Park.use$park == "Conference House"], probs=0.25, na.rm=TRUE)*100
      quantile(Park.use$Pb.HL[Park.use$site.type == "Trail" & Park.use$park == "Conference House"], probs=0.75, na.rm=TRUE)*100
      
#HL in Open spaces
      median(Park.use$Pb.HL[Park.use$site.type == "Open Space" & Park.use$park == "Conference House"],na.rm = TRUE)
      quantile(Park.use$Pb.HL[Park.use$site.type == "Open Space" & Park.use$park == "Conference House"], probs=0.25, na.rm=TRUE)
      quantile(Park.use$Pb.HL[Park.use$site.type == "Open Space" & Park.use$park == "Conference House"], probs=0.75, na.rm=TRUE)
      quantile(Park.use$Pb.HL[Park.use$site.type == "Open Space" & Park.use$park == "Conference House"], probs=0.99, na.rm=TRUE)*100
```

Build the plot      

```{r}
library(vioplot)
Park.use$park <- factor(Park.use$park , levels=c("Clove Lakes", "Willowbrook", "Conference House"))

pdf("pbtick.pdf", width = 4, height = 8)
par(mfrow=c(3,1))
IS.plot <- vioplot(Pb.IS ~ site.type*park,
        data= Park.use,
        ylab="Probability tick encounter (per 30min)",
        cex.lab=0.8,
        main="I. scapularis" , 
        col=c("darkslateblue", "yellow"),  
        xaxt="n",
        xlab = "",
        ylim=c(0,0.04),
        #outline=FALSE,
        lwd=0.1,
        horizontal = F,
        pchMed = 15,
        colMed = "red",
        frame.plot = F)
        legend("topleft", legend = c("Open Space", "Trail"), fill = c("darkslateblue", "yellow"), cex = 0.8, bty = "n")
        axis(side=1,at=c(1.5,3.5,5.5),labels=c("Clove Lakes", "Willowbrook", "Conference House"))
         text(x = c(1:6), y = c(0.02,0.02,0.02,0.02,0.04,0.03), labels = round(IS.plot$median, 3))

AA.plot <- vioplot(Pb.AA ~ site.type*park,
        data= Park.use,
        ylab="Probability tick encounter (per 30min)",
        cex.lab=0.8,
        main="A. americanum" , 
        col=c("darkslateblue", "yellow"),  
        xaxt="n",
        xlab = "",
        ylim=c(0,0.1),
        #outline=FALSE,
        lwd=0.1,
        horizontal = F,
        pchMed = 15,
        colMed = "red",
        frame.plot = F)
        legend("topleft", legend = c("Open Space", "Trail"), fill = c("darkslateblue", "yellow"), cex = 0.8, bty = "n")
        axis(side=1,at=c(1.5,3.5,5.5),labels=c("Clove Lakes", "Willowbrook", "Conference House"))
         text(x = c(1:6), y = c(0.02,0.02,0.02,0.02,0.08,0.06), labels = round(AA.plot$median, 3))

HL.plot <- vioplot(Pb.HL ~ site.type*park,
        data= Park.use,
        ylab="Probability tick encounter (per 30min)",
        cex.lab=0.8,
        main="H. longicornis" , 
        col=c("darkslateblue", "yellow"),  
        xaxt="n",
        xlab = "",
        ylim=c(0,0.6),
        #outline=FALSE,
        lwd=0.1,
        horizontal = F,
        pchMed = 15,
        colMed = "red",
        frame.plot = F)
        legend("topleft", legend = c("Open Space", "Trail"), fill = c("darkslateblue", "yellow"), cex = 0.8, bty = "n")
        axis(side=1,at=c(1.5,3.5,5.5),labels=c("Clove Lakes", "Willowbrook", "Conference House"))
         text(x = c(5:6), y = c(0.6,0.6), labels = round(HL.plot$median, 3))
    
IS.plot$q1
IS.plot$q3
IS.plot$median
```
         
# Exposure time

```{r}
Park.use$exposure.IS <- Park.use$elapsed*Park.use$median.IS.1m
Park.use$exposure.AA <- Park.use$elapsed*Park.use$median.AA.1m
Park.use$exposure.HL <- Park.use$elapsed*Park.use$median.HL.1m
```        
Lastly, to summarize exposure time per person, we sum their exposure as potetial risk
```{r}
Park.use$ind_id <- as.factor(Park.use$ind_id)
        
Risk.index.person <- Park.use %>%
    group_by(ind_id) %>%
    dplyr::summarize(n.records = n(),
                     site_ID = matchID,
                     Park = park,
                     Site = site.type,
                      risk.IS = mean(Pb.IS, na.rm = TRUE),
                      risk.AA = mean(Pb.AA, na.rm = TRUE),
                      risk.HL = mean(Pb.HL, na.rm = TRUE),
                     exposure.IS.person = sum(exposure.IS, na.rm = TRUE),
                     exposure.AA.person = sum(exposure.AA, na.rm = TRUE),
                     exposure.HL.person = sum(exposure.HL, na.rm = TRUE)
                     )
Risk.index.person <- Risk.index.person[!duplicated(Risk.index.person$ind_id), ]
```

Descriptive analyses
```{r}
#IS in trails
median(Risk.index.person$exposure.IS.person[Risk.index.person$Site == "Trail"],na.rm = TRUE)
quantile(Risk.index.person$exposure.IS.person[Risk.index.person$Site == "Trail"], probs=0.25, na.rm=TRUE)
quantile(Risk.index.person$exposure.IS.person[Risk.index.person$Site == "Trail"], probs=0.75, na.rm=TRUE)

#IS in Open spaces
median(Risk.index.person$exposure.IS.person[Risk.index.person$Site == "Open Space"],na.rm = TRUE)
quantile(Risk.index.person$exposure.IS.person[Risk.index.person$Site == "Open Space"], probs=0.25, na.rm=TRUE)
quantile(Risk.index.person$exposure.IS.person[Risk.index.person$Site == "Open Space"], probs=0.75, na.rm=TRUE)
quantile(Risk.index.person$exposure.IS.person[Risk.index.person$Site == "Open Space"], probs=0.99, na.rm=TRUE)

#AA in trails
median(Risk.index.person$exposure.AA.person[Risk.index.person$Site == "Trail"],na.rm = TRUE)
quantile(Risk.index.person$exposure.AA.person[Risk.index.person$Site == "Trail"], probs=0.25, na.rm=TRUE)
quantile(Risk.index.person$exposure.AA.person[Risk.index.person$Site == "Trail"], probs=0.75, na.rm=TRUE)

#AA in Open spaces
median(Risk.index.person$exposure.AA.person[Risk.index.person$Site == "Open Space"],na.rm = TRUE)
quantile(Risk.index.person$exposure.AA.person[Risk.index.person$Site == "Open Space"], probs=0.25, na.rm=TRUE)
quantile(Risk.index.person$exposure.IS.person[Risk.index.person$Site == "Open Space"], probs=0.75, na.rm=TRUE)
quantile(Risk.index.person$exposure.AA.person[Risk.index.person$Site == "Open Space"], probs=0.99, na.rm=TRUE)

#HL in trails
median(Risk.index.person$exposure.HL.person[Risk.index.person$Site == "Trail" & Risk.index.person$Park == "Conference House"],na.rm = TRUE)
quantile(Risk.index.person$exposure.HL.person[Risk.index.person$Site == "Trail" & Risk.index.person$Park == "Conference House"], probs=0.25, na.rm=TRUE)
quantile(Risk.index.person$exposure.HL.person[Risk.index.person$Site == "Trail" & Risk.index.person$Park == "Conference House"], probs=0.75, na.rm=TRUE)

#HL in Open spaces
median(Risk.index.person$exposure.HL.person[Risk.index.person$Site == "Open Space" & Risk.index.person$Park == "Conference House"],na.rm = TRUE)
quantile(Risk.index.person$exposure.HL.person[Risk.index.person$Site == "Open Space" & Risk.index.person$Park == "Conference House"], probs=0.25, na.rm=TRUE)
quantile(Risk.index.person$exposure.HL.person[Risk.index.person$Site == "Open Space" & Risk.index.person$Park == "Conference House"], probs=0.75, na.rm=TRUE)
quantile(Risk.index.person$exposure.HL.person[Risk.index.person$Site == "Open Space" & Risk.index.person$Park == "Conference House"], probs=0.99, na.rm=TRUE)
```

Build the plots
```{r plot risk time}
Park.use$park <- factor(Park.use$park , levels=c("Clove Lakes", "Willowbrook", "Conference House"))

pdf("risktick.pdf", width = 4, height = 8)
par(mfrow=c(3,1))
IS.exp <- vioplot(exposure.IS.person ~ Site*Park,
        data= Risk.index.person,
        ylab="Risk of tick exposure per person (min*m2)",
        cex.lab=0.8,
        main="I. scapularis" , 
        col=c("darkslateblue", "yellow"),  
        xaxt="n",
        xlab = "",
        ylim=c(0,0.6),
        #outline=FALSE,
        lwd=0.005,
        horizontal = F,
        pchMed = 15,
        colMed = "red",
        frame.plot = F)
        legend("topleft", legend = c("Open Space", "Trail"), fill = c("darkslateblue", "yellow"), cex = 0.8, bty = "n")
        axis(side=1,at=c(1.5,3.5,5.5),labels=c("Clove Lakes", "Willowbrook", "Conference House"))
         text(x = c(1:6), y = c(0.2,0.2,0.2,0.3,0.6,0.6), labels = round(IS.exp$median, 2))

AA.exp <- vioplot(exposure.AA.person ~ Site*Park,
        data= Risk.index.person,
        ylab="Risk of tick exposure per person (min*m2)",
        cex.lab=0.8,
        main="A. americanum" , 
        col=c("darkslateblue", "yellow"),  
        xaxt="n",
        xlab = "",
        ylim=c(0,1),
        #outline=FALSE,
        lwd=0.005,
        horizontal = F,
        pchMed = 15,
        colMed = "red",
        frame.plot = F)
        legend("topleft", legend = c("Open Space", "Trail"), fill = c("darkslateblue", "yellow"), cex = 0.8, bty = "n")
        axis(side=1,at=c(1.5,3.5,5.5),labels=c("Clove Lakes", "Willowbrook", "Conference House"))
         text(x = c(1:6), y = c(0.2,0.2,0.2,0.2,1,1), labels = round(AA.exp$median, 2))
         
HL.exp <- vioplot(exposure.HL.person ~ Site*Park,
        data= Risk.index.person,
        ylab="Risk of tick exposure per person (min*m2)",
        cex.lab=0.8,
        main="H. longicornis" , 
        col=c("darkslateblue", "yellow"),  
        xaxt="n",
        xlab = "",
        ylim=c(0,15),
        #outline=FALSE,
        lwd=0.1,
        horizontal = F,
        pchMed = 15,
        colMed = "red",
        frame.plot = F)
        legend("topleft", legend = c("Open Space", "Trail"), fill = c("darkslateblue", "yellow"), cex = 0.8, bty = "n")
        axis(side=1,at=c(1.5,3.5,5.5),labels=c("Clove Lakes", "Willowbrook", "Conference House"))
         text(x = c(5:6), y = c(15,15), labels = round(HL.exp$median, 3))
```
