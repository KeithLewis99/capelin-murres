## load libraries ----
#library(ggplot2)
library(lubridate)
library(cowplot)

############################
### COD CONDITION DATA#########
############################

## read data ----
cc <- read.csv('data-raw/COMU_condition.csv',header=T, as.is = T)

## manipulate data ----
cc <- subset(cc, colony == 'Funk')
cc <- subset(cc, species == 'COMU')
cc$dates <- dmy(cc$date)
cc$condition <- cc$bird_weight/cc$winglength

cc[which(cc$stage == 'Adult'),'stage'] <- 'adult'


## create data frames to store the information on density distribution and measures of central tendency for each year and stage ----
conddens <- data.frame('x' = NA,'y' = NA,'year' = NA,'stage' = NA)
centraltendency <- data.frame('meank' = NA,'mediank' = NA,'year' = NA,'stage' = NA)

## choose only rows that willl be used and na.omit on condition ----
ccc <- na.omit(cc[,c('year', 'stage', 'condition')])

## loop through years and stage divisions to obtain density distributions, means and medians of condition
for (i in min(unique(ccc$year)):max(unique(ccc$year))) {
   for (j in unique(ccc$stage)) {
      dat <- subset(ccc, year == i & stage == j)
      if (nrow(dat) > 2) {
         yy <- density(dat$condition)
         yy <- data.frame('x' = yy$x,'y' = yy$y)
         yy$year <- rep(i,nrow(yy))
         yy$stage <- rep(j,nrow(yy))
         conddens <- rbind(conddens,yy)
         ct <- data.frame('meank'=mean(dat$condition),'mediank'=  median(dat$condition),'year'=i,'stage'=j)
         centraltendency <- rbind(centraltendency,ct)
      }
   }
}

## obtain the max density per year and stage div and merge it with density distribution
maxy <- aggregate(conddens$y, by=list(conddens$stage,conddens$year),FUN=max)
names(maxy) <- c ('stage','year','maxy')
conddens <- merge(conddens, maxy)

## merge data frames with information on density distributions and centraltendency
conddens <- merge(conddens,centraltendency)

## sum year to the max density per year and stage div so that each histogram is ploted along the x-axis in the right year.
## The max density each curve takes is slightly shifted down so that distribution in year t does not overlap with distribution in year t+1
conddens$yy <- with(conddens,year+(y/(maxy+maxy*.02)))

## obtain the range of conditions that encompass 95% of the data per year and nfo div and merge it with density distribution data
bounds <- aggregate(conddens$x, by=list(conddens$stage,conddens$year),FUN= function(x) quantile(x, probs = c(0.025, 0.975)))
bounds <- data.frame(bounds[,1:2],bounds[,3][,1],bounds[,3][,2])
names(bounds) <- c ('stage','year','lb','ub')
conddens <- merge(conddens, bounds)

## define the label for the y-axis
#fultonlab<-expression(paste("Fulton's ", italic(K[s])))


## Start ggplot

 ## define working dataset, x and y variables
 p <- ggplot(conddens, aes(yy, x))
 ## this draws the histograms
 p <- p + geom_line()
 ## Plot by stage Div
 p <- p + facet_grid(stage ~ .)
 ## Add x and y axis labels
 p <- p + xlab("Year") + ylab('condition')
 ## Add the grey ribbons that encompass 95% of data. Argument alpha controls the amount of transparency
 p <- p + geom_ribbon(data = conddens,
                       aes(x = yy,
                           ymax = ub,
                           ymin = lb),
                           alpha = 0.15)
 ## Add the median per year and stage div
 p <- p + geom_line(data=conddens,aes(x=yy,y=mediank),colour='red',lwd=1)
 ## the next 3 lines plot Lambert and Dutil's benchmark values. I commented them  out
# hline.data <- data.frame(stage = rep(c('2J','3K','3L'),each=2),z = rep(c(0.7,0.85),3))
# colores <-  rep(c('darkgreen','red'),3)
# p <- p + geom_hline(data=hline.data, aes(yintercept = z,colour=colores),linetype='longdash')

 ## Aesthetic options
 p <- p  + theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust=0.5))  +  theme(legend.position="none")
 p <- p + scale_x_continuous(breaks=seq(from=1978,to=2014,by=2),limits=c(1982, 2014))
 p <- p + scale_y_continuous(breaks=seq(from=0.3,to=1.5,by=0.6))
 p <- p + theme_bw()
 ##print plot
 print(p)

 
g <- ggplot(cc, aes(year, condition))
g <- g + geom_violin() 
g <- g + stat_summary(aes(year), fun.y = median, geom = "point", fill = "black", shape = 21, size = 1.5)
g <- g + xlab("Year") + ylab('Condition')
g <- g + facet_grid(stage ~ .)
g <- g + theme_bw()
print(g)

w <- ggplot(cc, aes(year, bird_weight))
w <- w + geom_violin()
w <- w + stat_summary(aes(year), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
w <- w + xlab("Year") + ylab('Weight')
w <- w + facet_grid(stage ~ ., scales = "free_y")
w <- w + theme_bw()
print(w)



pdf(file='output/COMUcondition.pdf',width=26/2.5, height=15.5/2.5,  family = "sans", bg = "white",   pointsize = 8)
  print(g)
dev.off()

pdf(file='output/COMUweight.pdf',width=26/2.5, height=15.5/2.5,  family = "sans", bg = "white",   pointsize = 8)
  print(w)
dev.off()

library(dplyr)
avw <- cc %>%
  filter(stage == 'adult', year > 2003) %>%
  filter(complete.cases(bird_weight)) %>%
  group_by(year, stage) %>%
  summarize(avg = mean(bird_weight))

dif <- max(avw[,3]) - min(avw[,3])
av <-sum(avw[,3])/nrow(avw)
dif/av


adw <- filter(cc, stage == 'adult' & year > 2000)
summary(lm(bird_weight ~ as.integer(year), data = adw))

adw %>%
  group_by(year)  %>%
  summarise(mean(bird_weight, na.rm =T))

g <- ggplot(adw, aes(year, bird_weight))
g <- g + geom_violin() 
g <- g + stat_summary(aes(year), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
g <- g + xlab("Year") + ylab('Body weight (g)')
g <- g + theme_cowplot(font_size = 10) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
#g <- g + facet_grid(stage ~ .)
#g <- g + theme_bw()
print(g)
save_plot("output/AdultWeights.png", g, base_aspect_ratio = 1.4,  base_width = 6, bg = "transparent") # make room for figure legend)


flc <- filter(cc, stage != 'adult' & year > 1990) %>%
  select(year, condition, stage)
y <- 1990:2017
flc <- rbind(flc, data.frame(year = y[which(!y %in% sort(as.integer(unique(flc$year))))], condition = NA, stage = 'chick'))
flc[which(flc$condition > 9), 'condition'] <- NA
summary(lm(condition ~ as.integer(year) + as.factor(stage), data = flc))

g <- ggplot(flc, aes(as.factor(year), condition))
g <- g + geom_violin() 
g <- g + stat_summary(aes(year), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
g <- g + xlab("Year") + ylab('Condition (g/cm)')
g <- g + facet_grid(stage ~ ., scales = "free")
#g <- g + theme_cowplot(font_size = 10) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
g <- g + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0, size = 8))
 print(g)
 save_plot("output/FledglingCondition.png", g, base_aspect_ratio = 1.4,  base_width = 6, bg = "transparent") # make room for figure legend)

 
 flc <- filter(cc, stage != 'adult' & year > 1990) %>%
   select(year, condition, stage, bird_weight)
 y <- 1990:2017
 flc <- rbind(flc, data.frame(year = y[which(!y %in% sort(as.integer(unique(flc$year))))], condition = NA, stage = 'chick', bird_weight = NA))
 flc[which(flc$condition > 9), 'condition'] <- NA
 flc[which(flc$bird_weight > 900), 'bird_weight'] <- NA
 
 w <- ggplot(flc, aes(as.factor(year), bird_weight))
 w <- w + geom_violin() 
 w <- w + stat_summary(aes(year), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
 w <- w + xlab("Year") + ylab('Weight (g)')
 w <- w + facet_grid(stage ~ ., scales = "free")
 #w <- w + theme_cowplot(font_size = 10) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
 w <- w + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
 w <- w + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0, size = 8))
 save_plot("output/FledglingWeight.png", w, base_aspect_ratio = 1.4,  base_width = 6, bg = "transparent") # make room for figure legend)
 
 #filter(flc, stage == 'fledgling' & year == 2009)
 
 
 wl <- filter(cc, stage != 'adult' & year > 1990) %>%
   select(year, stage, winglength)
 y <- 1990:2017
 wl <- rbind(wl, data.frame(year = y[which(!y %in% sort(as.integer(unique(wl$year))))], stage = 'chick', winglength = NA))
 
 ww <- ggplot(wl, aes(as.factor(year), winglength))
 ww <- ww + geom_violin() 
 ww <- ww + stat_summary(aes(year), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
 ww <- ww + xlab("Year") + ylab('Wing length (cm)')
 ww <- ww + facet_grid(stage ~ ., scales = "free")
 #w <- w + theme_cowplot(font_size = 10) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
 ww <- ww+ theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
 ww <- ww + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0, size = 8))
 print(ww)
 save_plot("output/FledglingWlength.png", ww, base_aspect_ratio = 1.4,  base_width = 6, bg = "transparent") # make room for figure legend)
