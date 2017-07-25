# First let's load some libraries
#install.packages("/Users/RMJ/Desktop/XML_3.98-1.9.tgz", repos=NULL, type="source") 
install.packages("magrittr")
install.packages("XML")
#(esto se corre en el console y le quitas el # del principio)

# I have borrowed Andrie's code from stackoverflow
# http://goo.gl/noYVo7
source("http://goo.gl/w64gfp")
library(magrittr)
library(XML)
# Load in the google spreadsheet
# http://goo.gl/tObo1r
u <- "https://drive.google.com/open?id=1sqClbEvcwqqrjlYxIlFZWEullmL0XS971SONKrtYr1c"

acled <- readGoogleSheet(u) %>% cleanGoogleTable(table=1) %>% as.data.frame

acled$Date <- as.Date(acled$event_date, format="%d.%m.%Y")
acled$Day  <- acled$Date-min(acled$Date)

# Force columns to be text
acled[,2:ncol(acled)] <- sapply(acled[,2:ncol(acled)], as.numeric)

# Create totals across countries
# Total Suspected
ebola$totalSus <- with(acled, Awdal+LibSus+NigSus+SLSus)
# Total Deaths
ebola$totalDeath <- with(ebola, GuinDeath+LibDeath+NigDeath+SLDeath)
# Total Laboritory Confirmed Cases
ebola$totalLab <- with(ebola, GuinLab+LibLab+NigLab+SLLab)

ebola.long <- melt(ebola, id.vars=c('Date','Day'))

# Create a variable for country
ebola.long$Country <- ''
ebola.long$Country[grepl("Guin",ebola.long$variable)] <- "Guinea"
ebola.long$Country[grepl("Lib",ebola.long$variable)] <- "Liberia"
ebola.long$Country[grepl("Nig",ebola.long$variable)] <- "Nigeria"
ebola.long$Country[grepl("SL",ebola.long$variable)] <- "Sierra Leone"

# Specify the type
ebola.long$type <- "Suspected Infected"
ebola.long$type[grepl("Death",ebola.long$variable)] <- "Death"
ebola.long$type[grepl("Lab",ebola.long$variable)  ] <- 
  "Laboratory Confirmed"
ebola.long$type[grepl("total",ebola.long$variable)] <- "Total"


png(filename='2014-09-06Suspected.png', width=700, height=400)
ggplot(subset(ebola.long, type=="Suspected Infected"), 
       aes(y=value, x=Date, 
           group=Country, color=Country, shape=Country)) + 
  geom_smooth(method=loess)+geom_point(size=3)+
  scale_x_date(name="Month") +
  scale_y_continuous(name="Number of Cases") +
  ggtitle('Suspected Number of Cases by Country')
dev.off()

png(filename='2014-09-06Death.png', width=700, height=400)
ggplot(subset(ebola.long, type=="Death"), 
       aes(y=value, x=Date, 
           group=Country, color=Country, shape=Country)) + 
  geom_smooth(method=loess)+geom_point(size=3)+
  scale_x_date(name="Month") +
  scale_y_continuous(name="Number of Deaths") +
  ggtitle('Deaths by Country')
dev.off()

png(filename='2014-09-06Lab.png', width=700, height=400)
ggplot(subset(ebola.long, type=="Laboratory Confirmed"), 
       aes(y=value, x=Date, 
           group=Country, color=Country, shape=Country)) + 
  geom_smooth(method=loess)+geom_point(size=3)+
  scale_x_date(name="Month") +
  scale_y_continuous(name="Number of Cases") +
  ggtitle('Number of Laboratory Confirmed Cases by Country')
dev.off()

png(filename='2014-09-06Total1.png', width=700, height=400)
ggplot(subset(ebola.long, type=="Total"), 
       aes(y=value, x=Date, 
           group=variable, color=variable, shape=variable)) + 
  geom_smooth(method=loess)+geom_point(size=3)+
  scale_x_date(name="Month") +
  scale_y_continuous(name="Totals") +
  scale_colour_discrete(name  ="Value",
                        breaks=c("totalSus", "totalDeath", 'totalLab' ),
                        labels=c("Suspected Cases", "Deaths", "Lab Confirm")) +
  scale_shape_discrete(name  ="Value",
                       breaks=c("totalSus", "totalDeath", 'totalLab' ),
                       labels=c("Suspected Cases", "Deaths", "Lab Confirm")) +
  ggtitle('Total Number of Cases Across Countries')
dev.off()

# Log transformation
ebola.long$logvalue <- log(ebola.long$value)

png(filename='2014-09-06TotalLog.png', width=700, height=400)
ggplot(subset(ebola.long, type=="Total" & Day>25), 
       aes(y=logvalue, x=Day, group=variable, 
           color=variable, shape=variable)) + 
  geom_smooth(method=loess)+geom_point(size=3) +
  scale_x_continuous(name="Days since Discovery") +
  scale_y_continuous(name="Totals in Natural Log") +
  scale_colour_discrete(name  ="Value",
                        breaks=c("totalSus", "totalDeath", 'totalLab' ),
                        labels=c("Suspected Cases", "Deaths", "Lab Confirm")) +
  scale_shape_discrete(name  ="Value",
                       breaks=c("totalSus", "totalDeath", 'totalLab' ),
                       labels=c("Suspected Cases", "Deaths", "Lab Confirm")) +
  ggtitle('Total Across Countries')
dev.off()

LSratio <- with(ebola, data.frame(
  Day=Day,
  Guinea=GuinLab/GuinSus,
  Liberia=LibLab/LibSus,
  Nigeria=NigLab/NigSus,
  SL=SLLab/SLSus),
)


LS.long <- melt(LSratio, id.vars='Day')
LS.long <- LS.long[!is.na(LS.long$value),]

ggplot(LS.long,
       aes(y=value, x=Day, 
           group=variable, color=variable, shape=variable)) + 
  geom_line() + geom_point(size=3) +
  scale_x_continuous(name="Days since Discovery") +
  scale_y_continuous(name="Number of Cases") +
  ggtitle('Number of Laboratory Confirmed Cases by Country')

#here is plot
(Days <- max(ebola$Day))


ebola$Day2 <- ebola$Day^2
ebolasub <- subset(ebola,Day>21)
lm(log(totalSus)~Day,  weight=Day, data=ebolasub)$coefficients
lm(log(totalDeath)~Day,weight=Day, data=ebolasub)$coefficients
lm(log(totalLab)~Day,  weight=Day, data=ebolasub)$coefficients

ebola$Day2 <- ebola$Day^2
ebolasub <- subset(ebola,Day>21)
shat=lm(log(totalSus)~Day+Day2,  weight=Day, data=ebolasub)$coefficients
dhat=lm(log(totalDeath)~Day+Day2,weight=Day, data=ebolasub)$coefficients
lhat=lm(log(totalLab)~Day+Day2,  weight=Day, data=ebolasub)$coefficients

projMax <- 28
projDay <- 1:(Days+projMax)

ebolahat <- data.frame(Day=projDay, 
                       Esus = (shat[1]+shat[2]*projDay+shat[3]*projDay^2) %>% exp %>% round,
                       Edea = (dhat[1]+dhat[2]*projDay+dhat[3]*projDay^2) %>% exp %>% round,
                       Elab = (lhat[1]+lhat[2]*projDay+lhat[3]*projDay^2) %>% exp %>% round)

ebolahat$totalSus<-ebolahat$totalDeath<-ebolahat$totalLab<-''

ebolahat$totalSus[ebola$Day+1]   <- ebola$totalSus
ebolahat$totalDeath[ebola$Day+1] <- ebola$totalDeath
ebolahat$totalLab[ebola$Day+1]   <- ebola$totalLab

ebolahat$date <- ebola$Date[1]+projDay-1

ebolahat[c('date','Day','totalDeath', 'Edea', 'totalSus', 'Esus', 'totalLab', 'Elab')]

ebolahat.long <- melt(ebolahat, id.vars=c('date','Day'))
ebolahat.long <- ebolahat.long[!is.na(ebolahat.long$value),]


png(filename='2014-09-06TotalProj.png', width=700, height=400)
ggplot(subset(ebolahat.long,Day>50) ,
       aes(y=value, x=date, group=variable, 
           color=variable, shape=variable)) + 
  geom_line(data=ebolahat.long[ebolahat.long$variable %in% c('Esus','Edea','Elab'),], size=3, alpha=.4)+
  geom_point(data=ebolahat.long[ebolahat.long$variable %in% c("totalSus", "totalDeath", 'totalLab'),], size=3) +
  scale_x_date(name="Days since Discovery") +
  scale_y_continuous(name="Total Number") +
  scale_colour_discrete(name  ="Value",
                        breaks=c("totalSus", "totalDeath", 'totalLab',
                                 'Esus','Edea','Elab'),
                        labels=c("Suspected Cases", "Deaths", "Lab Confirm",
                                 "E Suspected", "E Deaths", "E Lab")) +
  scale_shape_discrete(name  ="Value",
                       breaks=c("totalSus", "totalDeath", 'totalLab',
                                'Esus','Edea','Elab'),
                       labels=c("Suspected Cases", "Deaths", "Lab Confirm",
                                "E Suspected", "E Deaths", "E Lab")) +
  ggtitle('Total Cases in West Africa')
dev.off()

# Six months out
projMax <- 360
projDay <- 1:(Days+projMax)

ebolahat <- data.frame(Day=projDay, 
                       Esus = (shat[1]+shat[2]*projDay+shat[3]*projDay^2) %>% exp %>% round,
                       Edea = (dhat[1]+dhat[2]*projDay+dhat[3]*projDay^2) %>% exp %>% round,
                       Elab = (lhat[1]+lhat[2]*projDay+lhat[3]*projDay^2) %>% exp %>% round)

ebolahat$totalSus<-ebolahat$totalDeath<-ebolahat$totalLab<-NA

ebolahat$totalSus[ebola$Day+1]   <- ebola$totalSus
ebolahat$totalDeath[ebola$Day+1] <- ebola$totalDeath
ebolahat$totalLab[ebola$Day+1]   <- ebola$totalLab

ebolahat$date <- ebola$date[1]+1:(Days+projMax)-1

ebolahat[nrow(ebolahat),]

