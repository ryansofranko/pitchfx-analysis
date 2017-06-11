# Install pitchRX package
install.packages('pitchRx')
library(pitchRx)

# Store game data into frame
bdat <- scrape(game.ids = "gid_2017_06_10_phimlb_slnmlb_1")

# check to see what data frames are available
names(bdat)

# make new variables
atbat <- bdat$atbat
pitch <- bdat$pitch

# inspect atbat
head(atbat)

# load dplyr
library(dplyr)

# inner join on num column for complete game(cg), 
# filtered by top of first for Carlos Martinez
# select variables
cg <- inner_join(atbat,pitch,by="num") %>%
  filter(inning_side.x=="top")%>%
  select(num,start_tfs,stand,event,inning.x,batter_name,des,tfs,start_speed,px,pz,pitch_type)

# inspect
head(cg)

# create strike zone dataframe
 x<-c(-.95,.95,.95,-.95,-.95)
 z<-c(1.6,1.6,3.5,3.5,1.6)
 sz <- data.frame(x,z)
sz

# use ggplot to create strike zone vis and show all pitches
library(ggplot2)
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=cg, aes(x=px,y=pz, size=start_speed))+
  scale_size(range=c(1,5))

# visualize pitch type
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=cg, aes(x=px,y=pz, size=start_speed,color=pitch_type))+
  scale_size(range=c(1,5))

# place pitch_type in data frame for better labeling in ggplot and subsetting examples
temp<-cg$pitch_type
temp[which(temp=="FF")]<-"fastball"
temp[which(temp=="CH")]<-"changeup"
temp[which(temp=="CU")]<-"curveball"
temp[which(temp=="FT")]<-"two-seam fastball"
temp[which(temp=="SL")]<-"slider"
temp

cg$pitch_description<-temp 
cg

# plot with new pitch_descriptions
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=cg, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(1,5))

# plot with hue 
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=cg, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(1,5))+
  scale_color_hue(c=85)

# plot with brewer palette https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=cg, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(1,5))+
  scale_color_brewer(palette = "Dark2")

# plot with custom colors
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=cg, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red3","purple2","seagreen","orange2","steelblue"))

# to see list of all manual color values
colors()

# facet versus batter stance
head(cg)
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=cg, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red3","purple2","seagreen","orange2","steelblue"))+
  facet_wrap(~stand)+
  geom_text(data=cg,aes(label=stand),x=1,y=2,size=10)

# customize the R/L placement in the graph
stand_xcoord<-cg$stand
stand_xcoord[which(stand_xcoord=="R")]<--1.5
stand_xcoord[which(stand_xcoord=="L")]<-1.5
stand_xcoord<-as.numeric(stand_xcoord)
stand_xcoord
cg$stand_xcoord<-stand_xcoord

str(cg$stand)
cg$stand<-factor(cg$stand, levels=c("R","L"))

ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=cg, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red3","purple2","seagreen","orange2","steelblue"))+
  facet_wrap(~stand)+
  geom_text(data=cg,aes(label=stand, x=stand_xcoord), y=2.5,size=10)

#facet by atbat (num), batter name (batter_name), inning (inning.x)
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=cg, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red3","purple2","seagreen","orange2","steelblue"))+
  facet_wrap(~num)+
  geom_text(data=cg,aes(label=stand, x=stand_xcoord), y=2.5,size=3)+
  geom_text(data=cg,aes(label=batter_name), x=0,y=.5, size=3)+
  geom_text(data=cg,aes(label=inning.x),x=0,y=4, size=3)+
  geom_text(data=cg,aes(label=event),x=0,y=1, size=3)

#filter by specific atbat, batter and inning
#create variables
batter="Freddy Galvis"
inning=5
ab<-cg%>%filter(batter_name==batter, inning.x==inning)

#draw from ab dataframe
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=ab, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red3","purple2","seagreen","orange2","steelblue"))+
  geom_text(data=ab,aes(label=stand, x=stand_xcoord), y=2.5,size=20)+
  xlim(-2,2)+
  ylim(0,4.5)+
  ggtitle(paste("Inning ",inning,": ", batter,sep=""))

#label pitches
cg$des
head(cg)

ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=ab, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red3","purple2","seagreen","orange2","steelblue"))+
  geom_text(data=ab,aes(label=stand, x=stand_xcoord), y=2.5,size=20)+
  xlim(-2,2)+
  ylim(0,4.5)+
  ggtitle(paste("Inning ",inning,": ", batter,sep=""))+
  geom_text(data = ab,aes(label=des, x=px,y=pz), vjust=1.5)

#replace in play, out with event
des<-cg$des
des

indices <- which(des=="In play, out(s)")
indices
event<-cg$event
event
des[indices]<-event[indices]
des

cg$des2<-des
head(cg)

#pull label from new des2
batter="Freddy Galvis"
inning=5
ab<-cg%>%filter(batter_name==batter, inning.x==inning)

ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=ab, aes(x=px,y=pz, size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red3","purple2","seagreen","orange2","steelblue"))+
  geom_text(data=ab,aes(label=stand, x=stand_xcoord), y=2.5,size=20)+
  xlim(-2,2)+
  ylim(0,4.5)+
  ggtitle(paste("Inning ",inning,": ", batter,sep=""))+
  geom_text(data = ab,aes(label=des2, x=px,y=pz), vjust=1.5)


