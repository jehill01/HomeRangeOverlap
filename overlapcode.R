library(amt)
library(lubridate)
library(dplyr)
library(stringr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(emmeans)
library(glmmTMB)

data<-read.csv("datanew2.csv")
data$Name<-as.character(sprintf("%03d", data$ID))
data$Timestamp<-as.POSIXct(data$Timestamp, tz="EST", 
                           format="%m/%d/%Y %H:%M:%S")
data<-data[!(data$ID=="896"),] #removing data deficient ones
data<-data[!(data$ID=="921"),]

datalist<-split(data, data$Name, drop = TRUE)
track<-list()
trast<-list()
kde<-list()
list2<-list()
over<-list()
for (i in names(datalist) ){ ## i=names(datalist)[2]
  track[[i]]<-make_track(datalist[[i]], Easting,  
                         Northing, Timestamp, 
                         id=ID, crs=('+proj=utm +zone=17N +datum=NAD83'))
  trast[[i]]<-make_trast(track[[i]], res=50)
  track[[i]]$month <-month(track[[i]]$t_)
  track[[i]]$year<-year(track[[i]]$t_)
  track[[i]]$monthyear<-sprintf("%04d%02d%03d", track[[i]]$year, track[[i]]$month, track[[i]]$id) 
  #making the identifier needed for each home range for splitting
  list2[[i]]<-split(track[[i]], track[[i]]$monthyear)
  kde[[i]] <-map(list2[[i]], hr_kde, trast = trast[[i]], levels = c(0.95, 0.6))
  over[[i]]<-hr_overlap(kde[[i]], type="udoi", which="consecutive", conditional=TRUE )
  
}

rf<-bind_rows(over)
rf$ID<-str_sub(rf$from, -3) #did this to get the animal ID for joining
rf$start<-str_sub(rf$from, 5,6)
rf$end<-str_sub(rf$to, 5,6)
rf$Name<-rf$ID
df2<- data %>% distinct (Name, .keep_all = TRUE) #getting the animal habitat/sex info to join 
r2<-data.frame(rf)
dataset<-rf %>% left_join(df2, by="Name")
#sometimes theres more than a month gap between home ranges so need to delete these gaps
dataset$start<-as.numeric(dataset$start) #make these numeric
dataset$end<-as.numeric(dataset$end)
r3<- r3 %>%
  mutate(subs=end-start)
dataset<-subset(r3, r3$subs==1|r3$subs==-11) #keeping only a month difference (-11 is acceptable b/c its dec to jan)
over60<-dataset[(dataset$levels==0.60),]
over95<-dataset[(dataset$levels==0.95),]

over60<-over60[!(over60$overlap>0.999),] #deleting these few where the value is >1
over95<-over95[!(over95$overlap>0.999),]

over95 %>% 
  group_by(habitat, sex) %>%
  summarise(Mean95=mean(overlap),
          
  )%>% print(n=98)

#this runs a mixed beta regression with animal ID as a random effect

#60% home range overlap
model<-glmmTMB(overlap ~ sex*habitat +UD60 + (1|ID.x), ziformula = ~1,  over60, family = beta_family(),  na.action=na.pass)
dredge(model)
modelfit<-glmmTMB(overlap ~ habitat + (1|ID.x), ziformula = ~1,  over60, family = beta_family(),  na.action=na.pass) #the fitted  model
emmeans(modelsec, pairwise~habitat, type="response")
emmip(modelsec, ~habitat, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")

#95% home range overlap
model2<-glmmTMB(overlap ~ sex*habitat+UD95 + (1|ID.x), ziformula = ~1,  over95, family = beta_family(),  na.action=na.pass)
dredge(model2) #top model is the null so nothing further

#Figure
emmip(modelsec, ~habitat, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")+
  theme(panel.border = element_rect(colour='black', fill='NA'), 
        panel.background = element_rect(fill='gray97'),panel.grid = element_line(colour = NA), 
    axis.text=element_text(size=11), axis.title.y = element_text(size=12, vjust=3.9), strip.text = element_text(size=12, face="bold"))+
  theme(plot.margin=unit(c(0.2,0.2,0.2,0.5), "cm"))+ylab("Monthly home range overlap")+xlab("Habitat")
