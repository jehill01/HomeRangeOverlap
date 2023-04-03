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
data<-data[!(data$ID=="896"),] #datadeficient
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
  list2[[i]]<-split(track[[i]], track[[i]]$monthyear)
  kde[[i]] <-map(list2[[i]], hr_kde, trast = trast[[i]], levels = c(0.95, 0.6))
  over[[i]]<-hr_overlap(kde[[i]], type="udoi", which="consecutive", conditional=TRUE )
  
}

rf
rf<-bind_rows(over)
rf$ID<-str_sub(rf$from, -3)
rf$start<-str_sub(rf$from, 5,6)
rf$end<-str_sub(rf$to, 5,6)
rf$Name<-rf$ID
df2<- data %>% distinct (Name, .keep_all = TRUE)
r2<-data.frame(rf)
dataset<-rf %>% left_join(df2, by="Name")
dataset$start<-as.numeric(dataset$start)
dataset$end<-as.numeric(dataset$end)
r3<-arrange(dataset, ID.x, levels, start)
head(r3)
r3<- r3 %>%
  mutate(subs=end-start)

r3%>% print(n=500)

r3<-r3[-c(15,18),]
nrow(r3)
inputdata<-r3
inputdata<-dataset
saveRDS(inputdata, "inputdata.RDS")
write.csv(inputdata, "overlap.csv")
dataset<-read.csv("overlap.csv")
over60<-dataset[(dataset$levels==0.60),]
over95<-dataset[(dataset$levels==0.95),]
over60<-r3[(r3$levels==0.60),]
over95<-r3[(r3$levels==0.95),]


over60<-r3[(r3$levels==0.60),]
over95<-r3[(r3$levels==0.95),]

head(over60)
range(over60$overlap)
over60[order(over60$overlap, decreasing =TRUE),]
over95[order(over95$overlap, decreasing =TRUE),]
over60 %>% print(n=500)
over60<-over60[!(over60$overlap>0.999),]
over95<-over95[!(over95$overlap>0.999),]

nrow(over60)
library(gamlss)
over60$overlap<-over60[!(over60$overlap>1.00),]
range(over60$overlap)
head(over60)



model<-betareg(overlaptrans~sex+habitat, data=over60, na.action=na.pass, link="logit", link.phi=NULL)
fit<-betareg(overlap~habitat, data=over95, na.action=na.pass, link="logit", link.phi=NULL)
emmeans(fit, pairwise~habitat, type="response")
emmip(fit, ~habitat, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")
m1<-gamlss(overlap~sex*habitat+UD60, data=over60, family=BEZI, trace=F)

summary(m1)
dredge(m1)
dredge(model)
summary(model)

over95 %>% 
  group_by(habitat, sex) %>%
  summarise(Mean95=mean(overlap),
          
  )%>% print(n=98)

range(over95$overlap)
model<-glmmTMB(overlap ~ sex*habitat +UD60 + (1|ID.x), ziformula = ~1,  over60, family = beta_family(),  na.action=na.pass)
dredge(model)
modelsec<-glmmTMB(overlap ~ habitat + (1|ID.x), ziformula = ~1,  over60, family = beta_family(),  na.action=na.pass)
emmeans(modelsec, pairwise~habitat, type="response")
emmip(model2, ~habitat, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")

model2<-glmmTMB(overlap ~ sex*habitat+UD95 + (1|ID.x), ziformula = ~1,  over95, family = beta_family(),  na.action=na.pass)
dredge(model2)
emmeans(model2, pairwise~sex, type="response")
emmip(model2, ~sex, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")
data1<-data[(data$Individual=="733.03.21"),]
data2<-data[(data$Individual=="733.04.21"),]
t1<-make_track(data, Easting, Northing, Timestamp, id=Individual, crs=('+proj=utm +zone=17N +datum=NAD83'))
t2<-make_track(data2, Easting, Northing, Timestamp, id=Individual)
trast<-make_trast(t1, res=100)
k1<-hr_kde(t1, trast=trast, levels=c(0.5, 0.95))
k2<-hr_kde(t2, trast=trast, levels=c(0.5, 0.95))
hr_overlap(k1,k2, type="vi", conditional=FALSE)
t1<-make_track(data3, Easting, Northing, Timestamp, id=ID)
t1
c<-hr_kde(t1, trast=trast, levels=c(0.95))
points(c$ud)

dat <- t1 %>% group_by(id) %>%
  mutate(month = lubridate::floor_date(t_, "month")) %>% 
  nest(data = -month) %>% 
  mutate(kde = map(data, hr_kde, trast = trast, levels = c(0.5, 0.95, 0.99)))
t1

dat <- data %>% 
  mutate(month = lubridate::floor_date(t_, "month")) %>% 
  nest(data = -month) %>% 
  mutate(kde = map(data, hr_kde, trast = trast, levels = c(0.5, 0.95, 0.99)))
dat <- t1 %>% 
  mutate(month = lubridate::floor_date(t_, "month")) %>% 
  nest(data = -month) %>% 
  mutate(kde = map(data, hr_kde, trast = trast, levels = c(0.5)))
dat


hr_overlap(dat$kde, type="vi", labels=dat$month)


