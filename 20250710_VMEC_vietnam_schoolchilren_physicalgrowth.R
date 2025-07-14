#' ---
#' title: "Exploratory analysis of Vinschool student data"
#' author: "Nhan Thi Ho"
#' date: '`r format(Sys.Date(), "%Y-%B-%d")`'
#' output: 
#'  html_document: 
#'   toc: yes
#'   toc_float: true
#' ---

#+ comment="",cache=FALSE,message=FALSE,warning=FALSE,echo=FALSE
rm(list=ls()) # clear all
library(knitr)

opts_chunk$set(comment="",cache=FALSE,message=FALSE,warning=FALSE,echo=FALSE)
library(plyr)
library(tidyverse)
library(rio)
library(arsenal)
library(stringr)
library(lubridate)
library(survminer)
library(survival)
library(gridExtra)
library(corrplot)
library(openxlsx)
library(Hmisc)
library(anthro)
library(hrbrthemes)
library(MASS)
library(gamlss)
library(lme4)
library(lmerTest)
library(ggplot2)
library(viridis)
library(confintr)
library(rcompanion)
library(DescTools)
library(RVAideMemoire) # test median trend with Mood's Median Test
library(geepack)
library(sjPlot)
library(emmeans)
library(DescTools)
library(ggpubr)
library(ggpmisc)
library(patchwork)


load("C:/Users/ADMIN/Desktop/vsc2020.2023/vschooldat.rda")

#' Summary of unique children
#' All sites
dati<-dat %>%
  group_by(hospitalcode,pid)%>%
  summarise(n = n())
nrow(dati)
# Number of visits
#table(dati$n)
#' Number of pid by number of visits
dati$n[dati$n>6]<-">6"
table(dati$n)
#' By hospital
table(dati$hospitalcode)
#' Number of visits by hospital
table(dati$hospitalcode,dati$n)


#' # Summary tables
my_controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3","meanCI","range","Nmiss2"),
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing",
    meanCI = "Mean (95%CI)"
  )
)
nvar<-c("weight","height","bmi")
svar<-c("age.cat","sex","bmi.cat","hospitalcode","year","age",nvar)
mylabels <-as.list(svar)
names(mylabels)<-svar
#' ## By gender
#' ### Overall
tabs <- tableby(as.formula(paste("sex",paste(svar[!svar %in% "sex"],collapse="+"),sep="~")),
                data = dat,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' ### Stratified by age.cat
tabs <- tableby(as.formula(paste("sex",paste(svar[!svar %in% "sex"],collapse="+"),sep="~")),
                data = dat,
                strata=age.cat,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' ### Stratified by hospitalcode
tabs <- tableby(as.formula(paste("sex",paste(svar[!svar %in% "sex"],collapse="+"),sep="~")),
                data = dat,
                strata=hospitalcode,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' ### Stratified by year
tabs <- tableby(as.formula(paste("sex",paste(svar[!svar %in% "sex"],collapse="+"),sep="~")),
                data = dat,
                strata=year,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))

#' ## By age category
#' ### Overall 
tabs <- tableby(as.formula(paste("age.cat",paste(svar[!svar %in% "age.cat"],collapse="+"),sep="~")),
                data = dat,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' ### Stratified by sex
tabs <- tableby(as.formula(paste("age.cat",paste(svar[!svar %in% "age.cat"],collapse="+"),sep="~")),
                data = dat,
                strata=sex,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' ### Stratified by hospitalcode
tabs <- tableby(as.formula(paste("age.cat",paste(svar[!svar %in% "age.cat"],collapse="+"),sep="~")),
                data = dat,
                strata=hospitalcode,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' ### Stratified by year
tabs <- tableby(as.formula(paste("age.cat",paste(svar[!svar %in% "age.cat"],collapse="+"),sep="~")),
                data = dat,
                strata=year,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))

#' ## By hospital 
#' ### Overall 
tabs <- tableby(as.formula(paste("hospitalcode",paste(svar[!svar %in% "hospitalcode"],collapse="+"),sep="~")),
                data = dat,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' Stratified by sex
tabs <- tableby(as.formula(paste("hospitalcode",paste(svar[!svar %in% "hospitalcode"],collapse="+"),sep="~")),
                data = dat,
                strata=sex,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' Stratified by age.cat
tabs <- tableby(as.formula(paste("hospitalcode",paste(svar[!svar %in% "hospitalcode"],collapse="+"),sep="~")),
                data = dat,
                strata=age.cat,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' Stratified by age.cat: males
dat.m<-subset(dat, sex %in% "Male")
tabs <- tableby(as.formula(paste("hospitalcode",paste(svar[!svar %in% "hospitalcode"],collapse="+"),sep="~")),
                data = dat.m,
                strata=age.cat,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' Stratified by age.cat: females
dat.f<-subset(dat, sex %in% "Female")
tabs <- tableby(as.formula(paste("hospitalcode",paste(svar[!svar %in% "hospitalcode"],collapse="+"),sep="~")),
                data = dat.f,
                strata=age.cat,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))


#' Stratified by year
tabs <- tableby(as.formula(paste("hospitalcode",paste(svar[!svar %in% "hospitalcode"],collapse="+"),sep="~")),
                data = dat,
                strata=year,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))


#' ## By yearvisit 
#' ### Overall
tabs <- tableby(as.formula(paste("year",paste(svar[!svar %in% "year"],collapse="+"),sep="~")),
                data = dat,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' ### Stratified by sex
tabs <- tableby(as.formula(paste("year",paste(svar[!svar %in% "year"],collapse="+"),sep="~")),
                data = dat,
                strata=sex,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' ### Stratified by hospitalcode
tabs <- tableby(as.formula(paste("year",paste(svar[!svar %in% "year"],collapse="+"),sep="~")),
                data = dat,
                strata=hospitalcode,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))
#' ### Stratified by age.cat
tabs <- tableby(as.formula(paste("year",paste(svar[!svar %in% "year"],collapse="+"),sep="~")),
                data = dat,
                strata=age.cat,
                control = my_controls) 
kable(summary(tabs,labelTranslations = mylabels, text=TRUE, pfootnote=TRUE))



#' # Difference of height, weight, BMI between calendar years and cities by age and gender
#get age 18 months to 18 years
dat118<-dat[dat$age>=1.5 &dat$age<18.5 &!is.na(dat$age),]
#only get first, last year for better visualization, comparison 
dat118.ymm<-dat[dat$age>=1.5 &dat$age<18.5 &!is.na(dat$age) &
                  dat$yearvisit %in% c("2019",max(dat$yearvisit)),]
# Define function to calculate IQR at given quantiles
iqr = function(z, lower = 0.25, upper = 0.75) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
medci<-function(z,ci=0.95){
  data.frame(
    y = median(z),
    ymin = MedianCI(z, conf.level = ci, na.rm=TRUE, method="exact")[2],
    ymax = MedianCI(z, conf.level = ci, na.rm=TRUE, method="exact")[3]
  )
}
  

#' ## Height 
#+ fig.width=8, fig.height=4
ggplot(dat118, aes(x=age.y, y=height, group=sex)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2) +
    theme_bw() +labs(x = "age (year)")

#' By sex and hospitalcode
ggplot(dat118, aes(x=age.y, y=height, color=sex, shape=hospitalcode, group=interaction(sex,hospitalcode))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+labs(x = "age (year)")
#' median iqr
ggplot(dat118, aes(x=age.y, y=height, color=sex, shape=hospitalcode, group=interaction(sex,hospitalcode))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+labs(x = "Age (year)")
#' median 95%CI
ags.h<-ggplot(dat118, aes(x=age.y, y=height, color=sex, shape=hospitalcode, group=interaction(sex,hospitalcode))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  ggtitle("a")+
  theme(plot.title = element_text(face = "bold"))+
  labs(x = "Age (year)", y="Height (cm)",shape="City", color = "Sex")+
  scale_shape_manual(values = c("HCP"=15,"HHN"=16,"HHP"=17),labels=c("HoChiMinh","HaNoi","HaiPhong"))
ags.h
#' By sex and year
ggplot(dat118, aes(x=age.y, y=height, color=sex, shape=year, group=interaction(sex,year))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+labs(x = "age (year)")
#' median iqr
ggplot(dat118, aes(x=age.y, y=height, color=sex, shape=year, group=interaction(sex,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+labs(x = "age (year)")
#' median 95%CI
ggplot(dat118, aes(x=age.y, y=height, color=sex, shape=year, group=interaction(sex,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+labs(x = "age (year)")
#' Height mean, error bar By hospitalcode, year, facet sex
ggplot(dat118, aes(x=age.y, y=height, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' Height median, iqr By hospitalcode, year, facet sex
ggplot(dat118, aes(x=age.y, y=height, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' Height median, 95%CI By hospitalcode, year, facet sex
ggplot(dat118, aes(x=age.y, y=height, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' Height median, iqr By hospitalcode, year first last, facet sex
ggplot(dat118.ymm, aes(x=age.y, y=height, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' Height median, 95%CI By hospitalcode, year first last, facet sex
ggplot(dat118.ymm, aes(x=age.y, y=height, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")

#' ### Calculate median, median difference and 95%CI
#' #### Male
#require(DescTools)
datme<-dat118[dat118$sex %in% "Male",]
datme$hospitalcode<-as.factor(datme$hospitalcode)
#' ##### Height Median, 95%CI for all males by age year
hk<-NULL
for (k in 2:18){
  h<-MedianCI(datme$height[datme$age.y %in% k],
              conf.level = 0.95, na.rm=TRUE, method="exact")
  hk<-rbind(hk,h)
  }
age<-as.character(2:18)
hk<-cbind(age, hk)
rownames(hk)<-NULL
hb<- cbind(age=age, median.ci=paste(hk[,2]," (",hk[,3],", ",hk[,4],")", sep=""))
kable(hb)

#' Median, 95%CI for groups 
hmci.male<-list()
hj<-list()
for (i in levels(datme$hospitalcode)){
  for (j in levels(datme$year)){
    hk<-NULL
    for (k in 2:18){
      h<-MedianCI(datme$height[datme$age.y %in% k &
                                  datme$year %in% j &
                                  datme$hospitalcode %in% i],
                   conf.level = 0.95, na.rm=TRUE, method="exact")
      hk<-rbind(hk,h)
    }
    rownames(hk)<-as.character(2:18)
    hj[[j]]<-hk
  }
  names(hj)<-levels(datme$year)
  hmci.male[[i]]<-hj
}
names(hmci.male)<-levels(datme$hospitalcode)
hmci.male

#' ##### Test for trend in median over calendar years
#' Mood's Median Test is a non-parametric test used to compare the medians of two or more independent groups. 
#' It is particularly useful when the data is ordinal, interval, or ratio and 
#' does not meet the assumptions of normality or homogeneity of variance.
mt<-NULL
for (k in 2:18){
  dath<-datme[datme$age.y %in% k,]
  m<-round(mood.medtest(height ~ yearvisit,data=dath)$p.value,5)
  mt<-rbind(mt,m)
}
age<-as.character(2:18)
colnames(mt)<-"trend.year.p"
mt<-cbind(age, mt)
kable(mt)

#' ##### Calculate median difference between first-last year, 95%CI for median difference
#' As in year 2018 there were too few students, so 2019 is chosen as first year. 
#require(confintr)
#'Two-sided 95% bootstrap confidence interval for the population value of
#'median(x)-median(y) based on 999 bootstrap replications and the bca method
 
#' All sites
datd<-datme
ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                  datd$year %in% max(datd$yearvisit)],
                    datd$height[datd$age.y %in% k &
                                  datd$year %in% "2019"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)

#' By site
hos<-c("HHN","HCP","HHP")
hmdci.male<-list()
for (i in hos){
  datd<-subset(datme,hospitalcode %in% i)
  ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
    hk<-NULL
    for (k in ageu){
      h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                 datd$year %in% max(datd$yearvisit) &
                                 datd$hospitalcode %in% i],
                        datd$height[datd$age.y %in% k &
                                       datd$year %in% "2019" &
                                       datd$hospitalcode %in% i],
                        probs = c(0.025, 0.975),
                        type = "bootstrap",
                        boot_type = "basic",  #intervals calculated using the basic bootstrap method
                        R = 999)
      hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
      hk<-rbind(hk,hd)
    }
    rownames(hk)<-as.character(ageu)
  hmdci.male[[i]]<-hk
}
names(hmdci.male)<-hos
hmdci.male

#' ##### Calculate difference between sites 
#' all years 
#' HHN vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHN"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHN<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHN"]))
ageu<-sort(aHCP[aHCP %in% aHHN])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$height[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHN"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' HHP vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHP"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHP"]))
ageu<-sort(aHCP[aHCP %in% aHHP])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$height[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHP"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)

#' Calculate difference between sites (HCP as ref) in first and last year 
#' HHN vs. HCP 
ymm<-c("2019", tail(levels(datme$year),1))
hmdci.cphn.male<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHN"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHN<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHN"]))
  ageu<-sort(aHCP[aHCP %in% aHHN])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$height[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHN"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  hmdci.cphn.male[[i]]<-hk
}
names(hmdci.cphn.male)<-ymm
hmdci.cphn.male
#' HHP vs. HCP 
hmdci.cphp.male<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHP"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHP"]))
  ageu<-sort(aHCP[aHCP %in% aHHP])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$height[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHP"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  hmdci.cphp.male[[i]]<-hk
}
names(hmdci.cphp.male)<-ymm
hmdci.cphp.male

#' #### Female
datme<-dat118[dat118$sex %in% "Female",]
datme$hospitalcode<-as.factor(datme$hospitalcode)
#' ##### Height Median, 95%CI for all females by age year
hk<-NULL
for (k in 2:18){
  h<-MedianCI(datme$height[datme$age.y %in% k],
              conf.level = 0.95, na.rm=TRUE, method="exact")
  hk<-rbind(hk,h)
}
age<-as.character(2:18)
hk<-cbind(age, hk)
rownames(hk)<-NULL
hb<- cbind(age=age, median.ci=paste(hk[,2]," (",hk[,3],", ",hk[,4],")", sep=""))
kable(hb)

#' Calculate median, 95%CI height by sites, years
hmci.female<-list()
hj<-list()
for (i in levels(datme$hospitalcode)){
  for (j in levels(datme$year)){
    hk<-NULL
    for (k in 2:18){
      h<-MedianCI(datme$height[datme$age.y %in% k &
                                 datme$year %in% j &
                                 datme$hospitalcode %in% i],
                  conf.level = 0.95, na.rm=TRUE, method="exact")
      hk<-rbind(hk,h)
    }
    rownames(hk)<-as.character(2:18)
    hj[[j]]<-hk
  }
  names(hj)<-levels(datme$year)
  hmci.female[[i]]<-hj
}
names(hmci.female)<-levels(datme$hospitalcode)
hmci.female
#' ##### Test for trend in median over calendar years
#' Mood's non-parametric Median Test  
mt<-NULL
for (k in 2:18){
  dath<-datme[datme$age.y %in% k,]
  m<-round(mood.medtest(height ~ yearvisit,data=dath)$p.value,5)
  mt<-rbind(mt,m)
}
age<-as.character(2:18)
colnames(mt)<-"trend.year.p"
mt<-cbind(age, mt)
kable(mt)

#' ##### Calculate median difference between first-last year, 95%CI for median difference
#' As in year 2018 there were too few students, so 2019 is chosen as first year. 
#require(confintr)
#'Two-sided 95% bootstrap confidence interval for the population value of
#'median(x)-median(y) based on 999 bootstrap replications and the bca method

#' All sites
datd<-datme
ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                  datd$year %in% max(datd$yearvisit)],
                    datd$height[datd$age.y %in% k &
                                  datd$year %in% "2019"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)

#' By sites
hos<-c("HHN","HCP","HHP")
hmdci.female<-list()
for (i in hos){
  datd<-subset(datme,hospitalcode %in% i)
  ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                     datd$year %in% max(datd$yearvisit) &
                                     datd$hospitalcode %in% i],
                      datd$height[datd$age.y %in% k &
                                     datd$year %in% "2019" &
                                     datd$hospitalcode %in% i],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  hmdci.female[[i]]<-hk
}
names(hmdci.female)<-hos
hmdci.female

#' ##### Calculate difference between sites 
#' All years 
#' HHN vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHN"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHN<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHN"]))
ageu<-sort(aHCP[aHCP %in% aHHN])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$height[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHN"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' HHP vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHP"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHP"]))
ageu<-sort(aHCP[aHCP %in% aHHP])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$height[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHP"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)

#' Calculate difference between sites (HCP as ref) in first and last year 
#' HHN vs. HCP 
ymm<-c("2019", tail(levels(datme$year),1))
hmdci.cphn.female<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHN"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHN<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHN"]))
  ageu<-sort(aHCP[aHCP %in% aHHN])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$height[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHN"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  hmdci.cphn.female[[i]]<-hk
}
names(hmdci.cphn.female)<-ymm
hmdci.cphn.female
#' HHP vs. HCP 
# remove age 17,18 due to missing values 
hmdci.cphp.female<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHP"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHP"]))
  ageu<-sort(aHCP[aHCP %in% aHHP])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$height[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$height[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHP"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  hmdci.cphp.female[[i]]<-hk
}
names(hmdci.cphp.female)<-ymm
hmdci.cphp.female
 

#' ## Weight 
#+ fig.width=8, fig.height=4
ggplot(dat118, aes(x=age.y, y=weight, group=sex)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2) +
  theme_bw()+labs(x = "age (year)")
#' By sex and hospitalcode
ggplot(dat118, aes(x=age.y, y=weight, color=sex, shape=hospitalcode, group=interaction(sex,hospitalcode))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+labs(x = "age (year)")
#' median iqr
ggplot(dat118, aes(x=age.y, y=weight, color=sex, shape=hospitalcode, group=interaction(sex,hospitalcode))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+labs(x = "age (year)")
#' median 95%ci
ags.w<-ggplot(dat118, aes(x=age.y, y=weight, color=sex, shape=hospitalcode, group=interaction(sex,hospitalcode))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  ggtitle("b")+
  theme(plot.title = element_text(face = "bold"))+
  labs(x = "Age (year)", y="Weight (kg)",shape="City", color = "Sex")+
  scale_shape_manual(values = c("HCP"=15,"HHN"=16,"HHP"=17),labels=c("HoChiMinh","HaNoi","HaiPhong"))
ags.w
#' By sex and year
ggplot(dat118, aes(x=age.y, y=weight, color=sex, shape=year, group=interaction(sex,year))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+labs(x = "age (year)")
#' median iqr
ggplot(dat118, aes(x=age.y, y=weight, color=sex, shape=year, group=interaction(sex,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+labs(x = "age (year)")
#' median 95%CI
ggplot(dat118, aes(x=age.y, y=weight, color=sex, shape=year, group=interaction(sex,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+labs(x = "age (year)")
#' Weight mean, error bar By hospitalcode, year, facet sex
ggplot(dat118, aes(x=age.y, y=weight, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' Weight mean, error bar By hospitalcode, year first last, facet sex
ggplot(dat118.ymm, aes(x=age.y, y=weight, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' Weight median, iqr By hospitalcode, year, facet sex
ggplot(dat118, aes(x=age.y, y=weight, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' Weight median, 95%CI By hospitalcode, year, facet sex
ggplot(dat118, aes(x=age.y, y=weight, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' Weight median, iqr By hospitalcode, year first last, facet sex
ggplot(dat118.ymm, aes(x=age.y, y=weight, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' Weight median, 95%CI By hospitalcode, year first last, facet sex
ggplot(dat118.ymm, aes(x=age.y, y=weight, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")

#' ### Calculate median, median difference and 95%CI
#' #### Male
#require(DescTools)
datme<-dat118[dat118$sex %in% "Male",]
datme$hospitalcode<-as.factor(datme$hospitalcode)
#' ##### Median, 95%CI for all males by age year
hk<-NULL
for (k in 2:18){
  h<-MedianCI(datme$weight[datme$age.y %in% k],
              conf.level = 0.95, na.rm=TRUE, method="exact")
  hk<-rbind(hk,h)
}
age<-as.character(2:18)
hk<-cbind(age, hk)
rownames(hk)<-NULL
hb<- cbind(age=age, median.ci=paste(hk[,2]," (",hk[,3],", ",hk[,4],")", sep=""))
kable(hb)

#' Median, 95%CI for groups 
wmci.male<-list()
hj<-list()
for (i in levels(datme$hospitalcode)){
  for (j in levels(datme$year)){
    hk<-NULL
    for (k in 2:18){
      h<-MedianCI(datme$weight[datme$age.y %in% k &
                                 datme$year %in% j &
                                 datme$hospitalcode %in% i],
                  conf.level = 0.95, na.rm=TRUE, method="exact")
      hk<-rbind(hk,h)
    }
    rownames(hk)<-as.character(2:18)
    hj[[j]]<-hk
  }
  names(hj)<-levels(datme$year)
  wmci.male[[i]]<-hj
}
names(wmci.male)<-levels(datme$hospitalcode)
wmci.male
#' ##### Test for trend in median over calendar years
#' Mood's non-parametric Median Test 
mt<-NULL
for (k in 2:18){
  dath<-datme[datme$age.y %in% k,]
  m<-round(mood.medtest(weight ~ yearvisit,data=dath)$p.value,5)
  mt<-rbind(mt,m)
}
age<-as.character(2:18)
colnames(mt)<-"trend.year.p"
mt<-cbind(age, mt)
kable(mt)
#' ##### Calculate median difference between first-last year, 95%CI for median difference
#' As in year 2018 there were too few students, so 2019 is chosen as first year. 
#require(confintr)
#'Two-sided 95% bootstrap confidence interval for the population value of
#'median(x)-median(y) based on 999 bootstrap replications and the bca method

#' All sites
datd<-datme
ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                  datd$year %in% max(datd$yearvisit)],
                    datd$weight[datd$age.y %in% k &
                                  datd$year %in% "2019"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)


#' By site
#' Calculate median difference between first-last year, 95%CI for median difference
#require(confintr)
# HHP does not have data for age 17,18 => only for HHN, HCP
#hos<-levels(datme$hospitalcode)
hos<-c("HHN","HCP","HHP")
wmdci.male<-list()
for (i in hos){
  datd<-subset(datme,hospitalcode %in% i)
  ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                     datd$year %in% max(datd$yearvisit) &
                                     datd$hospitalcode %in% i],
                      datd$weight[datd$age.y %in% k &
                                     datd$year %in% "2019" &
                                     datd$hospitalcode %in% i],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  wmdci.male[[i]]<-hk
}
names(wmdci.male)<-hos
wmdci.male

#' ##### Calculate difference between sites 
#' All years 
#' HHN vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHN"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHN<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHN"]))
ageu<-sort(aHCP[aHCP %in% aHHN])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$weight[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHN"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' HHP vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHP"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHP"]))
ageu<-sort(aHCP[aHCP %in% aHHP])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$weight[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHP"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)

#' Calculate difference between sites (HCP as ref) in first and last year 
#' HHN vs. HCP 
ymm<-c("2019", tail(levels(datme$year),1))
wmdci.cphn.male<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHN"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHN<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHN"]))
  ageu<-sort(aHCP[aHCP %in% aHHN])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$weight[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHN"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  wmdci.cphn.male[[i]]<-hk
}
names(wmdci.cphn.male)<-ymm
wmdci.cphn.male
#' HHP vs. HCP 
# remove age 17,18 due to missing values 
wmdci.cphp.male<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHP"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHP"]))
  ageu<-sort(aHCP[aHCP %in% aHHP])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$weight[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHP"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  wmdci.cphp.male[[i]]<-hk
}
names(wmdci.cphp.male)<-ymm
wmdci.cphp.male

#' #### Female
datme<-dat118[dat118$sex %in% "Female",]
datme$hospitalcode<-as.factor(datme$hospitalcode)
#' ##### Median, 95%CI for all females by age year
hk<-NULL
for (k in 2:18){
  h<-MedianCI(datme$weight[datme$age.y %in% k],
              conf.level = 0.95, na.rm=TRUE, method="exact")
  hk<-rbind(hk,h)
}
age<-as.character(2:18)
hk<-cbind(age, hk)
rownames(hk)<-NULL
hb<- cbind(age=age, median.ci=paste(hk[,2]," (",hk[,3],", ",hk[,4],")", sep=""))
kable(hb)

#' Calculate median, 95%CI by group
wmci.female<-list()
hj<-list()
for (i in levels(datme$hospitalcode)){
  for (j in levels(datme$year)){
    hk<-NULL
    for (k in 2:18){
      h<-MedianCI(datme$weight[datme$age.y %in% k &
                                 datme$year %in% j &
                                 datme$hospitalcode %in% i],
                  conf.level = 0.95, na.rm=TRUE, method="exact")
      hk<-rbind(hk,h)
    }
    rownames(hk)<-as.character(2:18)
    hj[[j]]<-hk
  }
  names(hj)<-levels(datme$year)
  wmci.female[[i]]<-hj
}
names(wmci.female)<-levels(datme$hospitalcode)
wmci.female
#' ##### Test for trend in median over calendar years
#' Mood's non-parametric Median Test 
mt<-NULL
for (k in 2:18){
  dath<-datme[datme$age.y %in% k,]
  m<-round(mood.medtest(weight ~ yearvisit,data=dath)$p.value,5)
  mt<-rbind(mt,m)
}
age<-as.character(2:18)
colnames(mt)<-"trend.year.p"
mt<-cbind(age, mt)
kable(mt)
#' ##### Calculate median difference between first-last year, 95%CI for median difference
#' As in year 2018 there were too few students, so 2019 is chosen as first year. 
#require(confintr)
#'Two-sided 95% bootstrap confidence interval for the population value of
#'median(x)-median(y) based on 999 bootstrap replications and the bca method

#' All sites
datd<-datme
ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                  datd$year %in% max(datd$yearvisit)],
                    datd$weight[datd$age.y %in% k &
                                  datd$year %in% "2019"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)

#' By site
#' Calculate median difference between first-last year, 95%CI for median difference
#require(confintr)
# HHP does not have data for age 17,18 => only for HHN, HCP
#hos<-levels(datme$hospitalcode)
hos<-c("HHN","HCP","HHP")
wmdci.female<-list()
for (i in hos){
  datd<-subset(datme,hospitalcode %in% i)
  ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                     datd$year %in% max(datd$yearvisit) &
                                     datd$hospitalcode %in% i],
                      datd$weight[datd$age.y %in% k &
                                     datd$year %in% "2019" &
                                     datd$hospitalcode %in% i],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  wmdci.female[[i]]<-hk
}
names(wmdci.female)<-hos
wmdci.female

#' ##### Calculate difference between sites 
#' All years 
#' HHN vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHN"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHN<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHN"]))
ageu<-sort(aHCP[aHCP %in% aHHN])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$weight[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHN"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' HHP vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHP"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHP"]))
ageu<-sort(aHCP[aHCP %in% aHHP])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$weight[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHP"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' Calculate difference between sites (HCP as ref) in first and last year 
#' HHN vs. HCP 
ymm<-c("2019", tail(levels(datme$year),1))
wmdci.cphn.female<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHN"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHN<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHN"]))
  ageu<-sort(aHCP[aHCP %in% aHHN])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$weight[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHN"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  wmdci.cphn.female[[i]]<-hk
}
names(wmdci.cphn.female)<-ymm
wmdci.cphn.female
#' HHP vs. HCP 
# remove age 17,18 due to missing values 
wmdci.cphp.female<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHP"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHP"]))
  ageu<-sort(aHCP[aHCP %in% aHHP])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$weight[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$weight[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHP"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  wmdci.cphp.female[[i]]<-hk
}
names(wmdci.cphp.female)<-ymm
wmdci.cphp.female


#' ## BMI 
#+ fig.width=8, fig.height=4
ggplot(dat118, aes(x=age.y, y=bmi, group=sex)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2) +
  theme_bw()+labs(x = "age (year)")
#' By sex and hospitalcode
ggplot(dat118, aes(x=age.y, y=bmi, color=sex, shape=hospitalcode, group=interaction(sex,hospitalcode))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+labs(x = "age (year)")
#' median iqr
ggplot(dat118, aes(x=age.y, y=bmi, color=sex, shape=hospitalcode, group=interaction(sex,hospitalcode))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+labs(x = "age (year)")
#' median 95%CI
ags.b<-ggplot(dat118, aes(x=age.y, y=bmi, color=sex, shape=hospitalcode, group=interaction(sex,hospitalcode))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  ggtitle("c")+
  theme(plot.title = element_text(face = "bold"))+
  labs(x="Age (year)",y="BMI (kg/m^2)",color="Sex",shape="City")+
  scale_shape_manual(values = c("HCP"=15,"HHN"=16,"HHP"=17),labels=c("HoChiMinh","HaNoi","HaiPhong"))
ags.b
#' By sex and year
ggplot(dat118, aes(x=age.y, y=bmi, color=sex, shape=year, group=interaction(sex,year))) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(linetype=year)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+labs(x = "age (year)")
#' median iqr
ggplot(dat118, aes(x=age.y, y=bmi, color=sex, shape=year, group=interaction(sex,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+labs(x = "age (year)")
#' median 95%CI
ggplot(dat118, aes(x=age.y, y=bmi, color=sex, shape=year, group=interaction(sex,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  labs(x = "age (year)")
#' BMI mean, error bar By hospitalcode, year, facet sex
ggplot(dat118, aes(x=age.y, y=bmi, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' BMI mean, error bar By hospitalcode, year first last, facet sex
ggplot(dat118.ymm, aes(x=age.y, y=bmi, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = mean,
    geom='line') +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' BMI median, iqr By hospitalcode, year, facet sex
ggplot(dat118, aes(x=age.y, y=bmi, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' BMI median, 95%CI By hospitalcode, year, facet sex
ggplot(dat118, aes(x=age.y, y=bmi, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' BMI median, iqr By hospitalcode, year first last, facet sex
ggplot(dat118.ymm, aes(x=age.y, y=bmi, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")
#' BMI median, 95%CI By hospitalcode, year first last, facet sex
ggplot(dat118.ymm, aes(x=age.y, y=bmi, color=hospitalcode, shape=year, group=interaction(hospitalcode,year))) +
  stat_summary(
    fun = median,
    geom='line') +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci)+
  theme_bw()+
  facet_wrap(~sex)+labs(x = "age (year)")

#' ### Calculate median, median difference and 95%CI
#' #### Male
#require(DescTools)
datme<-dat118[dat118$sex %in% "Male",]
datme$hospitalcode<-as.factor(datme$hospitalcode)
#' ##### BMI Median, 95%CI for all males by age year
hk<-NULL
for (k in 2:18){
  h<-MedianCI(datme$bmi[datme$age.y %in% k],
              conf.level = 0.95, na.rm=TRUE, method="exact")
  hk<-rbind(hk,h)
}
age<-as.character(2:18)
hk<-cbind(age, hk)
rownames(hk)<-NULL
hb<- cbind(age=age, median.ci=paste(hk[,2]," (",hk[,3],", ",hk[,4],")", sep=""))
kable(hb)

#' Median, 95%CI for groups 
bmci.male<-list()
hj<-list()
for (i in levels(datme$hospitalcode)){
  for (j in levels(datme$year)){
    hk<-NULL
    for (k in 2:18){
      h<-MedianCI(datme$bmi[datme$age.y %in% k &
                                 datme$year %in% j &
                                 datme$hospitalcode %in% i],
                  conf.level = 0.95, na.rm=TRUE, method="exact")
      hk<-rbind(hk,h)
    }
    rownames(hk)<-as.character(2:18)
    hj[[j]]<-hk
  }
  names(hj)<-levels(datme$year)
  bmci.male[[i]]<-hj
}
names(bmci.male)<-levels(datme$hospitalcode)
bmci.male
#' ##### Test for trend in median over calendar years
#' Mood's non-parametric Median Test 
mt<-NULL
for (k in 2:18){
  dath<-datme[datme$age.y %in% k,]
  m<-round(mood.medtest(bmi ~ yearvisit,data=dath)$p.value,5)
  mt<-rbind(mt,m)
}
age<-as.character(2:18)
colnames(mt)<-"trend.year.p"
mt<-cbind(age, mt)
kable(mt)
#' ##### Calculate median difference between first-last year, 95%CI for median difference
#' As in year 2018 there were too few students, so 2019 is chosen as first year. 
#require(confintr)
#'Two-sided 95% bootstrap confidence interval for the population value of
#'median(x)-median(y) based on 999 bootstrap replications and the bca method

#' All sites
datd<-datme
ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                  datd$year %in% max(datd$yearvisit)],
                    datd$bmi[datd$age.y %in% k &
                                  datd$year %in% "2019"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' By site
#' Calculate median difference between first-last year, 95%CI for median difference
#require(confintr)
# HHP does not have data for age 17,18 => only for HHN, HCP
#hos<-levels(datme$hospitalcode)
hos<-c("HHN","HCP","HHP")
bmdci.male<-list()
for (i in hos){
  datd<-subset(datme,hospitalcode %in% i)
  ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                     datd$year %in% max(datd$yearvisit) &
                                     datd$hospitalcode %in% i],
                      datd$bmi[datd$age.y %in% k &
                                     datd$year %in% "2019" &
                                     datd$hospitalcode %in% i],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  bmdci.male[[i]]<-hk
}
names(bmdci.male)<-hos
bmdci.male

#' ##### Calculate difference between sites 
#' All years 
#' HHN vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHN"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHN<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHN"]))
ageu<-sort(aHCP[aHCP %in% aHHN])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$bmi[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHN"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' HHP vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHP"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHP"]))
ageu<-sort(aHCP[aHCP %in% aHHP])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$bmi[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHP"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' Calculate difference between sites (HCP as ref) in first and last year 
#' HHN vs. HCP 
ymm<-c("2019", tail(levels(datme$year),1))
bmdci.cphn.male<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHN"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHN<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHN"]))
  ageu<-sort(aHCP[aHCP %in% aHHN])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$bmi[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHN"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  bmdci.cphn.male[[i]]<-hk
}
names(bmdci.cphn.male)<-ymm
bmdci.cphn.male
#' HHP vs. HCP 
# remove age 17,18 due to missing values 
bmdci.cphp.male<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHP"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHP"]))
  ageu<-sort(aHCP[aHCP %in% aHHP])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$bmi[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHP"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  bmdci.cphp.male[[i]]<-hk
}
names(bmdci.cphp.male)<-ymm
bmdci.cphp.male

#' #### Female
datme<-dat118[dat118$sex %in% "Female",]
datme$hospitalcode<-as.factor(datme$hospitalcode)
#' ##### BMI Median, 95%CI for all females by age year
hk<-NULL
for (k in 2:18){
  h<-MedianCI(datme$bmi[datme$age.y %in% k],
              conf.level = 0.95, na.rm=TRUE, method="exact")
  hk<-rbind(hk,h)
}
age<-as.character(2:18)
hk<-cbind(age, hk)
rownames(hk)<-NULL
hb<- cbind(age=age, median.ci=paste(hk[,2]," (",hk[,3],", ",hk[,4],")", sep=""))
kable(hb)

#' Calculate median, 95%CI 
bmci.female<-list()
hj<-list()
for (i in levels(datme$hospitalcode)){
  for (j in levels(datme$year)){
    hk<-NULL
    for (k in 2:18){
      h<-MedianCI(datme$bmi[datme$age.y %in% k &
                                 datme$year %in% j &
                                 datme$hospitalcode %in% i],
                  conf.level = 0.95, na.rm=TRUE, method="exact")
      hk<-rbind(hk,h)
    }
    rownames(hk)<-as.character(2:18)
    hj[[j]]<-hk
  }
  names(hj)<-levels(datme$year)
  bmci.female[[i]]<-hj
}
names(bmci.female)<-levels(datme$hospitalcode)
bmci.female
#' ##### Test for trend in median over calendar years
#' Mood's non-parametric Median Test 
mt<-NULL
for (k in 2:18){
  dath<-datme[datme$age.y %in% k,]
  m<-round(mood.medtest(bmi ~ yearvisit,data=dath)$p.value,5)
  mt<-rbind(mt,m)
}
age<-as.character(2:18)
colnames(mt)<-"trend.year.p"
mt<-cbind(age, mt)
kable(mt)
#' ##### Calculate median difference between first-last year, 95%CI for median difference
#' As in year 2018 there were too few students, so 2019 is chosen as first year. 
#require(confintr)
#'Two-sided 95% bootstrap confidence interval for the population value of
#'median(x)-median(y) based on 999 bootstrap replications and the bca method

#' All sites
datd<-datme
ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                               datd$year %in% max(datd$yearvisit)],
                    datd$bmi[datd$age.y %in% k &
                               datd$year %in% "2019"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' Calculate median difference between first-last year, 95%CI for median difference
#require(confintr)
# HHP does not have data for age 17,18 => only for HHN, HCP
#hos<-levels(datme$hospitalcode)
hos<-c("HHN","HCP","HHP")
bmdci.female<-list()
for (i in hos){
  datd<-subset(datme,hospitalcode %in% i)
  ageu<-sort(unique(datd$age.y[datd$year %in% "2019"]))
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                     datd$year %in% max(datd$yearvisit) &
                                     datd$hospitalcode %in% i],
                      datd$bmi[datd$age.y %in% k &
                                     datd$year %in% "2019" &
                                     datd$hospitalcode %in% i],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  bmdci.female[[i]]<-hk
}
names(bmdci.female)<-hos
bmdci.female

#' ##### Calculate difference between sites 
#' All years 
#' HHN vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHN"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHN<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHN"]))
ageu<-sort(aHCP[aHCP %in% aHHN])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$bmi[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHN"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' HHP vs. HCP 
datd<-subset(datme,hospitalcode %in% c("HCP","HHP"))
aHCP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HCP"]))
aHHP<-sort(unique(datd$age.y[datd$hospitalcode %in% "HHP"]))
ageu<-sort(aHCP[aHCP %in% aHHP])
hk<-NULL
for (k in ageu){
  h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HCP"],
                    datd$bmi[datd$age.y %in% k &
                                  datd$hospitalcode %in% "HHP"],
                    probs = c(0.025, 0.975),
                    type = "bootstrap",
                    boot_type = "basic",  #intervals calculated using the basic bootstrap method
                    R = 999)
  hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
  hk<-rbind(hk,hd)
}
hk<-round(hk,2)
rownames(hk)<-as.character(ageu)
hkb<- cbind(age=as.character(ageu), median.diff.ci=paste(hk[,1]," (",hk[,2],", ",hk[,3],")",sep=""))
kable(hkb)
#' Calculate difference between sites (HCP as ref) in first and last year 
#' HHN vs. HCP 
ymm<-c("2019", tail(levels(datme$year),1))
bmdci.cphn.female<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHN"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHN<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHN"]))
  ageu<-sort(aHCP[aHCP %in% aHHN])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$bmi[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHN"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  bmdci.cphn.female[[i]]<-hk
}
names(bmdci.cphn.female)<-ymm
bmdci.cphn.female
#' HHP vs. HCP 
# remove age 17,18 due to missing values 
bmdci.cphp.female<-list()
for (i in ymm){
  datd<-subset(datme,year %in% i & hospitalcode %in% c("HCP","HHP"))
  aHCP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HCP"]))
  aHHP<-sort(unique(datd$age.y[datd$year %in% i & datd$hospitalcode %in% "HHP"]))
  ageu<-sort(aHCP[aHCP %in% aHHP])
  hk<-NULL
  for (k in ageu){
    h<-ci_median_diff(datd$bmi[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HCP"],
                      datd$bmi[datd$age.y %in% k &
                                     datd$year %in% i &
                                     datd$hospitalcode %in% "HHP"],
                      probs = c(0.025, 0.975),
                      type = "bootstrap",
                      boot_type = "basic",  #intervals calculated using the basic bootstrap method
                      R = 999)
    hd<-cbind(median.diff=h$estimate,l2.5=h$interval[1],l97.5=h$interval[2])
    hk<-rbind(hk,hd)
  }
  rownames(hk)<-as.character(ageu)
  bmdci.cphp.female[[i]]<-hk
}
names(bmdci.cphp.female)<-ymm
bmdci.cphp.female

#' ## Combined plot for paper
#+ fig.width=6, fig.height=9
ags_hwb<-ggarrange(ags.h, ags.w, ags.b,nrow=3, common.legend = TRUE, legend="right")
ags_hwb 
#



#' # Longitudinal analysis: trend over calendar years
#' ## GEE model
#' ### Males
#' #### Height 
dat118.mi<-na.omit(dat118[,c("pid","yearvisit","sex","age.y","hospitalcode","height","weight","bmi")])
#' standardize yearvisit by minus 2018
dat118.mi$yearstd<-dat118.mi$yearvisit-2018
dat118.mi$age.y<-droplevels(dat118.mi$age.y)
dat118.m<-subset(dat118.mi, sex %in% "Male")
ghm<-geeglm(height ~ yearstd +age.y+age.y*yearstd, id=pid,data=dat118.m)
summary(ghm)
#' get slopes for trend over years
myModelSlopes <- lstrends(ghm, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.hm<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                            Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.hm)
gg.slc.hm <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.hm))
gg.slc.hm
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.hm<-plot_model(ghm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                   axis.title =c("Year","Height (cm)"),
                   title = "Males",
                   legend.title = "Age (year)",
                   axis.labels=as.character(seq(2018,2024,by=1)),
                   colors = as.character(seq(2,18,by=1)))
gee.hm

#' "unstructured" correlation 
ghm.u<-geeglm(height ~ yearstd +age.y+age.y*yearstd, id=pid, 
            corstr = "unstructured", data=dat118.m)
summary(ghm.u)
#' get slopes for trend over years
myModelSlopes <- lstrends(ghm.u, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.hm.u<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                              Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.hm.u)
gg.slc.hm.u <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.hm.u))
gg.slc.hm.u
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.hm.u<-plot_model(ghm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                     axis.title =c("Year","Height (cm)"),
                     title = "Males",
                     legend.title = "Age (year)",
                     axis.labels=as.character(seq(2018,2024,by=1)),
                     colors = as.character(seq(2,18,by=1)))
gee.hm.u
#' AR1
ghm.ar<-geeglm(height ~ yearstd +age.y+age.y*yearstd, id=pid, 
            corstr = "ar1", data=dat118.m)
summary(ghm.ar)
#' get slopes for trend over years
myModelSlopes <- lstrends(ghm.ar, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.hm.ar<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.hm.ar)
gg.slc.hm.ar <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.hm.ar))
gg.slc.hm.ar
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.hm.ar<-plot_model(ghm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","Height (cm)"),
                      title = "Males",
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.hm.ar
#' Exchangeable
ghm.ex<-geeglm(height ~ yearstd +age.y+age.y*yearstd, id=pid, 
            corstr = "exchangeable", data=dat118.m)
summary(ghm.ex)
#' get slopes for trend over years
myModelSlopes <- lstrends(ghm.ex, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.hm.ex<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.hm.ex)
gg.slc.hm.ex <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.hm.ex))
gg.slc.hm.ex
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.hm.ex<-plot_model(ghm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","Height (cm)"),
                      title = "Males",
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.hm.ex
#' Quasi Information Criterion
cor.str<-c("- Independence","- Unstructured","- Autoregressive_1","- Exchangeable")
ghm.qic<-QIC(ghm,ghm.u,ghm.ar,ghm.ex)
ghm.qic<-as.data.frame(round(ghm.qic,0))
ghm.qic<-cbind(Correlation_Structure=cor.str,ghm.qic)
gn<-c("Height_Males", rep("",6))
names(gn)<-colnames(ghm.qic)
ghm.qic<-rbind(gn,ghm.qic)
rownames(ghm.qic)<-NULL
kable(ghm.qic)

#' #### Weight 
gwm<-geeglm(weight ~ yearstd +age.y+age.y*yearstd, id=pid,data=dat118.m)
summary(gwm)
#' get slopes for trend over years
myModelSlopes <- lstrends(gwm, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.wm<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                            Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.wm)
gg.slc.wm <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.wm))
gg.slc.wm
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.wm<-plot_model(gwm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                   axis.title =c("Year","Weight (kg)"),
                   title = "Males",
                   legend.title = "Age (year)",
                   axis.labels=as.character(seq(2018,2024,by=1)),
                   colors = as.character(seq(2,18,by=1)))
gee.wm

#' Unstructured correlation
gwm.u<-geeglm(weight ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "unstructured", data=dat118.m)
summary(gwm.u)
#' get slopes for trend over years
myModelSlopes <- lstrends(gwm.u, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.wm.u<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                              Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.wm.u)
gg.slc.wm.u <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.wm.u))
gg.slc.wm.u
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.wm.u<-plot_model(gwm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                     axis.title =c("Year","Weight (kg)"),
                     title = "Males",
                     legend.title = "Age (year)",
                     axis.labels=as.character(seq(2018,2024,by=1)),
                     colors = as.character(seq(2,18,by=1)))
gee.wm.u
#' AR1
gwm.ar<-geeglm(weight ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "ar1", data=dat118.m)
summary(gwm.ar)
#' get slopes for trend over years
myModelSlopes <- lstrends(gwm.ar, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.wm.ar<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.wm.ar)
gg.slc.wm.ar <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.wm.ar))
gg.slc.wm.ar
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.wm.ar<-plot_model(gwm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","Weight (kg)"),
                      title = "Males",
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.wm.ar
#' Exchangeable
gwm.ex<-geeglm(weight ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "exchangeable", data=dat118.m)
summary(gwm.ex)
#' get slopes for trend over years
myModelSlopes <- lstrends(gwm.ex, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.wm.ex<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.wm.ex)
gg.slc.wm.ex <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.wm.ex))
gg.slc.wm.ex
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.wm.ex<-plot_model(gwm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","Weight (kg)"),
                      title = "Males",
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.wm.ex
#' Quasi Information Criterion
gwm.qic<-QIC(gwm,gwm.u,gwm.ar,gwm.ex)
gwm.qic<-as.data.frame(round(gwm.qic,0))
gwm.qic<-cbind(Correlation_Structure=cor.str,gwm.qic)
gn<-c("Weight_Males", rep("",6))
names(gn)<-colnames(gwm.qic)
gwm.qic<-rbind(gn,gwm.qic)
rownames(gwm.qic)<-NULL
kable(gwm.qic)

#' #### BMI 
gbm<-geeglm(bmi ~ yearstd +age.y+age.y*yearstd, id=pid,data=dat118.m)
summary(gbm)
#' get slopes for trend over years
myModelSlopes <- lstrends(gbm, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.bm<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                            Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.bm)
gg.slc.bm <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.bm))
gg.slc.bm
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.bm<-plot_model(gbm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                   axis.title =c("Year","BMI (kg/m^2)"),
                   title = "Males",
                   ylim=c(5,50),
                   legend.title = "Age (year)",
                   axis.labels=as.character(seq(2018,2024,by=1)),
                   colors = as.character(seq(2,18,by=1)))
gee.bm

#' Unstructured correlation
gbm.u<-geeglm(bmi ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "unstructured", data=dat118.m)
summary(gbm.u)
#' get slopes for trend over years
myModelSlopes <- lstrends(gbm.u, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.bm.u<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                              Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.bm.u)
gg.slc.bm.u <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.bm.u))
gg.slc.bm.u
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.bm.u<-plot_model(gbm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                     axis.title =c("Year","BMI (kg/m^2)"),
                     title = "Males",
                     ylim=c(5,50),
                     legend.title = "Age (year)",
                     axis.labels=as.character(seq(2018,2024,by=1)),
                     colors = as.character(seq(2,18,by=1)))
gee.bm.u
#' AR1
gbm.ar<-geeglm(bmi ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "ar1", data=dat118.m)
summary(gbm.ar)
#' get slopes for trend over years
myModelSlopes <- lstrends(gbm.ar, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.bm.ar<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.bm.ar)
gg.slc.bm.ar <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.bm.ar))
gg.slc.bm.ar
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.bm.ar<-plot_model(gbm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","BMI (kg/m^2)"),
                      title = "Males",
                      ylim=c(5,50),
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.bm.ar
#' Exchangeable
gbm.ex<-geeglm(bmi ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "exchangeable", data=dat118.m)
summary(gbm.ex)
#' get slopes for trend over years
myModelSlopes <- lstrends(gbm.ex, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.bm.ex<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.bm.ex)
gg.slc.bm.ex <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.bm.ex))
gg.slc.bm.ex
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.bm.ex<-plot_model(gbm, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","BMI (kg/m^2)"),
                      title = "Males",
                      ylim=c(5,50),
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.bm.ex
#' Quasi Information Criterion
gbm.qic<-QIC(gbm,gbm.u,gbm.ar,gbm.ex)
gbm.qic<-as.data.frame(round(gbm.qic,0))
gbm.qic<-cbind(Correlation_Structure=cor.str,gbm.qic)
gn<-c("BMI_Males", rep("",6))
names(gn)<-colnames(gbm.qic)
gbm.qic<-rbind(gn,gbm.qic)
rownames(gbm.qic)<-NULL
kable(gbm.qic)


#' ### Females
dat118.f<-subset(dat118.mi, sex %in% "Female")
#' #### Height 
ghf<-geeglm(height ~ yearstd +age.y+age.y*yearstd, id=pid,data=dat118.f)
summary(ghf)
#' get slopes for trend over years
myModelSlopes <- lstrends(ghf, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.hf<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                            Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.hf)
gg.slc.hf <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.hf))
gg.slc.hf
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.hf<-plot_model(ghf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                   axis.title =c("Year","Height (cm)"),
                   title = "Females",
                   legend.title = "Age (year)",
                   axis.labels=as.character(seq(2018,2024,by=1)),
                   colors = as.character(seq(2,18,by=1)))
gee.hf

#' Unstructured correlation 
ghf.u<-geeglm(height ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "unstructured", data=dat118.f)
summary(ghf.u)
#' get slopes for trend over years
myModelSlopes <- lstrends(ghf.u, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.hf.u<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                              Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.hf.u)
gg.slc.hf.u <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.hf.u))
gg.slc.hf.u
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.hf.u<-plot_model(ghf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                     axis.title =c("Year","Height (cm)"),
                     title = "Females",
                     legend.title = "Age (year)",
                     axis.labels=as.character(seq(2018,2024,by=1)),
                     colors = as.character(seq(2,18,by=1)))
gee.hf.u
#' AR1
ghf.ar<-geeglm(height ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "ar1", data=dat118.f)
summary(ghf.ar)
#' get slopes for trend over years
myModelSlopes <- lstrends(ghf.ar, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.hf.ar<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.hf.ar)
gg.slc.hf.ar <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.hf.ar))
gg.slc.hf.ar
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.hf.ar<-plot_model(ghf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","Height (cm)"),
                      title = "Females",
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.hf.ar
#' Exchangeble 
ghf.ex<-geeglm(height ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "exchangeable", data=dat118.f)
summary(ghf.ex)
#' get slopes for trend over years
myModelSlopes <- lstrends(ghf.ex, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.hf.ex<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.hf.ex)
gg.slc.hf.ex <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.hf.ex))
gg.slc.hf.ex
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.hf.ex<-plot_model(ghf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","Height (cm)"),
                      title = "Females",
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.hf.ex
#' Quasi Information Criterion
ghf.qic<-QIC(ghf,ghf.u,ghf.ar,ghf.ex)
ghf.qic<-as.data.frame(round(ghf.qic,0))
ghf.qic<-cbind(Correlation_Structure=cor.str,ghf.qic)
gn<-c("Height_Females", rep("",6))
names(gn)<-colnames(ghf.qic)
ghf.qic<-rbind(gn,ghf.qic)
rownames(ghf.qic)<-NULL
kable(ghf.qic)


#' #### Weight 
gwf<-geeglm(weight ~ yearstd +age.y+age.y*yearstd, id=pid,data=dat118.f)
summary(gwf)
#' get slopes for trend over years
myModelSlopes <- lstrends(gwf, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.wf<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                            Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.wf)
gg.slc.wf <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.wf))
gg.slc.wf
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.wf<-plot_model(gwf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                   axis.title =c("Year","Weight (kg)"),
                   title = "Females",
                   legend.title = "Age (year)",
                   axis.labels=as.character(seq(2018,2024,by=1)),
                   colors = as.character(seq(2,18,by=1)))
gee.wf

#' Unstructured correlation
gwf.u<-geeglm(weight ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "unstructured", data=dat118.f)
summary(gwf.u)
#' get slopes for trend over years
myModelSlopes <- lstrends(gwf.u, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.wf.u<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                              Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.wf.u)
gg.slc.wf.u <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.wf.u))
gg.slc.wf.u
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.wf.u<-plot_model(gwf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                     axis.title =c("Year","Weight (kg)"),
                     title = "Females",
                     legend.title = "Age (year)",
                     axis.labels=as.character(seq(2018,2024,by=1)),
                     colors = as.character(seq(2,18,by=1)))
gee.wf.u
#' AR1
gwf.ar<-geeglm(weight ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "ar1", data=dat118.f)
summary(gwf.ar)
#' get slopes for trend over years
myModelSlopes <- lstrends(gwf.ar, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.wf.ar<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.wf.ar)
gg.slc.wf.ar <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.wf.ar))
gg.slc.wf.ar
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.wf.ar<-plot_model(gwf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","Weight (kg)"),
                      title = "Females",
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.wf.ar
#' exchangeble
gwf.ex<-geeglm(weight ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "exchangeable", data=dat118.f)
summary(gwf.ex)
#' get slopes for trend over years
myModelSlopes <- lstrends(gwf.ex, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.wf.ex<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.wf.ex)
gg.slc.wf.ex <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.wf.ex))
gg.slc.wf.ex
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.wf.ex<-plot_model(gwf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","Weight (kg)"),
                      title = "Females",
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.wf.ex
#' Quasi Information Criterion
gwf.qic<-QIC(gwf,gwf.u,gwf.ar,gwf.ex)
gwf.qic<-as.data.frame(round(gwf.qic,0))
gwf.qic<-cbind(Correlation_Structure=cor.str,gwf.qic)
gn<-c("Weight_Females", rep("",6))
names(gn)<-colnames(gwf.qic)
gwf.qic<-rbind(gn,gwf.qic)
rownames(gwf.qic)<-NULL
kable(gwf.qic)


#' #### BMI 
gbf<-geeglm(bmi ~ yearstd +age.y+age.y*yearstd, id=pid,data=dat118.f)
summary(gbf)
#' get slopes for trend over years
myModelSlopes <- lstrends(gbf, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.bf<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                            Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.bf)
gg.slc.bf <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.bf))
gg.slc.bf
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.bf<-plot_model(gbf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                   axis.title =c("Year","BMI (kg/m^2)"),
                   title = "Females",
                   p.val = TRUE,
                   ylim=c(5,50),
                   legend.title = "Age (year)",
                   axis.labels=as.character(seq(2018,2024,by=1)),
                   colors = as.character(seq(2,18,by=1)))
gee.bf

#' Unstructured correlation
gbf.u<-geeglm(bmi ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "unstructured", data=dat118.f)
summary(gbf.u)
#' get slopes for trend over years
myModelSlopes <- lstrends(gbf.u, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.bf.u<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                              Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.bf.u)
gg.slc.bf.u <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.bf.u))
gg.slc.bf.u
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.bf.u<-plot_model(gbf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                     axis.title =c("Year","BMI (kg/m^2)"),
                     title = "Females",
                     p.val = TRUE,
                     ylim=c(5,50),
                     legend.title = "Age (year)",
                     axis.labels=as.character(seq(2018,2024,by=1)),
                     colors = as.character(seq(2,18,by=1)))
gee.bf.u

#' AR1 correlation
gbf.ar<-geeglm(bmi ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "ar1", data=dat118.f)
summary(gbf.ar)
#' get slopes for trend over years
myModelSlopes <- lstrends(gbf.ar, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.bf.ar<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.bf.ar)
gg.slc.bf.ar <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.bf.ar))
gg.slc.bf.ar
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.bf.ar<-plot_model(gbf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","BMI (kg/m^2)"),
                      title = "Females",
                      p.val = TRUE,
                      ylim=c(5,50),
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.bf.ar
#' Exchangeable
gbf.ex<-geeglm(bmi ~ yearstd +age.y+age.y*yearstd, id=pid,
            corstr = "exchangeable", data=dat118.f)
summary(gbf.ex)
#' get slopes for trend over years
myModelSlopes <- lstrends(gbf.ex, "age.y", var="yearstd")
kable(myModelSlopes) 
sld<-as.data.frame(myModelSlopes)
sld[,c(2,3,5,6)]<-round(sld[,c(2,3,5,6)],2)
slc.bf.ex<-as.data.frame(cbind(Age=as.character(sld[,"age.y"]),
                               Slope.95CI=paste(sld[,"yearstd.trend"], " (",sld[,"lower.CL"],"; ",sld[,"upper.CL"],")",sep="")))
kable(slc.bf.ex)
gg.slc.bf.ex <- ggplot() +                            
  theme_void() +
  annotate(geom = "table",
           x = 1,
           y = 1,
           label = list(slc.bf.ex))
gg.slc.bf.ex
#' Plot trend slopes
#+ fig.width=4, fig.height=7 
gee.bf.ex<-plot_model(gbf, type="int",show.data=T,show.values = TRUE,show.p = TRUE,
                      axis.title =c("Year","BMI (kg/m^2)"),
                      title = "Females",
                      p.val = TRUE,
                      ylim=c(5,50),
                      legend.title = "Age (year)",
                      axis.labels=as.character(seq(2018,2024,by=1)),
                      colors = as.character(seq(2,18,by=1)))
gee.bf.ex
#' Quasi Information Criterion
gbf.qic<-QIC(gbf,gbf.u,gbf.ar,gbf.ex)
gbf.qic<-as.data.frame(round(gbf.qic,0))
gbf.qic<-cbind(Correlation_Structure=cor.str,gbf.qic)
gn<-c("BMI_Females", rep("",6))
names(gn)<-colnames(gbf.qic)
gbf.qic<-rbind(gn,gbf.qic)
rownames(gbf.qic)<-NULL
kable(gbf.qic)


#' ### Combined QIC
#' Correlation structure for GEE models was selected based on the smallest Quasi Information Criterion (QIC). 
#' For height, weight, BMI of both males and females, "independent" correlation structure had the smallest QIC and thus was selected. 
gall.qic<-rbind(ghm.qic,gwm.qic,gbm.qic,ghf.qic,gwf.qic,gbf.qic)
kable(gall.qic)



#' ### Combined plot for paper
#+ fig.width=8, fig.height=11
gee_hwb<-ggarrange(gee.hm, gg.slc.hm, gee.hf, gg.slc.hf,
                   gee.wm, gg.slc.wm, gee.wf, gg.slc.wf,
                   gee.bm, gg.slc.bm, gee.bf, gg.slc.bf,
                   ncol=4, nrow=3, common.legend = TRUE, legend="right")
gee_hwb


#' Unstructured correlation
#+ fig.width=8, fig.height=11
gee_hwb_u<-ggarrange(gee.hm.u, gg.slc.hm.u, gee.hf.u, gg.slc.hf.u,
                     gee.wm.u, gg.slc.wm.u, gee.wf.u, gg.slc.wf.u,
                     gee.bm.u, gg.slc.bm.u, gee.bf.u, gg.slc.bf.u,
                     ncol=4, nrow=3, common.legend = TRUE, legend="right")
gee_hwb_u


#' AR1 correlation
#+ fig.width=8, fig.height=11
gee_hwb_ar<-ggarrange(gee.hm.ar, gg.slc.hm.ar, gee.hf.ar, gg.slc.hf.ar,
                      gee.wm.ar, gg.slc.wm.ar, gee.wf.ar, gg.slc.wf.ar,
                      gee.bm.ar, gg.slc.bm.ar, gee.bf.ar, gg.slc.bf.ar,
                      ncol=4, nrow=3, common.legend = TRUE, legend="right")
gee_hwb_ar


#' Exchangeable correlation
#+ fig.width=8, fig.height=11
gee_hwb_ex<-ggarrange(gee.hm.ex, gg.slc.hm.ex, gee.hf.ex, gg.slc.hf.ex,
                      gee.wm.ex, gg.slc.wm.ex, gee.wf.ex, gg.slc.wf.ex,
                      gee.bm.ex, gg.slc.bm.ex, gee.bf.ex, gg.slc.bf.ex,
                      ncol=4, nrow=3, common.legend = TRUE, legend="right")
gee_hwb_ex



#' ## Plot over calendar year by age group, gender
#' ### Height 
#' Height (mean, error bar)
#+ fig.width=9, fig.height=6
dat.t<-dat118[!is.na(dat118$age.y),]
ggplot(dat.t, aes(x=yearvisit, y=height, group=sex, color=sex)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+labs(x = "Year")
#' Height (median, IQR)
#+ fig.width=9, fig.height=6
ggplot(dat.t, aes(x=yearvisit, y=height, group=sex, color=sex)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = iqr)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6) +labs(x = "Year")
#' Height (median, 95%CI)
#+ fig.width=9, fig.height=6
ggplot(dat.t, aes(x=yearvisit, y=height, group=sex, color=sex)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = medci)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6) +labs(x = "Year")
#stat_summary(
#  fun.data=iqr,
#  fun.args = list(lower = 0.1, upper = 0.90))

#' ### Height (median, IQR) by sex, sites
#' Male 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Male")
ggplot(dat.ts, aes(x=yearvisit, y=height, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = iqr)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Male")+labs(x = "Year")

#' Female 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Female")
ggplot(dat.ts, aes(x=yearvisit, y=height, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = iqr)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Female")+labs(x = "Year")
#' ### Height (median, 95%CI) by sex, sites
#' Male 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Male")
ggplot(dat.ts, aes(x=yearvisit, y=height, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = medci)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Male")+labs(x = "Year")

#' Female 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Female")
ggplot(dat.ts, aes(x=yearvisit, y=height, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = medci)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Female")+labs(x = "Year")

#' ### Weight 
#' Weight (mean, error bar)
#+ fig.width=9, fig.height=6
ggplot(dat.t, aes(x=yearvisit, y=weight, group=sex, color=sex)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+labs(x = "Year")
#' Weight (median, IQR)
#+ fig.width=9, fig.height=6
ggplot(dat.t, aes(x=yearvisit, y=weight, group=sex, color=sex)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+labs(x = "Year")
#' Weight (median, 95%CI)
#+ fig.width=9, fig.height=6
ggplot(dat.t, aes(x=yearvisit, y=weight, group=sex, color=sex)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+labs(x = "Year")

#' ### Weight (median, IQR) by sex, sites
#' Male 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Male")
ggplot(dat.ts, aes(x=yearvisit, y=weight, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = iqr)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Male")+labs(x = "Year")

#' Female 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Female")
ggplot(dat.ts, aes(x=yearvisit, y=weight, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = iqr)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Female")+labs(x = "Year")
#' ### Weight (median, 95%CI) by sex, sites
#' Male 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Male")
ggplot(dat.ts, aes(x=yearvisit, y=weight, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = medci)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Male")+labs(x = "Year")

#' Female 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Female")
ggplot(dat.ts, aes(x=yearvisit, y=weight, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = medci)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Female")+labs(x = "Year")

#' ### BMI 
#' BMI (mean, error bar)
#+ fig.width=9, fig.height=6
ggplot(dat.t, aes(x=yearvisit, y=bmi, group=sex, color=sex)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+labs(x = "Year")
#' BMI (median, IQR)
#+ fig.width=9, fig.height=6
ggplot(dat.t, aes(x=yearvisit, y=bmi, group=sex, color=sex)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=iqr) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+labs(x = "Year")
#' BMI (median, 95%CI)
#+ fig.width=9, fig.height=6
ggplot(dat.t, aes(x=yearvisit, y=bmi, group=sex, color=sex)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+labs(x = "Year")
#' ### BMI (median, IQR) by sex, sites
#' Male 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Male")
ggplot(dat.ts, aes(x=yearvisit, y=bmi, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = iqr)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Male")+labs(x = "Year")

#' Female 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Female")
ggplot(dat.ts, aes(x=yearvisit, y=bmi, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = iqr)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Female")+labs(x = "Year")

#' ### BMI (median, 95%CI) by sex, sites
#' Male 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Male")
ggplot(dat.ts, aes(x=yearvisit, y=bmi, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = medci)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Male")+labs(x = "Year")

#' Female 
#'  
#+ fig.width=9, fig.height=6
dat.ts<-subset(dat.t,sex %in% "Female")
ggplot(dat.ts, aes(x=yearvisit, y=bmi, color=hospitalcode)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=hospitalcode)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(fun.data = medci)+
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.y, ncol=6)+
  ggtitle("Female")+labs(x = "Year")


#' # Growth curve over 4 years by gender for each age in starting year 2020   
# get unique PID for those with 3 or 4 years from 2020 
#get only those with health check data in 2020 and >=3 visits
dat.g<-dat118[!is.na(dat118$age.start),]
dat.g$age.start<-factor(dat.g$age.start, levels=c(min(dat.g$age.start):max(dat.g$age.start)))
dat.g<-dat.g %>%
  group_by(pid)%>%
  filter(min(yearvisit)==2020 & n()>=3)

#' ## Height
#' Height Mean, SE 
#+ fig.width=9, fig.height=6
ggplot(dat.g, aes(x=yearvisit, y=height, group=sex, color=sex)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.start, ncol=5)+labs(x = "Year")
#' Height Median, 95%CI 
#+ fig.width=9, fig.height=6
ggplot(dat.g, aes(x=yearvisit, y=height, group=sex, color=sex)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci,
    geom='errorbar',
    width=0.2) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.start, ncol=5)+labs(x = "Year")
#' ## weight 
#' weight Mean, SE
#+ fig.width=9, fig.height=6
ggplot(dat.g, aes(x=yearvisit, y=weight, group=sex, color=sex)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.start, ncol=5)+labs(x = "Year")
#' weight Median, 95%CI
#+ fig.width=9, fig.height=6
ggplot(dat.g, aes(x=yearvisit, y=weight, group=sex, color=sex)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci,
    geom='errorbar',
    width=0.2) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.start, ncol=5)+labs(x = "Year")
#' ## BMI 
#' BMI Mean, SE
#+ fig.width=9, fig.height=6
ggplot(dat.g, aes(x=yearvisit, y=bmi, group=sex, color=sex)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=mean,
    geom='point') +
  stat_summary(
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.start, ncol=5)+labs(x = "Year")
#' BMI Median, 95%CI
#+ fig.width=9, fig.height=6
ggplot(dat.g, aes(x=yearvisit, y=bmi, group=sex, color=sex)) +
  stat_summary(
    fun = median,
    geom='line',
    aes(color=sex)) +
  stat_summary(
    fun=median,
    geom='point') +
  stat_summary(
    fun.data=medci,
    geom='errorbar',
    width=0.2) +
  theme_bw()+
  guides(x =  guide_axis(angle = 45))+
  facet_wrap(~age.start, ncol=5)+labs(x = "Year")


#' # Comparing with WHO standard 
#get age 1 to 18 (18 as upper limit)
dat118<-dat[dat$age>=1.5 &dat$age<=18 &!is.na(dat$age),]
colors<-c("Vschool mean" = "red",
          "WHO mean"="green",
          "+4SD"="blue",
          "+3SD"="blue",
          "+2SD"="blue",
          "+1SD"="blue",
          "-1SD"="purple",
          "-2SD"="purple",
          "-3SD"="purple",
          "-4SD"="purple")
colors2<-c("Vschool median" = "red",
          "WHO median"="green",
          "+4SD"="blue",
          "+3SD"="blue",
          "+2SD"="blue",
          "+1SD"="blue",
          "-1SD"="purple",
          "-2SD"="purple",
          "-3SD"="purple",
          "-4SD"="purple")
hl<-c("height >+4SD",
      "+3SD< height <=+4SD",
      "+2SD< height <=+3SD",
      "+1SD< height <=+2SD",
      "-1SD<= height <=+1SD",
      "-2SD<= height <-1SD",
      "-3SD<= height <-2SD",
      "-4SD<= height <-3SD",
      "height <-4SD")
wl<-c("weight >+4SD",
      "+3SD< weight <=+4SD",
      "+2SD< weight <=+3SD",
      "+1SD< weight <=+2SD",
      "-1SD<= weight <=+1SD",
      "-2SD<= weight <-1SD",
      "-3SD<= weight <-2SD",
      "-4SD<= weight <-3SD",
      "weight <-4SD")
bl<-c("BMI >+4SD",
      "+3SD< BMI <=+4SD",
      "+2SD< BMI <=+3SD",
      "+1SD< BMI <=+2SD",
      "-1SD<= BMI <=+1SD",
      "-2SD<= BMI <-1SD",
      "-3SD<= BMI <-2SD",
      "-4SD<= BMI <-3SD",
      "BMI <-4SD")
whl<-c("WFH >+4SD",
       "+3SD< WFH <=+4SD",
       "+2SD< WFH <=+3SD",
       "+1SD< WFH <=+2SD",
       "-1SD<= WFH <=+1SD",
       "-2SD<= WFH <-1SD",
       "-3SD<= WFH <-2SD",
       "-4SD<= WFH <-3SD",
       "WFH <-4SD")
stl<-c("Extreme tallness","Normal HFA","Stunting")
w5l<-c("Obesity","Overweight","Normal WFH","Wasting")
t18l<-c("Obesity","Overweight","Normal BMI","Thinness")
#' ## Age <=5 years (by month) 
#' ### Boys 
dat05b<-dat118[dat118$age<=5 & dat118$sex %in% "Male",]
#' #### Height
#' Mean, SE
#+ fig.width=8, fig.height=4
hfa.boy.z.05<-who5$`lhfa-boys-zscore-expanded-tables`
hfa.boy.z.05$age.m<-round(hfa.boy.z.05$day/30,0)
hfa.boy.z.05.m<-hfa.boy.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat05b, mapping=aes(x=age.m, y=height), size=0.1)+
  stat_summary(dat05b, mapping=aes(x=age.m, y=height, color="Vschool mean"),
    fun = mean,
    geom='line') +
  stat_summary(dat05b, mapping=aes(x=age.m, y=height),
    fun=mean,
    geom='point', colour="red") +
  stat_summary(dat05b, mapping=aes(x=age.m, y=height),
    fun.data=mean_cl_boot,
    geom='errorbar',
    width=0.2, colour="red") +
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(30,150))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Males,18 months to 5 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
hfa.boy.z.05.m<-hfa.boy.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.5hm<-ggplot() +
  geom_point(dat05b, mapping=aes(x=age.m, y=height), size=0.1)+
  stat_summary(dat05b, mapping=aes(x=age.m, y=height, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat05b, mapping=aes(x=age.m, y=height),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat05b, mapping=aes(x=age.m, y=height),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(hfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(30,150))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Males,18 months to 5 years old")
who.5hm
#' ##### Classification of Vinschool student as compared to WHO reference 
dat05b.h<-na.omit(dat05b[, c("pid","height","age.m","year","hospitalcode")])
# merge with WHO data
dat05b.h.o<-merge(dat05b.h, hfa.boy.z.05.m, by="age.m", all.x=TRUE)
dat05b.h.o$height.cat<-NA
dat05b.h.o$height.cat[dat05b.h.o$height>=dat05b.h.o$sd1neg&
                        dat05b.h.o$height<=dat05b.h.o$sd1]<-"-1SD<= height <=+1SD"
dat05b.h.o$height.cat[dat05b.h.o$height>dat05b.h.o$sd1&
                        dat05b.h.o$height<=dat05b.h.o$sd2]<-"+1SD< height <=+2SD"
dat05b.h.o$height.cat[dat05b.h.o$height>dat05b.h.o$sd2&
                        dat05b.h.o$height<=dat05b.h.o$sd3]<-"+2SD< height <=+3SD"
dat05b.h.o$height.cat[dat05b.h.o$height>dat05b.h.o$sd3&
                        dat05b.h.o$height<=dat05b.h.o$sd4]<-"+3SD< height <=+4SD"
dat05b.h.o$height.cat[dat05b.h.o$height>dat05b.h.o$sd4]<-"height >+4SD"
dat05b.h.o$height.cat[dat05b.h.o$height>=dat05b.h.o$sd2neg&
                        dat05b.h.o$height<dat05b.h.o$sd1neg]<-"-2SD<= height <-1SD"
dat05b.h.o$height.cat[dat05b.h.o$height>=dat05b.h.o$sd3neg&
                        dat05b.h.o$height<dat05b.h.o$sd2neg]<-"-3SD<= height <-2SD"
dat05b.h.o$height.cat[dat05b.h.o$height>=dat05b.h.o$sd4neg&
                        dat05b.h.o$height<dat05b.h.o$sd3neg]<-"-4SD<= height <-3SD"
dat05b.h.o$height.cat[dat05b.h.o$height<dat05b.h.o$sd4neg]<-"height <-4SD"

dat05b.h.o$height.cat<-factor(dat05b.h.o$height.cat, levels=hl)
tbl <- table(dat05b.h.o$height.cat)
#' Proportion with 95%CI by "sisonglaz" method 
#' Glaz, J., Sison, C.P. (1999) Simultaneous confidence intervals for multinomial proportions. 
#' Journal of Statistical Planning and Inference 82:251-262. 
#'  
pci<-round(MultinomCI(tbl,
           conf.level=0.95,
           method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05b.h.o$height.cat,dat05b.h.o$year)
ty<-as.matrix(ty)
# proportion with 95%CI
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:9,],ty.cp[10:18,],ty.cp[19:27,])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05b.h.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05b.h.o$height.cat,dat05b.h.o$year)
ty<-as.data.frame(ty)
caty.5hm <- ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
caty.5hm 
#' Stacked plot for proportion by sites
ty<-table(dat05b.h.o$height.cat,dat05b.h.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5hm <- ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
cats.5hm 
chisq.test(table(dat05b.h.o$height.cat,dat05b.h.o$hospitalcode))
#' Stacked plot for proportion over calendar years, facet hospitalcode
ty<-table(dat05b.h.o$height.cat,dat05b.h.o$year,dat05b.h.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5hm <- ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males,18 months to 5 years old")
catys.5hm
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05b.h.o$year,dat05b.h.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' ##### Height categories for stunting classification
dat05b.h.o$height.cats<-NA
dat05b.h.o$height.cats[dat05b.h.o$height>dat05b.h.o$sd3]<-"Extreme tallness"
dat05b.h.o$height.cats[dat05b.h.o$height>=dat05b.h.o$sd2neg&
                         dat05b.h.o$height<=dat05b.h.o$sd3]<-"Normal HFA"
dat05b.h.o$height.cats[dat05b.h.o$height<dat05b.h.o$sd2neg]<-"Stunting"
dat05b.h.o$height.cats<-factor(dat05b.h.o$height.cats, levels=stl)
tbl <- table(dat05b.h.o$height.cats)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05b.h.o$height.cats,dat05b.h.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(stl),],ty.cp[(length(stl)+1):(2*length(stl)),],ty.cp[(2*length(stl)+1):(3*length(stl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05b.h.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05b.h.o$height.cats,dat05b.h.o$year)
ty<-as.data.frame(ty)
caty.5hsm <- ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
caty.5hsm
#' Stacked plot for proportion by sites
chisq.test(dat05b.h.o$height.cats,dat05b.h.o$hospitalcode)
ty<-table(dat05b.h.o$height.cats,dat05b.h.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5hsm <- ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
cats.5hsm
#' Stacked plot for proportion over calendar years, facet hospitalcode
ty<-table(dat05b.h.o$height.cats,dat05b.h.o$year,dat05b.h.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5hsm <- ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males,18 months to 5 years old")
catys.5hsm
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05b.h.o$year,dat05b.h.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)

#' #### Weight
#' Mean, SE
#+ fig.width=8, fig.height=4
wfa.boy.z.05<-who5$`wfa-boys-zscore-expanded-tables`
wfa.boy.z.05$age.m<-round(wfa.boy.z.05$day/30,0)
wfa.boy.z.05.m<-wfa.boy.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat05b, mapping=aes(x=age.m, y=weight), size=0.1)+
  stat_summary(dat05b, mapping=aes(x=age.m, y=weight, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat05b, mapping=aes(x=age.m, y=weight),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat05b, mapping=aes(x=age.m, y=weight),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Weight (kg)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(0,50))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Males,18 months to 5 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
wfa.boy.z.05.m<-wfa.boy.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.5wm<-ggplot() +
  geom_point(dat05b, mapping=aes(x=age.m, y=weight), size=0.1)+
  stat_summary(dat05b, mapping=aes(x=age.m, y=weight, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat05b, mapping=aes(x=age.m, y=weight),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat05b, mapping=aes(x=age.m, y=weight),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(wfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Weight (kg)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(0,50))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Males,18 months to 5 years old")
who.5wm
#' ##### Classification of Vschool student as compared to WHO reference 
dat05b.w<-na.omit(dat05b[, c("pid","weight","age.m","year","hospitalcode")])
# merge with WHO data
dat05b.w.o<-merge(dat05b.w, wfa.boy.z.05.m, by="age.m", all.x=TRUE)
dat05b.w.o$weight.cat<-NA
dat05b.w.o$weight.cat[dat05b.w.o$weight>=dat05b.w.o$sd1neg&
                        dat05b.w.o$weight<=dat05b.w.o$sd1]<-"-1SD<= weight <=+1SD"
dat05b.w.o$weight.cat[dat05b.w.o$weight>dat05b.w.o$sd1&
                        dat05b.w.o$weight<=dat05b.w.o$sd2]<-"+1SD< weight <=+2SD"
dat05b.w.o$weight.cat[dat05b.w.o$weight>dat05b.w.o$sd2&
                        dat05b.w.o$weight<=dat05b.w.o$sd3]<-"+2SD< weight <=+3SD"
dat05b.w.o$weight.cat[dat05b.w.o$weight>dat05b.w.o$sd3&
                        dat05b.w.o$weight<=dat05b.w.o$sd4]<-"+3SD< weight <=+4SD"
dat05b.w.o$weight.cat[dat05b.w.o$weight>dat05b.w.o$sd4]<-"weight >+4SD"
dat05b.w.o$weight.cat[dat05b.w.o$weight>=dat05b.w.o$sd2neg&
                        dat05b.w.o$weight<dat05b.w.o$sd1neg]<-"-2SD<= weight <-1SD"
dat05b.w.o$weight.cat[dat05b.w.o$weight>=dat05b.w.o$sd3neg&
                        dat05b.w.o$weight<dat05b.w.o$sd2neg]<-"-3SD<= weight <-2SD"
dat05b.w.o$weight.cat[dat05b.w.o$weight>=dat05b.w.o$sd4neg&
                        dat05b.w.o$weight<dat05b.w.o$sd3neg]<-"-4SD<= weight <-3SD"
dat05b.w.o$weight.cat[dat05b.w.o$weight<dat05b.w.o$sd4neg]<-"weight <-4SD"

dat05b.w.o$weight.cat<-factor(dat05b.w.o$weight.cat, levels=wl)
tbl <- table(dat05b.w.o$weight.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05b.w.o$weight.cat,dat05b.w.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(wl),],ty.cp[(length(wl)+1):(2*length(wl)),],ty.cp[(2*length(wl)+1):(3*length(wl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05b.w.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05b.w.o$weight.cat,dat05b.w.o$year)
ty<-as.data.frame(ty)
caty.5wm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
caty.5wm
#' Stacked plot for proportion by sites
chisq.test(dat05b.w.o$weight.cat,dat05b.w.o$hospitalcode)
ty<-table(dat05b.w.o$weight.cat,dat05b.w.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5wm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
cats.5wm
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat05b.w.o$weight.cat,dat05b.w.o$year,dat05b.w.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5wm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males,18 months to 5 years old")
catys.5wm
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05b.w.o$year,dat05b.w.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)

#' #### BMI
#' Mean, SE
#+ fig.width=8, fig.height=4
bfa.boy.z.05<-who5$`bfa-boys-zscore-expanded-tables`
bfa.boy.z.05$age.m<-round(bfa.boy.z.05$day/30,0)
bfa.boy.z.05.m<-bfa.boy.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat05b, mapping=aes(x=age.m, y=bmi), size=0.1)+
  stat_summary(dat05b, mapping=aes(x=age.m, y=bmi, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat05b, mapping=aes(x=age.m, y=bmi),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat05b, mapping=aes(x=age.m, y=bmi),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="BMI (kg/m^2)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(0,50))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Males,18 months to 5 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
bfa.boy.z.05.m<-bfa.boy.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.5bm<-ggplot() +
  geom_point(dat05b, mapping=aes(x=age.m, y=bmi), size=0.1)+
  stat_summary(dat05b, mapping=aes(x=age.m, y=bmi, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat05b, mapping=aes(x=age.m, y=bmi),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat05b, mapping=aes(x=age.m, y=bmi),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(bfa.boy.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="BMI (kg/m^2)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(0,50))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Males,18 months to 5 years old")
who.5bm
#' ##### Classification of Vinschool student as compared to WHO reference 
dat05b.b<-na.omit(dat05b[, c("pid","bmi","age.m","year","hospitalcode")])
# merge with WHO data
dat05b.b.o<-merge(dat05b.b, bfa.boy.z.05.m, by="age.m", all.x=TRUE)
dat05b.b.o$bmi.cat<-NA
dat05b.b.o$bmi.cat[dat05b.b.o$bmi>=dat05b.b.o$sd1neg&
                     dat05b.b.o$bmi<=dat05b.b.o$sd1]<-"-1SD<= BMI <=+1SD"
dat05b.b.o$bmi.cat[dat05b.b.o$bmi>dat05b.b.o$sd1&
                        dat05b.b.o$bmi<=dat05b.b.o$sd2]<-"+1SD< BMI <=+2SD"
dat05b.b.o$bmi.cat[dat05b.b.o$bmi>dat05b.b.o$sd2&
                        dat05b.b.o$bmi<=dat05b.b.o$sd3]<-"+2SD< BMI <=+3SD"
dat05b.b.o$bmi.cat[dat05b.b.o$bmi>dat05b.b.o$sd3&
                        dat05b.b.o$bmi<=dat05b.b.o$sd4]<-"+3SD< BMI <=+4SD"
dat05b.b.o$bmi.cat[dat05b.b.o$bmi>dat05b.b.o$sd4]<-"BMI >+4SD"
dat05b.b.o$bmi.cat[dat05b.b.o$bmi>=dat05b.b.o$sd2neg&
                        dat05b.b.o$bmi<dat05b.b.o$sd1neg]<-"-2SD<= BMI <-1SD"
dat05b.b.o$bmi.cat[dat05b.b.o$bmi>=dat05b.b.o$sd3neg&
                        dat05b.b.o$bmi<dat05b.b.o$sd2neg]<-"-3SD<= BMI <-2SD"
dat05b.b.o$bmi.cat[dat05b.b.o$bmi>=dat05b.b.o$sd4neg&
                        dat05b.b.o$bmi<dat05b.b.o$sd3neg]<-"-4SD<= BMI <-3SD"
dat05b.b.o$bmi.cat[dat05b.b.o$bmi<dat05b.b.o$sd4neg]<-"BMI <-4SD"
dat05b.b.o$bmi.cat<-factor(dat05b.b.o$bmi.cat, levels=bl)
tbl <- table(dat05b.b.o$bmi.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05b.b.o$bmi.cat,dat05b.b.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(bl),],ty.cp[(length(bl)+1):(2*length(bl)),],ty.cp[(2*length(bl)+1):(3*length(bl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05b.b.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05b.b.o$bmi.cat,dat05b.b.o$year)
ty<-as.data.frame(ty)
caty.5bm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
caty.5bm
#' Stacked plot for proportion by sites
chisq.test(dat05b.b.o$bmi.cat,dat05b.b.o$hospitalcode)
ty<-table(dat05b.b.o$bmi.cat,dat05b.b.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5bm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
cats.5bm
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat05b.b.o$bmi.cat,dat05b.b.o$year,dat05b.b.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5bm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males,18 months to 5 years old")
catys.5bm
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05b.b.o$year,dat05b.b.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)

#' #### Weight for height 
#' Mean, SE
#+ fig.width=8, fig.height=4
wfh.boy.z.05<-who5$`wfh-boys-zscore-expanded-tables`
dat05b$heightr<-round(dat05b$height,0)
ggplot() +
  geom_point(dat05b, mapping=aes(x=heightr, y=weight), size=0.1)+
  stat_summary(dat05b, mapping=aes(x=heightr, y=weight, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat05b, mapping=aes(x=heightr, y=weight),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat05b, mapping=aes(x=heightr, y=weight),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd0, color="WHO mean"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd1, color="+1SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd2, color="+2SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd3, color="+3SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd4, color="+4SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd1neg, color="-1SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd2neg, color="-2SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd3neg, color="-3SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Height (cm)", y="Weight (kg)")+
  coord_cartesian(xlim = c(75, 120),ylim=c(0,50))+
  scale_x_discrete(limits =seq(75, 120, by = 5))+
  ggtitle("Males,18 months to 5 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
who.5whm<-ggplot() +
  geom_point(dat05b, mapping=aes(x=heightr, y=weight), size=0.1)+
  stat_summary(dat05b, mapping=aes(x=heightr, y=weight, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat05b, mapping=aes(x=heightr, y=weight),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat05b, mapping=aes(x=heightr, y=weight),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd0, color="WHO median"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd1, color="+1SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd2, color="+2SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd3, color="+3SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd4, color="+4SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd1neg, color="-1SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd2neg, color="-2SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd3neg, color="-3SD"))+
  geom_line(wfh.boy.z.05, mapping=aes(x=height, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Height (cm)", y="Weight (kg)")+
  coord_cartesian(xlim = c(76, 120),ylim=c(0,50))+
  scale_x_discrete(limits =seq(76, 120, by = 4))+
  scale_y_discrete(limits =seq(0, 50, by = 5))+
  ggtitle("Males,18 months to 5 years old")
who.5whm
#' ##### Classification weight for height of Vinschool student as compared to WHO reference 
dat05b.wh<-na.omit(dat05b[, c("pid","weight","height","year","hospitalcode")])
# merge with WHO data
dat05b.wh.o<-merge(dat05b.wh, wfh.boy.z.05, by="height", all.x=TRUE)
dat05b.wh.o$wh.cat<-NA
dat05b.wh.o$wh.cat[dat05b.wh.o$weight>=dat05b.wh.o$sd1neg&
                     dat05b.wh.o$weight<=dat05b.wh.o$sd1]<-"-1SD<= WFH <=+1SD"
dat05b.wh.o$wh.cat[dat05b.wh.o$weight>dat05b.wh.o$sd1&
                     dat05b.wh.o$weight<=dat05b.wh.o$sd2]<-"+1SD< WFH <=+2SD"
dat05b.wh.o$wh.cat[dat05b.wh.o$weight>dat05b.wh.o$sd2&
                     dat05b.wh.o$weight<=dat05b.wh.o$sd3]<-"+2SD< WFH <=+3SD"
dat05b.wh.o$wh.cat[dat05b.wh.o$weight>dat05b.wh.o$sd3&
                     dat05b.wh.o$weight<=dat05b.wh.o$sd4]<-"+3SD< WFH <=+4SD"
dat05b.wh.o$wh.cat[dat05b.wh.o$weight>dat05b.wh.o$sd4]<-"WFH >+4SD"
dat05b.wh.o$wh.cat[dat05b.wh.o$weight>=dat05b.wh.o$sd2neg&
                     dat05b.wh.o$weight<dat05b.wh.o$sd1neg]<-"-2SD<= WFH <-1SD"
dat05b.wh.o$wh.cat[dat05b.wh.o$weight>=dat05b.wh.o$sd3neg&
                     dat05b.wh.o$weight<dat05b.wh.o$sd2neg]<-"-3SD<= WFH <-2SD"
dat05b.wh.o$wh.cat[dat05b.wh.o$weight>=dat05b.wh.o$sd4neg&
                     dat05b.wh.o$weight<dat05b.wh.o$sd3neg]<-"-4SD<= WFH <-3SD"
dat05b.wh.o$wh.cat[dat05b.wh.o$weight<dat05b.wh.o$sd4neg]<-"WFH <-4SD"
dat05b.wh.o$wh.cat<-factor(dat05b.wh.o$wh.cat, levels=whl)
tbl <- table(dat05b.wh.o$wh.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05b.wh.o$wh.cat,dat05b.wh.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(whl),],ty.cp[(length(whl)+1):(2*length(whl)),],ty.cp[(2*length(whl)+1):(3*length(whl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05b.wh.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05b.wh.o$wh.cat,dat05b.wh.o$year)
ty<-as.data.frame(ty)
caty.5whm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
caty.5whm
#' Stacked plot for proportion by sites
chisq.test(dat05b.wh.o$wh.cat,dat05b.wh.o$hospitalcode)
ty<-table(dat05b.wh.o$wh.cat,dat05b.wh.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5whm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
cats.5whm
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat05b.wh.o$wh.cat,dat05b.wh.o$year,dat05b.wh.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5whm <- ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males,18 months to 5 years old")
catys.5whm
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05b.wh.o$year,dat05b.wh.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' ##### WHF categories for wasting classification
#' Combined overweight & obesity
w5lc<-c("Overweight & Obesity","Normal WFH","Wasting")
dat05b.wh.o$wh.catw<-NA
dat05b.wh.o$wh.catw[dat05b.wh.o$weight>dat05b.wh.o$sd2]<-"Overweight & Obesity"
dat05b.wh.o$wh.catw[dat05b.wh.o$weight>=dat05b.wh.o$sd2neg&
                      dat05b.wh.o$weight<=dat05b.wh.o$sd2]<-"Normal WFH"
dat05b.wh.o$wh.catw[dat05b.wh.o$weight<dat05b.wh.o$sd2neg]<-"Wasting"
dat05b.wh.o$wh.catw<-factor(dat05b.wh.o$wh.catw, levels=w5lc)
tbl <- table(dat05b.wh.o$wh.catw)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' Separate overweight, obesity
w5l<-c("Obesity","Overweight","Normal WFH","Wasting")
dat05b.wh.o$wh.catw<-NA
dat05b.wh.o$wh.catw[dat05b.wh.o$weight>dat05b.wh.o$sd2&
                      dat05b.wh.o$weight<=dat05b.wh.o$sd3]<-"Overweight"
dat05b.wh.o$wh.catw[dat05b.wh.o$weight>dat05b.wh.o$sd3]<-"Obesity"
dat05b.wh.o$wh.catw[dat05b.wh.o$weight>=dat05b.wh.o$sd2neg&
                      dat05b.wh.o$weight<=dat05b.wh.o$sd2]<-"Normal WFH"
dat05b.wh.o$wh.catw[dat05b.wh.o$weight<dat05b.wh.o$sd2neg]<-"Wasting"
dat05b.wh.o$wh.catw<-factor(dat05b.wh.o$wh.catw, levels=w5l)
tbl <- table(dat05b.wh.o$wh.catw)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05b.wh.o$wh.catw,dat05b.wh.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(w5l),],ty.cp[(length(w5l)+1):(2*length(w5l)),],ty.cp[(2*length(w5l)+1):(3*length(w5l)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05b.wh.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05b.wh.o$wh.catw,dat05b.wh.o$year)
ty<-as.data.frame(ty)
caty.5whom<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
caty.5whom
#' Stacked plot for proportion by sites
chisq.test(dat05b.wh.o$wh.catw,dat05b.wh.o$hospitalcode)
ty<-table(dat05b.wh.o$wh.catw,dat05b.wh.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5whom<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males,18 months to 5 years old")
cats.5whom
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat05b.wh.o$wh.catw,dat05b.wh.o$year,dat05b.wh.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5whom<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males,18 months to 5 years old")
catys.5whom
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05b.wh.o$year,dat05b.wh.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' #### Tabulate HFA and WFH
vwh5<-c("pid","year","hospitalcode","wh.cat","wh.catw")
vh5<-c("pid","year","hospitalcode","height.cat","height.cats")
cordat5b<-merge(dat05b.h.o[,vh5],dat05b.wh.o[,vwh5],by=c("pid","year","hospitalcode"),all.x=T)
tab.wh5b<-table(cordat5b$wh.catw,cordat5b$height.cats)
p.wh5b<-round(tab.wh5b/sum(tab.wh5b),5)
tp.wh5b<-cbind(wcat=rownames(tab.wh5b),
                extreme.tallness=paste(tab.wh5b[,1], " (",p.wh5b[,1],")",sep=""),
                normal.hfa=paste(tab.wh5b[,2], " (",p.wh5b[,2],")",sep=""),
                stunting=paste(tab.wh5b[,3], " (",p.wh5b[,3],")",sep=""))
kable(tp.wh5b)


#' ### Girls 
dat05g<-dat118[dat118$age<=5 & dat118$sex %in% "Female",]
#' #### Height
#' Mean, SE 
#+ fig.width=8, fig.height=4
hfa.girl.z.05<-who5$`lhfa-girls-zscore-expanded-tables`
hfa.girl.z.05$age.m<-round(hfa.girl.z.05$day/30,0)
hfa.girl.z.05.m<-hfa.girl.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat05g, mapping=aes(x=age.m, y=height), size=0.1)+
  stat_summary(dat05g, mapping=aes(x=age.m, y=height, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat05g, mapping=aes(x=age.m, y=height),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat05g, mapping=aes(x=age.m, y=height),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(0, 60),ylim=c(30,150))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Females, 18 months to 5 years old")
#' Median, 95%CI 
#+ fig.width=8, fig.height=4
hfa.girl.z.05.m<-hfa.girl.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.5hf<-ggplot() +
  geom_point(dat05g, mapping=aes(x=age.m, y=height), size=0.1)+
  stat_summary(dat05g, mapping=aes(x=age.m, y=height, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat05g, mapping=aes(x=age.m, y=height),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat05g, mapping=aes(x=age.m, y=height),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(hfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(0, 60),ylim=c(30,150))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Females, 18 months to 5 years old")
who.5hf
#' ##### Classification of Vinschool student as compared to WHO reference 
dat05g.h<-na.omit(dat05g[, c("pid","height","age.m","year","hospitalcode")])
# merge with WHO data
dat05g.h.o<-merge(dat05g.h, hfa.girl.z.05.m, by="age.m", all.x=TRUE)
dat05g.h.o$height.cat<-NA
dat05g.h.o$height.cat[dat05g.h.o$height>=dat05g.h.o$sd1neg&
                        dat05g.h.o$height<=dat05g.h.o$sd1]<-"-1SD<= height <=+1SD"
dat05g.h.o$height.cat[dat05g.h.o$height>dat05g.h.o$sd1&
                        dat05g.h.o$height<=dat05g.h.o$sd2]<-"+1SD< height <=+2SD"
dat05g.h.o$height.cat[dat05g.h.o$height>dat05g.h.o$sd2&
                        dat05g.h.o$height<=dat05g.h.o$sd3]<-"+2SD< height <=+3SD"
dat05g.h.o$height.cat[dat05g.h.o$height>dat05g.h.o$sd3&
                        dat05g.h.o$height<=dat05g.h.o$sd4]<-"+3SD< height <=+4SD"
dat05g.h.o$height.cat[dat05g.h.o$height>dat05g.h.o$sd4]<-"height >+4SD"
dat05g.h.o$height.cat[dat05g.h.o$height>=dat05g.h.o$sd2neg&
                        dat05g.h.o$height<dat05g.h.o$sd1neg]<-"-2SD<= height <-1SD"
dat05g.h.o$height.cat[dat05g.h.o$height>=dat05g.h.o$sd3neg&
                        dat05g.h.o$height<dat05g.h.o$sd2neg]<-"-3SD<= height <-2SD"
dat05g.h.o$height.cat[dat05g.h.o$height>=dat05g.h.o$sd4neg&
                        dat05g.h.o$height<dat05g.h.o$sd3neg]<-"-4SD<= height <-3SD"
dat05g.h.o$height.cat[dat05g.h.o$height<dat05g.h.o$sd4neg]<-"height <-4SD"
dat05g.h.o$height.cat<-factor(dat05g.h.o$height.cat, levels=hl)
tbl <- table(dat05g.h.o$height.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05g.h.o$height.cat,dat05g.h.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(hl),],ty.cp[(length(hl)+1):(2*length(hl)),],ty.cp[(2*length(hl)+1):(3*length(hl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05g.h.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05g.h.o$height.cat,dat05g.h.o$year)
ty<-as.data.frame(ty)
caty.5hf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
caty.5hf
#' Stacked plot for proportion by sites
chisq.test(dat05g.h.o$height.cat,dat05g.h.o$hospitalcode)
ty<-table(dat05g.h.o$height.cat,dat05g.h.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5hf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
cats.5hf
#' Stacked plot for proportion over calendar years, facet hospitalcode
ty<-table(dat05g.h.o$height.cat,dat05g.h.o$year,dat05g.h.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5hf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females, <= 5 years old")
catys.5hf
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05g.h.o$year,dat05g.h.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' ##### Height categories for stunting classification
dat05g.h.o$height.cats<-NA
dat05g.h.o$height.cats[dat05g.h.o$height>dat05g.h.o$sd3]<-"Extreme tallness"
dat05g.h.o$height.cats[dat05g.h.o$height>=dat05g.h.o$sd2neg&
                         dat05g.h.o$height<=dat05g.h.o$sd3]<-"Normal HFA"
dat05g.h.o$height.cats[dat05g.h.o$height<dat05g.h.o$sd2neg]<-"Stunting"
dat05g.h.o$height.cats<-factor(dat05g.h.o$height.cats, levels=stl)
tbl <- table(dat05g.h.o$height.cats)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05g.h.o$height.cats,dat05g.h.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(stl),],ty.cp[(length(stl)+1):(2*length(stl)),],ty.cp[(2*length(stl)+1):(3*length(stl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05g.h.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05g.h.o$height.cats,dat05g.h.o$year)
ty<-as.data.frame(ty)
caty.5hsf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
caty.5hsf
#' Stacked plot for proportion by sites
chisq.test(dat05g.h.o$height.cats,dat05g.h.o$hospitalcode)
ty<-table(dat05g.h.o$height.cats,dat05g.h.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5hsf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
cats.5hsf
#' Stacked plot for proportion over calendar years, facet hospitalcode
ty<-table(dat05g.h.o$height.cats,dat05g.h.o$year,dat05g.h.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5hsf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females, 18 months to 5 years old")
catys.5hsf
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05g.h.o$year,dat05g.h.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)


#' #### Weight
#' Mean, SE
#+ fig.width=8, fig.height=4
wfa.girl.z.05<-who5$`wfa-girls-zscore-expanded-tables`
wfa.girl.z.05$age.m<-round(wfa.girl.z.05$day/30,0)
wfa.girl.z.05.m<-wfa.girl.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat05g, mapping=aes(x=age.m, y=weight), size=0.1)+
  stat_summary(dat05g, mapping=aes(x=age.m, y=weight, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat05g, mapping=aes(x=age.m, y=weight),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat05g, mapping=aes(x=age.m, y=weight),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Weight (kg)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(0,50))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Females, 18 months to 5 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
wfa.girl.z.05<-who5$`wfa-girls-zscore-expanded-tables`
wfa.girl.z.05$age.m<-round(wfa.girl.z.05$day/30,0)
wfa.girl.z.05.m<-wfa.girl.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.5wf<-ggplot() +
  geom_point(dat05g, mapping=aes(x=age.m, y=weight), size=0.1)+
  stat_summary(dat05g, mapping=aes(x=age.m, y=weight, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat05g, mapping=aes(x=age.m, y=weight),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat05g, mapping=aes(x=age.m, y=weight),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(wfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Weight (kg)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(0,50))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Females, 18 months to 5 years old")
who.5wf
#' ##### Classification of Vinschool student as compared to WHO reference 
dat05g.w<-na.omit(dat05g[, c("pid","weight","age.m","year","hospitalcode")])
# merge with WHO data
dat05g.w.o<-merge(dat05g.w, wfa.girl.z.05.m, by="age.m", all.x=TRUE)
dat05g.w.o$weight.cat<-NA
dat05g.w.o$weight.cat[dat05g.w.o$weight>=dat05g.w.o$sd1neg&
                        dat05g.w.o$weight<=dat05g.w.o$sd1]<-"-1SD<= weight <=+1SD"
dat05g.w.o$weight.cat[dat05g.w.o$weight>dat05g.w.o$sd1&
                        dat05g.w.o$weight<=dat05g.w.o$sd2]<-"+1SD< weight <=+2SD"
dat05g.w.o$weight.cat[dat05g.w.o$weight>dat05g.w.o$sd2&
                        dat05g.w.o$weight<=dat05g.w.o$sd3]<-"+2SD< weight <=+3SD"
dat05g.w.o$weight.cat[dat05g.w.o$weight>dat05g.w.o$sd3&
                        dat05g.w.o$weight<=dat05g.w.o$sd4]<-"+3SD< weight <=+4SD"
dat05g.w.o$weight.cat[dat05g.w.o$weight>dat05g.w.o$sd4]<-"weight >+4SD"
dat05g.w.o$weight.cat[dat05g.w.o$weight>=dat05g.w.o$sd2neg&
                        dat05g.w.o$weight<dat05g.w.o$sd1neg]<-"-2SD<= weight <-1SD"
dat05g.w.o$weight.cat[dat05g.w.o$weight>=dat05g.w.o$sd3neg&
                        dat05g.w.o$weight<dat05g.w.o$sd2neg]<-"-3SD<= weight <-2SD"
dat05g.w.o$weight.cat[dat05g.w.o$weight>=dat05g.w.o$sd4neg&
                        dat05g.w.o$weight<dat05g.w.o$sd3neg]<-"-4SD<= weight <-3SD"
dat05g.w.o$weight.cat[dat05g.w.o$weight<dat05g.w.o$sd4neg]<-"weight <-4SD"
dat05g.w.o$weight.cat<-factor(dat05g.w.o$weight.cat, levels=wl)
tbl <- table(dat05g.w.o$weight.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05g.w.o$weight.cat,dat05g.w.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(wl),],ty.cp[(length(wl)+1):(2*length(wl)),],ty.cp[(2*length(wl)+1):(3*length(wl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05g.w.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05g.w.o$weight.cat,dat05g.w.o$year)
ty<-as.data.frame(ty)
caty.5wf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
caty.5wf
#' Stacked plot for proportion by sites
chisq.test(dat05g.w.o$weight.cat,dat05g.w.o$hospitalcode)
ty<-table(dat05g.w.o$weight.cat,dat05g.w.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5wf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
cats.5wf
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat05g.w.o$weight.cat,dat05g.w.o$year,dat05g.w.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5wf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females, 18 months to 5 years old")
catys.5wf
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05g.w.o$year,dat05g.w.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)

#' #### BMI
#' Mean, SE
#+ fig.width=8, fig.height=4
bfa.girl.z.05<-who5$`bfa-girls-zscore-expanded-tables`
bfa.girl.z.05$age.m<-round(bfa.girl.z.05$day/30,0)
bfa.girl.z.05.m<-bfa.girl.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat05g, mapping=aes(x=age.m, y=bmi), size=0.1)+
  stat_summary(dat05g, mapping=aes(x=age.m, y=bmi, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat05g, mapping=aes(x=age.m, y=bmi),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat05g, mapping=aes(x=age.m, y=bmi),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="BMI (kg/m^2)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(0,40))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Females, 18 months to 5 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
bfa.girl.z.05.m<-bfa.girl.z.05 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.5bf<-ggplot() +
  geom_point(dat05g, mapping=aes(x=age.m, y=bmi), size=0.1)+
  stat_summary(dat05g, mapping=aes(x=age.m, y=bmi, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat05g, mapping=aes(x=age.m, y=bmi),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat05g, mapping=aes(x=age.m, y=bmi),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(bfa.girl.z.05.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="BMI (kg/m^2)")+
  coord_cartesian(xlim = c(0, 60), ylim=c(0,50))+
  scale_x_discrete(limits =seq(0, 60, by = 6))+
  ggtitle("Females, 18 months to 5 years old")
who.5bf
#' ##### Classification of Vinschool student as compared to WHO reference 
dat05g.b<-na.omit(dat05g[, c("pid","bmi","age.m","year","hospitalcode")])
# merge with WHO data
dat05g.b.o<-merge(dat05g.b, bfa.girl.z.05.m, by="age.m", all.x=TRUE)
dat05g.b.o$bmi.cat<-NA
dat05g.b.o$bmi.cat[dat05g.b.o$bmi>=dat05g.b.o$sd1neg&
                     dat05g.b.o$bmi<=dat05g.b.o$sd1]<-"-1SD<= BMI <=+1SD"
dat05g.b.o$bmi.cat[dat05g.b.o$bmi>dat05g.b.o$sd1&
                     dat05g.b.o$bmi<=dat05g.b.o$sd2]<-"+1SD< BMI <=+2SD"
dat05g.b.o$bmi.cat[dat05g.b.o$bmi>dat05g.b.o$sd2&
                     dat05g.b.o$bmi<=dat05g.b.o$sd3]<-"+2SD< BMI <=+3SD"
dat05g.b.o$bmi.cat[dat05g.b.o$bmi>dat05g.b.o$sd3&
                     dat05g.b.o$bmi<=dat05g.b.o$sd4]<-"+3SD< BMI <=+4SD"
dat05g.b.o$bmi.cat[dat05g.b.o$bmi>dat05g.b.o$sd4]<-"BMI >+4SD"
dat05g.b.o$bmi.cat[dat05g.b.o$bmi>=dat05g.b.o$sd2neg&
                     dat05g.b.o$bmi<dat05g.b.o$sd1neg]<-"-2SD<= BMI <-1SD"
dat05g.b.o$bmi.cat[dat05g.b.o$bmi>=dat05g.b.o$sd3neg&
                     dat05g.b.o$bmi<dat05g.b.o$sd2neg]<-"-3SD<= BMI <-2SD"
dat05g.b.o$bmi.cat[dat05g.b.o$bmi>=dat05g.b.o$sd4neg&
                     dat05g.b.o$bmi<dat05g.b.o$sd3neg]<-"-4SD<= BMI <-3SD"
dat05g.b.o$bmi.cat[dat05g.b.o$bmi<dat05g.b.o$sd4neg]<-"BMI <-4SD"
dat05g.b.o$bmi.cat<-factor(dat05g.b.o$bmi.cat, levels=bl)
tbl <- table(dat05g.b.o$bmi.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05g.b.o$bmi.cat,dat05g.b.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(bl),],ty.cp[(length(bl)+1):(2*length(bl)),],ty.cp[(2*length(bl)+1):(3*length(bl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05g.b.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05g.b.o$bmi.cat,dat05g.b.o$year)
ty<-as.data.frame(ty)
caty.5bf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
caty.5bf
#' Stacked plot for proportion by sites
chisq.test(dat05g.b.o$bmi.cat,dat05g.b.o$hospitalcode)
ty<-table(dat05g.b.o$bmi.cat,dat05g.b.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5bf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
cats.5bf
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat05g.b.o$bmi.cat,dat05g.b.o$year,dat05g.b.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5bf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females, 18 months to 5 years old")
catys.5bf
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05g.b.o$year,dat05g.b.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)

#' #### Weight for height 
#+ fig.width=8, fig.height=4
wfh.girl.z.05<-who5$`wfh-girls-zscore-expanded-tables`
dat05g$heightr<-round(dat05g$height,0)
ggplot() +
  geom_point(dat05g, mapping=aes(x=heightr, y=weight), size=0.1)+
  stat_summary(dat05g, mapping=aes(x=heightr, y=weight, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat05g, mapping=aes(x=heightr, y=weight),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat05g, mapping=aes(x=heightr, y=weight),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd0, color="WHO mean"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd1, color="+1SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd2, color="+2SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd3, color="+3SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd4, color="+4SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd1neg, color="-1SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd2neg, color="-2SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd3neg, color="-3SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Height (cm)", y="Weight (kg)")+
  coord_cartesian(xlim = c(75, 120), ylim=c(0,50))+
  scale_x_discrete(limits =seq(75, 120, by = 5))+
  ggtitle("Females, 18 months to 5 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
who.5whf<-ggplot() +
  geom_point(dat05g, mapping=aes(x=heightr, y=weight), size=0.1)+
  stat_summary(dat05g, mapping=aes(x=heightr, y=weight, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat05g, mapping=aes(x=heightr, y=weight),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat05g, mapping=aes(x=heightr, y=weight),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd0, color="WHO median"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd1, color="+1SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd2, color="+2SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd3, color="+3SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd4, color="+4SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd1neg, color="-1SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd2neg, color="-2SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd3neg, color="-3SD"))+
  geom_line(wfh.girl.z.05, mapping=aes(x=height, y=sd4neg, color="-4SD"))+
  theme_bw()+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Height (cm)", y="Weight (kg)")+
  coord_cartesian(xlim = c(76, 120), ylim=c(0,50))+
  scale_x_discrete(limits =seq(76, 120, by = 4))+
  scale_y_discrete(limits =seq(0, 50, by = 5))+
  ggtitle("Females, 18 months to 5 years old")
who.5whf 
#' ##### Classification weight for height of Vinschool student as compared to WHO reference 
dat05g.wh<-na.omit(dat05g[, c("pid","weight","height","year","hospitalcode")])
# merge with WHO data
dat05g.wh.o<-merge(dat05g.wh, wfh.girl.z.05, by="height", all.x=TRUE)
dat05g.wh.o$wh.cat<-NA
dat05g.wh.o$wh.cat[dat05g.wh.o$weight>=dat05g.wh.o$sd1neg&
                     dat05g.wh.o$weight<=dat05g.wh.o$sd1]<-"-1SD<= WFH <=+1SD"
dat05g.wh.o$wh.cat[dat05g.wh.o$weight>dat05g.wh.o$sd1&
                     dat05g.wh.o$weight<=dat05g.wh.o$sd2]<-"+1SD< WFH <=+2SD"
dat05g.wh.o$wh.cat[dat05g.wh.o$weight>dat05g.wh.o$sd2&
                     dat05g.wh.o$weight<=dat05g.wh.o$sd3]<-"+2SD< WFH <=+3SD"
dat05g.wh.o$wh.cat[dat05g.wh.o$weight>dat05g.wh.o$sd3&
                     dat05g.wh.o$weight<=dat05g.wh.o$sd4]<-"+3SD< WFH <=+4SD"
dat05g.wh.o$wh.cat[dat05g.wh.o$weight>dat05g.wh.o$sd4]<-"WFH >+4SD"
dat05g.wh.o$wh.cat[dat05g.wh.o$weight>=dat05g.wh.o$sd2neg&
                     dat05g.wh.o$weight<dat05g.wh.o$sd1neg]<-"-2SD<= WFH <-1SD"
dat05g.wh.o$wh.cat[dat05g.wh.o$weight>=dat05g.wh.o$sd3neg&
                     dat05g.wh.o$weight<dat05g.wh.o$sd2neg]<-"-3SD<= WFH <-2SD"
dat05g.wh.o$wh.cat[dat05g.wh.o$weight>=dat05g.wh.o$sd4neg&
                     dat05g.wh.o$weight<dat05g.wh.o$sd3neg]<-"-4SD<= WFH <-3SD"
dat05g.wh.o$wh.cat[dat05g.wh.o$weight<dat05g.wh.o$sd4neg]<-"WFH <-4SD"
dat05g.wh.o$wh.cat<-factor(dat05g.wh.o$wh.cat, levels=whl)
tbl <- table(dat05g.wh.o$wh.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05g.wh.o$wh.cat,dat05g.wh.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(whl),],ty.cp[(length(whl)+1):(2*length(whl)),],ty.cp[(2*length(whl)+1):(3*length(whl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05g.wh.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05g.wh.o$wh.cat,dat05g.wh.o$year)
ty<-as.data.frame(ty)
caty.5whf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
caty.5whf
#' Stacked plot for proportion by sites
chisq.test(dat05g.wh.o$wh.cat,dat05g.wh.o$hospitalcode)
ty<-table(dat05g.wh.o$wh.cat,dat05g.wh.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5whf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
cats.5whf
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat05g.wh.o$wh.cat,dat05g.wh.o$year,dat05g.wh.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5whf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females, 18 months to 5 years old")
catys.5whf
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05g.wh.o$year,dat05g.wh.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' ##### WHF categories for wasting classification
#' Combine overweight & obesity
w5lc<-c("Overweight & Obesity","Normal WFH","Wasting")
dat05g.wh.o$wh.catw<-NA
dat05g.wh.o$wh.catw[dat05g.wh.o$weight>dat05g.wh.o$sd2]<-"Overweight & Obesity"
dat05g.wh.o$wh.catw[dat05g.wh.o$weight>=dat05g.wh.o$sd2neg&
                      dat05g.wh.o$weight<=dat05g.wh.o$sd2]<-"Normal WFH"
dat05g.wh.o$wh.catw[dat05g.wh.o$weight<dat05g.wh.o$sd2neg]<-"Wasting"
dat05g.wh.o$wh.catw<-factor(dat05g.wh.o$wh.catw, levels=w5lc)
tbl <- table(dat05g.wh.o$wh.catw)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' Separate overweight obesity 
w5l<-c("Obesity","Overweight","Normal WFH","Wasting")
dat05g.wh.o$wh.catw<-NA
dat05g.wh.o$wh.catw[dat05g.wh.o$weight>dat05g.wh.o$sd2&
                      dat05g.wh.o$weight<=dat05g.wh.o$sd3]<-"Overweight"
dat05g.wh.o$wh.catw[dat05g.wh.o$weight>dat05g.wh.o$sd3]<-"Obesity"
dat05g.wh.o$wh.catw[dat05g.wh.o$weight>=dat05g.wh.o$sd2neg&
                      dat05g.wh.o$weight<=dat05g.wh.o$sd2]<-"Normal WFH"
dat05g.wh.o$wh.catw[dat05g.wh.o$weight<dat05g.wh.o$sd2neg]<-"Wasting"
dat05g.wh.o$wh.catw<-factor(dat05g.wh.o$wh.catw, levels=w5l)
tbl <- table(dat05g.wh.o$wh.catw)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat05g.wh.o$wh.catw,dat05g.wh.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(w5l),],ty.cp[(length(w5l)+1):(2*length(w5l)),],ty.cp[(2*length(w5l)+1):(3*length(w5l)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat05g.wh.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat05g.wh.o$wh.catw,dat05g.wh.o$year)
ty<-as.data.frame(ty)
caty.5whof<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
caty.5whof
#' Stacked plot for proportion by sites
chisq.test(dat05g.wh.o$wh.catw,dat05g.wh.o$hospitalcode)
ty<-table(dat05g.wh.o$wh.catw,dat05g.wh.o$hospitalcode)
ty<-as.data.frame(ty)
cats.5whof<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 18 months to 5 years old")
cats.5whof
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat05g.wh.o$wh.catw,dat05g.wh.o$year,dat05g.wh.o$hospitalcode)
ty<-as.data.frame(ty)
catys.5whof<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females, 18 months to 5 years old")
catys.5whof
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat05g.wh.o$year,dat05g.wh.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' #### Tabulate HFA and WFH
vwh5<-c("pid","year","hospitalcode","wh.cat","wh.catw")
vh5<-c("pid","year","hospitalcode","height.cat","height.cats")
cordat5g<-merge(dat05g.h.o[,vh5],dat05g.wh.o[,vwh5],by=c("pid","year","hospitalcode"),all.x=T)
tab.wh5g<-table(cordat5g$wh.catw,cordat5g$height.cats)
p.wh5g<-round(tab.wh5g/sum(tab.wh5g),5)
tp.wh5g<-cbind(wcat=rownames(tab.wh5g),
                extreme.tallness=paste(tab.wh5g[,1], " (",p.wh5g[,1],")",sep=""),
                normal.hfa=paste(tab.wh5g[,2], " (",p.wh5g[,2],")",sep=""),
                stunting=paste(tab.wh5g[,3], " (",p.wh5g[,3],")",sep=""))
kable(tp.wh5g)


#' ## Age 6-18 years (by year) 
#' ### Boys 
dat619b<-dat118[dat118$age>5 & dat118$sex %in% "Male",]
#' #### Height
#' Mean, SE
#+ fig.width=8, fig.height=4
hfa.boy.z.618<-who619$hfa_boys_z_who2007
hfa.boy.z.618$age.m<-hfa.boy.z.618$month
hfa.boy.z.618$age.y<-round(hfa.boy.z.618$month/12,0)
hfa.boy.z.618.m<-hfa.boy.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat619b, mapping=aes(x=age.m, y=height), size=0.1)+
  stat_summary(dat619b, mapping=aes(x=age.m, y=height, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat619b, mapping=aes(x=age.m, y=height),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat619b, mapping=aes(x=age.m, y=height),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(75,225))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  ggtitle("Males, 5 to 18 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
hfa.boy.z.618.m<-hfa.boy.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.18hm<-ggplot() +
  geom_point(dat619b, mapping=aes(x=age.m, y=height), size=0.1)+
  stat_summary(dat619b, mapping=aes(x=age.m, y=height, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat619b, mapping=aes(x=age.m, y=height),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat619b, mapping=aes(x=age.m, y=height),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(hfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(75,225))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  ggtitle("Males, 5 to 18 years old")
who.18hm
#' ##### Classification of Vinschool student as compared to WHO reference 
dat619b.h<-na.omit(dat619b[, c("pid","height","age.m","year","hospitalcode")])
# merge with WHO data
dat619b.h.o<-merge(dat619b.h, hfa.boy.z.618.m, by="age.m", all.x=TRUE)
dat619b.h.o$height.cat<-NA
dat619b.h.o$height.cat[dat619b.h.o$height>=dat619b.h.o$sd1neg&
                         dat619b.h.o$height<=dat619b.h.o$sd1]<-"-1SD<= height <=+1SD"
dat619b.h.o$height.cat[dat619b.h.o$height>dat619b.h.o$sd1&
                        dat619b.h.o$height<=dat619b.h.o$sd2]<-"+1SD< height <=+2SD"
dat619b.h.o$height.cat[dat619b.h.o$height>dat619b.h.o$sd2&
                        dat619b.h.o$height<=dat619b.h.o$sd3]<-"+2SD< height <=+3SD"
dat619b.h.o$height.cat[dat619b.h.o$height>dat619b.h.o$sd3&
                        dat619b.h.o$height<=dat619b.h.o$sd4]<-"+3SD< height <=+4SD"
dat619b.h.o$height.cat[dat619b.h.o$height>dat619b.h.o$sd4]<-"height >+4SD"
dat619b.h.o$height.cat[dat619b.h.o$height>=dat619b.h.o$sd2neg&
                        dat619b.h.o$height<dat619b.h.o$sd1neg]<-"-2SD<= height <-1SD"
dat619b.h.o$height.cat[dat619b.h.o$height>=dat619b.h.o$sd3neg&
                        dat619b.h.o$height<dat619b.h.o$sd2neg]<-"-3SD<= height <-2SD"
dat619b.h.o$height.cat[dat619b.h.o$height>=dat619b.h.o$sd4neg&
                        dat619b.h.o$height<dat619b.h.o$sd3neg]<-"-4SD<= height <-3SD"
dat619b.h.o$height.cat[dat619b.h.o$height<dat619b.h.o$sd4neg]<-"height <-4SD"
dat619b.h.o$height.cat<-factor(dat619b.h.o$height.cat, levels=hl)
tbl <- table(dat619b.h.o$height.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619b.h.o$height.cat,dat619b.h.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(hl),],ty.cp[(length(hl)+1):(2*length(hl)),],ty.cp[(2*length(hl)+1):(3*length(hl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619b.h.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619b.h.o$height.cat,dat619b.h.o$year)
ty<-as.data.frame(ty)
caty.18hm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
caty.18hm
#' Stacked plot for proportion by sites
chisq.test(dat619b.h.o$height.cat,dat619b.h.o$hospitalcode)
ty<-table(dat619b.h.o$height.cat,dat619b.h.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18hm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
cats.18hm
#' Stacked plot for proportion over calendar years, facet hospitalcode
ty<-table(dat619b.h.o$height.cat,dat619b.h.o$year,dat619b.h.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18hm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males, 5 to 18 years old")
catys.18hm
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619b.h.o$year,dat619b.h.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' ##### Height categories for stunting classification
dat619b.h.o$height.cats<-NA
dat619b.h.o$height.cats[dat619b.h.o$height>dat619b.h.o$sd3]<-"Extreme tallness"
dat619b.h.o$height.cats[dat619b.h.o$height>=dat619b.h.o$sd2neg&
                          dat619b.h.o$height<=dat619b.h.o$sd3]<-"Normal HFA"
dat619b.h.o$height.cats[dat619b.h.o$height<dat619b.h.o$sd2neg]<-"Stunting"
dat619b.h.o$height.cats<-factor(dat619b.h.o$height.cats, levels=stl)
tbl <- table(dat619b.h.o$height.cats)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619b.h.o$height.cats,dat619b.h.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(stl),],ty.cp[(length(stl)+1):(2*length(stl)),],ty.cp[(2*length(stl)+1):(3*length(stl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619b.h.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619b.h.o$height.cats,dat619b.h.o$year)
ty<-as.data.frame(ty)
caty.18hsm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
caty.18hsm
#' Stacked plot for proportion by sites
chisq.test(dat619b.h.o$height.cats,dat619b.h.o$hospitalcode)
ty<-table(dat619b.h.o$height.cats,dat619b.h.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18hsm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
cats.18hsm
#' Stacked plot for proportion over calendar years, facet hospitalcode
ty<-table(dat619b.h.o$height.cats,dat619b.h.o$year,dat619b.h.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18hsm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males, 5 to 18 years old")
catys.18hsm
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619b.h.o$year,dat619b.h.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' ##### Effect of height at prepuberty on height growth 
test<-dat619b.h.o
test<-test %>%
  group_by(pid)%>%
  distinct_at("age.y", .keep_all = TRUE)%>%
  ungroup()
#' Height categories at 11 years old
test<-test %>%
  group_by(pid)%>%
  mutate(caths=NA)%>%
  mutate(
    caths = case_when(
      age.y=="11" & height.cats %in% "Extreme tallness" ~ 3,
      age.y=="11" & height.cats %in% "Normal HFA" ~ 2,
      age.y=="11" & height.cats %in% "Stunting" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(caths.11 = case_when(
    sum(caths, na.rm=T) == 3~ "Extreme tallness",
    sum(caths, na.rm=T) == 2~ "Normal HFA",
    sum(caths, na.rm=T) == 1~ "Stunting",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$caths.11<-factor(test$caths.11, levels=c("Stunting","Normal HFA","Extreme tallness"))
testn<-test[!is.na(test$caths.11),]
m.hs.11<-ggplot(testn, aes(x = age.m, y = height, colour=caths.11))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, height 11 years old")
m.hs.11
test<-test %>%
  group_by(pid)%>%
  mutate(cath=NA)%>%
  mutate(
    cath = case_when(
      age.y=="11" & height.cat %in% "height >+4SD" ~ 9,
      age.y=="11" & height.cat %in% "+3SD< height <=+4SD" ~ 8,
      age.y=="11" & height.cat %in% "+2SD< height <=+3SD" ~ 7,
      age.y=="11" & height.cat %in% "+1SD< height <=+2SD" ~ 6,
      age.y=="11" & height.cat %in% "-1SD<= height <=+1SD" ~ 5,
      age.y=="11" & height.cat %in% "-2SD<= height <-1SD" ~ 4,
      age.y=="11" & height.cat %in% "-3SD<= height <-2SD" ~ 3,
      age.y=="11" & height.cat %in% "-4SD<= height <-3SD" ~ 2,
      age.y=="11" & height.cat %in% "height <-4SD" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cath.11 = case_when(
    sum(cath, na.rm=T) == 9~ "height >+4SD",
    sum(cath, na.rm=T) == 8~ "+3SD< height <=+4SD",
    sum(cath, na.rm=T) == 7~ "+2SD< height <=+3SD",
    sum(cath, na.rm=T) == 6~ "+1SD< height <=+2SD",
    sum(cath, na.rm=T) == 5~ "-1SD<= height <=+1SD",
    sum(cath, na.rm=T) == 4~ "-2SD<= height <-1SD",
    sum(cath, na.rm=T) == 3~ "-3SD<= height <-2SD",
    sum(cath, na.rm=T) == 2~ "-4SD<= height <-3SD",
    sum(cath, na.rm=T) == 1~ "height <-4SD",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cath.11<-factor(test$cath.11, levels=levels(dat619b.h.o$height.cat))
outc<-c("height >+4SD","height <-4SD","-4SD<= height <-3SD")
testn<-test[!is.na(test$cath.11) &
              !test$cath.11 %in% outc,]
m.h.11<-ggplot(testn, aes(x = age.m, y = height, colour=cath.11))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, height 11 years old")
m.h.11

#' Height categories at 12 years old
test<-test %>%
  group_by(pid)%>%
  mutate(cath=NA)%>%
  mutate(
    cath = case_when(
      age.y=="12" & height.cat %in% "height >+4SD" ~ 9,
      age.y=="12" & height.cat %in% "+3SD< height <=+4SD" ~ 8,
      age.y=="12" & height.cat %in% "+2SD< height <=+3SD" ~ 7,
      age.y=="12" & height.cat %in% "+1SD< height <=+2SD" ~ 6,
      age.y=="12" & height.cat %in% "-1SD<= height <=+1SD" ~ 5,
      age.y=="12" & height.cat %in% "-2SD<= height <-1SD" ~ 4,
      age.y=="12" & height.cat %in% "-3SD<= height <-2SD" ~ 3,
      age.y=="12" & height.cat %in% "-4SD<= height <-3SD" ~ 2,
      age.y=="12" & height.cat %in% "height <-4SD" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cath.12 = case_when(
    sum(cath, na.rm=T) == 9~ "height >+4SD",
    sum(cath, na.rm=T) == 8~ "+3SD< height <=+4SD",
    sum(cath, na.rm=T) == 7~ "+2SD< height <=+3SD",
    sum(cath, na.rm=T) == 6~ "+1SD< height <=+2SD",
    sum(cath, na.rm=T) == 5~ "-1SD<= height <=+1SD",
    sum(cath, na.rm=T) == 4~ "-2SD<= height <-1SD",
    sum(cath, na.rm=T) == 3~ "-3SD<= height <-2SD",
    sum(cath, na.rm=T) == 2~ "-4SD<= height <-3SD",
    sum(cath, na.rm=T) == 1~ "height <-4SD",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cath.12<-factor(test$cath.12, levels=levels(dat619b.h.o$height.cat))
outc<-c("height >+4SD","height <-4SD","-4SD<= height <-3SD")
testn<-test[!is.na(test$cath.12) &
              !test$cath.12 %in% outc,]
m.h.12<-ggplot(testn, aes(x = age.m, y = height, colour=cath.12))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, height 12 years old")
m.h.12


#' #### Weight
#' Mean, SE
#+ fig.width=8, fig.height=4
wfa.boy.z.618<-who619$wfa_boys_z_who2007
wfa.boy.z.618$age.m<-wfa.boy.z.618$month
wfa.boy.z.618$age.y<-round(wfa.boy.z.618$month/12,0)
wfa.boy.z.618.m<-wfa.boy.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat619b, mapping=aes(x=age.m, y=weight), size=0.1)+
  stat_summary(dat619b, mapping=aes(x=age.m, y=weight, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat619b, mapping=aes(x=age.m, y=weight),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat619b, mapping=aes(x=age.m, y=weight),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Weight (kg)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(0,150))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  ggtitle("Males, 5 to 18 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
wfa.boy.z.618.m<-wfa.boy.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.18wm<-ggplot() +
  geom_point(dat619b, mapping=aes(x=age.m, y=weight), size=0.1)+
  stat_summary(dat619b, mapping=aes(x=age.m, y=weight, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat619b, mapping=aes(x=age.m, y=weight),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat619b, mapping=aes(x=age.m, y=weight),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(wfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Weight (kg)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(0,150))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  ggtitle("Males, 5 to 18 years old")
who.18wm
#' ##### Classification of Vinschool student as compared to WHO reference 
dat619b.w<-na.omit(dat619b[, c("pid","weight","age.m","year","hospitalcode")])
# merge with WHO data
dat619b.w.o<-merge(dat619b.w, wfa.boy.z.618.m, by="age.m", all.x=TRUE)
dat619b.w.o$weight.cat<-NA
dat619b.w.o$weight.cat[dat619b.w.o$weight>=dat619b.w.o$sd1neg&
                         dat619b.w.o$weight<=dat619b.w.o$sd1]<-"-1SD<= weight <=+1SD"
dat619b.w.o$weight.cat[dat619b.w.o$weight>dat619b.w.o$sd1&
                         dat619b.w.o$weight<=dat619b.w.o$sd2]<-"+1SD< weight <=+2SD"
dat619b.w.o$weight.cat[dat619b.w.o$weight>dat619b.w.o$sd2&
                         dat619b.w.o$weight<=dat619b.w.o$sd3]<-"+2SD< weight <=+3SD"
dat619b.w.o$weight.cat[dat619b.w.o$weight>dat619b.w.o$sd3&
                         dat619b.w.o$weight<=dat619b.w.o$sd4]<-"+3SD< weight <=+4SD"
dat619b.w.o$weight.cat[dat619b.w.o$weight>dat619b.w.o$sd4]<-"weight >+4SD"
dat619b.w.o$weight.cat[dat619b.w.o$weight>=dat619b.w.o$sd2neg&
                         dat619b.w.o$weight<dat619b.w.o$sd1neg]<-"-2SD<= weight <-1SD"
dat619b.w.o$weight.cat[dat619b.w.o$weight>=dat619b.w.o$sd3neg&
                         dat619b.w.o$weight<dat619b.w.o$sd2neg]<-"-3SD<= weight <-2SD"
dat619b.w.o$weight.cat[dat619b.w.o$weight>=dat619b.w.o$sd4neg&
                         dat619b.w.o$weight<dat619b.w.o$sd3neg]<-"-4SD<= weight <-3SD"
dat619b.w.o$weight.cat[dat619b.w.o$weight<dat619b.w.o$sd4neg]<-"weight <-4SD"
dat619b.w.o$weight.cat<-factor(dat619b.w.o$weight.cat, levels=wl)
tbl <- table(dat619b.w.o$weight.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619b.w.o$weight.cat,dat619b.w.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(wl),],ty.cp[(length(wl)+1):(2*length(wl)),],ty.cp[(2*length(wl)+1):(3*length(wl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619b.w.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619b.w.o$weight.cat,dat619b.w.o$year)
ty<-as.data.frame(ty)
caty.18wm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
caty.18wm
#' Stacked plot for proportion by sites
chisq.test(dat619b.w.o$weight.cat,dat619b.w.o$hospitalcode)
ty<-table(dat619b.w.o$weight.cat,dat619b.w.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18wm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
cats.18wm
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat619b.w.o$weight.cat,dat619b.w.o$year,dat619b.w.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18wm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males, 5 to 18 years old")
catys.18wm
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619b.w.o$year,dat619b.w.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)

#' #### BMI
#' Mean, SE
#+ fig.width=8, fig.height=4
bfa.boy.z.618<-who619$bmi_boys_z_who2007
bfa.boy.z.618$age.m<-bfa.boy.z.618$month
bfa.boy.z.618$age.y<-round(bfa.boy.z.618$month/12,0)
bfa.boy.z.618.m<-bfa.boy.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat619b, mapping=aes(x=age.m, y=bmi), size=0.1)+
  stat_summary(dat619b, mapping=aes(x=age.m, y=bmi, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat619b, mapping=aes(x=age.m, y=bmi),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat619b, mapping=aes(x=age.m, y=bmi),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="BMI (kg/m^2)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(5,50))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  ggtitle("Males, 5 to 18 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
bfa.boy.z.618.m<-bfa.boy.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.18bm<-ggplot() +
  geom_point(dat619b, mapping=aes(x=age.m, y=bmi), size=0.1)+
  stat_summary(dat619b, mapping=aes(x=age.m, y=bmi, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat619b, mapping=aes(x=age.m, y=bmi),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat619b, mapping=aes(x=age.m, y=bmi),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(bfa.boy.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="BMI (kg/m^2)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(5,50))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  ggtitle("Males, 5 to 18 years old")
who.18bm
#' ##### Classification of Vinschool student as compared to WHO reference 
dat619b.b<-na.omit(dat619b[, c("pid","bmi","age.m","height","year","hospitalcode")])
# merge with WHO data
dat619b.b.o<-merge(dat619b.b, bfa.boy.z.618.m, by="age.m", all.x=TRUE)
dat619b.b.o$bmi.cat<-NA
dat619b.b.o$bmi.cat[dat619b.b.o$bmi>=dat619b.b.o$sd1neg&
                      dat619b.b.o$bmi<=dat619b.b.o$sd1]<-"-1SD<= BMI <=+1SD"
dat619b.b.o$bmi.cat[dat619b.b.o$bmi>dat619b.b.o$sd1&
                         dat619b.b.o$bmi<=dat619b.b.o$sd2]<-"+1SD< BMI <=+2SD"
dat619b.b.o$bmi.cat[dat619b.b.o$bmi>dat619b.b.o$sd2&
                         dat619b.b.o$bmi<=dat619b.b.o$sd3]<-"+2SD< BMI <=+3SD"
dat619b.b.o$bmi.cat[dat619b.b.o$bmi>dat619b.b.o$sd3&
                         dat619b.b.o$bmi<=dat619b.b.o$sd4]<-"+3SD< BMI <=+4SD"
dat619b.b.o$bmi.cat[dat619b.b.o$bmi>dat619b.b.o$sd4]<-"BMI >+4SD"
dat619b.b.o$bmi.cat[dat619b.b.o$bmi>=dat619b.b.o$sd2neg&
                         dat619b.b.o$bmi<dat619b.b.o$sd1neg]<-"-2SD<= BMI <-1SD"
dat619b.b.o$bmi.cat[dat619b.b.o$bmi>=dat619b.b.o$sd3neg&
                         dat619b.b.o$bmi<dat619b.b.o$sd2neg]<-"-3SD<= BMI <-2SD"
dat619b.b.o$bmi.cat[dat619b.b.o$bmi>=dat619b.b.o$sd4neg&
                         dat619b.b.o$bmi<dat619b.b.o$sd3neg]<-"-4SD<= BMI <-3SD"
dat619b.b.o$bmi.cat[dat619b.b.o$bmi<dat619b.b.o$sd4neg]<-"BMI <-4SD"
dat619b.b.o$bmi.cat<-factor(dat619b.b.o$bmi.cat, levels=bl)
tbl <- table(dat619b.b.o$bmi.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619b.b.o$bmi.cat,dat619b.b.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(bl),],ty.cp[(length(bl)+1):(2*length(bl)),],ty.cp[(2*length(bl)+1):(3*length(bl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619b.b.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619b.b.o$bmi.cat,dat619b.b.o$year)
ty<-as.data.frame(ty)
caty.18bm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
caty.18bm
#' Stacked plot for proportion by sites
chisq.test(dat619b.b.o$bmi.cat,dat619b.b.o$hospitalcode)
ty<-table(dat619b.b.o$bmi.cat,dat619b.b.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18bm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
cats.18bm
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat619b.b.o$bmi.cat,dat619b.b.o$year,dat619b.b.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18bm<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males, 5 to 18 years old")
catys.18bm
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619b.b.o$year,dat619b.b.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' ##### BMI categories for thinness classification 
#' Combine obesity overweight
t18lc<-c("Overweight & Obesity","Normal BMI","Thinness")
dat619b.b.o$bmi.catt<-NA
dat619b.b.o$bmi.catt[dat619b.b.o$bmi>dat619b.b.o$sd1]<-"Overweight & Obesity"
dat619b.b.o$bmi.catt[dat619b.b.o$bmi>=dat619b.b.o$sd2neg&
                       dat619b.b.o$bmi<=dat619b.b.o$sd1]<-"Normal BMI"
dat619b.b.o$bmi.catt[dat619b.b.o$bmi<dat619b.b.o$sd2neg]<-"Thinness"
dat619b.b.o$bmi.catt<-factor(dat619b.b.o$bmi.catt, levels=t18lc)
tbl <- table(dat619b.b.o$bmi.catt)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' Separate obesity overweight
t18l<-c("Obesity","Overweight","Normal BMI","Thinness")
dat619b.b.o$bmi.catt<-NA
dat619b.b.o$bmi.catt[dat619b.b.o$bmi>dat619b.b.o$sd1&
                      dat619b.b.o$bmi<=dat619b.b.o$sd2]<-"Overweight"
dat619b.b.o$bmi.catt[dat619b.b.o$bmi>dat619b.b.o$sd2]<-"Obesity"
dat619b.b.o$bmi.catt[dat619b.b.o$bmi>=dat619b.b.o$sd2neg&
                      dat619b.b.o$bmi<=dat619b.b.o$sd1]<-"Normal BMI"
dat619b.b.o$bmi.catt[dat619b.b.o$bmi<dat619b.b.o$sd2neg]<-"Thinness"
dat619b.b.o$bmi.catt<-factor(dat619b.b.o$bmi.catt, levels=t18l)
tbl <- table(dat619b.b.o$bmi.catt)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619b.b.o$bmi.catt,dat619b.b.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(t18l),],ty.cp[(length(t18l)+1):(2*length(t18l)),],ty.cp[(2*length(t18l)+1):(3*length(t18l)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619b.b.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619b.b.o$bmi.catt,dat619b.b.o$year)
ty<-as.data.frame(ty)
caty.18bom<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
caty.18bom
#' Stacked plot for proportion by sites
chisq.test(dat619b.b.o$bmi.catt,dat619b.b.o$hospitalcode)
ty<-table(dat619b.b.o$bmi.catt,dat619b.b.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18bom<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Males, 5 to 18 years old")
cats.18bom
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat619b.b.o$bmi.catt,dat619b.b.o$year,dat619b.b.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18bom<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Males, 5 to 18 years old")
catys.18bom
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619b.b.o$year,dat619b.b.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' #### Tabulate HFA and BMI
vb18<-c("pid","year","hospitalcode","bmi.cat","bmi.catt")
vh18<-c("pid","year","hospitalcode","height.cat","height.cats")
cordat18b<-merge(dat619b.h.o[,vh18],dat619b.b.o[,vb18],by=c("pid","year","hospitalcode"),all.x=T)
tab.wh18b<-table(cordat18b$bmi.catt,cordat18b$height.cats)
p.wh18b<-round(tab.wh18b/sum(tab.wh18b),5)
tp.wh18b<-cbind(wcat=rownames(tab.wh18b),
               extreme.tallness=paste(tab.wh18b[,1], " (",p.wh18b[,1],")",sep=""),
               normal.hfa=paste(tab.wh18b[,2], " (",p.wh18b[,2],")",sep=""),
               stunting=paste(tab.wh18b[,3], " (",p.wh18b[,3],")",sep=""))
kable(tp.wh18b)

#' #### Obesity status at 8 to 15 years old
test<-dat619b.b.o
test<-test %>%
  group_by(pid)%>%
  distinct_at("age.y", .keep_all = TRUE)%>%
  ungroup()
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="8" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="8" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="8" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="8" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.8 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.8<-factor(test$cato.8, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.8),]
m.hb.8<-ggplot(testn, aes(x = age.m, y = height, colour=cato.8))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 8 years old")
m.hb.8
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="9" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="9" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="9" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="9" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.9 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.9<-factor(test$cato.9, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.9),]
m.hb.9<-ggplot(testn, aes(x = age.m, y = height, colour=cato.9))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 9 years old")
m.hb.9
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="10" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="10" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="10" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="10" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.10 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.10<-factor(test$cato.10, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.10),]
m.hb.10<-ggplot(testn, aes(x = age.m, y = height, colour=cato.10))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 10 years old")
m.hb.10
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="11" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="11" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="11" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="11" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.11 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.11<-factor(test$cato.11, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.11),]
m.hb.11<-ggplot(testn, aes(x = age.m, y = height, colour=cato.11))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 11 years old")
m.hb.11
#' Box plots and statistical test 
testhb11<-testn[testn$age.y=="11",]
kt<-kruskal.test(height ~ cato.11, data = testhb11)  
kt
pk.11m<-ifelse(kt$p.value>=0.0001,paste("p",round(kt$p.value,4),sep="="),"p<0.0001")
m.hb.box1111<-ggplot(testhb11, aes(x=cato.11,y=height,fill=cato.11)) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  guides(x =  guide_axis(angle = 45))+
  labs(x = "", y="Height (cm) 11 years old")+
  ggtitle("Males, BMI 11 years old")+
  annotate("text", x=1, y=170, label=pk.11m)
m.hb.box1111
testhb17<-testn[testn$age.y=="17",]
kt<-kruskal.test(height ~ cato.11, data = testhb17)  
kt
pk.17m<-ifelse(kt$p.value>=0.0001,paste("p",round(kt$p.value,4),sep="="),"p<0.0001")
m.hb.box1117<-ggplot(testhb17, aes(x=cato.11,y=height,fill=cato.11)) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  guides(x =  guide_axis(angle = 45))+
  labs(x = "", y="Height (cm) 17 years old")+
  ggtitle("Males, BMI 11 years old")+
  annotate("text", x=1, y=190, label=pk.17m)
m.hb.box1117

#' Annotated plot
m.hb.11.t<-m.hb.11+ geom_segment(aes(x = 132, xend = 132, y = 120, yend = 170), color = "black", size = 0.5)+
  geom_segment(aes(x = 204, xend = 204, y = 160, yend = 190), color = "black", size = 0.5)+
  annotate("text", x=132, y=170, label=pk.11m)+
  annotate("text", x=204, y=190, label=pk.17m)
m.hb.11.t


test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="12" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="12" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="12" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="12" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.12 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.12<-factor(test$cato.12, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.12),]
m.hb.12<-ggplot(testn, aes(x = age.m, y = height, colour=cato.12))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 12 years old")
m.hb.12
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="13" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="13" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="13" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="13" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.13 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.13<-factor(test$cato.13, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.13),]
m.hb.13<-ggplot(testn, aes(x = age.m, y = height, colour=cato.13))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 13 years old")
m.hb.13
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="14" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="14" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="14" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="14" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.14 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.14<-factor(test$cato.14, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.14),]
m.hb.14<-ggplot(testn, aes(x = age.m, y = height, colour=cato.14))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 14 years old")
m.hb.14
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="15" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="15" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="15" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="15" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.15 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.15<-factor(test$cato.15, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.15),]
m.hb.15<-ggplot(testn, aes(x = age.m, y = height, colour=cato.15))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI  15 years old")
m.hb.15

#' #### Change in height growth if change in obesity status
#' BMI at 10 and 13 years old
test<-test %>%
  group_by(pid)%>%
  mutate(cato.1013=case_when(
    !is.na(cato.10) & !is.na(cato.13) ~paste(cato.10, cato.13,sep="_"),
    TRUE ~ NA
  ))%>%
  ungroup()
testn<-test[!is.na(test$cato.1013),]
m.hb.1013<-ggplot(testn, aes(x = age.m, y = height, colour=cato.1013))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 10 & 13 years old")
m.hb.1013
#' Select BMI categories
os<-c("Obesity_Obesity","Obesity_Normal BMI",
      "Overweight_Normal BMI","Overweight_Obesity",
      "Normal BMI_Normal BMI",
      "Thinness_Normal BMI","Thinness_Thinness")
testn<-test[!is.na(test$cato.1013) &
              test$cato.1013 %in% os,]
m.hb.1013s<-ggplot(testn, aes(x = age.m, y = height, colour=cato.1013))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 10 & 13 years old")
m.hb.1013s

#' BMI at 11 and 14 years old
test<-test %>%
  group_by(pid)%>%
  mutate(cato.1114=case_when(
    !is.na(cato.11) & !is.na(cato.14) ~paste(cato.11, cato.14,sep="_"),
    TRUE ~ NA
  ))%>%
  ungroup()
testn<-test[!is.na(test$cato.1114),]
m.hb.1114<-ggplot(testn, aes(x = age.m, y = height, colour=cato.1114))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 11 & 14 years old")
m.hb.1114
#' Select BMI categories
testn<-test[!is.na(test$cato.1114) &
              test$cato.1114 %in% os,]
m.hb.1114s<-ggplot(testn, aes(x = age.m, y = height, colour=cato.1114))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 11 & 14 years old")
m.hb.1114s
#' BMI at 11 and 13 years old
test<-test %>%
  group_by(pid)%>%
  mutate(cato.1113=case_when(
    !is.na(cato.11) & !is.na(cato.13) ~paste(cato.11, cato.13,sep="_"),
    TRUE ~ NA
  ))%>%
  ungroup()
testn<-test[!is.na(test$cato.1113),]
m.hb.1113<-ggplot(testn, aes(x = age.m, y = height, colour=cato.1113))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 11 & 13 years old")
m.hb.1113
#' Select BMI categories
testn<-test[!is.na(test$cato.1113) &
              test$cato.1113 %in% os,]
m.hb.1113s<-ggplot(testn, aes(x = age.m, y = height, colour=cato.1113))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(110,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(110, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Males, BMI 11 & 13 years old")
m.hb.1113s




#' ### Girls 
dat619g<-dat118[dat118$age>5 & dat118$sex %in% "Female",]
#' #### Height
#' Mean, SE
#+ fig.width=8, fig.height=4
hfa.girl.z.618<-who619$hfa_girls_z_who2007
hfa.girl.z.618$age.m<-hfa.girl.z.618$month
hfa.girl.z.618$age.y<-round(hfa.girl.z.618$month/12,0)
hfa.girl.z.618.m<-hfa.girl.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat619g, mapping=aes(x=age.m, y=height), size=0.1)+
  stat_summary(dat619g, mapping=aes(x=age.m, y=height, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat619g, mapping=aes(x=age.m, y=height),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat619g, mapping=aes(x=age.m, y=height),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(75,225))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  ggtitle("Females, 5 to 18 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
hfa.girl.z.618.m<-hfa.girl.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.18hf<-ggplot() +
  geom_point(dat619g, mapping=aes(x=age.m, y=height), size=0.1)+
  stat_summary(dat619g, mapping=aes(x=age.m, y=height, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat619g, mapping=aes(x=age.m, y=height),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat619g, mapping=aes(x=age.m, y=height),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(hfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 215), ylim=c(75,225))+
  scale_x_discrete(limits =seq(60, 215, by = 12))+ #issue with 95%CI
  ggtitle("Females, 5 to 18 years old")
who.18hf
#' ##### Classification of Vinschool student as compared to WHO reference 
dat619g.h<-na.omit(dat619g[, c("pid","height","age.m","year","hospitalcode")])
# merge with WHO data
dat619g.h.o<-merge(dat619g.h, hfa.girl.z.618.m, by="age.m", all.x=TRUE)
dat619g.h.o$height.cat<-NA
dat619g.h.o$height.cat[dat619g.h.o$height>=dat619g.h.o$sd1neg&
                         dat619g.h.o$height<=dat619g.h.o$sd1]<-"-1SD<= height <=+1SD"
dat619g.h.o$height.cat[dat619g.h.o$height>dat619g.h.o$sd1&
                         dat619g.h.o$height<=dat619g.h.o$sd2]<-"+1SD< height <=+2SD"
dat619g.h.o$height.cat[dat619g.h.o$height>dat619g.h.o$sd2&
                         dat619g.h.o$height<=dat619g.h.o$sd3]<-"+2SD< height <=+3SD"
dat619g.h.o$height.cat[dat619g.h.o$height>dat619g.h.o$sd3&
                         dat619g.h.o$height<=dat619g.h.o$sd4]<-"+3SD< height <=+4SD"
dat619g.h.o$height.cat[dat619g.h.o$height>dat619g.h.o$sd4]<-"height >+4SD"
dat619g.h.o$height.cat[dat619g.h.o$height>=dat619g.h.o$sd2neg&
                         dat619g.h.o$height<dat619g.h.o$sd1neg]<-"-2SD<= height <-1SD"
dat619g.h.o$height.cat[dat619g.h.o$height>=dat619g.h.o$sd3neg&
                         dat619g.h.o$height<dat619g.h.o$sd2neg]<-"-3SD<= height <-2SD"
dat619g.h.o$height.cat[dat619g.h.o$height>=dat619g.h.o$sd4neg&
                         dat619g.h.o$height<dat619g.h.o$sd3neg]<-"-4SD<= height <-3SD"
dat619g.h.o$height.cat[dat619g.h.o$height<dat619g.h.o$sd4neg]<-"height <-4SD"
dat619g.h.o$height.cat<-factor(dat619g.h.o$height.cat, levels=hl)
tbl <- table(dat619g.h.o$height.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619g.h.o$height.cat,dat619g.h.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(hl),],ty.cp[(length(hl)+1):(2*length(hl)),],ty.cp[(2*length(hl)+1):(3*length(hl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619g.h.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619g.h.o$height.cat,dat619g.h.o$year)
ty<-as.data.frame(ty)
caty.18hf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
caty.18hf
#' Stacked plot for proportion by sites
chisq.test(dat619g.h.o$height.cat,dat619g.h.o$hospitalcode)
ty<-table(dat619g.h.o$height.cat,dat619g.h.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18hf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
cats.18hf
#' Stacked plot for proportion over calendar years, facet hospitalcode
ty<-table(dat619g.h.o$height.cat,dat619g.h.o$year,dat619g.h.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18hf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females,5 to 18 years old")
catys.18hf
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619g.h.o$year,dat619g.h.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' ##### Height categories for stunting classification
dat619g.h.o$height.cats<-NA
dat619g.h.o$height.cats[dat619g.h.o$height>dat619g.h.o$sd3]<-"Extreme tallness"
dat619g.h.o$height.cats[dat619g.h.o$height>=dat619g.h.o$sd2neg&
                          dat619g.h.o$height<=dat619g.h.o$sd3]<-"Normal HFA"
dat619g.h.o$height.cats[dat619g.h.o$height<dat619g.h.o$sd2neg]<-"Stunting"
dat619g.h.o$height.cats<-factor(dat619g.h.o$height.cats, levels=stl)
tbl <- table(dat619g.h.o$height.cats)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619g.h.o$height.cats,dat619g.h.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(stl),],ty.cp[(length(stl)+1):(2*length(stl)),],ty.cp[(2*length(stl)+1):(3*length(stl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619g.h.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619g.h.o$height.cats,dat619g.h.o$year)
ty<-as.data.frame(ty)
caty.18hsf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
caty.18hsf
#' Stacked plot for proportion by sites
chisq.test(dat619g.h.o$height.cats,dat619g.h.o$hospitalcode)
ty<-table(dat619g.h.o$height.cats,dat619g.h.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18hsf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
cats.18hsf
#' Stacked plot for proportion over calendar years, facet hospitalcode
ty<-table(dat619g.h.o$height.cats,dat619g.h.o$year,dat619g.h.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18hsf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females,5 to 18 years old")
catys.18hsf
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619g.h.o$year,dat619g.h.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)

#' ##### Effect of height at prepuberty on height growth 
test<-dat619g.h.o
test<-test %>%
  group_by(pid)%>%
  distinct_at("age.y", .keep_all = TRUE)%>%
  ungroup()
#' Height categories at 9 years old
test<-test %>%
  group_by(pid)%>%
  mutate(caths=NA)%>%
  mutate(
    caths = case_when(
      age.y=="9" & height.cats %in% "Extreme tallness" ~ 3,
      age.y=="9" & height.cats %in% "Normal HFA" ~ 2,
      age.y=="9" & height.cats %in% "Stunting" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(caths.9 = case_when(
    sum(caths, na.rm=T) == 3~ "Extreme tallness",
    sum(caths, na.rm=T) == 2~ "Normal HFA",
    sum(caths, na.rm=T) == 1~ "Stunting",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$caths.9<-factor(test$caths.9, levels=c("Stunting","Normal HFA","Extreme tallness"))
testn<-test[!is.na(test$caths.9),]
f.hs.9<-ggplot(testn, aes(x = age.m, y = height, colour=caths.9))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, height 9 years old")
f.hs.9
test<-test %>%
  group_by(pid)%>%
  mutate(cath=NA)%>%
  mutate(
    cath = case_when(
      age.y=="9" & height.cat %in% "height >+4SD" ~ 9,
      age.y=="9" & height.cat %in% "+3SD< height <=+4SD" ~ 8,
      age.y=="9" & height.cat %in% "+2SD< height <=+3SD" ~ 7,
      age.y=="9" & height.cat %in% "+1SD< height <=+2SD" ~ 6,
      age.y=="9" & height.cat %in% "-1SD<= height <=+1SD" ~ 5,
      age.y=="9" & height.cat %in% "-2SD<= height <-1SD" ~ 4,
      age.y=="9" & height.cat %in% "-3SD<= height <-2SD" ~ 3,
      age.y=="9" & height.cat %in% "-4SD<= height <-3SD" ~ 2,
      age.y=="9" & height.cat %in% "height <-4SD" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cath.9 = case_when(
    sum(cath, na.rm=T) == 9~ "height >+4SD",
    sum(cath, na.rm=T) == 8~ "+3SD< height <=+4SD",
    sum(cath, na.rm=T) == 7~ "+2SD< height <=+3SD",
    sum(cath, na.rm=T) == 6~ "+1SD< height <=+2SD",
    sum(cath, na.rm=T) == 5~ "-1SD<= height <=+1SD",
    sum(cath, na.rm=T) == 4~ "-2SD<= height <-1SD",
    sum(cath, na.rm=T) == 3~ "-3SD<= height <-2SD",
    sum(cath, na.rm=T) == 2~ "-4SD<= height <-3SD",
    sum(cath, na.rm=T) == 1~ "height <-4SD",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cath.9<-factor(test$cath.9, levels=levels(dat619g.h.o$height.cat))
outc<-c("height >+4SD","height <-4SD","-4SD<= height <-3SD")
testn<-test[!is.na(test$cath.9) &
              !test$cath.9 %in% outc,]
f.h.9<-ggplot(testn, aes(x = age.m, y = height, colour=cath.9))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,200))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100, 200, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, height 9 years old")
f.h.9

#' Height categories at 10 years old
test<-test %>%
  group_by(pid)%>%
  mutate(cath=NA)%>%
  mutate(
    cath = case_when(
      age.y=="10" & height.cat %in% "height >+4SD" ~ 9,
      age.y=="10" & height.cat %in% "+3SD< height <=+4SD" ~ 8,
      age.y=="10" & height.cat %in% "+2SD< height <=+3SD" ~ 7,
      age.y=="10" & height.cat %in% "+1SD< height <=+2SD" ~ 6,
      age.y=="10" & height.cat %in% "-1SD<= height <=+1SD" ~ 5,
      age.y=="10" & height.cat %in% "-2SD<= height <-1SD" ~ 4,
      age.y=="10" & height.cat %in% "-3SD<= height <-2SD" ~ 3,
      age.y=="10" & height.cat %in% "-4SD<= height <-3SD" ~ 2,
      age.y=="10" & height.cat %in% "height <-4SD" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cath.10 = case_when(
    sum(cath, na.rm=T) == 9~ "height >+4SD",
    sum(cath, na.rm=T) == 8~ "+3SD< height <=+4SD",
    sum(cath, na.rm=T) == 7~ "+2SD< height <=+3SD",
    sum(cath, na.rm=T) == 6~ "+1SD< height <=+2SD",
    sum(cath, na.rm=T) == 5~ "-1SD<= height <=+1SD",
    sum(cath, na.rm=T) == 4~ "-2SD<= height <-1SD",
    sum(cath, na.rm=T) == 3~ "-3SD<= height <-2SD",
    sum(cath, na.rm=T) == 2~ "-4SD<= height <-3SD",
    sum(cath, na.rm=T) == 1~ "height <-4SD",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cath.10<-factor(test$cath.10, levels=levels(dat619g.h.o$height.cat))
outc<-c("height >+4SD","height <-4SD","-4SD<= height <-3SD")
testn<-test[!is.na(test$cath.10) &
              !test$cath.10 %in% outc,]
f.h.10<-ggplot(testn, aes(x = age.m, y = height, colour=cath.10))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100, 180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, height 10 years old")
f.h.10


#' #### Weight
#' Mean, SE
#+ fig.width=8, fig.height=4
wfa.girl.z.618<-who619$wfa_girls_z_who2007
wfa.girl.z.618$age.m<-wfa.girl.z.618$month
wfa.girl.z.618$age.y<-round(wfa.girl.z.618$month/12,0)
wfa.girl.z.618.m<-wfa.girl.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat619g, mapping=aes(x=age.m, y=weight), size=0.1)+
  stat_summary(dat619g, mapping=aes(x=age.m, y=weight, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat619g, mapping=aes(x=age.m, y=weight),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat619g, mapping=aes(x=age.m, y=weight),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Weight (kg)")+
  coord_cartesian(xlim = c(60, 228),ylim=c(0,150))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  ggtitle("Females, 5 to 18 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
wfa.girl.z.618.m<-wfa.girl.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.18wf<-ggplot() +
  geom_point(dat619g, mapping=aes(x=age.m, y=weight), size=0.1)+
  stat_summary(dat619g, mapping=aes(x=age.m, y=weight, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat619g, mapping=aes(x=age.m, y=weight),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat619g, mapping=aes(x=age.m, y=weight),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(wfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="Weight (kg)")+
  coord_cartesian(xlim = c(60, 215),ylim=c(0,150))+
  scale_x_discrete(limits =seq(60, 215, by = 12))+
  ggtitle("Females, 5 to 18 years old")
who.18wf
#' ##### Classification of Vinschool student as compared to WHO reference 
dat619g.w<-na.omit(dat619g[, c("pid","weight","age.m","year","hospitalcode")])
# merge with WHO data
dat619g.w.o<-merge(dat619g.w, wfa.girl.z.618.m, by="age.m", all.x=TRUE)
dat619g.w.o$weight.cat[is.na(dat619g.w.o$sd2)]<-NA
dat619g.w.o$weight.cat[dat619g.w.o$weight>=dat619g.w.o$sd1neg&
                         dat619g.w.o$weight<=dat619g.w.o$sd1]<-"-1SD<= weight <=+1SD"
dat619g.w.o$weight.cat[dat619g.w.o$weight>dat619g.w.o$sd1&
                         dat619g.w.o$weight<=dat619g.w.o$sd2]<-"+1SD< weight <=+2SD"
dat619g.w.o$weight.cat[dat619g.w.o$weight>dat619g.w.o$sd2&
                         dat619g.w.o$weight<=dat619g.w.o$sd3]<-"+2SD< weight <=+3SD"
dat619g.w.o$weight.cat[dat619g.w.o$weight>dat619g.w.o$sd3&
                         dat619g.w.o$weight<=dat619g.w.o$sd4]<-"+3SD< weight <=+4SD"
dat619g.w.o$weight.cat[dat619g.w.o$weight>dat619g.w.o$sd4]<-"weight >+4SD"
dat619g.w.o$weight.cat[dat619g.w.o$weight>=dat619g.w.o$sd2neg&
                         dat619g.w.o$weight<dat619g.w.o$sd1neg]<-"-2SD<= weight <-1SD"
dat619g.w.o$weight.cat[dat619g.w.o$weight>=dat619g.w.o$sd3neg&
                         dat619g.w.o$weight<dat619g.w.o$sd2neg]<-"-3SD<= weight <-2SD"
dat619g.w.o$weight.cat[dat619g.w.o$weight>=dat619g.w.o$sd4neg&
                         dat619g.w.o$weight<dat619g.w.o$sd3neg]<-"-4SD<= weight <-3SD"
dat619g.w.o$weight.cat[dat619g.w.o$weight<dat619g.w.o$sd4neg]<-"weight <-4SD"
dat619g.w.o$weight.cat<-factor(dat619g.w.o$weight.cat, levels=wl)
tbl <- table(dat619g.w.o$weight.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619g.w.o$weight.cat,dat619g.w.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(wl),],ty.cp[(length(wl)+1):(2*length(wl)),],ty.cp[(2*length(wl)+1):(3*length(wl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619g.w.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619g.w.o$weight.cat,dat619g.w.o$year)
ty<-as.data.frame(ty)
caty.18wf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
caty.18wf
#' Stacked plot for proportion by sites
chisq.test(dat619g.w.o$weight.cat,dat619g.w.o$hospitalcode)
ty<-table(dat619g.w.o$weight.cat,dat619g.w.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18wf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
cats.18wf
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat619g.w.o$weight.cat,dat619g.w.o$year,dat619g.w.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18wf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females, 5 to 18 years old")
catys.18wf
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619g.w.o$year,dat619g.w.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)

#' #### BMI
#' Mean, SE
#+ fig.width=8, fig.height=4
bfa.girl.z.618<-who619$bmi_girls_z_who2007
bfa.girl.z.618$age.m<-bfa.girl.z.618$month
bfa.girl.z.618$age.y<-round(bfa.girl.z.618$month/12,0)
bfa.girl.z.618.m<-bfa.girl.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), mean))
ggplot() +
  geom_point(dat619g, mapping=aes(x=age.m, y=bmi), size=0.1)+
  stat_summary(dat619g, mapping=aes(x=age.m, y=bmi, color="Vschool mean"),
               fun = mean,
               geom='line') +
  stat_summary(dat619g, mapping=aes(x=age.m, y=bmi),
               fun=mean,
               geom='point', colour="red") +
  stat_summary(dat619g, mapping=aes(x=age.m, y=bmi),
               fun.data=mean_cl_boot,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO mean"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors,
                     breaks = names(colors)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="BMI (kg/m^2)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(5,50))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  ggtitle("Females, 5 to 18 years old")
#' Median, 95%CI
#+ fig.width=8, fig.height=4
bfa.girl.z.618.m<-bfa.girl.z.618 %>%
  group_by(age.m)%>%
  summarise(across(everything(), median))
who.18bf<-ggplot() +
  geom_point(dat619g, mapping=aes(x=age.m, y=bmi), size=0.1)+
  stat_summary(dat619g, mapping=aes(x=age.m, y=bmi, color="Vschool median"),
               fun = median,
               geom='line') +
  stat_summary(dat619g, mapping=aes(x=age.m, y=bmi),
               fun=median,
               geom='point', colour="red") +
  stat_summary(dat619g, mapping=aes(x=age.m, y=bmi),
               fun.data=medci,
               geom='errorbar',
               width=0.2, colour="red") +
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd0, color="WHO median"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1, color="+1SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2, color="+2SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3, color="+3SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4, color="+4SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd1neg, color="-1SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd2neg, color="-2SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd3neg, color="-3SD"))+
  geom_line(bfa.girl.z.618.m, mapping=aes(x=age.m, y=sd4neg, color="-4SD"))+
  scale_color_manual(name = "", values = colors2,
                     breaks = names(colors2)[c(1:10)])+ 
  theme_bw()+
  labs(x = "Age (month)", y="BMI (kg/m^2)")+
  coord_cartesian(xlim = c(60, 215), ylim=c(5,50))+
  scale_x_discrete(limits =seq(60, 215, by = 12))+
  ggtitle("Females, 5 to 18 years old")
who.18bf
#' ##### Classification of Vinschool student as compared to WHO reference 
dat619g.b<-na.omit(dat619g[, c("pid","bmi","height","age.m","year","hospitalcode")])
# merge with WHO data
dat619g.b.o<-merge(dat619g.b, bfa.girl.z.618.m, by="age.m", all.x=TRUE)
dat619g.b.o$bmi.cat<-NA
dat619g.b.o$bmi.cat[dat619g.b.o$bmi>=dat619g.b.o$sd1neg&
                      dat619g.b.o$bmi<=dat619g.b.o$sd1]<-"-1SD<= BMI <=+1SD"
dat619g.b.o$bmi.cat[dat619g.b.o$bmi>dat619g.b.o$sd1&
                      dat619g.b.o$bmi<=dat619g.b.o$sd2]<-"+1SD< BMI <=+2SD"
dat619g.b.o$bmi.cat[dat619g.b.o$bmi>dat619g.b.o$sd2&
                      dat619g.b.o$bmi<=dat619g.b.o$sd3]<-"+2SD< BMI <=+3SD"
dat619g.b.o$bmi.cat[dat619g.b.o$bmi>dat619g.b.o$sd3&
                      dat619g.b.o$bmi<=dat619g.b.o$sd4]<-"+3SD< BMI <=+4SD"
dat619g.b.o$bmi.cat[dat619g.b.o$bmi>dat619g.b.o$sd4]<-"BMI >+4SD"
dat619g.b.o$bmi.cat[dat619g.b.o$bmi>=dat619g.b.o$sd2neg&
                      dat619g.b.o$bmi<dat619g.b.o$sd1neg]<-"-2SD<= BMI <-1SD"
dat619g.b.o$bmi.cat[dat619g.b.o$bmi>=dat619g.b.o$sd3neg&
                      dat619g.b.o$bmi<dat619g.b.o$sd2neg]<-"-3SD<= BMI <-2SD"
dat619g.b.o$bmi.cat[dat619g.b.o$bmi>=dat619g.b.o$sd4neg&
                      dat619g.b.o$bmi<dat619g.b.o$sd3neg]<-"-4SD<= BMI <-3SD"
dat619g.b.o$bmi.cat[dat619g.b.o$bmi<dat619g.b.o$sd4neg]<-"BMI <-4SD"
dat619g.b.o$bmi.cat<-factor(dat619g.b.o$bmi.cat, levels=bl)
tbl <- table(dat619g.b.o$bmi.cat)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619g.b.o$bmi.cat,dat619g.b.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(bl),],ty.cp[(length(bl)+1):(2*length(bl)),],ty.cp[(2*length(bl)+1):(3*length(bl)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619g.b.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619g.b.o$bmi.cat,dat619g.b.o$year)
ty<-as.data.frame(ty)
caty.18bf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
caty.18bf
#' Stacked plot for proportion by sites
chisq.test(dat619g.b.o$bmi.cat,dat619g.b.o$hospitalcode)
ty<-table(dat619g.b.o$bmi.cat,dat619g.b.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18bf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
cats.18bf
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat619g.b.o$bmi.cat,dat619g.b.o$year,dat619g.b.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18bf<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females, 5 to 18 years old")
catys.18bf
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619g.b.o$year,dat619g.b.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' ##### BMI categories for thinness classification 
#' Combine obesity overweight
t18lc<-c("Overweight & Obesity","Normal BMI","Thinness")
dat619g.b.o$bmi.catt<-NA
dat619g.b.o$bmi.catt[dat619g.b.o$bmi>dat619g.b.o$sd1]<-"Overweight & Obesity"
dat619g.b.o$bmi.catt[dat619g.b.o$bmi>=dat619g.b.o$sd2neg&
                       dat619g.b.o$bmi<=dat619g.b.o$sd1]<-"Normal BMI"
dat619g.b.o$bmi.catt[dat619g.b.o$bmi<dat619g.b.o$sd2neg]<-"Thinness"
dat619g.b.o$bmi.catt<-factor(dat619g.b.o$bmi.catt, levels=t18lc)
tbl <- table(dat619g.b.o$bmi.catt)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' Separate obesity overweight
t18l<-c("Obesity","Overweight","Normal BMI","Thinness")
dat619g.b.o$bmi.catt<-NA
dat619g.b.o$bmi.catt[dat619g.b.o$bmi>dat619g.b.o$sd1&
                       dat619g.b.o$bmi<=dat619g.b.o$sd2]<-"Overweight"
dat619g.b.o$bmi.catt[dat619g.b.o$bmi>dat619g.b.o$sd2]<-"Obesity"
dat619g.b.o$bmi.catt[dat619g.b.o$bmi>=dat619g.b.o$sd2neg&
                       dat619g.b.o$bmi<=dat619g.b.o$sd1]<-"Normal BMI"
dat619g.b.o$bmi.catt[dat619g.b.o$bmi<dat619g.b.o$sd2neg]<-"Thinness"
dat619g.b.o$bmi.catt<-factor(dat619g.b.o$bmi.catt, levels=t18l)
tbl <- table(dat619g.b.o$bmi.catt)
pci<-round(MultinomCI(tbl,
                      conf.level=0.95,
                      method="sisonglaz"),5)
tblb<- cbind(count=tbl, p.95ci=paste(pci[,1]," (",pci[,2],", ",pci[,3],")",sep=""))
kable(tblb)
#' By year
ty<-table(dat619g.b.o$bmi.catt,dat619g.b.o$year)
ty<-as.matrix(ty)
#cp<-function(x){cbind(count=x,proportion=prop.table(x))}
cp<-function(x){round(MultinomCI(x,conf.level=0.95,method="sisonglaz"),5)}
ty.cp<-apply(ty,2,cp)
ty.c<-ty
colnames(ty.c)<-paste("n",colnames(ty.c),sep=".")
rownames(ty.c)<-rownames(ty)
ty.p<-cbind(ty.cp[1:length(t18l),],ty.cp[(length(t18l)+1):(2*length(t18l)),],ty.cp[(2*length(t18l)+1):(3*length(t18l)),])
colnames(ty.p)[1:6]<-paste("p",colnames(ty.p)[1:6],sep=".")
colnames(ty.p)[7:12]<-paste("ll",colnames(ty.p)[7:12],sep=".")
colnames(ty.p)[13:18]<-paste("ul",colnames(ty.p)[13:18],sep=".")
rownames(ty.p)<-rownames(ty)
# add trend test
ny<-table(dat619g.b.o$year)
tp<-NULL
for (i in 1:nrow(ty)){
  pp<-round(prop.trend.test(ty[i,], ny)$p.value,5)
  tp<-rbind(tp,pp)
}
colnames(tp)<-"p.trend"
ty.cp2<-cbind(ty.c,ty.p,tp)
kable(ty.cp2)
#' Stacked plot for proportion over calendar years
ty<-table(dat619g.b.o$bmi.catt,dat619g.b.o$year)
ty<-as.data.frame(ty)
caty.18bof<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
caty.18bof
#' Stacked plot for proportion by sites
chisq.test(dat619g.b.o$bmi.catt,dat619g.b.o$hospitalcode)
ty<-table(dat619g.b.o$bmi.catt,dat619g.b.o$hospitalcode)
ty<-as.data.frame(ty)
cats.18bof<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme_ipsum()+
  ggtitle("Females, 5 to 18 years old")
cats.18bof
#' Stacked plot for proportion over calendar years, facet hospital code
ty<-table(dat619g.b.o$bmi.catt,dat619g.b.o$year,dat619g.b.o$hospitalcode)
ty<-as.data.frame(ty)
catys.18bof<-ggplot(ty, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("Proportion")+
  guides(fill=guide_legend(title=""))+
  guides(x =  guide_axis(angle = 45))+
  theme(axis.text.x = element_text(size = 8))+ 
  theme_ipsum()+
  facet_wrap(~Var3, ncol=3)+
  ggtitle("Females, 5 to 18 years old")
catys.18bof
#' Add trend test over year by hospitalcode
hy<-as.data.frame(table(dat619g.b.o$year,dat619g.b.o$hospitalcode))
wc<-unique(ty$Var1)
hos<-unique(ty$Var3)
tyh<-NULL
for (i in hos){
  du<-ty[ty$Var3 %in% i,]
  da<-hy$Freq[hy$Var2 %in% i]
  tp<-NULL
  for (k in wc){
    dw<-du$Freq[du$Var1 %in% k]
    if (sum(dw)>0){
      pp<-round(prop.trend.test(dw, da)$p.value,5)
    }
    if (sum(dw)==0){
      pp<-NA
    }
    tp<-rbind(tp,pp)
  }
  tyh<-cbind(tyh,tp)
}
rownames(tyh)<-wc
colnames(tyh)<-hos
kable(tyh)
#' #### Tabulate HFA and BMI
vb18<-c("pid","year","hospitalcode","bmi.cat","bmi.catt")
vh18<-c("pid","year","hospitalcode","height.cat","height.cats")
cordat18g<-merge(dat619g.h.o[,vh18],dat619g.b.o[,vb18],by=c("pid","year","hospitalcode"),all.x=T)
tab.wh18g<-table(cordat18g$bmi.catt,cordat18g$height.cats)
p.wh18g<-round(tab.wh18g/sum(tab.wh18g),5)
tp.wh18g<-cbind(wcat=rownames(tab.wh18g),
                extreme.tallness=paste(tab.wh18g[,1], " (",p.wh18g[,1],")",sep=""),
                normal.hfa=paste(tab.wh18g[,2], " (",p.wh18g[,2],")",sep=""),
                stunting=paste(tab.wh18g[,3], " (",p.wh18g[,3],")",sep=""))
kable(tp.wh18g)

#' #### Obesity status at 7 to 13 years old
test<-dat619g.b.o
test<-test %>%
  group_by(pid)%>%
  distinct_at("age.y", .keep_all = TRUE)%>%
  ungroup()
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="7" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="7" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="7" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="7" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.7 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.7<-factor(test$cato.7, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.7),]
f.hb.7<-ggplot(testn, aes(x = age.m, y = height, colour=cato.7))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, BMI 7 years old")
f.hb.7
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="8" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="8" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="8" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="8" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.8 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.8<-factor(test$cato.8, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.8),]
f.hb.8<-ggplot(testn, aes(x = age.m, y = height, colour=cato.8))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, BMI 8 years old")
f.hb.8
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="9" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="9" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="9" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="9" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.9 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.9<-factor(test$cato.9, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.9),]
f.hb.9<-ggplot(testn, aes(x = age.m, y = height, colour=cato.9))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, BMI 9 years old")
f.hb.9

#' Box plots and statistical test 
testhb9<-testn[testn$age.y=="9",]
kt<-kruskal.test(height ~ cato.9, data = testhb9)  
kt
pk.9f<-ifelse(kt$p.value>=0.0001,paste("p",round(kt$p.value,4),sep="="),"p<0.0001")
f.hb.box99<-ggplot(testhb9, aes(x=cato.9,y=height,fill=cato.9)) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  guides(x =  guide_axis(angle = 45))+
  labs(x = "", y="Height (cm) 9 years old")+
  ggtitle("Females, BMI 9 years old")+
  annotate("text", x=1, y=170, label=pk.9f)
f.hb.box99
testhb15<-testn[testn$age.y=="15",]
kt<-kruskal.test(height ~ cato.9, data = testhb15)  
kt
pk.15f<-ifelse(kt$p.value>=0.0001,paste("p",round(kt$p.value,4),sep="="),"p<0.0001")
f.hb.box915<-ggplot(testhb15, aes(x=cato.9,y=height,fill=cato.9)) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  guides(x =  guide_axis(angle = 45))+
  labs(x = "", y="Height (cm) 15 years old")+
  ggtitle("Females, BMI 9 years old")+
  annotate("text", x=1, y=170, label=pk.15f)
f.hb.box915
f.hb.9.t<-f.hb.9+ geom_segment(aes(x = 108, xend = 108, y = 120, yend = 150), color = "black", size = 0.5)+
  geom_segment(aes(x = 180, xend = 180, y = 150, yend = 170), color = "black", size = 0.5)+
  annotate("text", x=108, y=150, label=pk.9f)+
  annotate("text", x=180, y=170, label=pk.15f)
f.hb.9.t
  

test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="10" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="10" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="10" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="10" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.10 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.10<-factor(test$cato.10, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.10),]
f.hb.10<-ggplot(testn, aes(x = age.m, y = height, colour=cato.10))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, BMI 10 years old")
f.hb.10
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="11" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="11" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="11" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="11" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.11 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.11<-factor(test$cato.11, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.11),]
f.hb.11<-ggplot(testn, aes(x = age.m, y = height, colour=cato.11))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, BMI 11 years old")
f.hb.11


test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="12" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="12" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="12" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="12" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.12 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.12<-factor(test$cato.12, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.12),]
f.hb.12<-ggplot(testn, aes(x = age.m, y = height, colour=cato.12))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, BMI 12 years old")
f.hb.12
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="13" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="13" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="13" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="13" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.13 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.13<-factor(test$cato.13, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.13),]
f.hb.13<-ggplot(testn, aes(x = age.m, y = height, colour=cato.13))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("females, BMI 13 years old")
f.hb.13
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="14" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="14" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="14" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="14" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.14 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.14<-factor(test$cato.14, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.14),]
f.hb.14<-ggplot(testn, aes(x = age.m, y = height, colour=cato.14))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("females, BMI 14 years old")
f.hb.14
test<-test %>%
  group_by(pid)%>%
  mutate(cato=NA)%>%
  mutate(
    cato = case_when(
      age.y=="15" & bmi.catt %in% "Obesity" ~ 4,
      age.y=="15" & bmi.catt %in% "Overweight" ~ 3,
      age.y=="15" & bmi.catt %in% "Normal BMI" ~ 2,
      age.y=="15" & bmi.catt %in% "Thinness" ~ 1,
      TRUE ~ NA  # Default category for other cases
    )
  ) %>%
  mutate(cato.15 = case_when(
    sum(cato, na.rm=T) == 4~ "Obesity",
    sum(cato, na.rm=T) == 3~ "Overweight",
    sum(cato, na.rm=T) == 2~ "Normal BMI",
    sum(cato, na.rm=T) == 1~ "Thinness",
    TRUE ~ NA 
  ))%>%
  ungroup()
test$cato.15<-factor(test$cato.15, levels=c("Thinness","Normal BMI","Overweight","Obesity"))
testn<-test[!is.na(test$cato.15),]
f.hb.15<-ggplot(testn, aes(x = age.m, y = height, colour=cato.15))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("females, BMI  15 years old")
f.hb.15

#' #### Change in height growth if change in obesity status
#' BMI at 8 and 11 years old
test<-test %>%
  group_by(pid)%>%
  mutate(cato.811=case_when(
    !is.na(cato.8) & !is.na(cato.11) ~paste(cato.8, cato.11,sep="_"),
    TRUE ~ NA
  ))%>%
  ungroup()
testn<-test[!is.na(test$cato.811),]
f.hb.811<-ggplot(testn, aes(x = age.m, y = height, colour=cato.811))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, BMI 8 & 11 years old")
f.hb.811
#' Select BMI categories
os<-c("Obesity_Obesity","Obesity_Normal BMI",
      "Overweight_Normal BMI","Overweight_Obesity",
      "Normal BMI_Normal BMI",
      "Thinness_Normal BMI","Thinness_Thinness")
testn<-test[!is.na(test$cato.811) &
              test$cato.811 %in% os,]
f.hb.811s<-ggplot(testn, aes(x = age.m, y = height, colour=cato.811))+
  stat_smooth(method = 'loess',size=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x = "Age (month)", y="Height (cm)")+
  coord_cartesian(xlim = c(60, 228), ylim=c(100,180))+
  scale_x_discrete(limits =seq(60, 228, by = 12))+
  scale_y_discrete(limits =seq(100,180, by = 10))+
  guides(x =  guide_axis(angle = 45))+
  ggtitle("Females, BMI 8 & 11 years old")
f.hb.811s





#' ## Combined plot
#' ### WHO WFH, BMI 
#' WHO WFH 5, BMI 18
#+ fig.width=10, fig.height=6
who_wfh5bmi18<-ggarrange(who.5whm, who.5whf, 
                         who.18bm, who.18bf,
                   ncol=2, nrow=2, common.legend = TRUE, legend="right")
who_wfh5bmi18


#' wfh5bmi518
#+ fig.width=12, fig.height=9
who_wfh5bmi518<-ggarrange(who.5whm, who.5whf, 
                         who.5bm, who.5bf,
                         who.18bm, who.18bf,
                         ncol=2, nrow=3, common.legend = TRUE, legend="right")
who_wfh5bmi518


#' bmi5
#+ fig.width=12, fig.height=5
who_bmi5<-ggarrange(who.5bm, who.5bf,
                          ncol=2, nrow=1, common.legend = TRUE, legend="right")
who_bmi5

#+ fig.width=12, fig.height=9
who_bmi518<-ggarrange(who.5bm, who.5bf,
                      who.18bm, who.18bf,
                    ncol=2, nrow=2, common.legend = TRUE, legend="right")
who_bmi518


#' ### WHO Height
#+ fig.width=10, fig.height=6
who_518mf_h<-ggarrange(who.5hm, who.5hf, 
                         who.18hm, who.18hf,
                         ncol=2, nrow=2, common.legend = TRUE, legend="right")
who_518mf_h


#' ### WHO weight
#+ fig.width=10, fig.height=6
who_518mf_w<-ggarrange(who.5wm, who.5wf, 
                       who.18wm, who.18wf,
                       ncol=2, nrow=2, common.legend = TRUE, legend="right")
who_518mf_w


#' ### Obesity
#' Obesity all ages, gender by year
#+ fig.width=9, fig.height=7
caty_518mf_o<-ggarrange(caty.5whom, caty.5whof, 
                          caty.18bom, caty.18bof,
                          ncol=2, nrow=2, common.legend = FALSE, legend="right")
caty_518mf_o


#' Obesity all ages, gender by sites
#+ fig.width=9, fig.height=7
cats_518mf_o<-ggarrange(cats.5whom, cats.5whof, 
                        cats.18bom, cats.18bof,
                        ncol=2, nrow=2, common.legend = FALSE, legend="right")
cats_518mf_o


#+ fig.width=9, fig.height=14
catys4_518mf_o<-ggarrange(caty.5whom, caty.5whof, 
                        caty.18bom, caty.18bof,
                        cats.5whom, cats.5whof, 
                        cats.18bom, cats.18bof,
                        ncol=2, nrow=4, common.legend = FALSE, legend="right")
catys4_518mf_o


#' Obesity all ages, gender by year, sites
#+ fig.width=12, fig.height=9
catys_518mf_o<-ggarrange(catys.5whom, catys.5whof, 
                      catys.18bom, catys.18bof,
                      ncol=2, nrow=2, common.legend = FALSE, legend="right")
catys_518mf_o


#' ### Height categories 
#+ fig.width=12, fig.height=9
catys_518mf_h<-ggarrange(catys.5hm, catys.5hf, 
                       catys.18hm, catys.18hf,
                       ncol=2, nrow=2, common.legend = TRUE, legend="right")
catys_518mf_h


#' Height categories by year
#+ fig.width=12, fig.height=9
caty_518mf_h<-ggarrange(caty.5hm, caty.5hf, 
                         caty.18hm, caty.18hf,
                         ncol=2, nrow=2, common.legend = TRUE, legend="right")
caty_518mf_h


#' Height categories by sites
#+ fig.width=12, fig.height=9
cats_518mf_h<-ggarrange(cats.5hm, cats.5hf, 
                        cats.18hm, cats.18hf,
                        ncol=2, nrow=2, common.legend = TRUE, legend="right")
cats_518mf_h


#' Height categories by year, sites
#+ fig.width=9, fig.height=14
catys4_518mf_h<-ggarrange(caty.5hm, caty.5hf, 
                          caty.18hm, caty.18hf,
                          cats.5hm, cats.5hf, 
                        cats.18hm, cats.18hf,
                        ncol=2, nrow=4, common.legend = TRUE, legend="right")
catys4_518mf_h


#' Height stunting categories by year, site
#+ fig.width=12, fig.height=9
catys_518mf_hs<-ggarrange(catys.5hsm, catys.5hsf, 
                         catys.18hsm, catys.18hsf,
                         ncol=2, nrow=2, common.legend = TRUE, legend="right")
catys_518mf_hs


#' Height stunting categories by year
#+ fig.width=12, fig.height=9
caty_518mf_hs<-ggarrange(caty.5hsm, caty.5hsf, 
                          caty.18hsm, caty.18hsf,
                          ncol=2, nrow=2, common.legend = TRUE, legend="right")
caty_518mf_hs


#' Height stunting categories by sites
#+ fig.width=12, fig.height=9
cats_518mf_hs<-ggarrange(cats.5hsm, cats.5hsf, 
                         cats.18hsm, cats.18hsf,
                         ncol=2, nrow=2, common.legend = TRUE, legend="right")
cats_518mf_hs


#' Height stunting categories by year, sites 5 18
#+ fig.width=8, fig.height=12
catys4_518mf_hs<-ggarrange(caty.5hsm, caty.5hsf, 
                           caty.18hsm, caty.18hsf,
                           cats.5hsm, cats.5hsf, 
                         cats.18hsm, cats.18hsf,
                         ncol=2, nrow=4, common.legend = TRUE, legend="right")
catys4_518mf_hs



#' ### Weight categories
#' Weight categories by year, sites 
#+ fig.width=12, fig.height=9
catys_518mf_w<-ggarrange(catys.5wm, catys.5wf, 
                         catys.18wm, catys.18wf,
                         ncol=2, nrow=2, common.legend = TRUE, legend="right")
catys_518mf_w


#' Weight categories by years 
#+ fig.width=12, fig.height=9
caty_518mf_w<-ggarrange(caty.5wm, caty.5wf, 
                        caty.18wm, caty.18wf,
                        ncol=2, nrow=2, common.legend = TRUE, legend="right")
caty_518mf_w


#' Weight categories by sites
#+ fig.width=12, fig.height=9
cats_518mf_w<-ggarrange(cats.5wm, cats.5wf, 
                        cats.18wm, cats.18wf,
                        ncol=2, nrow=2, common.legend = TRUE, legend="right")
cats_518mf_w


#' ### BMI SD categories
#' BMI SD categories by year, sites
#+ fig.width=14, fig.height=9
catys_518mf_bmi<-ggarrange(catys.5bm, catys.5bf, 
                         catys.18bm, catys.18bf,
                         ncol=2, nrow=2, common.legend = TRUE, legend="right")
catys_518mf_bmi


#' BMI SD categories by year
#+ fig.width=12, fig.height=9
caty_518mf_bmi<-ggarrange(caty.5bm, caty.5bf, 
                        caty.18bm, caty.18bf,
                        ncol=2, nrow=2, common.legend = TRUE, legend="right")
caty_518mf_bmi


#' BMI SD categories by sites
#+ fig.width=12, fig.height=9
cats_518mf_bmi<-ggarrange(cats.5bm, cats.5bf, 
                          cats.18bm, cats.18bf,
                          ncol=2, nrow=2, common.legend = TRUE, legend="right")
cats_518mf_bmi


#' ### Weight for height categories
#' Weight for height categories by year, sites
#+ fig.width=12, fig.height=5
catys_518mf_wh<-ggarrange(catys.5whm, catys.5whf,
                         ncol=2, nrow=1, common.legend = TRUE, legend="right")
catys_518mf_wh

#' Weight for height categories by year
#+ fig.width=12, fig.height=5
caty_518mf_wh<-ggarrange(caty.5whm, caty.5whf, 
                            ncol=2, nrow=1, common.legend = TRUE, legend="right")
caty_518mf_wh

#' Weight for height categories by sites
#+ fig.width=12, fig.height=5
cats_518mf_wh<-ggarrange(cats.5whm, cats.5whf, 
                            ncol=2, nrow=1, common.legend = TRUE, legend="right")
cats_518mf_wh


#' ### Effect of prepuberty BMI,height on height growth
#' #### BMI categories for males 11, females 9 years old
#+ fig.width=10, fig.height=5
pre.bh.9f11m<-ggarrange(m.hb.11.t, f.hb.9.t,
                         ncol=2, nrow=1, common.legend = TRUE, legend="right")
pre.bh.9f11m


#' #### Height categories for males 11, females 9 years old
#+ fig.width=10, fig.height=5
pre.hh.9f11m<-ggarrange(m.h.11, f.h.9, 
                        ncol=2, nrow=1, common.legend = TRUE, legend="right")
pre.hh.9f11m


#' #### BMI categories for males 9 to 14 years old
#+ fig.width=8, fig.height=12
pre.bh.914m<-ggarrange(m.hb.9, m.hb.10,
                       m.hb.11,m.hb.12,
                       m.hb.13,m.hb.14,
                      ncol=2, nrow=3, common.legend = TRUE, legend="right")
pre.bh.914m

#' #### BMI categories for females 7 to 12 years old
#+ fig.width=8, fig.height=12
pre.bh.712f<-ggarrange(f.hb.7, f.hb.8,
                       f.hb.9, f.hb.10,
                       f.hb.11, f.hb.12,
                       ncol=2, nrow=3, common.legend = TRUE, legend="right")
pre.bh.712f



#' # WHO LMS method for constructing normalized growth standards
#' Using Vinschool student data as reference for Vietnam. 
#' LMS: (Lambda Mu and Sigma method), which enables efficient calculation of percentiles and Z-scores and smoothing of growth curve. 
#' Using Package gamlss version 5.4-22
#'    
#' ## Age <=5 years (by month) 
#' ### Boys 
#get age 1 to 18
dat118<-dat[dat$age>=1.5 &dat$age<=18 &!is.na(dat$age),]
#age 1-5
dat05b<-dat118[dat118$age<=5 & dat118$sex %in% "Male",]
#' #### Height
dat05b.h<-na.omit(dat05b[, c("height","age.m","pid","datevisit","age","yearvisit")])
lms05b.h<-lms(height,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat05b.h,points=FALSE,legend=TRUE,
        k=3,calibration=F, trans.x=T)
lms05b.h$family
lms05b.h$power
centiles(lms05b.h, dat05b.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Height (cm)", xlab="Age (month)",legend=FALSE,ylim=c(70,130),
         main="Centile curves height for age (boys, 18 to 60 months)",
         yaxt = "n", xaxt = "n",
         points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(70, 130, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(70, 130, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
centiles.fan(lms05b.h,dat05b.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Height (cm)", xlab="Age (month)",ylim=c(70,130),
             yaxt = "n", xaxt = "n", 
             main="Fan-chart height for age (boys, 18 to 60 months)",
             points=FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(70, 130, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(70, 130, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")


#' #### Weight
dat05b.w<-na.omit(dat05b[, c("weight","age.m","pid","datevisit","age","yearvisit")])
lms05b.w<-lms(weight,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat05b.w,
        k=3,calibration=F, trans.x=T)
lms05b.w$family
lms05b.w$power
centiles(lms05b.w,dat05b.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Weight (kg)", xlab="Age (month)",
         main="Centile curves weight for age (boys, 18 to 60 months)",
         ylim=c(5,45),
         legend=FALSE,
         yaxt = "n", xaxt = "n",
         points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 45, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 45, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
centiles.fan(lms05b.w,dat05b.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6),
             ylim=c(5,45),
             yaxt = "n", xaxt = "n",
             main="Fan-chart weight for age (boys, 18 to 60 months)",
             ylab="Weight", xlab="Age (month)")
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 45, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 45, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")

#' #### BMI
dat05b.b<-na.omit(dat05b[, c("bmi","age.m","pid","datevisit","age","yearvisit")])
lms05b.b<-lms(bmi,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat05b.b,
        k=3,calibration=F, trans.x=T)
lms05b.b$family
lms05b.b$power
centiles(lms05b.b,dat05b.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="BMI (kg/m2)", xlab="Age (month)",
         main="Centile curves BMI for age (boys, 18 to 60 months)",
         ylim=c(5,35),
         yaxt = "n", xaxt = "n",
         legend=FALSE,points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 35, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 35, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
centiles.fan(lms05b.b,dat05b.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylim=c(5,35),
             yaxt = "n", xaxt = "n",
             main="Fan-chart BMI for age (boys, 18 to 60 months)",
             ylab="BMI", xlab="Age (month)")
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 35, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 35, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")

#' #### Weight for height
dat05b.wh<-na.omit(dat05b[, c("weight","height","pid","datevisit","age","yearvisit")])
dat05b.wh$heightr<-round(dat05b.wh$height,0)
lms05b.wh<-lms(weight,heightr,families=c("BCCGo","BCPEo","BCTo"),
              data=dat05b.wh,
              k=3,calibration=F, trans.x=T)
lms05b.wh$family
lms05b.wh$power
centiles(lms05b.wh,dat05b.wh$heightr,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Weight (kg)", xlab="Height (cm)",
         main="Centile curves weight for height (boys, 18 to 60 months)",
         ylim=c(5,50),xlim=c(65,120),
         yaxt = "n", xaxt = "n",
         legend=FALSE,points=FALSE, save = FALSE)
axis(1, at = seq(65, 120, by = 5), las=2)
axis(2, at = seq(5, 50, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 50, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(65, 120, by = 1),  lty = 2, col = "lightgrey")
centiles.fan(lms05b.wh,dat05b.wh$heightr,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylim=c(5,50),xlim=c(65,120),
             yaxt = "n", xaxt = "n",
             main="Fan-chart weight for height (boys, 18 to 60 months)",
             ylab="Weight (kg)", xlab="Height (cm)")
axis(1, at = seq(65, 120, by = 5), las=2)
axis(2, at = seq(5, 50, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 50, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(65, 120, by = 1),  lty = 2, col = "lightgrey")

#' ### Girls
dat05g<-dat118[dat118$age<=5 & dat118$sex %in% "Female",]
#' #### Height
dat05g.h<-na.omit(dat05g[, c("height","age.m","pid","datevisit","age","yearvisit")])
lms05g.h<-lms(height,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat05g.h,
        k=3,calibration=F, trans.x=T)
lms05g.h$family
lms05g.h$power
centiles(lms05g.h,dat05g.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Height (cm)", xlab="Age (month)",
         legend=FALSE,ylim=c(70,130),
         yaxt = "n", xaxt = "n",
         main="Centile curves height for age (girls, 18 to 60 months)",
         points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(70, 130, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(70, 130, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
centiles.fan(lms05g.h,dat05g.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Height", xlab="Age (month)",
             legend=FALSE,ylim=c(70,130),
             yaxt = "n", xaxt = "n",
             main="Fan-chart height for age (girls, 18 to 60 months)",
             points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(70, 130, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(70, 130, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")

#' #### Weight
dat05g.w<-na.omit(dat05g[, c("weight","age.m","pid","datevisit","age","yearvisit")])
lms05g.w<-lms(weight,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat05g.w,
        k=3,calibration=F, trans.x=T)
lms05g.w$family
lms05g.w$power
centiles(lms05g.w,dat05g.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Weight (kg)", xlab="Age (month)",
         legend=FALSE,ylim=c(5,45),
         yaxt = "n", xaxt = "n",
         main="Centile curves weight for age (girls, 18 to 60 months)",
         points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 45, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 45, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
centiles.fan(lms05g.w,dat05g.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Weight", xlab="Age (month)",
             legend=FALSE,ylim=c(5,45),
             yaxt = "n", xaxt = "n",
             main="Fan-chart weight for age (girls, 18 to 60 months)",
             points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 45, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 45, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")

#' #### BMI
dat05g.b<-na.omit(dat05g[, c("bmi","age.m","pid","datevisit","age","yearvisit")])
lms05g.b<-lms(bmi,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat05g.b,
        k=3,calibration=F, trans.x=T)
lms05g.b$family
lms05g.b$power
centiles(lms05g.b,dat05g.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="BMI (kg/m2)", xlab="Age (month)",
         main="Centile curves BMI for age (girls, 18 to 60 months)",
         ylim=c(5,35),
         yaxt = "n", xaxt = "n",
         legend=FALSE,points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 35, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 35, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
centiles.fan(lms05g.b,dat05g.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="BMI (kg/m2)", xlab="Age (month)",
             ylim=c(5,35),
             yaxt = "n", xaxt = "n",
             main="Fan-chart BMI for age (girls, 18 to 60 months)",
             legend=FALSE,points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 35, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 35, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")

#' #### Weight for height
dat05g.wh<-na.omit(dat05g[, c("weight","height","pid","datevisit","age","yearvisit")])
dat05g.wh$heightr<-round(dat05g.wh$height,0)
lms05g.wh<-lms(weight,heightr,families=c("BCCGo","BCPEo","BCTo"),
               data=dat05g.wh,
               k=3,calibration=F, trans.x=T)
lms05g.wh$family
lms05g.wh$power
centiles(lms05g.wh,dat05g.wh$heightr,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Weight (kg)", xlab="Height (cm)",
         main="Centile curves weight for height (girls, 18 to 60 months)",
         ylim=c(5,50),xlim=c(65,120),
         yaxt = "n", xaxt = "n",
         legend=FALSE,points=FALSE, save = FALSE)
axis(1, at = seq(65, 120, by = 5), las=2)
axis(2, at = seq(5, 50, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 50, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(65, 120, by = 1),  lty = 2, col = "lightgrey")
centiles.fan(lms05g.wh,dat05g.wh$heightr,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylim=c(5,50),xlim=c(65,120),
             yaxt = "n", xaxt = "n",
             main="Fan-chart weight for height (girls, 18 to 60 months)",
             ylab="Weight (kg)", xlab="Height (cm)")
axis(1, at = seq(65, 120, by = 5), las=2)
axis(2, at = seq(5, 50, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 50, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(65, 120, by = 1),  lty = 2, col = "lightgrey")

#' ## Age 6-18 years (by year) 
#' ### Boys 
dat619b<-dat118[dat118$age>5 & dat118$sex %in% "Male",]
#' #### Height
dat619b.h<-na.omit(dat619b[, c("height","age.m","pid","datevisit","age","yearvisit")])
lms619b.h<-lms(height,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat619b.h,
        k=3,calibration=F, trans.x=T)
lms619b.h$family
lms619b.h$power
centiles(lms619b.h,dat619b.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Height (cm)", xlab="Age (month)",
         legend=FALSE,ylim=c(100,200),
         yaxt = "n", xaxt = "n",
         main="Centile curves height for age (boys, 5 to 18 years old)",
         points=FALSE, save = FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(100, 200, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(100, 200, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
centiles.fan(lms619b.h,dat619b.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             legend=FALSE,ylim=c(100,200),
             yaxt = "n", xaxt = "n",
             main="Fan-chart height for age (boys, 5 to 18 years old)",
             ylab="Height (cm)", xlab="Age (month)")
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(100, 200, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(100, 200, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")

#' #### Weight
dat619b.w<-na.omit(dat619b[, c("weight","age.m","pid","datevisit","age","yearvisit")])
lms619b.w<-lms(weight,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat619b.w,
        k=3,calibration=F, trans.x=T)
lms619b.w$family
lms619b.w$power
centiles(lms619b.w,dat619b.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Weight (kg)", xlab="Age (month)",
         legend=FALSE,ylim=c(10,150),
         yaxt = "n", xaxt = "n",
         main="Centile curves weight for age (boys, 5 to 18 years old)",
         points=FALSE, save = FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 150, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 150, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
centiles.fan(lms619b.w,dat619b.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Weight (kg)", xlab="Age (month)",
             yaxt = "n", xaxt = "n",ylim=c(10,150),
             main="Fan-chart weight for age (boys, 5 to 18 years old)",
             legend=FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 150, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 150, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")

#' #### BMI
dat619b.b<-na.omit(dat619b[, c("bmi","age.m","pid","datevisit","age","yearvisit")])
lms619b.b<-lms(bmi,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat619b.b,
        k=3,calibration=F, trans.x=T)
lms619b.b$family
lms619b.b$power
centiles(lms619b.b,dat619b.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="BMI (kg/m2)", xlab="Age (month)",
         legend=FALSE,ylim=c(10,40),
         yaxt = "n", xaxt = "n",
         main="Centile curves BMI for age (boys, 5 to 18 years old)",
         points=FALSE, save = FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 40, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 40, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
centiles.fan(lms619b.b,dat619b.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             xlab="Age (month)",
             ylab="BMI (kg/m2)", 
             legend=FALSE,ylim=c(10,40),
             yaxt = "n", xaxt = "n",
             main="Fan-chart BMI for age (boys, 5 to 18 years old)",
             points=FALSE, save = FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 40, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 40, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")

#' ### Girls
dat619g<-dat118[dat118$age>5 & dat118$sex %in% "Female",]
#' #### Height
dat619g.h<-na.omit(dat619g[, c("height","age.m","pid","datevisit","age","yearvisit")])
lms619g.h<-lms(height,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat619g.h,
        k=3,calibration=F, trans.x=T)
lms619g.h$family
lms619g.h$power
centiles(lms619g.h,dat619g.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Height (cm)", xlab="Age (month)",
         legend=FALSE,ylim=c(100,200),
         yaxt = "n", xaxt = "n",
         main="Centile curves height for age (girls, 5 to 18 years old)",
         points=FALSE, save = FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(100, 200, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(100, 200, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
centiles.fan(lms619g.h,dat619g.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Height (cm)", xlab="Age (month)",
             legend=FALSE,ylim=c(100,200),
             yaxt = "n", xaxt = "n",
             main="Fan-chart height for age (girls, 5 to 18 years old)",
             points=FALSE, save = FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(100, 200, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(100, 200, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")

#' #### Weight
dat619g.w<-na.omit(dat619g[, c("weight","age.m","pid","datevisit","age","yearvisit")])
lms619g.w<-lms(weight,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat619g.w,
        k=3,calibration=F, trans.x=T)
lms619g.w$family
lms619g.w$power
centiles(lms619g.w,dat619g.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="Weight (kg)", xlab="Age (month)",
         legend=FALSE,ylim=c(10,150),
         yaxt = "n", xaxt = "n",
         main="Centile curves weight for age (girls, 5 to 18 years old)",
         points=FALSE, save = FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 150, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 150, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
centiles.fan(lms619g.w,dat619g.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Weight (kg)", xlab="Age (month)",
             yaxt = "n", xaxt = "n",ylim=c(10,150),
             main="Fan-chart weight for age (girls, 5 to 18 years old)",
             legend=FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 150, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 150, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")

#' #### BMI
dat619g.b<-na.omit(dat619g[, c("bmi","age.m","pid","datevisit","age","yearvisit")])
lms619g.b<-lms(bmi,age.m,families=c("BCCGo","BCPEo","BCTo"),
        data=dat619g.b,
        k=3,calibration=F, trans.x=T)
lms619g.b$family
lms619g.b$power
centiles(lms619g.b,dat619g.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
         ylab="BMI (kg/m2)", xlab="Age (month)",
         legend=FALSE,ylim=c(10,40),
         yaxt = "n", xaxt = "n",
         main="Centile curves BMI for age (girls, 5 to 18 years old)",
         points=FALSE, save = FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 40, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 40, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
centiles.fan(lms619g.b,dat619g.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="BMI (kg/m2)", xlab="Age (month)",
             yaxt = "n", xaxt = "n",
             main="Fan-chart BMI for age (girls, 5 to 18 years old)",
             legend=FALSE,ylim=c(10,40))
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 40, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 40, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
#save lms model fit data for later use
#save(lms05b.h,lms05b.w,lms05b.b,lms05b.wh,
#     lms05g.h,lms05g.w,lms05g.b,lms05g.wh,
#     lms619b.h,lms619b.w,lms619b.b,
#     lms619g.h,lms619g.w,lms619g.b, 
#     file="C:/Users/ADMIN/Desktop/vsc2020.2023/lmsdat.rda")
#load("C:/Users/ADMIN/Desktop/vsc2020.2023/lmsdat.rda")

#ref: https://rpubs.com/Haibiostat/centile_estimation 


#load("C:/Users/ADMIN/Desktop/vsc2020.2023/lmsdat.rda")
# rerun dataset code
#get age 1 to 18
dat118<-dat[dat$age>=1.5 &dat$age<=18 &!is.na(dat$age),]
#age 1-5
dat05b<-dat118[dat118$age<=5 & dat118$sex %in% "Male",]
# #### Height
dat05b.h<-na.omit(dat05b[, c("height","age.m","pid","datevisit","age","yearvisit")])
#' #### Weight
dat05b.w<-na.omit(dat05b[, c("weight","age.m","pid","datevisit","age","yearvisit")])
# #### BMI
dat05b.b<-na.omit(dat05b[, c("bmi","age.m","pid","datevisit","age","yearvisit")])
# #### Weight for height
dat05b.wh<-na.omit(dat05b[, c("weight","height","pid","datevisit","age","yearvisit")])
dat05b.wh$heightr<-round(dat05b.wh$height,0)
# ### Girls
dat05g<-dat118[dat118$age<=5 & dat118$sex %in% "Female",]
# #### Height
dat05g.h<-na.omit(dat05g[, c("height","age.m","pid","datevisit","age","yearvisit")])
# #### Weight
dat05g.w<-na.omit(dat05g[, c("weight","age.m","pid","datevisit","age","yearvisit")])
# #### BMI
dat05g.b<-na.omit(dat05g[, c("bmi","age.m","pid","datevisit","age","yearvisit")])
# #### Weight for height
dat05g.wh<-na.omit(dat05g[, c("weight","height","pid","datevisit","age","yearvisit")])
dat05g.wh$heightr<-round(dat05g.wh$height,0)
# ## Age 6-18 years (by year) 
# ### Boys 
dat619b<-dat118[dat118$age>5 & dat118$sex %in% "Male",]
# #### Height
dat619b.h<-na.omit(dat619b[, c("height","age.m","pid","datevisit","age","yearvisit")])
# #### Weight
dat619b.w<-na.omit(dat619b[, c("weight","age.m","pid","datevisit","age","yearvisit")])
# #### BMI
dat619b.b<-na.omit(dat619b[, c("bmi","age.m","pid","datevisit","age","yearvisit")])
# ### Girls
dat619g<-dat118[dat118$age>5 & dat118$sex %in% "Female",]
# #### Height
dat619g.h<-na.omit(dat619g[, c("height","age.m","pid","datevisit","age","yearvisit")])
# #### Weight
dat619g.w<-na.omit(dat619g[, c("weight","age.m","pid","datevisit","age","yearvisit")])
# #### BMI
dat619g.b<-na.omit(dat619g[, c("bmi","age.m","pid","datevisit","age","yearvisit")])



#' ### Combined plot
#' Height
#+ fig.width=10, fig.height=7
par(mfrow=c(2,2))
##h5b
centiles.fan(lms05b.h,dat05b.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Height (cm)", xlab="Age (month)",ylim=c(70,130),
             yaxt = "n", xaxt = "n", 
             main="Males, 18 months to 60 months",
             points=FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(70, 130, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(70, 130, by = 2), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 2),  lty = 2, col = "lightgrey")
##h5b
centiles.fan(lms05g.h,dat05g.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Height (cm)", xlab="Age (month)",ylim=c(70,130),
             yaxt = "n", xaxt = "n", 
             main="Females, 18 months to 60 months",
             points=FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(70, 130, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(70, 130, by = 2), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 2),  lty = 2, col = "lightgrey")
##h18b
centiles.fan(lms619b.h,dat619b.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             legend=FALSE,ylim=c(100,200),
             yaxt = "n", xaxt = "n",
             main="Males, 5 years to 18 years old",
             ylab="Height (cm)", xlab="Age (month)")
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(100, 200, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(100, 200, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
##h18g
centiles.fan(lms619g.h,dat619g.h$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             legend=FALSE,ylim=c(100,200),
             yaxt = "n", xaxt = "n",
             main="Females, 5 years to 18 years old",
             ylab="Height (cm)", xlab="Age (month)")
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(100, 200, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(100, 200, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")


#' Weight
#+ fig.width=10, fig.height=7
par(mfrow=c(2,2))
##w5b
centiles.fan(lms05b.w,dat05b.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6),
             ylim=c(5,45),
             yaxt = "n", xaxt = "n",
             main="Males, 18 months to 60 months",
             ylab="Weight (kg)", xlab="Age (month)")
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 45, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 45, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
##w5g
centiles.fan(lms05g.w,dat05g.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Weight (kg)", xlab="Age (month)",
             legend=FALSE,ylim=c(5,45),
             yaxt = "n", xaxt = "n",
             main="Females, 18 months to 60 months",
             points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 45, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 45, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
##w18b
centiles.fan(lms619b.w,dat619b.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Weight (kg)", xlab="Age (month)",
             yaxt = "n", xaxt = "n",ylim=c(10,150),
             main="Males, 5 years to 18 years old",
             legend=FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 150, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 150, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
##w18g
centiles.fan(lms619g.w,dat619g.w$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="Weight (kg)", xlab="Age (month)",
             yaxt = "n", xaxt = "n",ylim=c(10,150),
             main="Females, 5 years to 18 years old",
             legend=FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 150, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 150, by = 5), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")


#' BMI
#+ fig.width=10, fig.height=7
par(mfrow=c(2,2))
##b5b
centiles.fan(lms05b.b,dat05b.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylim=c(5,35),
             yaxt = "n", xaxt = "n",
             main="Males, 18 months to 60 months",
             ylab="BMI (kg/m2)", xlab="Age (month)")
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 35, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 35, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
##b5g
centiles.fan(lms05g.b,dat05g.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="BMI (kg/m2)", xlab="Age (month)",
             ylim=c(5,35),
             yaxt = "n", xaxt = "n",
             main="Females, 18 months to 60 months",
             legend=FALSE,points=FALSE, save = FALSE)
axis(1, at = seq(16, 60, by = 2), las=2)
axis(2, at = seq(5, 35, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 35, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(16, 60, by = 1),  lty = 2, col = "lightgrey")
##b18b
centiles.fan(lms619b.b,dat619b.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             xlab="Age (month)",
             ylab="BMI (kg/m2)", 
             legend=FALSE,ylim=c(10,40),
             yaxt = "n", xaxt = "n",
             main="Males, 5 years to 18 years old",
             points=FALSE, save = FALSE)
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 40, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 40, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")
##b18g
centiles.fan(lms619g.b,dat619g.b$age.m,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylab="BMI (kg/m2)", xlab="Age (month)",
             yaxt = "n", xaxt = "n",
             main="Females, 5 years to 18 years old",
             legend=FALSE,ylim=c(10,40))
axis(1, at = seq(60, 228, by = 6), las=2)
axis(2, at = seq(10, 40, by = 5), las=2)
#Add horizontal grid  
abline(h =seq(10, 40, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(60, 228, by = 6),  lty = 2, col = "lightgrey")


#' WFH
#+ fig.width=10, fig.height=4
par(mfrow=c(1,2))
## wh5b
centiles.fan(lms05b.wh,dat05b.wh$heightr,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylim=c(5,50),xlim=c(65,120),
             yaxt = "n", xaxt = "n",
             main="Males, 18 months to 60 months",
             ylab="Weight (kg)", xlab="Height (cm)")
axis(1, at = seq(65, 120, by = 5), las=2)
axis(2, at = seq(5, 50, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 50, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(65, 120, by = 1),  lty = 2, col = "lightgrey")
## wh5g
centiles.fan(lms05g.wh,dat05g.wh$heightr,cent=c(0.4,2,10,25,50,75,90,98,99.6), 
             ylim=c(5,50),xlim=c(65,120),
             yaxt = "n", xaxt = "n",
             main="Females, 18 months to 60 months",
             ylab="Weight (kg)", xlab="Height (cm)")
axis(1, at = seq(65, 120, by = 5), las=2)
axis(2, at = seq(5, 50, by = 5), las=2)
#Add horizontal grid  
abline(h = seq(5, 50, by = 1), lty = 2, col = "lightgrey")
#Add vertical grid
abline(v = seq(65, 120, by = 1),  lty = 2, col = "lightgrey")

par(mfrow=c(1,1))