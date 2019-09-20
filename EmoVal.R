#######Created for Analyzing Enotional Valence data 
#for 2019 EEG_ANX experiment, houses, bodies, somatomap (rest as calibration)
##Casey 09/17/2019

##Use the NOHEADER somatomap .txt files to format with task in 

##Import Data
inputdir='/Users/casey/Desktop/EEG_ANX/data/source/emoval'
files=list.files(path='/Users/casey/Desktop/EEG_ANX/data/source/emoval',
                 pattern="*somatomap_NOheader(.*)csv$", recursive = T, full.names = TRUE)
emolist=lapply(files, read.csv)
emodf=do.call(rbind, emolist)
str(emodf)
outputdir='/Users/casey/Desktop/EEG_ANX/data/derivatives/emoval'
numsubs=4 #total number subjects
numses=1 #total number of sessions

##fix labels ###########DATASETSPECIFIC###############
emodf$Sub=emodf$Participant.Name
emodf$Sub=as.character(emodf$Sub) #make sure not a factor in order to change w/o NAs
emodf$Sub[emodf$Sub=="sub-1-1"]="0401"
emodf$Sub[emodf$Sub=="sub-4041"]="4041"
emodf$Sub[emodf$Sub=="sub-6033"]="6033"
emodf$Sub[emodf$Sub=="sub-6037"]="6037"

#add column for group
emodf$group= rep("temp", length(emodf$Sub))
emodf$group=ifelse(emodf$Sub=="4041", "anx", 
                   ifelse(emodf$Sub=="0401", "anx", 
                          ifelse(emodf$Sub=="6033", "con",
                                 ifelse(emodf$Sub=="6037", "con", NA))))

#add column for task
emodf$Task=rep("somatomap", length(emodf$Sub))

#subset, select, and factor relevant portions
emodf=subset(emodf, select=c(Sub, group, Task, Video.Time, Valence))
emodf=within(emodf, {
  Sub=factor(Sub)
  group=factor(group)
  Task=factor(Task)
  Valence=as.numeric(as.character(Valence))
  #Valence=as.numeric(levels(Valence))[Valence]
  #Video.Time=as.numeric(as.character(Video.Time))
  #because not saved as NA, need to convert from 
  #factor to character then numeric in order to force out NAs
})
sum(is.na(emodf$Valence))
sum(!is.na(emodf$Valence))
levels(emodf$Sub)
levels(emodf$group)

####add row for MS conversion
library(lubridate)
emodf$Time=ms(emodf$Video.Time)
#export as large tsv
write_tsv(emodf, file.path(outputdir, "task-somatomap_ses-1_dir-emoval_beh.tsv"))

#######Looking at the entire session

#identify functions of interest
funs=list(mean=~mean(., na.rm=TRUE), sd=~sd(., na.rm=TRUE))

##Simple Statistics
temp1= emodf %>%
  group_by(Sub, group) %>%
  summarise_at("Valence", funs)
temp2=emodf %>%
  group_by(group) %>%
  summarise_at("Valence", funs)
Alltime_Estats=bind_rows(temp1, temp2)
Alltime_Estats$Sub=ifelse(is.na(Alltime_Estats$Sub), "Overall", 
                          Alltime_Estats$Sub)
rm(temp2)

#print output as df
write_tsv(Alltime_Estats, file.path(outputdir, "task-somatomap_ses-1_acq-alltime_dir_emoval-stats-meansd.tsv"))

##T Test and GLMM
t.test(mean~group, alternative="less", data=temp1, na.rm=TRUE,
       paired=TRUE)
cohensD(mean~group, data=temp1)
#rm(temp1)

summary(lmer(Valence~group+(1+group|Sub), data=emodf, REML=FALSE, control=lmerControl(optimizer="Nelder_Mead")))
#Note, using a Nelder Mead Model instead of REML (standard) due to fit warnings with restricted max likelihood
##With more participants check the result with the following as well:::
##summary(lmer(Valence~group+(1+group|Sub), data=emodf))

#Plot
xyplot(mean~group, temp1, groups = Sub, type='p', cex=2,
       par.settings=ggplot2like(),axis=axis.grid, auto.key = TRUE, 
       main="Average Valence by Subject", ylab="Average Valence") 
