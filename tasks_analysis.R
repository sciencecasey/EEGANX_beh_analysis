#######Created for Analyzing EPrime data o
#2019 EEG ANX study
##Casey 09/23/2019

##Import Data
inputdir='/Users/casey/Desktop/EEG_ANX/data/source/eprime'
hfiles=list.files(path='/Users/casey/Desktop/EEG_ANX/data/source/eprime',
                 pattern="*houses(.*)csv$", recursive = T, full.names = TRUE)
hlist=lapply(hfiles, read.csv)
hdf=do.call(rbind, hlist)
str(hdf)
bfiles=list.files(path='/Users/casey/Desktop/EEG_ANX/data/source/eprime',
                  pattern="*bodies(.*)csv$", recursive = T, full.names = TRUE)
blist=lapply(bfiles, read.csv)
bdf=do.call(rbind, blist)
str(bdf)
outputdir='/Users/casey/Desktop/EEG_ANX/data/derivatives/emoval'
numsubs=4 #total number subjects
numses=1 #total number of sessions

#Make the colnames match between dfs
bdf$Type=bdf$CellLabel
bdf$CellLabel=as.character(bdf$CellLabel)
bdf=subset(bdf, select=-CellLabel)
str(bdf)
taskdf=rbind(hdf, bdf)
str(taskdf)

####DATA CLEANING DATASET SPECIFIC###
count(taskdf$Session[taskdf$Session==22])
count(taskdf$Session[taskdf$Session==2])
taskdf$Session[taskdf$Session==22]=2
count(taskdf$Session[taskdf$Session==22]) #check that above changed the 22 sessions to 2
count(taskdf$Session[taskdf$Session==2])

#Recode the Stimulus RT 0 as NA RT and ACC
sum(is.na(taskdf$Stimulus.RT)) #these NAs are rest periods in the task
sum(is.na(taskdf$Stimulus.ACC)) #These NA are rest periods in the task
sum(taskdf$Stimulus.RT==0, na.rm = TRUE) #these are the stimuli without responses to be recoded
sum(taskdf$Stimulus.ACC==0, na.rm=TRUE) 
is.na(taskdf$Stimulus.RT)=taskdf$Stimulus.RT==0 #recode RT 0 to NA
is.na(taskdf$Stimulus.ACC)=is.na(taskdf$Stimulus.RT) #recode the NA RT to NA acc
sum(taskdf$Stimulus.AC==0, na.rm = TRUE) #should be lower than before by the amount of RT=0 before

#Add a Group column
group=taskdf$Subject
group=ifelse(grepl("^4", group), "anx", "con")
taskdf$Group=group
tail(taskdf$Group)
#index=grepl(x=taskdf$Subject, pattern="^4")
#if(index==TRUE){taskdf$Group="anx"} else {taskdf$Group="con"}

#Add a logRT column
taskdf$logRT=log(taskdf$Stimulus.RT)

#Add a Task Column
task=taskdf$ExperimentName
task=ifelse(grepl("^Houses", task), "houses", "bodies")
taskdf$task=task

#Anova for RT
#By Group
g=lmer(logRT~Group+(1|Subject), data=taskdf)
summary(g)
emmeans(g, "Group")
emmip(g, ~Group, CIs=TRUE)
#By Group and Stim Type
gty=lmer(logRT~Group+Type+(1|Subject), data=taskdf)
summary(gty)
emmeans(gty, specs=c("Group", "Type"))
#By Group and TaskType
gta=lmer(logRT~Group+task+(1|Subject), data=taskdf)
summary(gta)
emmeans(gta, specs=c("Group", "task"))
plot(emmeans(gta, specs=c("Group", "task")), xlab="Response Time", main="Response Time by Group and Task")
#emmip(gta, ~Group+task, CIs=TRUE)

