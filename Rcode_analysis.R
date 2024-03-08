#################

# load packages

rm(list=ls())

#data manipulation packages
library(readxl)
library(dplyr) 
library(plotrix)

#modelling library
library(car) 
library(lme4)

#graphical packages
library(ggplot2) 
library(ggpubr)

###########################

# set directory by changing the path
setwd("path")
getwd()

##########################

# Loading data from "Dataset" .xlsx
# Sheet 2 = behavioural paramenter describing response of the larvae to the open-field test
# Sheet 3 = behavioural response to repeated stimulation during the habituation learning test
# Sheet 4 = average individual size (i.e., total length)

# The variable "animal_id": idetifaction code used for identifying each subject.
# Due to the repeated measures structure of data (within-subject observation), we consider the individual as random factor in the linear mixed-effect model

# First analysis: open-field test
data_OF<-data.frame(read_excel("./Dataset.xlsx",sheet=2))

str(data_OF)
data_OF$treatment<-as.factor(data_OF$treatment)

data_OF %>% 
  group_by(treatment) %>% 
  summarise(animal_id = length(unique(animal_id)))

# analyze "distance covered" parameter via lmer
m_distance<-(lmer(distance_covered~treatment*block+(1|animal_id),data=data_OF,na.action=na.omit))
qqPlot(residuals(m_distance)) #diagnostic plot

summary(m_distance)
Anova(m_distance)

# analyze "number of inspection" parameter via lmer
m_inspection<-(lmer(inspection_center_log~treatment*block+(1|animal_id),data=data_OF,na.action=na.omit))
hist(residuals(m_inspection))
shapiro.test(residuals(m_inspection))
qqPlot(residuals(m_inspection))

summary(m_inspection)
Anova(m_inspection)

######

# Second analysis: habituation learning test
data_HL<-data.frame(read_excel("./Dataset.xlsx",sheet=3))

str(data_HL)
data_HL$treatment<-as.factor(data_HL$treatment)

data_HL %>% 
  group_by(treatment) %>% 
  summarise(animal_id = length(unique(animal_id)))


# analyze overall habituation learning performance
leveneTest(mean_log_index~treatment,data=data_HL)
hist(data_HL$mean_log_index)
shapiro.test(data_HL$mean_log_index)
t.test(mean_log_index~treatment,data=data_HL,var.equal=TRUE)

# analyze temporal trend of habituation learning performance across repeated stimulation
data_HL$time_log<-log(data_HL$time+1)
m_indexHL<-(lmer(index_log~treatment*time_log+(1|animal_id),data=data_HL,na.action=na.omit))
qqPlot(residuals(m_indexHL))

summary(m_indexHL)
Anova(m_indexHL)


######

# Third analysis: average total length
data_TL<-data.frame(read_excel("./Dataset.xlsx",sheet=4))

str(data_TL)
data_TL$treatment<-as.factor(data_TL$treatment)

data_TL %>% 
  group_by(treatment) %>% 
  summarise(animal_id = length(unique(animal_id)))


# analyze overall habituation learning performance
tapply(data_TL$average_total_length,data_TL$treatment,mean)
tapply(data_TL$average_total_length,data_TL$treatment,sd)

leveneTest(average_total_length~treatment,data=data_TL)
hist(data_TL$average_total_length)
shapiro.test(data_TL$average_total_length)
t.test(average_total_length~treatment,data=data_TL,var.equal=TRUE)


#######################

# Figure 2

# Generating data for the graph

# Panel A: distance covered across the open-field test using the "data_OF" (sheet = 2 in .xlsx file)
ddof<-data_OF[!is.na(data_OF$distance_covered),]

av_distance<-tapply(ddof$distance_covered,interaction(ddof$block,ddof$treatment),mean,na.rm=TRUE)
se_dist<-tapply(ddof$distance_covered,interaction(ddof$block,ddof$treatment),std.error,na.rm=TRUE)
block<-rep(seq(1:6),2)
treatment<-c(rep("EE",6),rep("ST",6))
Datagraph_datadist<-data.frame(cbind(dist=av_distance,se_dist=se_dist,block,treatment))
Datagraph_datadist$dist<-as.numeric(Datagraph_datadist$dist)
Datagraph_datadist$se_dist<-as.numeric(Datagraph_datadist$se_dist)
Datagraph_datadist$block<-as.numeric(Datagraph_datadist$block)
Datagraph_datadist$treatment<-as.factor(Datagraph_datadist$treatment)

OF_distance_graph<- ggplot(Datagraph_datadist, aes(x=block, y=dist,colour=treatment)) + 
  geom_line(linewidth=1.8,position = position_dodge(width = 0.9),show.legend = F)+
  geom_point(size=3,position = position_dodge(width = 0.9),show.legend = F)+
  geom_errorbar(aes(ymin=dist-se_dist, ymax=dist+se_dist),width=0.4,linewidth=1,position = position_dodge(width = 0.9))+
  theme_classic()+
  ylab("distance covered (cm)")+xlab("block")+
  theme(strip.background = element_rect(color="black",linewidth=1.5, linetype="blank"))+
  scale_colour_manual(values=c('red','grey40'))+
  theme(legend.title=element_blank())+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.spacing.y = unit(1.0, 'cm'),
    legend.text=element_text(size=10))+
  guides(colour = guide_legend(byrow = TRUE))+
  scale_x_continuous("block", labels = as.character(block), breaks = block)
OF_distance_graph



# Panel B: number of inspection towards the center across the open-field test using the "data_OF" (sheet = 2 in .xlsx file)
av_inspection<-tapply(ddof$inspection_center_log,interaction(ddof$block,ddof$treatment),mean,na.rm=TRUE)
se_inspection<-tapply(ddof$inspection_center_log,interaction(ddof$block,ddof$treatment),std.error,na.rm=TRUE)
block<-rep(seq(1:6),2)
treatment<-c(rep("EE",6),rep("ST",6))
Datagraph_datafc<-data.frame(cbind(ins=av_inspection,se_ins=se_inspection,block,treatment))
Datagraph_datafc$ins<-as.numeric(Datagraph_datafc$ins)
Datagraph_datafc$se_ins<-as.numeric(Datagraph_datafc$se_ins)
Datagraph_datafc$block<-as.numeric(Datagraph_datafc$block)
Datagraph_datafc$treatment<-as.factor(Datagraph_datafc$treatment)

OF_inspection_graph<- ggplot(Datagraph_datafc, aes(x=block, y=ins,colour=treatment)) + 
  geom_line(linewidth=1.8,position = position_dodge(width = 0.9),show.legend = F)+
  geom_point(size=3,position = position_dodge(width = 0.9),show.legend = F)+
  geom_errorbar(aes(ymin=ins-se_ins, ymax=ins+se_ins),width=0.4,linewidth=1,position = position_dodge(width = 0.9))+
  theme_classic()+
  ylab("inspection towards the central sector (log)")+xlab("block")+
  theme(strip.background = element_rect(color="black",linewidth=1.5, linetype="blank"))+
  scale_colour_manual(values=c('red','grey40'))+
  theme(legend.title=element_blank())+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.spacing.y = unit(1.0, 'cm'),
    legend.text=element_text(size=10))+
  guides(colour = guide_legend(byrow = TRUE))+
  scale_x_continuous("block", labels = as.character(block), breaks = block)
OF_inspection_graph


# Panel C: overall performance in the habituation learning assay using the "data_HL" (sheet = 3 in .xlsx file)
ddhl<-data_HL[!is.na(data_HL$mean_log_index),]
Averange_indexHL_graph<- ggplot(ddhl, aes(x=treatment, y=mean_log_index,
                             colour=treatment))+
  ggdist::stat_halfeye(
    aes(fill = treatment, fill = after_scale(colorspace::lighten(fill, .7)))
  )+

  theme_classic()+
  ylab("habituation learning index (log)")+xlab("treatment")+
  theme(strip.background = element_rect(color="black",linewidth=1.5, linetype="blank"))+
  scale_fill_manual(values=c('red','grey40'))+
  scale_colour_manual(values=c('red','grey40'))+
  theme(legend.title=element_blank())
Averange_indexHL_graph


# Panel D: temporal trend of index across the habituation learning assay using the "data_HL" (sheet = 3 in .xlsx file)
index_HL<-tapply(data_HL$index_log,interaction(data_HL$time_log,data_HL$treatment),mean,na.rm=TRUE)
se_index<-tapply(data_HL$index_log,interaction(data_HL$time_log,data_HL$treatment),std.error,na.rm=TRUE)
block<-rep(seq(1:25),2)
treatment<-c(rep("EE",25),rep("ST",25))
Datagraph_indexT<-data.frame(cbind(index=index_HL,se_index=se_index,block,treatment))
Datagraph_indexT$index<-as.numeric(Datagraph_indexT$index)
Datagraph_indexT$se_index<-as.numeric(Datagraph_indexT$se_index)
Datagraph_indexT$block<-as.numeric(Datagraph_indexT$block)
Datagraph_indexT$treatment<-as.factor(Datagraph_indexT$treatment)

graph_indexHL_trend<- ggplot(Datagraph_indexT, aes(x=block, y=index,colour=treatment)) + 
  geom_line(linewidth=1.8,position = position_dodge(width = 0.9),show.legend = F)+
  geom_point(size=3,position = position_dodge(width = 0.9),show.legend = F)+
  geom_errorbar(aes(ymin=index-se_index, ymax=index+se_index),width=0.4,linewidth=1,position = position_dodge(width = 0.9),show.legend = F)+
  theme_classic()+
  ylab("habituation learning index (log)")+xlab("stimulation")+
  theme(strip.background = element_rect(color="black",linewidth=1.5, linetype="blank"))+
  scale_colour_manual(values=c('red','grey40'))+
  theme(legend.title=element_blank())+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.spacing.y = unit(1.0, 'cm'),
    legend.text=element_text(size=10))

graph_indexHL_trend




#Arrange all graph into an unique figure

ggarrange(OF_distance_graph,OF_inspection_graph,
          Averange_indexHL_graph,graph_indexHL_trend,
          nrow=2,ncol=2,labels = c("a", "b","c", "d"),
          legend = "none")+ bgcolor("white")+ border("white") 
ggsave("Figure 2.tiff",width=10,height=10)

