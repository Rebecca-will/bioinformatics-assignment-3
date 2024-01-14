library(dplyr)
library(tidyverse)
library(ggplot2)
assignment_data=read.csv("Assignment_dataset (2).csv")
attach(assignment_data)
library(sjstats)
library(pwr)
library(effsize)

## txt arms
Treatment_Arm_traz<-(Treatment_Arm[49:88])
IL1B_pre_Tras<-(IL1B_pre[49:88])
IL1B_post_Tras<-(IL1B_post[49:88])

shapiro.test(IL1B_pre_Tras)
shapiro.test(IL1B_post_Tras)
wilcox.test(IL1B_pre_Tras,IL1B_post_Tras, paired = TRUE)


shapiro.test(IL1B_pre_lap)
shapiro.test(IL1B_post_lap)
wilcox.test(IL1B_pre_lap,IL1B_post_lap)




IL1B_pre_lap<-(IL1B_pre[1:48])
IL1B_post_lap<-(IL1B_post[1:48])



CX3CL1_pre_Tras<-(CX3CL1_pre[49:88])
CX3CL1_post_Tras<-(CX3CL1_post[49:88])
shapiro.test(CX3CL1_pre_Tras)
shapiro.test(CX3CL1_post_Tras)
wilcox.test(CX3CL1_post_Tras,CX3CL1_pre_Tras)
###not significant->calculate power. 

CX3CL1_pre_lap<-(CX3CL1_pre[1:48])
CX3CL1_post_lap<-(CX3CL1_post[1:48])
shapiro.test(CX3CL1_pre_lap)
shapiro.test(CX3CL1_post_lap)
wilcox.test(CX3CL1_post_lap,CX3CL1_pre_lap, paired = TRUE)


TNFA_pre_Tras<-(TNFA_pre[49:88])
TNFA_post_Tras<-(TNFA_post[49:88])
shapiro.test(TNFA_pre_Tras)
shapiro.test(TNFA_post_Tras)
wilcox.test(TNFA_post_Tras,TNFA_pre_Tras)
## do power test 

TNFA_pre_lap<-(TNFA_pre[1:48])
TNFA_post_lap<-(TNFA_post[1:48])
shapiro.test(TNFA_pre_lap)
shapiro.test(TNFA_post_lap)
wilcox.test(TNFA_post_lap,TNFA_pre_lap)
##do power test 

CCL20_pre_Tras<-(CCL20_pre[49:88])
CCL20_post_Tras<-(CCL20_post[49:88])
shapiro.test(CCL20_pre_Tras)
shapiro.test(CCL20_post_Tras)
wilcox.test(CCL20_post_Tras,CCL20_pre_Tras)




CCL20_pre_lap<-(CCL20_pre[1:48])
CCL20_post_lap<-(CCL20_post[1:48])
shapiro.test(CCL20_pre_lap)
shapiro.test(CCL20_post_lap)
wilcox.test(CCL20_post_lap,CCL20_pre_lap)


CX3CL1_pre_Tras<-(CX3CL1_pre[49:88])
CX3CL1_pre_lap<-(CX3CL1_pre[1:48])
TNFA_pre_Tras<-(TNFA_pre[49:88])
TNFA_pre_lap<-(TNFA_pre[1:48])
CCL20_pre_Tras<-(CCL20_pre[49:88])
CCL20_pre_lap<-(CCL20_pre[1:48])

Pre_Data<- assignment_data%>%
  select(IL1B_pre,CX3CL1_pre,TNFA_pre,CCL20_pre,Patient_number,Treatment_Arm)

##tests


d<-cohen.d(CX3CL1_pre_lap,CX3CL1_pre_Tras)
d
var(CX3CL1_pre_lap)
var(CX3CL1_pre_Tras)
pwr.t.test( d= -0.3758389,n=NULL, sig.level =0.05 , power =0.8 )

pwr.t.test(d= NULL,n=113, sig.level =0.05 , power =0.8)


d1<-cohen.d(TNFA_pre_lap,TNFA_pre_Tras)
d1
pwr.t.test(d=0.251839,sig.level = 0.05,power=0.8)

d2<-cohen.d(CCL20_pre_lap,CCL20_pre_Tras)
d2
pwr.t.test(d=-0.2623667,sig.level = 0.05,power=0.8)
var(CCL20_pre_lap)
var(CCL20_pre_Tras)


####

IL1B_post_Tras<-(IL1B_pre[49:88])
IL1B_post_lap<-(IL1B_pre[1:48])


shapiro.test(IL1B_pre_lap)
shapiro.test(IL1B_post_lap)
##normal distribution 
var.test(IL1B_pre_lap,IL1B_post_lap)
##no variance 
t.test(IL1B_pre_lap,IL1B_post_lap,paired = TRUE)



#Power tests for non significant before and after txt. 
#TNFA lap ,CX3CL1 tras