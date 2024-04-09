data <- read.table("vm_tm.shared_specific.votu_num",header = T)
library(reshape2)
library(ggplot2)

data$group <- factor(data$group,levels=c("vm-specific","vm-shared","tm-specific","tm-shared"))
ggplot(data)+
  stat_boxplot(geom = "errorbar",width=0.3,aes(group,votu,color=group),position = position_dodge(1))+
  geom_boxplot(aes(group,votu,color=group,fill=group), width=0.5,position = position_dodge(1),outlier.shape = NA)+
  scale_fill_manual(values=c(rgb(89,161,79,max=255),rgb(220,220,220,max=255),rgb(210,160,190,max=255),rgb(220,220,220,max=255)))+
  scale_color_manual(values=c(rgb(0,0,0,max=255),rgb(0,0,0,max=255),rgb(0,0,0,max=255),rgb(0,0,0,max=255)))+
  labs(y="Number of vOTUs")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=10),axis.title.y = element_text(size=10),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=10,colour = 'black'),
        axis.text.y=element_text(size=10,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+expand_limits(y=250)+
  scale_y_continuous(limits = c(250,1000),breaks=seq(250,1000,250))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(250,1000))
