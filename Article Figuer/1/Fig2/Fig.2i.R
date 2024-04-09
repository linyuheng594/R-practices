data <- read.table("six_ind.vir_tem.sample_votu_num",header = T)
library(ggplot2)

data$group <- factor(data$group,levels=c("A1-Vir","A1-Tem","A2-Vir","A2-Tem","A3-Vir","A3-Tem",
                                         "A4-Vir","A4-Tem","B1-Vir","B1-Tem","B2-Vir","B2-Tem"))
ggplot(data)+
  stat_boxplot(geom = "errorbar",width=0.3,aes(group,votu,color=group),position = position_dodge(1))+
  geom_boxplot(aes(group,votu,color=group,fill=group), width=0.5,position = position_dodge(1),outlier.shape = NA)+
  scale_fill_manual(values=c(rgb(155,177,193,max=255),rgb(252,192,113,max=255),
                             rgb(155,177,193,max=255),rgb(252,192,113,max=255),
                             rgb(155,177,193,max=255),rgb(252,192,113,max=255),
                             rgb(155,177,193,max=255),rgb(252,192,113,max=255),
                             rgb(155,177,193,max=255),rgb(252,192,113,max=255),
                             rgb(155,177,193,max=255),rgb(252,192,113,max=255)))+
  scale_color_manual(values=c(rgb(0,0,0,max=255),rgb(0,0,0,max=255),rgb(0,0,0,max=255),
                              rgb(0,0,0,max=255),rgb(0,0,0,max=255),rgb(0,0,0,max=255),
                              rgb(0,0,0,max=255),rgb(0,0,0,max=255),rgb(0,0,0,max=255),
                              rgb(0,0,0,max=255),rgb(0,0,0,max=255),rgb(0,0,0,max=255)))+
  labs(y="Number of vOTUs")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 1,angle = 45,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+expand_limits(y=300)+
  scale_y_continuous(limits = c(300,1800),breaks=seq(300,1800,300))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(300,1800))
