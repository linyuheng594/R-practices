data <- read.table('phage.genome_feature',header = T)

#votu ratio
library(dplyr)
data2 <- distinct(data[,c(5,6,10,12)])
new <- aggregate(data2$repseq,by=list(data2$sample,data2$method,data2$lifestyle),FUN=length)
new$x <- as.numeric(as.character(new$x))
group <- paste(new$Group.3,new$Group.2)

new$group <- group
new$group <- factor(new$group,levels = c(unique(new$group)))


seq_num <- aggregate(data2$repseq,by=list(data2$sample,data2$method),FUN=length)
seq_num$x <- as.numeric(as.character(seq_num$x))

new2 <- rbind(seq_num,seq_num)

df <- data.frame(new,new2)
df <- df[,c(-2,-6,-7)]

colnames(df) <- c("sample","lifestyle","num","group","total_seq_num")
ratio <- as.numeric(df$num)/as.numeric(df$total_seq_num)
df <- data.frame(df,ratio)

df$group <- factor(df$group,levels=c("virulent virome","virulent meta","temperate virome","temperate meta"))
ggplot(df)+
  stat_boxplot(geom = "errorbar",width=0.3,aes(group,ratio,color=group),position = position_dodge(1))+
  geom_boxplot(aes(group,ratio,color=group,fill=group), width=0.5,position = position_dodge(1),outlier.shape = NA)+
  scale_fill_manual(values=c(rgb(155,177,193,max=255),rgb(155,177,193,max=255),
                             rgb(252,192,113,max=255),rgb(252,192,113,max=255)))+
  scale_color_manual(values=c(rgb(0,0,0,max=255),rgb(0,0,0,max=255),
                              rgb(0,0,0,max=255),rgb(0,0,0,max=255)))+
  labs(y="Ratio")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=12,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(linewidth =0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(0,1))
