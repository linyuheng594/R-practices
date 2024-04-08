data <-read.table('phage.genome_feature',header = T)
new_table <- aggregate(data$seq,by=list(data$group,data$library),FUN=length)

new_table$Group.1 <- factor(new_table$Group.1,levels = c("A1-virome","A1-meta",
                                                     "A2-virome","A2-meta",
                                                     "A3-virome","A3-meta",
                                                     "A4-virome","A4-meta",
                                                     "B1-virome","B1-meta",
                                                     "B2-virome","B2-meta"))
ggplot(new_table)+
  stat_boxplot(geom = "errorbar",width=0.3,aes(Group.1,x,color=Group.1),position = position_dodge(1))+
  geom_boxplot(aes(Group.1,x,color=Group.1), width=0.5,position = position_dodge(1),outlier.shape = NA)+
  scale_color_manual(values=c(rgb(89,161,79,max=255),rgb(210,160,190,max=255),
                             rgb(89,161,79,max=255),rgb(210,160,190,max=255),
                             rgb(89,161,79,max=255),rgb(210,160,190,max=255),
                             rgb(89,161,79,max=255),rgb(210,160,190,max=255),
                             rgb(89,161,79,max=255),rgb(210,160,190,max=255),
                             rgb(89,161,79,max=255),rgb(210,160,190,max=255)))+
  scale_fill_manual(values=c(rgb(255,255,255,max=255),rgb(255,255,255,max=255),rgb(255,255,255,max=255),
                              rgb(255,255,255,max=255),rgb(255,255,255,max=255),rgb(255,255,255,max=255),
                              rgb(255,255,255,max=255),rgb(255,255,255,max=255),rgb(255,255,255,max=255),
                              rgb(255,255,255,max=255),rgb(255,255,255,max=255),rgb(255,255,255,max=255)))+
  geom_jitter(aes(Group.1,x,color=Group.1),shape = 16,width = 0.2,alpha = 1)+
  labs(x = "Gibbon individuals",y="Number of genomes")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=4.5,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')

a1 <- subset(new_table,Group.1 == "A1-virome"|Group.1 == "A1-meta",select=c(Group.1,x))
a2 <- subset(new_table,Group.1 == "A2-virome"|Group.1 == "A2-meta",select=c(Group.1,x))
a3 <- subset(new_table,Group.1 == "A3-virome"|Group.1 == "A3-meta",select=c(Group.1,x))
a4 <- subset(new_table,Group.1 == "A4-virome"|Group.1 == "A4-meta",select=c(Group.1,x))
b1 <- subset(new_table,Group.1 == "B1-virome"|Group.1 == "B1-meta",select=c(Group.1,x))
b2 <- subset(new_table,Group.1 == "B2-virome"|Group.1 == "B2-meta",select=c(Group.1,x))

wilcox.test(a1$x~a1$Group.1)
wilcox.test(a2$x~a2$Group.1)
wilcox.test(a3$x~a3$Group.1)
wilcox.test(a4$x~a4$Group.1)
wilcox.test(b1$x~b1$Group.1)
wilcox.test(b2$x~b2$Group.1)

