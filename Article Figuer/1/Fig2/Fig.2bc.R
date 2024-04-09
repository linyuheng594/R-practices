library(ggside)

#dot and density plot for the rela and prevalence of vm-shared and vm-specific
all <- read.table('vm.genome_shared_specific.rela_preva',header = T)

all <- all[all$all.rel>0,]
wilcox.test(all$all.rel~all$group)
wilcox.test(all$all.pre~all$group)

all.rela.log <- log10(all$all.rel)
all$rela.log <- all.rela.log
all$group <- factor(all$group,levels = c("VM-shared","VM-specific"))
ggplot(all,aes(rela.log,all.pre))+
  geom_point(size=2,aes(color=group))+
  geom_xsidedensity(aes(y = after_stat(density),color = group))+
  geom_ysidedensity(aes(x = after_stat(density),color = group))+
  labs(x="Relative abundance",y="Prevalence")+
  scale_color_manual(values=c(rgb(220,220,220,max=255),rgb(89,161,79,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=12),axis.title.y = element_text(size=12),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=10,colour = 'black'),
        axis.text.y=element_text(size=10,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')


#dot and density plot for the rela and prevalence of tm-shared and tm-specific
all <- read.table('tm.genome_shared_specific.rela_preva',header = T)

all <- all[all$all.rel>0,]
wilcox.test(all$all.rel~all$group)
wilcox.test(all$all.pre~all$group)

all.rela.log <- log10(all$all.rel)
all$rela.log <- all.rela.log
all$group <- factor(all$group,levels = c("TM-shared","TM-specific"))
ggplot(all,aes(rela.log,all.pre))+
  geom_point(size=2,aes(color=group))+
  geom_xsidedensity(aes(y = after_stat(density),color = group))+
  geom_ysidedensity(aes(x = after_stat(density),color = group))+
  labs(x="Relative abundance",y="Prevalence")+
  scale_color_manual(values=c(rgb(220,220,220,max=255),rgb(210,160,190,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=12),axis.title.y = element_text(size=12),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=10,colour = 'black'),
        axis.text.y=element_text(size=10,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')
