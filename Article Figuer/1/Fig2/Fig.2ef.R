data<-read.table('vm_tm.124overlap_sample.votu_abun',header = T)

data_a1<-data[which(data$ind == "A1"),]
data_a2<-data[which(data$ind == "A2"),]
data_a3<-data[which(data$ind == "A3"),]
data_a4<-data[which(data$ind == "A4"),]
data_b1<-data[which(data$ind == "B1"),]
data_b2<-data[which(data$ind == "B2"),]

cor.test(data$vm.votu,data$tm.votu)
cor.test(data_a1$vm.votu,data_a1$tm.votu)
cor.test(data_a2$vm.votu,data_a2$tm.votu)
cor.test(data_a3$vm.votu,data_a3$tm.votu)
cor.test(data_a4$vm.votu,data_a4$tm.votu)
cor.test(data_b1$vm.votu,data_b1$tm.votu)
cor.test(data_b2$vm.votu,data_b2$tm.votu)

ggplot(data)+
  geom_point(aes(vm.votu,tm.votu,color=ind),size=2)+
  geom_smooth(aes(vm.votu,tm.votu),method='lm',fill=NA,color = rgb(0,0,0,max=255))+
  labs(x="Number of VM-vOTUs",y="Number of TM-vOTUs")+
  scale_color_manual(values=c("#FCC071","#D21626","#D2A0BE",
                              "#C67171","#9DB1C1","#236EB0"))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=600,y=600)+
  scale_x_continuous(limits = c(600,1500),breaks = seq(600,1500,300))+
  scale_y_continuous(limits = c(600,1500),breaks = seq(600,1500,300))  

cor.test(data$vm.abun,data$tm.abun)
cor.test(data_a1$vm.abun,data_a1$tm.abun)
cor.test(data_a2$vm.abun,data_a2$tm.abun)
cor.test(data_a3$vm.abun,data_a3$tm.abun)
cor.test(data_a4$vm.abun,data_a4$tm.abun)
cor.test(data_b1$vm.abun,data_b1$tm.abun)
cor.test(data_b2$vm.abun,data_b2$tm.abun)
ggplot(data)+
  geom_point(aes(vm.abun,tm.abun,color=ind),size=2)+
  geom_smooth(aes(vm.abun,tm.abun),method='lm',fill=NA,color = rgb(0,0,0,max=255))+
  labs(x="Normalized abundance of VM-vOTUs (x104)",y="Normalized abundance of TM-vOTUs  (x104)")+
  scale_color_manual(values=c("#FCC071","#D21626","#D2A0BE",
                              "#C67171","#9DB1C1","#236EB0"))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=0,y=0)+
  scale_x_continuous(limits = c(0,600000),breaks = seq(0,600000,200000))+
  scale_y_continuous(limits = c(0,300000),breaks = seq(0,300000,100000))  
