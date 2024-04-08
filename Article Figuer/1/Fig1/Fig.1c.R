data <- read.table('phage.genome_len2num',header = T)
orders <- data$order
library(reshape2)
library(ggplot2)
data <- melt(data[,1:3],id.vars = c("order"))
data$order <- factor(data$order,levels = orders)
data$variable <- factor(data$variable,levels = c("VM","TM"))
ggplot(data)+
  geom_bar(aes(order,value,fill=variable),stat = "identity",width = 1,position = "dodge")+
  #geom_line(aes(x=rank,y=hostrange,color = votu),size=1)+
  labs(x="Genome length",y="Number")+
  scale_fill_manual(values=c(rgb(89,161,79,max=255),rgb(210,160,190,max=255)))+
  #scale_x_continuous(limits = c(0,79),breaks=seq(0,79,1))+
  scale_y_continuous(limits = c(0,7000),breaks=seq(0,7000,1000))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=4.5,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(linewidth =0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')