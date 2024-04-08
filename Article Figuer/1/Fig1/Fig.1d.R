library(ggplot2)
data <- read.table('phage.genome_feature',header = T)

#genome num in each family
new <- aggregate(data$seq,by=list(data$type,data$method,data$family),FUN=length)
new$x <- as.numeric(as.character(new$x))

group <- paste(new$Group.3,new$Group.2)

new$group <- group

orders <- aggregate(data$seq,by=list(data$family),FUN=length)
orders <- orders[order(orders$x,decreasing = T),]
new$group <- factor(new$group,levels = as.character(rbind(paste(orders$Group.1,"virome"),paste(orders$Group.1,"meta"))))
new$Group.2 <- factor(new$Group.2,levels = c("virome","meta"))

ggplot(new)+
  geom_bar(aes(group,x,fill=Group.1,color = Group.2),stat = "identity",width = 0.6,position = "stack")+
  labs(x="Families",y="Genome number")+
  scale_fill_manual(values=c(rgb(220,220,220,max=255),rgb(10,10,10,max=255)))+
  scale_color_manual(values = c(rgb(89,161,79,max=255),rgb(210,160,190,max=255)))+
  scale_y_continuous(limits = c(0,10000),breaks=seq(0,10000,2000))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 1,angle = 45,size=5,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(linewidth =0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')

#phage genome completeness in vm and tm
data <- read.table("phage.genome_feature",header = T)
data$method <- factor(data$method,levels=c("virome","meta"))
data <- na.omit(data)
ggplot(data)+
  stat_boxplot(geom = "errorbar",width=0.1,aes(method,completeness,color=method),position = position_dodge(1))+
  geom_boxplot(aes(method,completeness,color=method,fill=method), width=0.2,position = position_dodge(1),outlier.shape = NA)+
  scale_fill_manual(values=c(rgb(89,161,79,max=255),rgb(210,160,190,max=255)))+
  scale_color_manual(values=c(rgb(0,0,0,max=255),rgb(0,0,0,max=255)))+
  labs(y="Genome completeness (%)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_y_continuous(limits = c(0,100),breaks=seq(0,100,20))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(0,100))





#prophage ratio in vm and tm
data <- read.table('phage.genome_feature',header = T)

new <- aggregate(data$seq,by=list(data$sample,data$method,data$type),FUN=length)
new$x <- as.numeric(as.character(new$x))
group <- paste(new$Group.3,new$Group.2)

new$group <- group
new$group <- factor(new$group,levels = c(unique(new$group)))

new1 <- new[new$Group.3=="prophage",]

seq_num <- aggregate(data$seq,by=list(data$sample,data$method),FUN=length)
seq_num$x <- as.numeric(as.character(seq_num$x))
libs <- paste(seq_num$Group.1,seq_num$Group.2)
seq_num$libs <- libs
seq_num <- seq_num[order(seq_num$libs,decreasing = FALSE),]

prophage_lib <- paste(new1$Group.1,new1$Group.2)
all_lib <- paste(seq_num$Group.1,seq_num$Group.2)
absent_lib <- setdiff(all_lib,prophage_lib)

library(stringr)

group12 <- str_split_fixed(absent_lib," ",2)
absent_prophage <- cbind(group12,replicate(length(absent_lib),"prophage"),
                         replicate(length(absent_lib),0))
absent_prophage <- as.data.frame(absent_prophage)
absent_group <- paste(absent_prophage$V3,absent_prophage$V2)
absent_prophage <- data.frame(absent_prophage,absent_group)
colnames(absent_prophage) <- colnames(new1)

new2 <- rbind(new1,absent_prophage)
lib <- paste(new2$Group.1,new2$Group.2)
new2$lib <- lib
new2 <- new2[order(new2$lib,decreasing = FALSE),]


df <- data.frame(new2,seq_num$x)
colnames(df) <- c("sample","method","type","prophage_num","group","lib","total_seq_num")
prophage_ratio <- as.numeric(df$prophage_num)*100/as.numeric(df$total_seq_num)
df <- data.frame(df,prophage_ratio)

df$method <- factor(df$method,levels=c("virome","meta"))
ggplot(df)+
  stat_boxplot(geom = "errorbar",width=0.1,aes(method,prophage_ratio,color=method),position = position_dodge(1))+
  geom_boxplot(aes(method,prophage_ratio,color=method,fill=method), width=0.2,position = position_dodge(1),outlier.shape = NA)+
  scale_fill_manual(values=c(rgb(89,161,79,max=255),rgb(210,160,190,max=255)))+
  scale_color_manual(values=c(rgb(0,0,0,max=255),rgb(0,0,0,max=255)))+
  labs(y="Proportion of prophages (%)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(linewidth =0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_y_continuous(limits = c(0,40),breaks=seq(0,40,10))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(0,40))
