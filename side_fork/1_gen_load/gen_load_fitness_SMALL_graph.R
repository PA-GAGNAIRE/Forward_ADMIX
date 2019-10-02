
ver = "v_5"
setwd(paste0("C:/Users/flavi/Documents/gen_load/", ver))

dia = format(Sys.Date(), "%d_%m_%Y")
dia

b = c(20000)
runs = c("run_1", "run_2", "run_3", "run_4", "run_5", "run_6", "run_7", "run_8", "run_9", "run_10", "run_11","run_12", "run_13", "run_14", "run_15", "run_16", "run_17", "run_18", "run_19", "run_20", "run_21", "run_22", "run_23", "run_24", "run_25", "run_26", "run_27", "run_28", "run_29", "run_30", "run_31", "run_32", "run_33", "run_34", "run_35", "run_36", "run_37", "run_38", "run_39", "run_40", "run_41", "run_42", "run_43", "run_44", "run_45", "run_46", "run_47", "run_48", "run_49", "run_50")


library(ggplot2)
library(ggpubr)


df = read.table("final_gen_load_all_pops.txt", h=T)
sla = seq(19900, 20100, 1)
crlho = c(seq(0, 20000, 400), seq(20050, 20200, 50))
bcta = c(seq(19800, 20000, 10), seq(20050, 20200, 10))


df$generation = as.factor(df$gen)


###fazendo uma amostra
prk = df[df$gen %in% crlho,] #long samp #51
ppk = df[df$gen %in% bcta,] #short sampling #21


data.segm1<-data.frame(x=51, y= 0.7, xend=51, yend=1)
data.segm2<-data.frame(x=21, y= 0.7, xend=21, yend=1)

p3 = ggplot(prk, aes(x= generation, y=fit, color=pop, fill = pop, alpha=0.5))+
  geom_boxplot(outlier.shape = NA)+
  # ggtitle("Fitness BEFORE admixture")+
  scale_fill_manual(values= c("#ff00ae", "#009200", "darkgray"))+
  scale_color_manual(values=c("#ff00ae", "#009200", "darkgray"))+
  geom_vline(xintercept = 50.5, linetype = "dashed", color = "black", size = 0.7)+
  #geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=F, linetype=2, color="black", size=0.7)+
  #scale_x_discrete(labels= c(as.character(seq(1, 20000, 1000))),breaks = seq(1, 20000, 1000))+
  # xlab("Time before admixture \n(generations)")+
  # ylab("Fitness")+
  labs(fill = "Population")+
  guides(color = FALSE, alpha = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.15, "lines"),
        legend.title = element_text(colour="black", size=14, face = "bold"),
        legend.text = element_text(colour="black", size=12, face = "bold"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 12),
        axis.text.x = element_text(angle=90, colour = "black", size = 8))


p4 = ggplot(ppk, aes(x= generation, y=fit, color=pop, fill = pop, alpha=0.5))+
  geom_boxplot(outlier.shape = NA)+
  scale_fill_manual(values= c("#ff00ae", "#009200", "darkgray"))+
  scale_color_manual(values=c("#ff00ae", "#009200", "darkgray"))+
  geom_vline(xintercept = 21.5, linetype = "dashed", color = "black", size = 0.7)+
  # xlab("Time before admixture \n(generations)")+
  # ylab("Fitness")+
  #geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=F, linetype=2, color="black", size=0.7)+
  labs(fill = "Population")+
  guides(alpha = FALSE, color = F)+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.15, "lines"),
        legend.title = element_text(colour="black", size=14, face = "bold"),
        legend.text = element_text(colour="black", size=12, face = "bold"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 12),
        axis.text.x = element_text(angle=90, colour = "black", size = 8))



png("fitness_porra_box_samp_2.png", width =42, height = 21, units = "cm", res = 300)
ggarrange(p3, p4,
          heights = 1:2,
          widths = 1:1,
          ncol = 1, nrow = 2,
          align = "v",
          legend = "right",
          common.legend = T)
dev.off()


xena = c(seq(0, 20000, 400), seq(20025, 20300, 25))
kct = df[df$gen %in% xena,] #51

png("fitness_PORRA_box_samp_3.png", width =42, height = 21, units = "cm", res = 300)

p5 = ggplot(kct, aes(x= generation, y=fit, color=pop, fill = pop, alpha=0.5))+
  geom_boxplot(outlier.shape = NA)+
  ggtitle("Fitness across time - [Adx = 20/80 _**_ Ne = 1000/500] - 20,000 gen")+
  scale_fill_manual(values= c("#ff00ae", "#009200", "darkgray"))+
  scale_color_manual(values=c("#ff00ae", "#009200", "darkgray"))+
  geom_vline(xintercept = 50.5, linetype = "dashed", color = "black", size = 0.7)+
  xlab("Time\n(generations)")+
  ylab("Fitness")+
  labs(fill = "Population")+
  guides(color = FALSE, alpha = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.15, "lines"),
        legend.title = element_text(colour="black", size=14, face = "bold"),
        legend.text = element_text(colour="black", size=12, face = "bold"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(size = 16, color = "black", face = "bold.italic"),
        axis.title.x = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.title.y = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 12),
        axis.text.x = element_text(angle=90, colour = "black", size = 8))
p5

dev.off()
