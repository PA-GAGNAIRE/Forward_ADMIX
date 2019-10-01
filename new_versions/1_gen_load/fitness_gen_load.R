#_____________#
# CHANGE HERE #
#_____________#

#version
ver = "v_5"

#batch number (here the time of divergence)
b = c(20000)

# replicate names
runs = c("run_1", "run_2", "run_3", "run_4", "run_5", "run_6", "run_7", "run_8", "run_9", "run_10", "run_11","run_12", "run_13", "run_14", "run_15", "run_16", "run_17", "run_18", "run_19", "run_20", "run_21", "run_22", "run_23", "run_24", "run_25", "run_26", "run_27", "run_28", "run_29", "run_30", "run_31", "run_32", "run_33", "run_34", "run_35", "run_36", "run_37", "run_38", "run_39", "run_40", "run_41", "run_42", "run_43", "run_44", "run_45", "run_46", "run_47", "run_48", "run_49", "run_50")
pops=c("P1", "P2", "P3")

setwd(paste0("C:/Users/flavi/Documents/gen_load/", ver))

dia = format(Sys.Date(), "%d_%m_%Y")

############################
# Load necessary libraries #
#__________________________#

library(plyr)
library(ggplot2)
library(ggpubr)

#############################
# Data preparation: FITNESS #
#___________________________#

gen_fit = c()
for (p in pops){
  for (r in runs){
    data = read.table(paste(b, "/", r, "/", p,"_mean_fit.txt", sep=""  ), header = F)
    names(data)=c("gen", "fit")
    data$replicate = r
    data$pop = p
    data$versao = ver
    gen_fit=rbind(gen_fit, data)
  }
  gen_fit=data.frame(gen_fit)
  names(gen_fit) = c("gen", "fit", "replicate", "pop", "versao")
  write.table(gen_fit, paste("fitness_pop_", p, "_all_rep.txt", sep = ""), col.names = T, row.names = F)
  gen_fit = c()
}

fp1 = read.table(paste("fitness_pop_", pops[1], "_all_rep.txt", sep = ""), header = T)
fp2 = read.table(paste("fitness_pop_", pops[2], "_all_rep.txt", sep = ""), header = T)
fp3 = read.table(paste("fitness_pop_", pops[3], "_all_rep.txt", sep = ""), header = T)

df = rbind(fp1, fp2, fp3) #2014850

write.table(df, "final_gen_load_all_pops.txt", col.names = T, row.names = F)

df = read.table("final_gen_load_all_pops.txt", h=T)
df$generation = as.factor(df$gen)

ds = df[df$gen <= 20000,] #only divergence
de = df[df$gen >= 20001,] #only after admixture

sla = seq(19900, 20100, 1)
dp = df[df$gen %in% sla,] #100 gen b4 admix + 100 after admixture

####################
# PLOTTING FITNESS #
#__________________#

png(paste0("FITNESS_b4_admix_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(ds, aes(x= generation, y=fit, color=pop, alpha=0.01))+
  geom_jitter()+
  ggtitle("Fitness BEFORE admixture")+
  scale_fill_manual(values= c("#ff00ae", "#009200"))+
  scale_color_manual(values=c("#ff00ae", "#009200"))+
  xlab("Time before admixture \n(generations)")+
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
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 12),
        axis.text.x = element_text(angle=90, colour = "black", size = 5))

dev.off()

png(paste0("FITNESS_after_admix_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(de, aes(x= generation, y=fit, color=pop, alpha=0.01))+
  geom_jitter()+
  ggtitle("Fitness AFTER admixture")+
  scale_fill_manual(values= c("lightgray"))+
  scale_color_manual(values=c("lightgray"))+
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
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 12),
        axis.text.x = element_text(angle=30, colour = "black", size = 8))

dev.off()

png(paste0("FITNESS_SHORT_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(dp, aes(x= generation, y=fit, color=pop, alpha=0.01))+
  geom_jitter()+
  ggtitle("Fitness BEFORE admixture")+
  scale_fill_manual(values= c("#ff00ae", "#009200", "darkgray"))+
  scale_color_manual(values=c("#ff00ae", "#009200", "darkgray"))+
  xlab("Time before admixture \n(generations)")+
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
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 12),
        axis.text.x = element_text(angle=90, colour = "black", size = 3))

dev.off()

png(paste0("FITNESS_giant_graph_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(df, aes(x= generation, y=fit, color=pop, alpha=0.01))+
  geom_jitter()+
  ggtitle("Fitness BEFORE admixture")+
  scale_fill_manual(values= c("#ff00ae", "#009200", "darkgray"))+
  scale_color_manual(values=c("#ff00ae", "#009200", "darkgray"))+
  xlab("Time before admixture \n(generations)")+
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
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 12),
        axis.text.x = element_text(angle=90, colour = "black", size = 1))

dev.off()

#REMINDER of what each data frame is...
# df #all the data
# ds = df[df$gen <= 20000,] #so a divergencia
# de = df[df$gen >= 20001,] #so admixture
# dp = df[df$gen %in% sla,] #um pouco dos dois


p1 = ggplot(df, aes(x= generation, y=fit, color=pop, alpha=0.01))+
  geom_jitter()+
  # ggtitle("Fitness BEFORE admixture")+
  scale_fill_manual(values= c("#ff00ae", "#009200", "darkgray"))+
  scale_color_manual(values=c("#ff00ae", "#009200", "darkgray"))+
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
        axis.text.x = element_text(angle=90, colour = "black", size = 1))


p2 = ggplot(dp, aes(x= generation, y=fit, color=pop, alpha=0.01))+
  geom_jitter()+
  scale_fill_manual(values= c("#ff00ae", "#009200", "darkgray"))+
  scale_color_manual(values=c("#ff00ae", "#009200", "darkgray"))+
  # xlab("Time before admixture \n(generations)")+
  # ylab("Fitness")+
  labs(fill = "Population")+
  guides(alpha = FALSE)+
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
        axis.text.x = element_text(angle=90, colour = "black", size = 3))



png(paste0("fitness_arranged_", dia, ".png"), width =21, height = 11, units = "cm", res = 300)
ggarrange(p1, p2,
          heights = 1:2,
          widths = 1:1,
          ncol = 1, nrow = 2,
          align = "v",
          legend = "right",
          common.legend = T)
dev.off()


p3 = ggplot(df, aes(x= generation, y=fit, color=pop, alpha=0.01))+
  geom_boxplot(outlier.shape = NA)+
  # ggtitle("Fitness BEFORE admixture")+
  scale_fill_manual(values= c("#ff00ae", "#009200", "darkgray"))+
  scale_color_manual(values=c("#ff00ae", "#009200", "darkgray"))+
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
        axis.text.x = element_text(angle=90, colour = "black", size = 1))


p4 = ggplot(dp, aes(x= generation, y=fit, color=pop, alpha=0.01))+
  geom_boxplot(outlier.shape = NA)+
  scale_fill_manual(values= c("#ff00ae", "#009200", "darkgray"))+
  scale_color_manual(values=c("#ff00ae", "#009200", "darkgray"))+
  # xlab("Time before admixture \n(generations)")+
  # ylab("Fitness")+
  labs(fill = "Population")+
  guides(alpha = FALSE)+
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
        axis.text.x = element_text(angle=90, colour = "black", size = 3))



png(paste0("fitness_arrange_", dia, ".png"), width =21, height = 11, units = "cm", res = 300)
ggarrange(p3, p4,
          heights = 1:2,
          widths = 1:1,
          ncol = 1, nrow = 2,
          align = "v",
          legend = "right",
          common.legend = T)
dev.off()



