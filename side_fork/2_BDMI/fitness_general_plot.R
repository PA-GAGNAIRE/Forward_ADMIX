#"C:/Users/flavi/Documents/dmi_long_divergence/dmi_avdqnf/parsed_data"
library(ggplot2)

dia = format(Sys.Date(), "%d_%m_%Y")
dia


v7 = read.table("../raw_data/v_7/mean_fitness_20000_all_rep_gen.txt", header = T)
v7$generation = as.factor(v7$generation)
v7$batch = as.factor(v7$batch)


v5 = read.table("../raw_data/v_5/mean_fitness_20000_all_rep_gen.txt", header = T)
v5$generation = as.factor(v5$generation)
v5$batch = as.factor(v5$batch)

v6 = read.table("../raw_data/v_6/mean_fitness_20000_all_rep_gen.txt", header = T)
v6$generation = as.factor(v6$generation)
v6$batch = as.factor(v6$batch)
v6$result = "Polymorphism"

rp7 = read.table(paste0("v_7/20000_persist/replicates_POLY.txt"), h=T, stringsAsFactors = F)
rp7 = unique(rp7[,1])
rl7 = read.table(paste0("v_7/20000_persist/replicates_LOSS.txt"), h=T, stringsAsFactors = F)
rl7 = unique(rl7[,1])

rp5 = read.table(paste0("v_5/20000_persist/replicates_POLY.txt"), h=T, stringsAsFactors = F)
rp5 = unique(rp5[,1])
rl5 = read.table(paste0("v_5/20000_persist/replicates_LOSS.txt"), h=T, stringsAsFactors = F)
rl5 = unique(rl5[,1])

rp7
rp5
rl7
rl5

v77 = v7[v7$raw_gen <= 20050,]
v55 = v5[v5$raw_gen <= 20050,]
v66 = v6[v6$raw_gen <= 20050,]

l5 = v55[v55$replicate %in% rl5,]
p5 = v55[v55$replicate %in% rp5,]
l5$result = "Loss"
p5$result = "Polymorphism"

l7 = v77[v77$replicate %in% rl7,]
p7 = v77[v77$replicate %in% rp7,]
l7$result = "Loss"
p7$result = "Polymorphism"



t5 = rbind(l5, p5)
t7 = rbind(l7, p7)
t5$ver = "80/20"
v66$ver = "50/50"
t7$ver = "20/80"
tudo = rbind(t5, v66, t7)

write.table(tudo, "fitness_50_all_ver.txt", col.names = T, row.names = F)

#tudo = rbind(l5, p5, l7, p7)

#write.table(tudo, "fitness_v5_v7.txt", col.names = T, row.names = F)

###esse est o grafico bom
tt = tudo[tudo$raw_gen <= 20030,]
tt = tt[tt$raw_gen > 20002,]


png(paste0("FITNESS_all_vers_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(tt, aes(x= generation, y=fitness, color = result, alpha=0.001))+
  geom_point()+
  #xlim(20002, 20030)+
  facet_wrap(~ver, ncol = 3)+
  ggtitle("Fitness over time")+
  #scale_fill_manual(values= c("#856fea", "#009200"))+
  scale_color_manual(values=c("#856fea", "#009200"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Fitness")+
  labs(color = "Simulation\nResult")+
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
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 12),
        axis.text.x = element_text(angle=30, colour = "black", size = 8))
dev.off()



plot(tudo$fitness ~ tudo$raw_gen)
ggplot(tudo, aes(x= generation, y=fitness, fill = result, color = result, alpha=0.5))+
  geom_boxplot(outlier.colour = NA)+
  facet_wrap(~ver, ncol = 1)




ggplot(tudo, aes(x= raw_gen, y=fitness, fill = result, color = result, alpha=0.01))+
  geom_jitter()+
  xlim(20002, 20050)+
  facet_wrap(~ver, ncol = 1)

str(tudo)
