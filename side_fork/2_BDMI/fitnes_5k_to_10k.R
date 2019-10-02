setwd("C:/Users/flavi/Documents/dmi_long_divergence/dmi_avdqnf/parsed_data")
#"C:/Users/flavi/Documents/dmi_long_divergence/dmi_avdqnf/parsed_data"
library(ggplot2)

dia = format(Sys.Date(), "%d_%m_%Y")
dia


v75 = read.table("../raw_data/v_7/mean_fitness_5000_all_rep_gen.txt", header = T)
v75$generation = as.factor(v75$generation)
v75$batch = as.factor(v75$batch)
v75 = v75[v75$raw_gen <= 5050,]

v78 = read.table("../raw_data/v_7/mean_fitness_8000_all_rep_gen.txt", header = T)
v78$generation = as.factor(v78$generation)
v78$batch = as.factor(v78$batch)
v78 = v78[v78$raw_gen <= 8050,]

v710 = read.table("../raw_data/v_7/mean_fitness_10000_all_rep_gen.txt", header = T)
v710$generation = as.factor(v710$generation)
v710$batch = as.factor(v710$batch)
v710 = v710[v710$raw_gen <= 10050,]

d7 = rbind(v75, v78, v710)


v55 = read.table("../raw_data/v_5/mean_fitness_5000_all_rep_gen.txt", header = T)
v55$generation = as.factor(v55$generation)
v55$batch = as.factor(v55$batch)
v55 = v55[v55$raw_gen <= 5050,]

v58 = read.table("../raw_data/v_5/mean_fitness_8000_all_rep_gen.txt", header = T)
v58$generation = as.factor(v58$generation)
v58$batch = as.factor(v58$batch)
v58 = v58[v58$raw_gen <= 8050,]

v510 = read.table("../raw_data/v_5/mean_fitness_10000_all_rep_gen.txt", header = T)
v510$generation = as.factor(v510$generation)
v510$batch = as.factor(v510$batch)
v510 = v510[v510$raw_gen <= 10050,]

d5 = rbind(v55, v58, v510)


v65 = read.table("../raw_data/v_6/mean_fitness_5000_all_rep_gen.txt", header = T)
v65$generation = as.factor(v65$generation)
v65$batch = as.factor(v65$batch)
v65 = v65[v65$raw_gen <= 5050,]

v68 = read.table("../raw_data/v_6/mean_fitness_8000_all_rep_gen.txt", header = T)
v68$generation = as.factor(v68$generation)
v68$batch = as.factor(v68$batch)
v68 = v68[v68$raw_gen <= 8050,]

v610 = read.table("../raw_data/v_6/mean_fitness_10000_all_rep_gen.txt", header = T)
v610$generation = as.factor(v610$generation)
v610$batch = as.factor(v610$batch)
v610 = v610[v610$raw_gen <= 10050,]

d6 = rbind(v65, v68, v610)




d5$ver = "80/20"
d6$ver = "50/50"
d7$ver = "20/80"


tudo = rbind(d5, d6, d7)

write.table(tudo, "fitness_50_all_ver_5k_to_10k.txt", col.names = T, row.names = F)


###esse est o grafico bom
# tt = tudo[tudo$raw_gen <= 20030,]
# tt = tt[tt$raw_gen > 20002,]


png(paste0("FITNESS_5k_to_10k_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(tudo, aes(x= generation, y=fitness, color = batch, alpha=0.001))+
  geom_point()+
  #xlim(20002, 20030)+
  facet_grid(batch ~ ver)+
  ggtitle("Fitness over time")+
  #scale_fill_manual(values= c("#856fea", "#009200"))+
  scale_color_manual(values=c("#ff00ae", "#4cbef0","#c27e1d"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Fitness")+
  labs(color = "Divergence\nTime")+
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
