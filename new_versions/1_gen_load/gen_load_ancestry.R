library(plyr)
library(ggplot2)

##RECOMB#####################
#       Recombination Level #
#############################
ver = "v_5"
setwd(paste0("C:/Users/flavi/Documents/gen_load/", ver))


#now that I have all the data separated in bins
#1/ i have to put another column for the value of the recombination
#2/ I want the mean ancestry of each run (per generation) 

runs = c("run_1", "run_2", "run_3", "run_4", "run_5", "run_6", "run_7", "run_8", "run_9", "run_10","run_12", "run_13", "run_14", "run_15", "run_16", "run_17", "run_18", "run_19", "run_20", "run_21", "run_22", "run_23", "run_24", "run_25", "run_26", "run_27", "run_28", "run_29", "run_30", "run_31", "run_32", "run_33", "run_34", "run_35", "run_36", "run_37", "run_38", "run_39", "run_40", "run_41", "run_42", "run_43", "run_44", "run_45", "run_46", "run_47", "run_48", "run_49", "run_50")
#removed run 11, b/c it was somehow incomplete


###############
# Change HERE #
###############
batch = c(20000)

for (b in batch){
  assign(paste0("gen_1_", b), seq(b+5, b+50, 5))
  assign(paste0("gen_2_", b), seq(b+60, b+300, 10))
  assign(paste0("gener_", b), append(get(paste0("gen_1_", b)), get(paste0("gen_2_", b))))
} 

###############
# Change HERE #
###############
generations = list(gener_20000)

for (i in 1:length(generations)){
  for (u in 1:length(generations[[i]])){
    generations[[i]][u]= toString(generations[[i]][u])
  }
}

#################
# MEAN ANCESTRY #
#_______________#

dr = c()
dg = c()
db = c()

for (b in 1:length(batch)){
  for (g in 1:length(generations[[b]])){
    for (r in runs){
      anc = read.table(paste0(batch[b], "/", r, "/ancestry_", generations[[b]][g], ".txt"), h=F)
      chr = read.table(paste0(batch[b], "/", r, "/breaks_", generations[[b]][g], ".txt"), h=F)
      data = cbind(chr, anc)
      names(data)=c("pos", "anc")
      data$replicate = r
      data$gen = generations[[b]][g]
      data$versao = ver
      dlow = data[data$pos <= 50000,]
      dhigh = data[data$pos > 50000,]
      dlow$recomb = "low"
      dhigh$recomb = "high"
      dl = ddply(dlow, .(replicate, gen, ver, recomb), summarise, anc = mean(anc))
      dh = ddply(dhigh, .(replicate, gen, ver, recomb), summarise, anc = mean(anc))
      tb = rbind(dh, dl)
      dr = rbind(dr, tb)
      cat(paste0(r, " ", generations[[b]][g], "\n"))
    }

  }
  write.table(dr, paste("mean_ancestry_", batch[b], "_all.txt", sep = ""), col.names = T, row.names = F)
}

data = read.table(paste0("mean_ancestry_", batch, "_all.txt"), header = T)

data$generation = as.factor(data$gen)
ds = data[data$gen <= (b+20),]

#LINE PLOT with all data
png("gen_load_ancestry_1.png", width = 42, height = 21, units = "cm", res = 300)
ggplot(data, aes(x=gen, y=anc, color=replicate, alpha = 0.1)) +
  geom_line()+
  facet_wrap(~recomb)+
  guides(color = F)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Minor parent ancestry across generations")

dev.off()

pallete = c("#9d565b", "#43c35f", "#ba5bcf", "#6fbb3c", "#7060d9", "#b7bf34", "#4875d7", "#8ea937", "#d449af", "#448928", "#e1408d", "#5fc695", "#d33c4b", "#47cebe", "#dd542f", "#3fbecb", "#e28c30", "#6299e5", "#d2a73b", "#a27fe1", "#83bd6b", "#7e459e", "#4a9b5c", "#df4f7c", "#35743b", "#d88bd4", "#8b8324", "#505a98", "#bcb86c", "#a7417e", "#3a997f", "#ae3254", "#1a6447", "#e287af", "#576621", "#a39bdd", "#ad4e20", "#50a4d3", "#a9712f", "#6176b6", "#e98464", "#2f7b63", "#9b62a2", "#869553", "#a54a62", "#d8a06e", "#905271", "#84672f", "#e28284", "#a45440")

#LINE PLOT with only 20 generations **after** admixture
png("gen_load_ancestry_SMALL.png", width = 42, height = 21, units = "cm", res = 300)
ggplot(ds, aes(x=gen, y=anc, color=replicate, alpha = 0.1)) +
  geom_line(size = 0.5)+
  facet_wrap(~recomb)+
  scale_color_manual(values = pallete)+
  guides(color = F, alpha=F)+
  xlab("Time\n(generations)")+
  ylab("Mean Ancestry")+
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
        axis.text.x = element_text(angle=0, colour = "black", size = 8))+
  ggtitle("P2 parent ancestry across generations")
dev.off()
