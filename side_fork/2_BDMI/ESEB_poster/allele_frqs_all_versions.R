library("ggpubr")
library("ggplot2")

setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/")
bb = 20000
batch = c(20000)

dia = format(Sys.Date(), "%d_%m_%Y")
dia

for (b in batch) {
  assign(paste0("gen_1_", b), seq(b + 2, b + 50, 2))
  assign(paste0("gen_2_", b), seq(b + 60, b + 300, 10))
  assign(paste0("gener_", b), append(get(paste0("gen_1_", b)), get(paste0("gen_2_", b))))
}

#depending on which you use, you have to ~refresh the generation values
generations = list(get(paste0("gener_", bb)))

for (i in 1:length(generations)) {
  for (u in 1:length(generations[[i]])) {
    generations[[i]][u] = toString(generations[[i]][u])
  }
}

short = c(generations[[1]][1:15])

v5=read.table(paste0("v_5/20000_persist/20000alleles_by_outcome.txt"), h=T)
v6=read.table(paste0("v_6/20000_persist/20000alleles_by_outcome.txt"), h=T) 
v7=read.table(paste0("v_7/20000_persist/20000alleles_by_outcome.txt"), h=T)

v5$ver = "80/20"
v6$ver = "50/50"
v7$ver = "20/80"

t4 = rbind(v5, v6, v7)

t4$time = as.factor(t4$gen.y)
t4 = t4[t4$gen.y != (bb+1),]

t8 = t4[t4$gen.y %in% generations[[1]],]
t9 = t8[t8$gen.y <= (bb+30),]


for (i in 1:nrow(t9)){
  if (t9$recomb[i] == "high"){t9$rate[i] = "1e-3"} else {t9$rate[i] = "1e-4"}
}


#write.table(t9, "allele_freq_all_versions_2.txt", col.names = T, row.names = F)

t9 = read.table("allele_freq_all_versions.txt", h=T)

v57 = c("20/80", "80/20")
certo = t9[t9$ver %in% v57,]
errado = t9[t9$ver == "50/50",]
corrigido = errado[errado$replicate != "run_1",]

t9 = rbind(certo, corrigido)

ruins = c("run_1", "run_2", "run_3", "run_4", "run_5", "run_6")
errado = t9[t9$ver =="50/50",]
certo = t9[t9 != "50/50",]
corrigido = errado[!errado$replicate %in% ruins,]

t9 = rbind(certo, corrigido)

write.table(t9, "allele_freq_all_versions_CORRIGIDO.txt", col.names = T, row.names = F)

allele = c("fix_a", "fixed_B")
t7 = t9[t9$state %in% allele,]
fb = t7[t7$state == "fixed_B",]
fa = t7[t7$state == "fix_a",]
fb$allele = "Derived B"
fa$allele = "Ancestral A"
t6 = rbind(fb, fa)

write.table(t6, "allele_freq_all_versions_CORRIGIDO_FINAL.txt", col.names = T, row.names = F)

t6 = read.table("allele_freq_all_versions_CORRIGIDO_FINAL.txt", h=T)

for (i in 1:nrow(t6)){
  t6$true_gen[i] = t6$gen.y[i] - 20000
}

t6$true_gen = as.factor(t6$true_gen)

write.table(t6, "data_allele_freqs_all_versions_CLEAN.txt", col.names = T, row.names = F)

t6 = read.table("data_allele_freqs_all_versions_CLEAN.txt", h=T)
t6$true_gen = as.factor(t6$true_gen)

data.segm1<-data.frame(x=0.5, y= 0.8, xend=15.5, yend=0.8, ver="20/80", rate="1e-3")
data.segm2<-data.frame(x=0.5, y= 0.8, xend=15.5, yend=0.8, ver="20/80", rate="1e-4")
data.segm3<-data.frame(x=0.5, y= 0.5, xend=15.5, yend=0.5, ver="50/50", rate="1e-3")
data.segm4<-data.frame(x=0.5, y= 0.5, xend=15.5, yend=0.5, ver="50/50", rate="1e-4")
data.segm5<-data.frame(x=0.5, y= 0.2, xend=15.5, yend=0.2, ver="80/20", rate="1e-3")
data.segm6<-data.frame(x=0.5, y= 0.2, xend=15.5, yend=0.2, ver="80/20", rate="1e-4")
# ggplot(t9, aes(x = time, y = freq.y, fill = result, color = result, alpha = 0.3)) +
#   geom_boxplot(outlier.shape = NA) +
#   facet_grid(recomb ~ state) +
#   guides(alpha = FALSE)+
#   geom_segment(aes(

lss = t6[t6$result == "lost",]
pol = t6[t6$result == "persist",]

versao = c("20/80", "80/20")

unbal = t6[t6$ver %in% versao,]
kct = unbal
kct$rate = as.character(kct$rate)

cores = c("#ff33cc", "#cc0099", "#ff3399", "#cc79a7", "#990066")

pie(rep(1, length(cores)), col = cores)

library("scales")

# azul = c("#001350")
azul = "#1c6780"
rosa = "#cc79a7"
cores = c(azul, rosa)
pie(rep(1, length(cores)), col = cores)



bcta = t6[t6$ver %in% "80/20",]
ppk = t6[t6$ver %in% "20/80",]

png(paste0("0002_allele_freqs_one_version", dia, ".png"), width = 11, height = 11, units = "cm", res = 300)
ggplot(ppk, aes(x = true_gen, y= freq.y, fill = result, color = result, alpha= 0.5))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(rate~allele, labeller= labeller(.multi_line = T))+
  #ylim(0, 0.9)+
  ggtitle("Allele frequencies over time")+
  # scale_fill_manual(values= c("black", "#ff3399"))+
  # scale_color_manual(values=c("black", "#ff3399"))+
  scale_fill_manual(values = c(azul, rosa))+
  scale_color_manual(values = c(azul, rosa))+
  xlab("Time after admixture (generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation Result")+
  guides(color = FALSE, alpha = FALSE)+
  # geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.15, "lines"),
        legend.title = element_text(colour="black", size=12, face = "bold"),
        legend.text = element_text(colour="black", size=10, face = "bold"),
        legend.position = "bottom",
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, vjust = 0.9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 10, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 10, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 8),
        axis.text.x = element_text(angle=30, colour = "black", size = 6))
dev.off()


  
png(paste0("009_allele_freqs_all_versions", dia, ".png"), width = 21, height = 11, units = "cm", res = 300)
ggplot(kct, aes(x = true_gen, y= freq.y, fill = result, color = result, alpha= 0.5))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(rate~ver+allele, labeller= labeller(.multi_line = T))+
  #ylim(0, 0.9)+
  ggtitle("Allele frequencies over time")+
  # scale_fill_manual(values= c("black", "#ff3399"))+
  # scale_color_manual(values=c("black", "#ff3399"))+
  scale_fill_manual(values = c(azul, rosa))+
  scale_color_manual(values = c(azul, rosa))+
  xlab("Time after admixture (generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation Result")+
  guides(color = FALSE, alpha = FALSE)+
  # geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.15, "lines"),
        legend.title = element_text(colour="black", size=12, face = "bold"),
        legend.text = element_text(colour="black", size=10, face = "bold"),
        legend.position = "bottom",
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, vjust = 0.9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 10, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 10, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 8),
        axis.text.x = element_text(angle=30, colour = "black", size = 6))
dev.off()

ggplot(t6, aes(x = true_gen, y= freq.y, fill = result, color = result, alpha= 0.5))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(recomb~ver+allele, labeller= labeller(.multi_line = T))

png(paste0("001_allele_freqs_all_versions", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(t6, aes(x = true_gen, y= freq.y, fill = result, color = result, alpha= 0.5))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(rate~ver+allele, labeller= labeller(.multi_line = T))+
  #ylim(0, 0.9)+
  ggtitle("Allele frequencies over time")+
  scale_fill_manual(values= c("#856fea", "#009200"))+
  scale_color_manual(values=c("#856fea", "#009200"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation\nResult")+
  guides(color = FALSE, alpha = FALSE)+
  # geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  # geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
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


png(paste0("7_allele_freqs_all_versions", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(t6, aes(x = true_gen, y= freq.y, fill = result, color = result, alpha= 0.01))+
  geom_jitter()+
  facet_grid(rate~ver+allele, labeller= labeller(.multi_line = T))+
  #ylim(0, 0.9)+
  ggtitle("Allele frequencies over time")+
  scale_fill_manual(values= c("#856fea", "#009200"))+
  scale_color_manual(values=c("#856fea", "#009200"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation\nResult")+
  guides(color = FALSE, alpha = FALSE)+
  geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
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

lss$true_gen = as.factor(lss$true_gen)
lh = lss[lss$recomb == "high",]
ph = pol[pol$recomb == "high",]


png(paste0("LOSS_allele_freqs_all_versions", dia, "-2.png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(lss, aes(x = true_gen, y= freq.y, alpha= 0.5, fill = result, color = result))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(rate~ver+allele, labeller= labeller(.multi_line = T))+
  ggtitle("Allele frequencies over time")+
  scale_fill_manual(values= c("#4cbef0"))+
  scale_color_manual(values=c("#4cbef0"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation\nResult")+
  guides(color = FALSE, alpha = FALSE)+
  #geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
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

pol$true_gen = as.factor(pol$true_gen)

png(paste0("PERSIST_allele_freqs_all_versions", dia, "-2.png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(pol, aes(x = true_gen, y= freq.y, alpha= 0.5, fill = result, color = result))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(rate~ver+allele, labeller= labeller(.multi_line = T))+
  ggtitle("Allele frequencies over time")+
  scale_fill_manual(values= c("#c27e1d"))+
  scale_color_manual(values=c("#c27e1d"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation\nResult")+
  guides(color = FALSE, alpha = FALSE)+
  #geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
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



png(paste0("ALL_diff_colors_allele_freqs_all_versions", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(t6, aes(x = true_gen, y= freq.y, alpha= 0.5, fill = result, color = result))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(rate~ver+allele, labeller= labeller(.multi_line = T))+
  ggtitle("Allele frequencies over time")+
  scale_fill_manual(values= c("#4cbef0","#c27e1d"))+
  scale_color_manual(values=c("#4cbef0", "#c27e1d"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation\nResult")+
  guides(color = FALSE, alpha = FALSE)+
  #geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
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

png(paste0("LOSS_allele_freqs_all_versions", dia, "-3.png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(lh, aes(x = true_gen, y= freq.y, alpha= 0.5, fill = result, color = result))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(rate~ver+allele, labeller= labeller(.multi_line = T))+
  ggtitle("Allele frequencies over time")+
  scale_fill_manual(values= c("#4cbef0"))+
  scale_color_manual(values=c("#4cbef0"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation\nResult")+
  guides(color = FALSE, alpha = FALSE)+
  #geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
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

quero = c("20/80", "80/20")
phh = ph[ph$ver %in% quero,]


png(paste0("PERSIST_allele_freqs_all_versions", dia, "-4.png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(phh, aes(x = true_gen, y= freq.y, alpha= 0.5, fill = result, color = result))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(rate~ver+allele, labeller= labeller(.multi_line = T))+
  ggtitle("Allele frequencies over time")+
  scale_fill_manual(values= c("#c27e1d"))+
  scale_color_manual(values=c("#c27e1d"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation\nResult")+
  guides(color = FALSE, alpha = FALSE)+
  #geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
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

th = t6[t6$recomb == "high",]

png(paste0("ALL_diff_colors_allele_freqs_all_versions", dia, "-3.png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(th, aes(x = true_gen, y= freq.y, alpha= 0.5, fill = result, color = result))+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(rate~ver+allele, labeller= labeller(.multi_line = T))+
  ggtitle("Allele frequencies over time")+
  scale_fill_manual(values= c("#4cbef0","#c27e1d"))+
  scale_color_manual(values=c("#4cbef0", "#c27e1d"))+
  xlab("Time after admixture \n(generations)")+
  ylab("Allele Frequency")+
  labs(fill = "Simulation\nResult")+
  guides(color = FALSE, alpha = FALSE)+
  #geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  #geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  scale_y_continuous(minor_breaks = seq(0.1, 1, .2), breaks = seq(0, 1, .2))+
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


