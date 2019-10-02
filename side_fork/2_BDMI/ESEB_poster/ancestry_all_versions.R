library("ggpubr")
library("ggplot2")
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(directlabels)
library(ggpubr)

setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/")

pallete = c("#9d565b",
           "#43c35f",
           "#ba5bcf",
           "#6fbb3c",
           "#7060d9",
           "#b7bf34",
           "#4875d7",
           "#8ea937",
           "#d449af",
           "#448928",
           "#e1408d",
           "#5fc695",
           "#d33c4b",
           "#47cebe",
           "#dd542f",
           "#3fbecb",
           "#e28c30",
           "#6299e5",
           "#d2a73b",
           "#a27fe1",
           "#83bd6b",
           "#7e459e",
           "#4a9b5c",
           "#df4f7c",
           "#35743b",
           "#d88bd4",
           "#8b8324",
           "#505a98",
           "#bcb86c",
           "#a7417e",
           "#3a997f",
           "#ae3254",
           "#1a6447",
           "#e287af",
           "#576621",
           "#a39bdd",
           "#ad4e20",
           "#50a4d3",
           "#a9712f",
           "#6176b6",
           "#e98464",
           "#2f7b63",
           "#9b62a2",
           "#869553",
           "#a54a62",
           "#d8a06e",
           "#905271",
           "#84672f",
           "#e28284",
           "#a45440")

ver = c(5, 6, 7)

for (i in ver){
  assign(paste0("d", i), read.table(paste0( "v_", i, "/mean_ancestry_all_data.txt"), header = T))
}

#i = 5
for (i in ver){
  vv = get(paste0("d", i))
  vv = vv[vv$generation <50,]
  vv$generation = as.factor(vv$generation)
  vv$batch = as.factor(vv$batch)
  if (i == 5){
    vv$prop = "80/20"
  } else if (i == 6){
    vv$prop = "50/50"} else if (i == 7){
      vv$prop = "20/80"
    }
  assign(paste0("d", i), vv)
  
}

ruins = c("run_1", "run_2", "run_3", "run_4", "run_5", "run_6")
d6 = d6[!d6$replicate %in% ruins,]


data = rbind(d5, d6, d7)
write.table(data, "ancestry_all_versions_plot.txt", col.names = T, row.names = F)

#bi = c("20000", "18000", "16000", "14000")
bi = c(5000, 8000, 10000, 12000)


data = data[data$batch %in% bi,]

for (i in 1:nrow(data)){
  if (data$recomb[i] == "high"){data$rate[i] = "1e-3"} else {data$rate[i] = "1e-4"}
}

#write.table(data, "ancestry_plot_data_all_versions.txt", col.names = T, row.names = F)
#write.table(data, "ancestry_plot_data_5k_to_12k.txt", col.names = T, row.names = F)

data =  read.table("ancestry_plot_data_all_versions.txt", h=T)

data.segm1<-data.frame(x=1, y= 0.8, xend=24, yend=0.8, prop="20/80", rate=as.numeric("1e-3"))
data.segm2<-data.frame(x=1, y= 0.8, xend=24, yend=0.8, prop="20/80", rate=as.numeric("1e-4"))
data.segm3<-data.frame(x=1, y= 0.5, xend=24, yend=0.5, prop="50/50", rate=as.numeric("1e-3"))
data.segm4<-data.frame(x=1, y= 0.5, xend=24, yend=0.5, prop="50/50", rate=as.numeric("1e-4"))
data.segm5<-data.frame(x=1, y= 0.2, xend=24, yend=0.2, prop="80/20", rate=as.numeric("1e-3"))
data.segm6<-data.frame(x=1, y= 0.2, xend=24, yend=0.2, prop="80/20", rate=as.numeric("1e-4"))



png("ancestry_all_versions_2.png", width = 42, height = 21, units = 'cm', res=300) 
ggplot(data, aes(x=generation, y=mean_anc, colour=replicate, alpha = 0.1, group=interaction(replicate, recomb))) +
  facet_grid(rate~prop*batch)+
  geom_line(size = 0.5)+
  guides(fill = F, alpha = F, color = F)+
  geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=F, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=F, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=F, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=F, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=F, linetype=2, color="black", size=0.7)+
  geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=F, linetype=2, color="black", size=0.7)+
  ggtitle("Ancestry - Varying recombination rate and divergence time")+
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

dd = data
#dd$generation = as.numeric(levels(data$generation))[data$generation]
#dd$generation = as.numeric(levels(data$generation))[data$generation]

dd = dd[dd$generation < 30,]

eseb = dd[dd$prop == "80/20",]
eseb = eseb[eseb$rate == 1e-4,]
eseb = droplevels(eseb)

png("ancestry_80_20_version_ESEB_003.png", width = 21, height = 11, units = 'cm', res=300) 
ggplot(eseb, aes(x=generation, y=mean_anc, alpha = 0.01, color = replicate, group=interaction(replicate, recomb))) +
  facet_wrap(~batch, nrow = 1)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pallete)+
  guides(fill = F, alpha = F, color = F)+
  xlab("Time after admixture \n(generations)")+
  ylab("Mean P2 Ancestry")+
  # geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  # geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  # geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  # geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  # geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  ggtitle("Ancestry - Varying recombination rate and divergence time")+
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

kct = eseb[eseb$batch > 15000,]

png("ancestry_80_20_version_ESEB_007.png", width = 21, height = 11, units = 'cm', res=300) 
ggplot(eseb, aes(x=generation, y=mean_anc, alpha = 0.01, color = replicate, group=interaction(replicate, recomb))) +
  facet_wrap(~batch, nrow = 1)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pallete)+
  guides(fill = F, alpha = F, color = F)+
  xlab("Time after admixture")+
  ylab("Mean P2 Ancestry")+
  # geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  # geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  # geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  # geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  # geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  #ggtitle("Ancestry - Varying recombination rate and divergence time")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.15, "lines"),
        # legend.title = element_text(colour="black", size=14, face = "bold"),
        # legend.text = element_text(colour="black", size=12, face = "bold"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 25, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 25, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 14),
        axis.text.x = element_text(angle=30, colour = "black", size = 12))

dev.off()




png("ancestry_all_versions_5k_to_12k.png", width = 42, height = 21, units = 'cm', res=300) 
ggplot(dd, aes(x=generation, y=mean_anc, alpha = 0.01, color = replicate, group=interaction(replicate, recomb))) +
  facet_grid(rate~prop*batch)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pallete)+
  guides(fill = F, alpha = F, color = F)+
  xlab("Time after admixture \n(generations)")+
  ylab("Mean Population 2 Ancestry")
  geom_segment(data=data.segm1, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  geom_segment(data=data.segm2, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  geom_segment(data=data.segm3, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  geom_segment(data=data.segm4, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  geom_segment(data=data.segm5, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  geom_segment(data=data.segm6, aes(x=x,y=y,yend=yend,xend=28),inherit.aes=F, linetype=2, color="black", size=0.4)+
  ggtitle("Ancestry - Varying recombination rate and divergence time")+
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


dp = read.table("ancestry_plot_data_all_versions.txt", h=T)
dm = read.table("ancestry_plot_data_5k_to_12k.txt", h=T)

m6 = dm[dm$prop == "50/50",]
p6 = dp[dp$prop == "50/50",]
v6 = rbind(m6, p6)

b14 = p6[p6$batch == "14000",]
b16 = p6[p6$batch == "16000",]
b18 = p6[p6$batch == "18000",]
b20 = p6[p6$batch == "20000",]

a20 = b20[b20$raw_gen > 20030,]
a18 = b18[b18$raw_gen > 18030,]
a16 = b16[b16$raw_gen > 16030,]
a14 = b14[b14$raw_gen > 14030,]
a6 = rbind(a20, a18, a16, a14)

for (i in 1:nrow(a6)){
  if (a6$mean_anc[i] >= 0.5){
    a6$out[i] = "increase"} else if (a6$mean_anc[i] < 0.5){
      a6$out[i] = "decrease"}
}


library(plyr)
pirok = ddply(a6, .(batch, recomb, generation, prop, out), summarise, count=length(out))


uu = read.table("data_grapfico_barras_minor_major.txt", h=T)
cont = ddply(uu, .(mfilter, outcome), summarise, conta=sum(num))#mesma bosta
kct = ddply(cont, .(mfilter, outcome), summarise, win=(sum(conta)))#mesma bosta

library(reshape)
ppk = melt(kct, id.vars = c("outcome", "mfilter"))
###CARALHO SAPORRA DEU CERTO
bcta = cast(ppk, outcome~mfilter)
qui = chisq.test(bcta)
write.table(bcta, "tabela_contingencia.txt", col.names = T, row.names = F)



v6 = d6
v6$recomb = as.character(v6$recomb)
v6$generation = as.numeric(levels(v6$generation))[v6$generation]

#tem que limpar a caralha do V6
png("v6_debugging.png", height = 14, width = 28, units = 'cm', res=300) 
ggplot(v6, aes(x=generation, y=mean_anc, colour=replicate, alpha = 0.1, group=interaction(replicate, recomb))) +
  facet_grid(recomb~batch)+
  geom_line(size = 0.5)+
  guides(fill = F, alpha = F, color = F)+
  xlim(c(1, 30))+
  geom_dl(aes(label = replicate), method = list("maxvar.points", cex = 0.9))
  #, "reduce.cex.lr"))+#, cex = 0.9))+
dev.off()


vv6 = v6[v6$generation == 2,]
ruim = vv6[vv6$mean_anc > 0.6,]
unique(ruim$replicate)
