setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/")

#verde        #rosa,     #azul,    vermelho,    amarelo,    roxo
#"#009200", "#ff00ae", "#4cbef0", "#f0382f",  "#c27e1d",  "#856fea"

#verde com      rosa/roxo/azul
#vermelho com   verde/roxo/azul
#azul com       vermelho/rosa/verde
#amarelo com    roxo/rosa/azul
#rosa com       verde/azul/amarelo
#roxo com       verde/vermelho/amarelo

#azul escuro POSTER "#001350"
azul = c("#001350")
rosa = c("#ff3399")

#rosas e azuis:
cores = c("#ff33cc", "#cc0099", "#ff3399", "#cc79a7", "#1c6780", "#990066", "#092c48", rosa, azul, "#1f7276")
pie(rep(1, length(cores)), col = cores)

rosa = "#cc79a7"
azul = "#1c6780"
rosa = "#ff33cc"



setwd("C:/Users/flavi/Documents/dmi_long_divergence/dmi_avdqnf/parsed_data")

library("ggpubr")
library("ggplot2")
library(plyr)

bb = 20000
dia = format(Sys.Date(), "%d_%m_%Y")
dia


data = read.table("minor_major_tudo_junto.txt", h=T)
uu = read.table("data_grapfico_barras_minor_major.txt", h=T)
mean(data$num)

vra = table(uu$mfilter, uu$outcome)
vra = table(uu$outcome, uu$filter)

#ppk = ftable(kct)
#ppk = table(kct, margin =2)
#ppk = xtabs(~outcome+mfilter, data=kct)

uu = read.table("data_grapfico_barras_minor_major.txt", h=T)
cont = ddply(uu, .(mfilter, outcome), summarise, conta=sum(num))
kct = ddply(cont, .(mfilter, outcome), summarise, win=(sum(conta)))

library(reshape)
ppk = melt(kct, id.vars = c("outcome", "mfilter"))
###CARALHO SAPORRA DEU CERTO
bcta = cast(ppk, outcome~mfilter)
qui = chisq.test(bcta)
write.table(bcta, "tabela_contingencia.txt", col.names = T, row.names = F)
# > bcta
# outcome f[minor +2 to +5] g[minor +1] h[0] i[major +1] j[major +2 to +5]
# 1         Loss               340         960  753        1016               451
# 2 Polymorphism               284         632  406         487               171

# > qui = chisq.test(bcta)
# > qui
# 
# Pearson's Chi-squared test
# 
# data:  bcta
# X-squared = 62.421, df = 4, p-value = 8.983e-13


# > qui$expected
#               f[minor +2 to +5]   g[minor +1]   h[0] i[major +1] j[major +2 to +5]
# Loss                    399.36     1018.88    741.76      961.92            398.08
# Polymorphism            224.64      573.12    417.24      541.08            223.92

# > sum(sum(bcta[1,2:6]),  sum(bcta[2,2:6]))
# [1] 5500

ggplot()



caneco = prop.table(bcta)

mutations = ggplot(data, aes(y = num, fill= result, x=result))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~parent, ncol = 2, strip.position = "top")+
  #coord_flip()+
  guides(color = F, alpha = F, fill =F)+
  #                             azul        amarelo
  # scale_fill_manual(values = c("#4cbef0", "#c27e1d"))+
  # scale_color_manual(values = c("#4cbef0", "#c27e1d"))+
  scale_fill_manual(values = c(azul, rosa))+
  scale_color_manual(values = c(azul, rosa))+
  #xlab("Simulation Result")+
  ylab("Number of Derived Alleles")+
  xlab("Simulation Results")+
  #ggtitle("Fixed mutations and the result of the simulations - V5 + v7")+
  labs(fill = "Simulation\nResult")+
  geom_segment(aes(x=0.4,y=36.04,yend=36.04,xend=2.6),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        legend.title = element_text(colour="black", size=16, face = "bold"),
        legend.text = element_text(colour="black", size=14, face = "bold"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        #axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 14),
        axis.title.x = element_text(size = 16),
        #axis.title.x=element_blank(),
        axis.text.x = element_text(angle=0, colour = "black", size = 12),
        axis.ticks.x=element_blank())
        
mutations

deltas = ggplot(uu, aes(x = mfilter, y = num, fill= outcome))+
  geom_bar(stat = "identity", position = "fill") +
  #facet_wrap(~ver, ncol=3)+
  guides(alpha = F)+
  scale_y_continuous(labels = scales::percent)+
  #ggtitle("Proportion of Runs by Simulation Results")+
  # scale_fill_manual(values = c("#4cbef0", "#c27e1d"))+
  # scale_color_manual(values = c("#4cbef0", "#c27e1d"))+
  #scale_fill_manual(values= c("#856fea", "#009200"))+
  #scale_color_manual(values=c("#856fea", "#009200"))+
  scale_fill_manual(values = c(azul, rosa))+
  scale_color_manual(values = c(azul, rosa))+
  xlab("Delta categories")+
  ylab("Percentage")+
  labs(fill = "Simulation\nResult")+
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
        axis.text.x = element_text(angle=0, colour = "black", size = 12))


png(paste0("NOVO_outcome_fixed_muts_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
mutations
dev.off()

png(paste0("NOVO_deltas_proportions_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
deltas
dev.off()

png("mutation_delta_effect_result.png", width = 42, height = 21, units = "cm", res = 300)
ggarrange(mutations, deltas,
          widths = 1:2,
          labels = c("1", "2"), hjust = c(-.7, -1.2), vjust = 1,  font.label = list(size = 18, face = "bold", color ="black"),
          ncol = 2, nrow = 1)
dev.off()

png("mutation_delta_effect_result_2.png", width = 42, height = 21, units = "cm", res = 300)
ggarrange(mutations, deltas,
          widths = 1:1,
          labels = c("1", "2"), hjust = c(-.7, -1.2), vjust = 1,  font.label = list(size = 18, face = "bold", color ="black"),
          ncol = 2, nrow = 1)
dev.off()

png("mutation_delta_effect_result_3.png", width = 42, height = 21, units = "cm", res = 300)
ggarrange(mutations, deltas,
          widths = 2:1.5,
          labels = c("1", "2"), hjust = c(-.7, -1.2), vjust = 1,  font.label = list(size = 18, face = "bold", color ="black"),
          ncol = 2, nrow = 1)
dev.off()

mutations2 = ggplot(data, aes(y = num, fill= result, x=result))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~parent, ncol = 2, strip.position = "top")+
  #coord_flip()+
  guides(alpha = F)+
  # scale_fill_manual(values = c("#4cbef0", "#c27e1d"))+
  # scale_color_manual(values = c("#4cbef0", "#c27e1d"))+
  scale_fill_manual(values = c(azul, rosa))+
  scale_color_manual(values = c(azul, rosa))+
  #xlab("Simulation Result")+
  ylab("Number of Derived Alleles")+
  xlab("Simulation Results")+
  #ggtitle("Fixed mutations and the result of the simulations - V5 + v7")+
  labs(fill = "Simulation Result")+
  geom_segment(aes(x=0.4,y=36.04,yend=36.04,xend=2.6),inherit.aes=FALSE, linetype=2, color="black", size=0.7)+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(colour="black", size=16, face = "bold"),
        legend.text = element_text(colour="black", size=14, face = "bold"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        axis.line.x = element_line(size = 0.1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y.right = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 20, vjust = 0.9),
        #axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        axis.text.y = element_text(angle=0, colour = "black", size = 14),
        axis.title.x = element_text(size = 16),
        #axis.title.x=element_blank(),
        axis.text.x = element_text(angle=0, colour = "black", size = 12),
        axis.ticks.x=element_blank())

mutations2

deltas2 = ggplot(uu, aes(x = mfilter, y = num, fill= outcome))+
  geom_bar(stat = "identity", position = "fill") +
  #facet_wrap(~ver, ncol=3)+
  guides(color = F, alpha = F, fill =F)+
  scale_y_continuous(labels = scales::percent)+
  #ggtitle("Proportion of Runs by Simulation Results")+
  # scale_fill_manual(values = c("#4cbef0", "#c27e1d"))+
  # scale_color_manual(values = c("#4cbef0", "#c27e1d"))+
  #scale_fill_manual(values= c("#856fea", "#009200"))+
  #scale_color_manual(values=c("#856fea", "#009200"))+
  scale_fill_manual(values = c(azul, rosa))+
  scale_color_manual(values = c(azul, rosa))+
  xlab("Delta categories")+
  ylab("Percentage")+
  labs(fill = "Simulation\nResult")+
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
        axis.text.x = element_text(angle=0, colour = "black", size = 12))

deltas2

png(paste0("mutation_delta_effect_result_11_", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
ggarrange(deltas2, mutations2,
          widths = 1:1,
          #labels = c("1", "2"), hjust = c(-.7, -1.2), vjust = 1,  font.label = list(size = 18, face = "bold", color ="black"),
          ncol = 2, nrow = 1)
dev.off()



