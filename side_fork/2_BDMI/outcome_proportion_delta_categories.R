setwd("C:/Users/flavi/Documents/dmi_long_divergence/dmi_avdqnf/parsed_data")

library("ggpubr")
library("ggplot2")

bb = 20000
dia = format(Sys.Date(), "%d_%m_%Y")
dia

l5 = read.table(paste0("v_5/", bb, "_loss/windows_LOSS_B.txt"), h=T)
l7 = read.table(paste0("v_7/", bb, "_loss/windows_LOSS_B.txt"), h=T)
p5 = read.table(paste0("v_5/", bb, "_persist/windows_POLY_B.txt"), h=T)
p7 = read.table(paste0("v_7/", bb, "_persist/windows_POLY_B.txt"), h=T)

l5$outcome = "Loss"
l7$outcome = "Loss"
p5$outcome = "Polymorphism"
p7$outcome = "Polymorphism"
l5$ver = "80/20"
p5$ver = "80/20"
l7$ver = "20/80"
p7$ver = "20/80"

window = rbind(l5, l7, p5, p7)

window$time = as.factor(window$gen)

w1 = window[window$gen == (bb+2),]

uu = w1

#80/20 = A is the major parent
#positive values --> major parent
#nega values == minor
#a[-5 to -2] == +2 to +5 minor
#b[-1] == +1 minor
#c[0] = zero
#d[+1] = +1 major
#e[+2 & +5] +2 a +5 major

#f[minor +2 to +5]
#g[minor +1]
#h[0]
#i[major +1]
#j[major +2 to +5]

#(uu$filter[i] == ){uu$mfilter[i] = }

for (i in 1:nrow(uu)) {
  if (uu$ver[i] == "80/20") {
    if (uu$filter[i] == "a[-5 to -2]") {
      uu$mfilter[i] = "f[minor +2 to +5]"
    }
    else if (uu$filter[i] == "b[-1]") {
      uu$mfilter[i] = "g[minor +1]"
    } else if (uu$filter[i] == "c[0]") {
      uu$mfilter[i] = "h[0]"
    } else if (uu$filter[i] == "d[+1]") {
      uu$mfilter[i] = "i[major +1]"
    } else if (uu$filter[i] == "e[+2 & +5]") {
      uu$mfilter[i] = "j[major +2 to +5]"
    }
  } else if (uu$ver[i] == "20/80") {
    if (uu$filter[i] == "e[+2 & +5]") {
      uu$mfilter[i] = "f[minor +2 to +5]"
    } else if (uu$filter[i] == "d[+1]") {
      uu$mfilter[i] = "g[minor +1]"
    } else if (uu$filter[i] == "c[0]") {
      uu$mfilter[i] = "h[0]"
    } else if (uu$filter[i] == "b[-1]") {
      uu$mfilter[i] = "i[major +1]"
    } else if (uu$filter[i] == "a[-5 to -2]") {
      uu$mfilter[i] = "j[major +2 to +5]"
    }
  }
  
}


write.table(uu, "data_grapfico_barras_minor_major.txt", col.names = T, row.names = F)


png(paste0("7_diff_outcomes_delta_v5_v7_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(w1, aes(x = filter, y = num, fill= outcome))+
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ver, ncol=3)+
  scale_y_continuous(labels = scales::percent)+
  ggtitle("Proportion of Runs by Simulation Results")+
  scale_fill_manual(values= c("#856fea", "#009200"))+
  scale_color_manual(values=c("#856fea", "#009200"))+
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
dev.off()


png(paste0("8_diff_outcomes_delta_v5_v7_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(w1, aes(x = filter, y = num, fill= outcome))+
  geom_bar(stat = "identity", position = "fill") +
  
  scale_y_continuous(labels = scales::percent)+
  ggtitle("Proportion of Runs by Simulation Results")+
  scale_fill_manual(values= c("#856fea", "#009200"))+
  scale_color_manual(values=c("#856fea", "#009200"))+
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
        axis.text.x = element_text(angle=0, colour = "black", size = 12))+
  #coord_flip()+
  facet_grid(~ver)
dev.off()



png(paste0("9_diff_outcomes_delta_v5_v7_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(uu, aes(x = mfilter, y = num, fill= outcome))+
  geom_bar(stat = "identity", position = "fill") +
  #facet_wrap(~ver, ncol=3)+
  scale_y_continuous(labels = scales::percent)+
  ggtitle("Proportion of Runs by Simulation Results")+
  scale_fill_manual(values = c("#4cbef0", "#c27e1d"))+
  scale_color_manual(values = c("#4cbef0", "#c27e1d"))+
  #scale_fill_manual(values= c("#856fea", "#009200"))+
  #scale_color_manual(values=c("#856fea", "#009200"))+
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
dev.off()
