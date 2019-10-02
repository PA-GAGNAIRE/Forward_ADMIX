##### SET SHIT UP ###############
setwd("~/dmi_long_divergence/dmi_avdqnf/raw_data/")


dia = format(Sys.Date(), "%d_%m_%Y")
dia
"ancestry_change_ver_loss_poly.txt"
d5 = read.table(paste0("v_5/ancestry_change_v_5_loss_poly.txt"), h=T)
d6 = read.table(paste0("v_6/ancestry_change_v_6_loss_poly.txt"), h=T)
d7 = read.table(paste0("v_7/ancestry_change_v_7_loss_poly.txt"), h=T)

d5$pb = "80/20"
d6$pb = "50/50"
d7$pb = "20/80"

data = rbind(d5, d6, d7)
data$admixture = as.factor(data$pb)
data$batch = as.factor(data$batch)

data = unique(data)

for (i in 1:nrow(data)){
  if (data$recomb[i] == "high"){data$rate[i] = "1e-3"} else {data$rate[i] = "1e-4"}
}

write.table(data, "absolute_ancestry_change_loss_poly.txt", col.names = T, row.names = F)



dd = unique(data)


png(paste0("Ancestry_absolute_change_", dia,  "_POLY_LOSS.png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(dd, aes(x = batch, y= change, fill =rate, color = rate, alpha = 0.5))+
  geom_boxplot(outlier.colour = NA)+
  facet_wrap(~admixture, ncol = 3)+
  ylim(0, 0.25)+
  xlab("Divergence Time\n(generations)")+
  ylab("Absolute Ancestry Change")+
  ggtitle("Mean Ancestry Change")+
  scale_color_manual(values = c("#ff00ae", "#4cbef0"))+
  scale_fill_manual(values = c("#ff00ae", "#4cbef0"))+
  guides(alpha = F, color = F)+
  labs(fill = "Recombination\nRate")+
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
        axis.text.x = element_text(angle=0, colour = "black", size = 8))
dev.off()
