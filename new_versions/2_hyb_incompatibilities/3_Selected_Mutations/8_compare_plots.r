##############
# MUDAR AQUI #
#~~~~~~~~~~~~~~~~~
batch = c(20000) #
bb = 20000       #
#~~~~~~~~~~~~~~~~~
##############
# MUDAR AQUI #
#~~~~~~~~~~~~~~~~~
#typ = "LOSS"    #
#tp = "loss"     #
#path = "loss"   #
typ = "POLY"     #
tp = "poly"      #
path = "persist" #
#~~~~~~~~~~~~~~~~~
##############
# MUDAR AQUI #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
computador="/media/pagagnaire/18E23B38E23B1A08/temp_FLAVIA/dmi_avdqnf/v_5/" #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd(paste0(computador, bb, "_", path))

getwd()

dia = format(Sys.Date(), "%d_%m_%Y")
dia

library("ggpubr")
library("ggplot2")

loss = read.table(paste0("../", bb, "_loss/windows_LOSS_B.txt"), h=T)
poly = read.table(paste0("../", bb, "_persist/windows_POLY_B.txt"), h=T)


loss$outcome = "loss"

poly$outcome = "persist"


window = rbind(loss, poly)

window$time = as.factor(window$gen)

w1 = window[window$gen == (bb+2),]

png(paste0("6_diff_outcomes_delta_proportions_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(w1, aes(x = filter, y = num, fill= outcome))+
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent)+
  ggtitle("Proportion of Runs by outcome (5 categories)")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "lightgray"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90))
dev.off()


per = read.table(paste0("../", bb, "_persist/ALL_ancestry_delta_poly.txt"), h=T)
lot = read.table(paste0("../", bb, "_loss/ALL_ancestry_delta_loss.txt"), h= T)
per$outcome = "persist"
lot$outcome = "loss"
perr = unique(per)
lott = unique(lot)

tudo = rbind(perr, lott)
t1 = tudo[tudo$gen == bb+2,]
t1$dfact = as.factor(t1$delta)


png(paste0("7_diff_outcomes_delta_raw_props_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(t1, aes(x =dfact, fill= outcome))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)+
  ggtitle("Proportion of Runs by outcome - raw data")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "lightgray"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90))
dev.off()


#############/##############
skpo = read.table(paste0("../", bb, "_persist/skewness_POLY.txt"), h=T)
sklo = read.table(paste0("../", bb, "_loss/skewness_LOSS.txt"), h=T)
skpo$outcome = "persit"
sklo$outcome = "loss"


tsk = rbind(skpo, sklo)
tsk$outcome = as.factor(tsk$outcome)

png("skewness_mean_1_16-01-2019.png", width = 42, height = 21, units = "cm", res = 300)
par(mfrow=c(1, 2))
plot(tsk$skew ~ tsk$outcome)
plot(tsk$mean ~ tsk$outcome)
dev.off()

par(mfrow = c(1,1))

um = ggplot(tsk, aes(x = outcome, y = skew, fill = outcome, alpha = 0.7))+
  geom_boxplot(outlier.shape = NA)+
  guides(alpha = FALSE, fill = FALSE)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "lightgray"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90))

dois = ggplot(tsk, aes(x= outcome, y = mean, fill = outcome, alpha = 0.7))+
  geom_boxplot(outlier.shape = NA)+
  guides(alpha = FALSE)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "lightgray"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90))



png("delta_skewness_mean_2.png", width = 42, height = 21, units = "cm", res = 300)
ggarrange(um, dois,
          labels = c("skewness", "mean"), hjust = c(-.7, -1.2), vjust = 1,  font.label = list(size = 18, face = "bold", color ="darkblue"),
          ncol = 2, nrow = 1)
dev.off()
