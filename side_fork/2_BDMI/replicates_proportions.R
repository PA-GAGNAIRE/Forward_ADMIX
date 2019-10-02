###########setting up#############

library(plyr)
library(ggplot2)

dia = format(Sys.Date(), "%d_%m_%Y")
dia

#############first scenario p2 = 80%####################
setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/v_7/")

batch = c(20000, 18000, 16000)
b = 20000
dado = c()
for (b in batch){
  loss = read.table(paste0(b, "_loss/replicates_LOSS.txt"), h=T, stringsAsFactors = F)
  loss$outcome = "loss"
  names(loss) = c("replicate", "outcome")
  poly = read.table(paste0(b, "_persist/replicates_POLY.txt"), h=T, stringsAsFactors = F)
  poly$outcome = "persist"
  names(poly) = c("replicate", "outcome")
  temp = rbind(poly, loss)
  temp$batch = b
  dado = rbind(dado, temp)
}
#kct = ddply(azz, .(filter, gen, replicate), summarise, num= length(filter))
resumo = ddply(dado, .(outcome, batch), summarise, freq=length(replicate))

resumo$scenario = "p2_80"
write.table(resumo, "resumo_proportions_outcomes.txt", col.names = T, row.names = F)


####second scenario p2 = 20%##########
setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/v_5/")


batch = c(20000, 18000, 16000, 14000)

dado = c()
for (b in batch){
  loss = read.table(paste0(b, "_loss/replicates_LOSS.txt"), h=T, stringsAsFactors = F)
  loss$outcome = "loss"
  names(loss) = c("replicate", "outcome")
  poly = read.table(paste0(b, "_persist/replicates_POLY.txt"), h=T, stringsAsFactors = F)
  poly$outcome = "persist"
  names(poly) = c("replicate", "outcome")
  temp = rbind(poly, loss)
  temp$batch = b
  dado = rbind(dado, temp)
}
#kct = ddply(azz, .(filter, gen, replicate), summarise, num= length(filter))
resumo = ddply(dado, .(outcome, batch), summarise, freq=length(replicate))

resumo$scenario = "p2_20"
write.table(resumo, "resumo_proportions_outcomes.txt", col.names = T, row.names = F)

######juntando todas as coisas############

setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/")


p80 = read.table("v_7/resumo_proportions_outcomes.txt", h=T)
p20 = read.table("v_5/resumo_proportions_outcomes.txt", h=T)

k14 = c("loss", 14000, 0, "p2_80")
k142 = c("persist", 14000, 50, "p2_80")
p80 = rbind(p80, k14, k142)
prop = rbind(p20, p80)
prop$freq = as.numeric(prop$freq)


lost = prop[prop$outcome == "loss",]

for (i in 1:nrow(lost)){
  if (lost$freq[i] > 0){lost$prop[i] = (lost$freq[i])/50} else {lost$prop[i] = 0}
}

for (i in 1:nrow(lost)){
  if (lost$scenario[i] == "p2_20"){
    lost$minor[i] = "B"} else {lost$minor[i] = "A"} 
}

#verde #rosa, #azul, vermelho, amarelo, roxo
#"#009200", "#ff00ae", "#4cbef0", "#f0382f",  "#c27e1d",  "#856fea"

#verde com      rosa/roxo/azul
#vermelho com   verde/roxo/azul
#azul com       vermelho/rosa/verde
#amarelo com    roxo/rosa/azul
#rosa com       verde/azul/amarelo
#roxo com       verde/vermelho/amarelo

png(paste0("2_outcome_proporptions_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(lost, aes(x=batch, y = prop, color = minor, group = minor))+
  geom_line(size=1)+
  geom_point(size = 2)+
  scale_color_manual(values = c("#009200", "#ff00ae"))+
  xlab("Divergence Time (generations)")+
  ylab("Proportion")+
  ggtitle("Proportion of Minor Ancestry Loss")+
  labs(color = "Minor parent")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "lightgray"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(angle=0, colour = "black", size = 14),
        axis.text.x = element_text(angle=0, colour = "black", size = 14))

dev.off()
