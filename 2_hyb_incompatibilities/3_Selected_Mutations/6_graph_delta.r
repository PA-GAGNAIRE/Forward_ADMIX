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


###variables########################
#              important variables #
####################################

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(directlabels)
library(plyr)


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

gen_wind = seq((bb+1), (bb+300), 1)

#####separating outcomes################################
#                           Separating runs by outcome #
########################################################

tata = read.table(paste0("replicates_", typ,".txt"), h=T, stringsAsFactors = F)
runs = unique(tata[,1])
                  
                  
######plots#####################
#           Delta vs. Ancestry #
################################
                  
dhigh = read.table(paste0("win_all_51_gen_CORRIGIDO_HIGH.txt"), h=T)
dlow = read.table(paste0("win_all_51_gen_CORRIGIDO_LOW.txt"), h=T)
dtlow = read.table(paste0("win_anc_all_rep_LOW_.txt"), h=T)
dthigh = read.table(paste0("win_anc_all_rep.txt"), h=T)

a = merge(dhigh, dthigh, by= c("pos", "gen", "replicate", "recomb"))
z = merge(dlow, dtlow, by= c("pos", "gen", "replicate", "recomb"))
a = a[order(a$pos, a$replicate),]
z = z[order(z$pos, z$replicate),]
a = unique(a) #122500 to 
z = unique(z) #12250
az = rbind(a, z)

##az = az[order(az$pos, az$replicate),]

write.table(a, paste0("high_ancestry_delta_", tp, ".txt"), col.names = T, row.names = F)
write.table(z, paste0("low_ancestry_delta_", tp, ".txt"), col.names = T, row.names = F)
write.table(az, paste0("ALL_ancestry_delta_", tp, ".txt"), col.names = T, row.names = F)
                
                  
a= read.table(paste0("high_ancestry_delta_", tp, ".txt"), h=T)
z= read.table(paste0("low_ancestry_delta_", tp, ".txt"), h=T)
az=read.table(paste0("ALL_ancestry_delta_", tp, ".txt"), h=T)
                  
a$dfact = as.factor(a$delta)
a$time = as.factor(a$gen)
z$dfact = as.factor(z$delta)
z$time = as.factor(z$gen)

az$dfact = as.factor(az$delta)
az$time = as.factor(az$gen)

png(paste0("delta_histo_", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
par(mfrow = c(1,2))
hist(a$delta, main = paste0("high recomb - ", typ))
hist(z$delta, main = paste0("low recomb - ", typ))
dev.off()
par(mfrow = c(1,1))

az1 = az[az$gen == bb+2,]
dval = length(unique(az1$filter))

png(paste0("delta_filter_histo_", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(az1, aes(x=filter, fill = recomb))+
  geom_histogram(stat = "count")+
  facet_wrap(~recomb, scales = "free")+
  ggtitle("Delta distributions for the FIRST generation, by recombination rates")+
  guides(alpha = FALSE, fill = FALSE)+
  #scale_x_continuous(breaks=c(min(td1$delta):max(td1$delta)))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "lightgray"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90, size = 9))
dev.off()



png(paste0("anc_vs_delta_all_gen_", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(az, aes(x = time, y= mean_anc, fill = filter))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~recomb, ncol=1)+
  ggtitle("Ancestry over time according to Delta - 2x RECOMB")+
  theme(panel.background = element_rect(fill = "white"),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"),
            panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                              colour = "lightgray"),
            axis.line = element_line(size = 0.5, linetype = "solid",
                   colour = "black"),
  plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(angle=90, size = 9))
dev.off()


azshort = az[az$gen <= bb+30,]
azshort$time = as.numeric(levels(azshort$time))[azshort$time]
azshort$time = as.factor(azshort$time)

png(paste0("anc_vs_delta_30_gen_", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(azshort, aes(x = time, y= mean_anc, fill = filter))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~recomb, ncol=1)+
  ggtitle("Ancestry over time according to Delta - 2x RECOMB")+
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
  
shape = c(97:127, 0:20, 48:57, 35:38)

zz = z[z$gen < 20030,]


# png(paste0("anc_delta_replicates_LOW_", dia,".png"), width = 42, height = 21, units = "cm", res = 300)
# ggplot(zz, aes(x= delta, y=mean_anc, color=replicate, size=1.1))+
#   geom_jitter(aes(shape = replicate, alpha = 0.5))+
#   #facet_wrap(~recomb, ncol = 1)+
#   scale_shape_manual(values=shape)+
#   guides(shape = guide_legend(override.aes = list(size = 5)))+
#   ggtitle(paste0("30 generations - Low recomb"))
# dev.off()

# png(paste0("anc_delta_replicates_both_recomb_", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
# ggplot(azshort, aes(x= delta, y=mean_anc, color=replicate, size=1.1))+
#   geom_jitter(aes(shape = replicate, alpha = 0.5))+
#   facet_wrap(~recomb, ncol = 1)+
#   scale_shape_manual(values=shape)+
#   guides(shape = guide_legend(override.aes = list(size = 5)), alpha = FALSE, size = FALSE)+
#   ggtitle(paste0("First 30 generations"))
# dev.off()


# aa = a[a$gen < 20030,]
# png(paste0("anc_delta_replicates_recomb_HIGH", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
# ggplot(aa, aes(x= delta, y=mean_anc, color=replicate, size=1.1))+
#   geom_jitter(aes(shape = replicate, alpha = 0.5))+
#   #facet_wrap(~recomb, ncol = 1)+
#   scale_shape_manual(values=shape)+
#   guides(shape = guide_legend(override.aes = list(size = 5)), alpha = FALSE, size = FALSE)+
#   ggtitle(paste0("First 30 gen - High recomb"))
# dev.off()





library(moments)
# skewness(data$delta)

dd = az[az$gen == bb+2,]
r = "run_1"
res = c()
for (r in runs){
dat = dd[dd$replicate == r,]
sk = skewness(dat$delta)
mn = mean(dat$delta)
linha = c(r, sk, mn)
res = rbind(res, linha)
  cat(r, " ",skewness(dat$delta), "\n")
}

res = data.frame(res, row.names = NULL)
names(res) = c("replicate", "skew", "mean")
write.table(res, paste0("skewness_", typ, ".txt"), col.names = T, row.names = F)
                  
png(paste0("delta_density_raw_", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
plot(density(az$delta))
lines(density(a$delta), col = "red")
lines(density(z$delta), col = "blue")
lines(density(z$delta), col = "white", lty=2)
dev.off()
