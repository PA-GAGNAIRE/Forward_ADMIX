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



tata = read.table(paste0("replicates_", typ,".txt"), h=T, stringsAsFactors = F)
runs = unique(tata[,1])
runs

##### intervals delta ##########################
#                       separating into groups #
################################################

lowdat = read.table(paste0("windows_low_all_rep_", typ, ".txt"), h = T)
highdat = read.table(paste0("windows_high_all_rep_", typ, ".txt"), h = T)
lowdat$recomb = "low"
highdat$recomb = "high"

tdo = rbind(highdat, lowdat)

######################
# preliminary graphs #

png(paste0("hist_delta_", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
par(mfrow = c(1,2))
hist(lowdat$delta, main = "low recomb - DELTA")
hist(highdat$delta, main = "high recomb - DELTA")
dev.off()


hg = c(summary(highdat$delta))
lw = c(summary(lowdat$delta))
all = c(summary(tdo$delta))
tb = rbind(all, hg, lw)
tb = data.frame(tb, row.names = NULL, stringsAsFactors = F)
tb$section = c("all", "high", "low")

write.table(paste0("summary_stats_delta_", bb, ".txt"), col.names = T, row.names = F)

td1 = tdo[tdo$gen == (bb+2),]
dval = length(unique(td1$delta))
td1$dfact = as.factor(td1$delta)

png(paste0("hist_delta_1st_gen_", dia, ".png"), width = 42, height = 21, units = "cm", res = 300)
ggplot(td1, aes(x=dfact, fill = recomb, alpha = 0.03))+
  geom_histogram(stat = "count")+
  facet_wrap(~recomb, scales = "free")+
  ggtitle("Delta distributions for the FIRST generation, by recombination rates")+
  guides(alpha = FALSE, fill = FALSE)+
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

##############################
# Separating into categories #
##############################

for (i in 1:nrow(lowdat)) {
  cat(lowdat$pos[i], " ", lowdat$gen[i], " ", toString(lowdat$replicate[i]), "\n")
  if (lowdat$delta[i] <= -2) {
    lowdat$filter[i] = "a[-5 to -2]"
  } else if (lowdat$delta[i] == -1) {
    lowdat$filter[i] = "b[-1]"
  } else if (lowdat$delta[i] == 0) {
    lowdat$filter[i] = "c[0]"
  } else if (lowdat$delta[i] == 1) {
    lowdat$filter[i] = "d[+1]"
  } else if (lowdat$delta[i] >= 2) {
    lowdat$filter[i] = "e[+2 & +5]"
  }
}

for (i in 1:nrow(highdat)) {
  cat(highdat$pos[i], " ", highdat$gen[i], " ", toString(highdat$replicate[i]), "\n")
  if (highdat$delta[i] <= -2) {
    highdat$filter[i] = "a[-5 to -2]"
  } else if (highdat$delta[i] == -1) {
    highdat$filter[i] = "b[-1]"
  } else if (highdat$delta[i] == 0) {
    highdat$filter[i] = "c[0]"
  } else if (highdat$delta[i] == 1) {
    highdat$filter[i] = "d[+1]"
  } else if (highdat$delta[i] >= 2) {
    highdat$filter[i] = "e[+2 & +5]"
  }
}

write.table(
  highdat,
  paste0("delta_categories_CORRIGIDO_HIGH.txt"),
  col.names = T,
  row.names = F
)

write.table(
  lowdat,
  paste0("delta_categories_CORRIGIDO_LOW.txt"),
  col.names = T,
  row.names = F
)

# first, I have a problem in relation to the generations in each data (ancestry is only 50 data points, while the delta/mut_freqs is 300 gen)

highdat = read.table("delta_categories_CORRIGIDO_HIGH.txt", h=T)
lowdat = read.table("delta_categories_CORRIGIDO_LOW.txt", h=T)
                     
                     

dfinal1 = highdat[highdat$gen %in% generations[[1]], ]
dfinal2 = lowdat[lowdat$gen %in% generations[[1]], ]

write.table(
  dfinal1,
  paste0("win_all_51_gen_CORRIGIDO_HIGH.txt"),
  col.names = T,
  row.names = F
)

write.table(
  dfinal2,
  paste0("win_all_51_gen_CORRIGIDO_LOW.txt"),
  col.names = T,
  row.names = F
)
