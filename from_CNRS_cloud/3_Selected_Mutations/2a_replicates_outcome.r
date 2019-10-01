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

alruns = c(
  "run_1",
  "run_2",
  "run_3",
  "run_4",
  "run_5",
  "run_6",
  "run_7",
  "run_8",
  "run_9",
  "run_10",
  "run_11",
  "run_12",
  "run_13",
  "run_14",
  "run_15",
  "run_16",
  "run_17",
  "run_18",
  "run_19",
  "run_20",
  "run_21",
  "run_22",
  "run_23",
  "run_24",
  "run_25",
  "run_26",
  "run_27",
  "run_28",
  "run_29",
  "run_30",
  "run_31",
  "run_32",
  "run_33",
  "run_34",
  "run_35",
  "run_36",
  "run_37",
  "run_38",
  "run_39",
  "run_40",
  "run_41",
  "run_42",
  "run_43",
  "run_44",
  "run_45",
  "run_46",
  "run_47",
  "run_48",
  "run_49",
  "run_50"
)

#####separating outcomes################################
#                           Separating runs by outcome #
########################################################

#count number of windows according to delta group

#atual_parsed_muts_20000_all_reps.txt
t2 = read.table(paste0("atual_parsed_muts_", bb, "_all_reps.txt"), h=T)

out = t2[t2$gen.y == (bb+300),]

sout = ddply(out, .(gen.y, state, replicate), summarise, mean_freq=mean(freq.y))
mod = c("fixed_B", "poly_B")
pout = sout[sout$state %in% mod, ]
pout$replicate = as.character(pout$replicate)
rpoly = c(unique(pout$replicate))
reppoly = rpoly
po = pout[pout$replicate %in% rpoly,]
rloss = sout[!sout$replicate %in% rpoly,]
rloss$replicate = as.character(rloss$replicate)
reploss = c(unique(rloss$replicate))

reploss #this is the runs that b lineage was lost
reppoly #these are the runs in which lineage b persisted

rss = data.frame(reploss, row.names = NULL, stringsAsFactors = F)
rss$reploss = as.character(rss$reploss)

rit = data.frame(reppoly, row.names = NULL, stringsAsFactors = F)
rit$reppoly = as.character(rit$reppoly)

write.table(rss, "replicates_LOSS.txt", col.names = T, row.names = F)
write.table(rit, "replicates_POLY.txt", col.names = T, row.names = F)

#tata = read.table(paste0("replicates_", typ,".txt", h=T, stringsAsFactors = F)
#runs = unique(tata[,1])