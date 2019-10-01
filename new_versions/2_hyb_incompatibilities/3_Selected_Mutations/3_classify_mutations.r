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
runs2 = c(runs)

######clas_muts#########################
#                Classifying mutations #
########################################

# This table has all alleles
promut = read.table(paste0("processed_muts_", batch, "_all_reps.txt"), h=T)

#tiro as que perderam a ancestria de "B"
promut = promut[promut$replicate %in% runs2, ]

#taking only the fixed alleles
fix = c("fixed_A", "fixed_B")

d2 = promut[promut$state %in% fix, ]

write.table(
  d2,
  paste0("class_muts_fixed_", typ, "correct.txt"),
  col.names = T,
  row.names = F
)