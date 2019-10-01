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

library(plyr)


az = read.table(paste0("ALL_ancestry_delta_", tp, ".txt"), h= T)
az$dfact = as.factor(az$delta)
az$time = as.factor(az$gen)

azz = unique(az)
caz = azz

kct = ddply(azz, .(filter, gen, replicate), summarise, num= length(filter))
kct$time = as.factor(kct$gen)

kctin = ddply(azz, .(filter, gen, replicate, dfact), summarise, num_filter = length(filter), num_delta = length(dfact))

write.table(kct, paste0("windows_", typ, "_B.txt"), col.names = T, row.names = F)

write.table(kctin, paste0("windows_", typ, "_B_2.txt"), col.names = T, row.names = F)
