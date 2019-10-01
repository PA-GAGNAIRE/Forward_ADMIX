ver = "v_6"

setwd(paste0("~/dmi_long_divergence/dmi_avdqnf/raw_data/", ver, "/"))

path = paste0("~/dmi_long_divergence/dmi_avdqnf/parsed_data/", ver, "/")


#proporcao inicial de B

if (ver == "v_5") {
  pb = 0.2
} else if (ver == "v_6") {
  pb = 0.5
} else if (ver == "v_7") {
  pb = 0.8
}

#12000, 
#batch = c(16000, 18000, 20000)
batch = c(12000, 14000, 16000, 18000, 20000)


#######analises#####


data = read.table("mean_ancestry_all_data.txt", header = T)

tudo = c()

tosamp = seq(50, 300, 1)


for (b in batch){
  dc = data[data$batch == b,]
  dc = dc[dc$generation %in% tosamp,]
  for (i in 1:nrow(dc)){
    dc$change[i] = abs(pb - dc$mean_anc[i])
  }
  rpoly = read.table(paste0(path, b, "_persist/replicates_POLY.txt"), h=T)
  rloss = read.table(paste0(path, b, "_loss/replicates_LOSS.txt" ), h=T)
  rpoly = unique(rpoly[,1])
  rloss = unique(rloss[,1])
  dcp = dc[dc$replicate %in% rpoly,]
  dcl = dc[dc$replicate %in% rloss,]
  dcp$outcome = "persist"
  dcl$outcome = "loss"
  tudo = rbind(tudo, dcp, dcl)
}

batch = c(12000, 14000)
for (b in batch){
  dc = data[data$batch == b,]
  dc = dc[dc$generation %in% tosamp,]
  for (i in 1:nrow(dc)){
    dc$change[i] = abs(pb - dc$mean_anc[i])
  }
  rpoly = read.table(paste0(path, b, "_persist/replicates_POLY.txt"), h=T)
  #rloss = read.table(paste0(path, b, "_loss/replicates_LOSS.txt" ), h=T)
  rpoly = unique(rpoly[,1])
  #rloss = unique(rloss[,1])
  dcp = dc[dc$replicate %in% rpoly,]
  #dcl = dc[dc$replicate %in% rloss,]
  dcp$outcome = "persist"
  #dcl$outcome = "loss"
  tudo = rbind(tudo, dcp)
}


write.table(tudo, paste0("ancestry_change_", ver, "_loss_poly.txt"), col.names = T, row.names = F)
