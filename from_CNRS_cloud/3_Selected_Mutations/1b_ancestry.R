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


runs = c("run_1", "run_2", "run_3", "run_4", "run_5", "run_6", "run_7", "run_8", "run_9", "run_10", "run_11","run_12", "run_13", "run_14", "run_15", "run_16", "run_17", "run_18", "run_19", "run_20", "run_21", "run_22", "run_23", "run_24", "run_25", "run_26", "run_27", "run_28", "run_29", "run_30", "run_31", "run_32", "run_33", "run_34", "run_35", "run_36", "run_37", "run_38", "run_39", "run_40", "run_41", "run_42", "run_43", "run_44", "run_45", "run_46", "run_47", "run_48", "run_49", "run_50")


for (b in batch){
  assign(paste0("gen_1_", b), seq(b+2, b+50, 2))
  assign(paste0("gen_2_", b), seq(b+60, b+300, 10))
  assign(paste0("gener_", b), append(get(paste0("gen_1_", b)), get(paste0("gen_2_", b))))
} 

#depending on which you use, you have to ~refresh the generation values
generations = list(get(paste0("gener_", bb)))

for (i in 1:length(generations)){
  for (u in 1:length(generations[[i]])){
    generations[[i]][u]= toString(generations[[i]][u])
  }
}

gen_wind = seq((bb+1), (bb+300), 1)

generations

########binned ancestry###############################
#                         cocatenating ancestry data #
######################################################+

temptab = c()
for (b in 1:length(batch)){
  for (g in 1:length(generations[[b]])){
    lala = read.table(paste0("../binned/recomb/bin_b_", generations[[b]][g], "_all.txt"), header = T)
    temptab = rbind(temptab, lala)
  }
}

write.table(temptab, paste0("all_ancestry.txt"), row.names = F, col.names = T)



#####ancestry w/ delta################################## 
#                       1cM windows with mean ancestry #
########################################################

anc = read.table("all_ancestry.txt", header = T)

#subset of the replicates in which b ancestry persisted
persist = anc[anc$replicate %in% runs,]
phigh = persist[persist$recomb == "high",]
plow = persist[persist$recomb == "low",]


#now I have to do that loop again with the windows
#this is binning ancestry to the right intervals

# # # # # # # # # # # #
# High recombination  #
# # # # # # # # # # # #

#####high recomb#####
janela= c()
win_table = c()
tudob = c()

for (b in 1:length(batch)){
  ma = phigh
  for (r in runs){
    maa = ma[ma$replicate == r,]
    win_table = c()
    for (g in generations[[b]]){
      #this is for high recomb
      mah = maa[maa$raw_gen == g,]
      d = 1
      end = 1000
      i = 1
      cat(paste0(r, " ", g, "\n"))
      while(end <= mah[nrow(mah),1]){
        while (mah$pos[i] < end){i = i+1}
        tab = mah[d:i-1,]
        mean_anc = mean(tab$ancestry)
        raw_gen = as.integer(g) - batch[b]
        res = c((end - 500),  g, mean_anc, raw_gen, "high", r)
        win_table = rbind(win_table, res)
        end = end +1000
        d = i
      }
      tab = mah[d:i-1,]
      mean_anc = mean(tab$ancestry)
      raw_gen = as.integer(g) - batch[b]
      res = c((end - 500),  g, mean_anc, raw_gen, "high", r)
      win_table = rbind(win_table, res)
      end = end+1000
      while (end <= 50000){
        #tab[1,] = c((end-500), g, 0, 0, 0, r)
        res = c((end - 500),  g, 0, 0, "high", r)
        win_table = rbind(win_table, res)
        end = end +1000
      }

      win_table = data.frame(win_table, row.names = NULL, stringsAsFactors = F)
      names(win_table) = c("pos", "gen", "mean_anc", "true_gen", "recomb", "replicate")
      write.table(win_table, paste0("win_anc_", r,".txt"), col.names = T, row.names = F)
    }
    names(win_table) = names(janela)
    janela = rbind(janela, win_table)
    janela = data.frame(janela, row.names = NULL, stringsAsFactors = F)
    names(janela) = c("pos", "gen", "mean_anc", "true_gen", "recomb", "replicate")
    write.table(janela, paste("win_anc_all_rep", ".txt", sep = ""), col.names = T, row.names = F)
  }
  tudob = rbind(tudob, janela)
  #names(tudob) = c("pos", "gen", "count_A", "count_B", "delta",   "replicate")
}


###########
# # # # # # # # # # # #
#  Low recombination  #
# # # # # # # # # # # #
#####low recomb#####

janela= c()
win_table = c()
tudob = c()

for (b in 1:length(batch)){
  ma = plow
  for (r in runs){
    maa = ma[ma$replicate == r,]
    win_table = c()
    for (g in generations[[b]]){
      #this is for high recomb
      mah = maa[maa$raw_gen == g,]
      d = 1
      end = 60000
      i = 1
      cat(paste0(r, " ", g, "\n"))
      while(end <= mah[nrow(mah),1]){
        while (mah$pos[i] < end){i = i+1}
        tab = mah[d:i-1,]
        mean_anc = mean(tab$ancestry)
        raw_gen = as.integer(g) - batch[b]
        res = c((end - 5000),  g, mean_anc, raw_gen, "low", r)
        win_table = rbind(win_table, res)
        end = end +10000
        d = i
      }
      tab = mah[d:i-1,]
      mean_anc = mean(tab$ancestry)
      raw_gen = as.integer(g) - batch[b]
      res = c((end - 5000),  g, mean_anc, raw_gen, "low", r)
      win_table = rbind(win_table, res)
      end = end+10000
      while (end <= 100000){
        #tab[1,] = c((end-500), g, 0, 0, 0, r)
        res = c((end - 5000),  g, 0, 0, "low", r)
        win_table = rbind(win_table, res)
        end = end +10000
      }
      
      win_table = data.frame(win_table, row.names = NULL, stringsAsFactors = F)
      names(win_table) = c("pos", "gen", "mean_anc", "true_gen", "recomb", "replicate")
      write.table(win_table, paste0("win_anc_low_", r,".txt"), col.names = T, row.names = F)
    }
    names(win_table) = names(janela)
    janela = rbind(janela, win_table)
    janela = data.frame(janela, row.names = NULL, stringsAsFactors = F)
    names(janela) = c("pos", "gen", "mean_anc", "true_gen", "recomb", "replicate")
    write.table(janela, paste("win_anc_all_rep_LOW_", ".txt", sep = ""), col.names = T, row.names = F)
  }
  tudob = rbind(tudob, janela)
  #names(tudob) = c("pos", "gen", "count_A", "count_B", "delta",   "replicate")
}

