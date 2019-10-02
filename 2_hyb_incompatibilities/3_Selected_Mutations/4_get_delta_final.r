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

#replicates to be used (either  loss of persistence of minor ancestry)
tata = read.table(paste0("replicates_", typ,".txt"), h=T, stringsAsFactors = F)
runs = unique(tata[,1])
runs

######delta##############################################
#             Delta calculations/ Windowns of 1cM built #
#########################################################

#####high recomb####
janela = c()
win_table = c()
tudob = c()


for (b in batch) {
  m = read.table(paste0("class_muts_fixed_", typ, "correct.txt"), header = T)
  for (r in runs) {
    ma = m[m$replicate == r, ]
    win_table = c()
    names(ma) = c("pos", "group", "gen", "freq", "replicate")
    for (g in gen_wind) {
      mah = ma[ma$pos <= 50000, ]
      mah = mah[mah$gen == g, ]
      mah = mah[order(mah$pos), ]
      d = 1
      end = 1000
      i = 1
      while (end <= mah[nrow(mah), 1]) {
        while (mah$pos[i] < end) {
          i = i + 1
        }
        tab = mah[d:i - 1, ]
        numA = 0
        numB = 0
        if (nrow(tab) == 0) {
          numA = 0
          numB = 0
        } else if (nrow(tab) != 0) {
          for (k in 1:nrow(tab)) {
            if (tab$group[k] == "fixed_A") {
              numA = numA + 1
            }   else if (tab$group[k] == "fixed_B") {
              numB = numB + 1
            }
          }
        }

        res = c((end - 500),  g, numA, numB, numA - numB, r)
        win_table = rbind(win_table, res)
        end = end + 1000
        d = i
      }
      tab = mah[d:i - 1, ]
      numA = 0
      numB = 0
      if (nrow(tab) == 0) {
        numA = 0
        numB = 0
        tab = c((end - 500), g, 0, 0, 0, r)
      } else if (nrow(tab) != 0) {
        for (k in 1:nrow(tab)) {
          if (tab$group[k] == "fixed_A") {
            numA = numA + 1
          }   else if (tab$group[k] == "fixed_B") {
            numB = numB + 1
          }
        }
      }

      res = c((end - 500),  g, numA, numB, numA - numB, r)
      win_table = rbind(win_table, res)
      end = end + 1000
      d = i

      while (end <= 50000) {
        #tab[1,] = c((end-500), g, 0, 0, 0, r)
        res = c((end - 500),  g, 0, 0, 0, r)
        win_table = rbind(win_table, res)
        end = end + 1000
      }

      win_table = data.frame(win_table,
                             row.names = NULL,
                             stringsAsFactors = F)
      write.table(
        win_table,
        paste0("windows_high_", tp, "_", r, ".txt"),
        col.names = T,
        row.names = F
      )
      cat(r, " ", g, " ", b, "\n")
    }
    names(win_table) = names(janela)
    janela = rbind(janela, win_table)
    janela = data.frame(janela,
                        row.names = NULL,
                        stringsAsFactors = F)
    names(janela) = c("pos", "gen", "count_A", "count_B", "delta",   "replicate")
    write.table(
      janela,
      paste("windows_high_all_rep_", typ, ".txt", sep = ""),
      col.names = T,
      row.names = F
    )
  }
  tudob = rbind(tudob, janela)
  names(tudob) = c("pos", "gen", "count_A", "count_B", "delta",   "replicate")
}

######low recomb####
# it changes the size of the windows (in bp from 500 to 5000)
# and the start (second half of the chromosome)

janela = c()
win_table = c()
tudob = c()

for (b in batch) {
  m = read.table(paste0("class_muts_fixed_", typ, "correct.txt"), header = T)
  for (r in runs) {
    ma = m[m$replicate == r, ]
    win_table = c()
    names(ma) = c("pos", "group", "gen", "freq", "replicate")
    for (g in gen_wind) {
      mah = ma[ma$pos > 50000, ]
      mah = mah[mah$gen == g, ]
      mah = mah[order(mah$pos), ]
      d = 1
      end = 60000
      i = 1
      while (end <= mah[nrow(mah), 1]) {
        while (mah$pos[i] < end) {
          i = i + 1
        }
        tab = mah[d:i - 1, ]
        numA = 0
        numB = 0
        if (nrow(tab) == 0) {
          numA = 0
          numB = 0
          tab = c((end - 5000), g, 0, 0, 0, r)
        } else if (nrow(tab) != 0) {
          for (k in 1:nrow(tab)) {
            if (tab$group[k] == "fixed_A") {
              numA = numA + 1
            }   else if (tab$group[k] == "fixed_B") {
              numB = numB + 1
            }
          }
        }
        
        res = c((end - 5000),  g, numA, numB, numA - numB, r)
        win_table = rbind(win_table, res)
        end = end + 10000
        d = i
      }
      tab = mah[d:i - 1, ]
      numA = 0
      numB = 0
      if (nrow(tab) == 0) {
        numA = 0
        numB = 0
        tab = c((end - 5000), g, 0, 0, 0, r)
      } else if (nrow(tab) != 0) {
        for (k in 1:nrow(tab)) {
          if (tab$group[k] == "fixed_A") {
            numA = numA + 1
          }   else if (tab$group[k] == "fixed_B") {
            numB = numB + 1
          }
        }
      }
      
      res = c((end - 5000),  g, numA, numB, numA - numB, r)
      win_table = rbind(win_table, res)
      end = end + 10000
      
      while (end <= 100000) {
        #tab[1,] = c((end-500), g, 0, 0, 0, r)
        res = c((end - 5000),  g, 0, 0, 0, r)
        win_table = rbind(win_table, res)
        end = end + 10000
      }
      
      win_table = data.frame(win_table,
                             row.names = NULL,
                             stringsAsFactors = F)
      write.table(
        win_table,
        paste0("windows_low_", tp, "_", r, ".txt"),
        col.names = T,
        row.names = F
      )
      cat(r, " ", g, " ", b, "\n")
    }
    names(win_table) = names(janela)
    janela = rbind(janela, win_table)
    janela = data.frame(janela,
                        row.names = NULL,
                        stringsAsFactors = F)
    names(janela) = c("pos", "gen", "count_A", "count_B", "delta",   "replicate")
    write.table(
      janela,
      paste("windows_low_all_rep_", typ, ".txt", sep = ""),
      col.names = T,
      row.names = F
    )
  }
  tudob = rbind(tudob, janela)
  names(tudob) = c("pos", "gen", "count_A", "count_B", "delta",   "replicate")
}

