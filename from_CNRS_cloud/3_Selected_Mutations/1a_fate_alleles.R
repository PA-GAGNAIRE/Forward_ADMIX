###################
# CHANGE HERE TOO #
###################

#this is depending on which outcome we are analysing

#typ = "LOSS"
#tp = "loss"
#path = "loss"

typ = "POLY"
tp = "poly"
path = "persist"

###############
# CHANGE HERE #
###############

#this refers to the divergence time

batch = c(20000)
bb = 20000

computador="/media/pagagnaire/18E23B38E23B1A08/temp_FLAVIA/dmi_avdqnf/v_6/"

setwd(paste0(computador, bb, "_", path))

getwd()

dia = format(Sys.Date(), "%d_%m_%Y")
dia

###variables########################
#              important variables #
####################################

#generations and replicate names

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

gen_wind = seq((bb + 1), (bb + 300), 1)
gtest = list(gen_wind)

runs = c(
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


parrep = c()
prorep = c()
prorep2 = c()
for (r in runs) {
  ma = read.table(paste0("../", bb, "_dmi_sum_mut/", r, "_mutation_freqs.txt"),
                  header = F)
  names(ma) = c("pos", "gen", "freq", "gen_or", "group", "sum_gen")
  ma =  ma[ma$gen == (bb+1), ]
  cat("loaded mut file for ", r, "\n")
  parmut = c()
  promut = c()
  promut2 = c()
  # Change ALLELE FREQs here, according to admixture proportions
  # but have to remember that the proportions are from a 1500 ind
  # in generation 1
  # ADMX = 50/50 --> ~ 0.5 a freq de cada allele fixo
  for (i in 1:nrow(ma)) {
    if ((ma$group[i] == "B") &
        ((ma$freq[i] >= 0.45) &
         (ma$freq[i] <= 1))) {
      ma$state[i] = "fixed_B"
    } else if ((ma$group[i] == "B") &
               (ma$freq[i] < 0.45)) {
      ma$state[i] = "poly_B"
    } else if ((ma$group[i] == "A") &
               ((ma$freq[i] >= 0.45) &
                (ma$freq[i] <= 1))) {
      ma$state[i] = "fixed_A"
    } else if ((ma$group[i] == "A") &
               (ma$freq[i] < 0.45)) {
      ma$state[i] = "poly_A"
    }
  }

  mutB = ma[ma$state != "mutA", ] #original mutations present in parent
  mutA = ma[ma$state != "mutA", ]
  muts = read.table(paste0("../", bb, "_dmi_sum_mut/", r, "_mutation_freqs.txt"),
                    header = F)#file with all mut trajectories
  names(muts) = c("pos", "gen", "freq", "gen_or", "group", "sum_gen")
  for (g in 1:length(gtest[[1]])) {
    cmuts =  muts[muts$gen == gtest[[1]][g], ]
    cB = cmuts[cmuts$group == "B", ]#current muts
    cA = cmuts[cmuts$group == "A", ]#in the gen 'g'
    remainB = merge(mutB, cB, by = c("pos", "group", "gen_or"))
    remainA = merge(mutA, cA, by = c("pos", "group", "gen_or"))
    remainA = remainA[, c(1, 7, 8, 9)]
    remainB = remainB[, c(1, 7, 8, 9)]
    #COLE AQUI
    if (nrow(remainB) != 0) {
      remainB$replicate = r
    }
    
    if (nrow(remainA) != 0) {
      remainA$replicate = r
    }

    if (nrow(remainB) != 0) {
      fix_B = remainB[remainB$state == "fixed_B", ]
      poly_B = remainB[remainB$state == "poly_B", ]
      anc_b = remainB
      for (i in 1:nrow(anc_b)) {
        anc_b$freq.y[i] = (1 - anc_b$freq.y[i])
        if (anc_b$state[i] == "fixed_B") {
          anc_b$state[i] = "fix_b"
        } else if (anc_b$state[i] == "poly_B") {
          anc_b$state[i] = "poly_b"
        }

      }
    }

    if (nrow(remainA) != 0) {
      fix_A = remainA[remainA$state == "fixed_A", ]
      poly_A = remainA[remainA$state == "poly_A", ]
      anc_a = remainA
      for (i in 1:nrow(anc_a)) {
        anc_a$freq.y[i] = (1 - anc_a$freq.y[i])
        if (anc_a$state[i] == "fixed_A") {
          anc_a$state[i] = "fix_a"
        } else if (anc_a$state[i] == "poly_A") {
          anc_a$state[i] = "poly_a"
        }

      }
    }
    #complete table with ALL alleles
    now = rbind(anc_b, remainB, anc_a, remainA)
    promut = rbind(promut, now)
    
    #only pop1 alleles
    now2 = rbind(anc_b, remainA)
    promut2 = rbind(promut2, now2)

    #only pop2 alleles
    agora = rbind(remainB, anc_a)
    parmut = rbind(parmut, agora)

    #zerando a cada geracao, pra nao ficar add a ultima linha de aleleos a que disapareceram
    remainB = c()
    anc_a= c()
    remainA = c()
    anc_b = c()
    #this table anc_a is the table with the frequencies with ancestral allele little "a";
    #freq.y is the frequency of ancestral "a" allele at the present generation
    write.table(
      promut,
      paste0(bb, "_processed_muts_", r, ".txt"),
      col.names = T,
      row.names = F
    )
    
    write.table(
      promut2,
      paste0(bb, "_inverse_parsed_", r, ".txt"),
      col.names = T,
      row.names = F
    )

    write.table(
      parmut,
      paste0(bb, "_atual_parsed_muts_", r, ".txt"),
      col.names = T,
      row.names = F
    )

    cat("finished for generation ", gtest[[1]][g], "replicate ", r, "\n")

  }

  parrep = rbind(parrep, parmut)
  prorep = rbind(prorep, promut)
  prorep2 = rbind(prorep2, promut2)
  
  write.table(
    parrep,
    paste0("atual_parsed_muts_", batch, "_all_reps.txt"),
    col.names = T,
    row.names = F
  )
  
  write.table(
    prorep,
    paste0("processed_muts_", batch, "_all_reps.txt"),
    col.names = T,
    row.names = F
  )
  
  write.table(
    prorep2,
    paste0("inverse_parsed_", batch, "_all_reps.txt"),
    col.names = T,
    row.names = F
  )

}


t2 = read.table(paste0("atual_parsed_muts_", bb, "_all_reps.txt"), h = T)

#adding recombination
for (i in 1:nrow(t2)) {
  if (t2$pos[i] <= 50000) {
    t2$recomb[i] = "high"
  } else if (t2$pos[i] > 50000) {
    t2$recomb[i] = "low"
  }
}

write.table(
  t2,
  "all_freq_anc_B_mutations_RECOMB.txt",
  col.names = T,
  row.names = F
)

t2$time = as.factor(t2$gen.y)


# now identify runs in which minor ancestry was lost or persisted

out = t2[t2$gen.y == (bb + 300), ]

sout = ddply(out, .(gen.y, state, replicate), summarise, mean_freq = mean(freq.y))
mod = c("fixed_B", "poly_B")
pout = sout[sout$state %in% mod, ]
pout$replicate = as.character(pout$replicate)
rpoly = c(unique(pout$replicate)) #these minor parent remained
rloss = sout[!sout$replicate %in% rpoly, ] #loss of minor parent
rloss$replicate = as.character(rloss$replicate)
reploss = c(unique(rloss$replicate))

t2 = unique(t2)

#put outcome in the data table
t3 = c()
for (i in 1:nrow(t2)) {
  if (t2$replicate[i] %in% rpoly) {
    t3$result[i] = "persist"
  } else {
    t3$result[i] = "lost"
  }
}

t3 = data.frame(t3)
t4 = cbind(t2, t3)
t4$time = as.factor(t4$gen.y)

write.table(t4, paste0(bb, "alleles_by_outcome.txt"), col.names = T, row.names = F)

t5 = t4[t4$gen.y <= (bb+30), ]

write.table(t5, paste0(bb, "alleles_by_outcome_30_gen.txt"), col.names = T, row.names = F)