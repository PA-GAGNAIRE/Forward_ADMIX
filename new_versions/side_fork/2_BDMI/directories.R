setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/v_7/20000_persist")
setwd("~/dmi_long_divergence/dmi_avdqnf/definite_scripts/LOSS_20_v7")
setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/v_6/20000_persist")
setwd("~/dmi_long_divergence/dmi_avdqnf/parsed_data/v_5/20000_persist")

#verde        #rosa,    #azul,    vermelho,     amarelo,    roxo
#"#009200", "#ff00ae", "#4cbef0", "#f0382f",  "#c27e1d",  "#856fea"

#verde com      rosa/roxo/azul
#vermelho com   verde/roxo/azul
#azul com       vermelho/rosa/verde
#amarelo com    roxo/rosa/azul
#rosa com       verde/azul/amarelo
#roxo com       verde/vermelho/amarelo


dia = format(Sys.Date(), "%d_%m_%Y")
dia

png(paste0("FITNESS_all_vers_", dia,  ".png"), width = 42, height = 21, units = "cm", res = 300)
dev.off()