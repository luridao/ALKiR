sink(paste("all_alt_",Slag,"_", year_yvirlit, "_", manFun(Arma),"_", paste(Drift,collapse="_"),".txt",sep=""))

cat("\n", "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"                , "\n"
, "> Dato                : ", date(), '\n' 
, "> Slag                : "   , Slag, '\n'
, "> Ár                  : "   , year_yvirlit, '\n' 
, "> Tiðarskeið          : "   , gsub("_", "-",toupper(manFun(Arma))), '\n'
, "> Skipabólk/-ar       : "   , paste(DriftV, collapse=" "), '\n' 
, "> Skipabólk/-ar, (longd lán) : "  , Drift_Lgd_Borrow                      , "\n"
, "> Skipabólk/-ar, (vekt lán)  : "  , Drift_Vekt_Borrow                     , "\n"
, "> Skipabólk/-ar, (aldur lán) : "  , Drift_Age_Borrow                      , "\n"
, "> ICES-øki            : "   , Oeki, '\n'
, "> -------- Frá øðrum ári ---------- "                                     , "\n"
, "> Skipabólk/-ar (lán) : "   , lanDrifts(), '\n'
, "> Lántar prøvar ALK   : "   , sort(unique(ArmaAlk[!(ArmaAlk %in% Arma)])), '\n'
, "> Lántar prøvar ALD   : "   , sort(unique(ArmaAld[!(ArmaAld %in% Arma)])), '\n'
, "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<", '\n')

cat("\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n");for(i in 1:6) cat(get(paste("abr",Slag,manFun(Arma),paste(Drift,collapse="_"),sep="_"))[i], fill=T, labels=lab.lwlogfit[i]);cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n\n");

cat("\n",">>>>>>>>>>>>>>","\t","ALK-lykil","\t","<<<<<<<<<<<<","\n")

lapply(tmp1,round,3)

cat("\n",">>>>>>>>>>>>>>","\t","ALD-lykil","\t","<<<<<<<<<<<<","\n")

lapply(tmp2,round,3)

resFun_alt(tmp2, tmp3, new_veida_obj =  new_veida_obj)

sink();sink()

