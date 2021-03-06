sink(paste("all_alt_",Slag,"_", year_yvirlit, "_", manFun(Arma),"_", paste(Drift,collapse="_"),".txt",sep=""))

cat("\n", "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"                , "\n"
, "> Dato                : ", date(), '\n' 
, "> Slag                : "   , Slag, '\n'
, "> �r                  : "   , year_yvirlit, '\n' 
, "> Ti�arskei�          : "   , gsub("_", "-",toupper(manFun(Arma))), '\n'
, "> Skipab�lk/-ar       : "   , paste(DriftV, collapse=" "), '\n' 
, "> Skipab�lk/-ar, (longd l�n) : "  , Drift_Lgd_Borrow                      , "\n"
, "> Skipab�lk/-ar, (vekt l�n)  : "  , Drift_Vekt_Borrow                     , "\n"
, "> Skipab�lk/-ar, (aldur l�n) : "  , Drift_Age_Borrow                      , "\n"
, "> ICES-�ki            : "   , Oeki, '\n'
, "> -------- Fr� ��rum �ri ---------- "                                     , "\n"
, "> Skipab�lk/-ar (l�n) : "   , lanDrifts(), '\n'
, "> L�ntar pr�var ALK   : "   , sort(unique(ArmaAlk[!(ArmaAlk %in% Arma)])), '\n'
, "> L�ntar pr�var ALD   : "   , sort(unique(ArmaAld[!(ArmaAld %in% Arma)])), '\n'
, "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<", '\n')

cat("\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n");for(i in 1:6) cat(get(paste("abr",Slag,manFun(Arma),paste(Drift,collapse="_"),sep="_"))[i], fill=T, labels=lab.lwlogfit[i]);cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n\n");

cat("\n",">>>>>>>>>>>>>>","\t","ALK-lykil","\t","<<<<<<<<<<<<","\n")

lapply(tmp1,round,3)

cat("\n",">>>>>>>>>>>>>>","\t","ALD-lykil","\t","<<<<<<<<<<<<","\n")

lapply(tmp2,round,3)

resFun_alt(tmp2, tmp3, new_veida_obj =  new_veida_obj)

sink();sink()

