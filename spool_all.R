sink(paste("alt_",Slag,"_", year_yvirlit, "_", manFun(Arma),"_", paste(Drift,collapse="_"),".txt",sep=""))

cat( "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"        , "\n"
        , "> Dato                                      : "  , date()              , "\n"
        , "> Slag                                      : "  , Slag                , "\n"
        , "> �r                                        : "  , year_yvirlit        , "\n"
        , "> Skipab�lk/-ar                             : "  , Drift               , "\n"
        , "> Byrjanar og enda m�na� fyri ALK/ALD       : "  , gsub("_", "-",toupper(manFun(Arma)))               , "\n"
        , "> Skipab�lk/-ar, sum longdir eru l�ntar fr� : "  , Drift_Lgd_Borrow    , "\n"
        , "> Skipab�lk/-ar, sum vektir eru l�ntar fr�  : "  , Drift_Vekt_Borrow   , "\n"
        , "> Skipab�lk/-ar, sum aldrar eru l�ntar fr�  : "  , Drift_Age_Borrow    , "\n"
        , "> ICES-�ki                                  : "  , Oeki                , "\n"
        , "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"        )

cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n");for(i in 1:6) cat(get(paste("abr",Slag,manFun(Arma),paste(Drift,collapse="_"),sep="_"))[i], fill=T, labels=lab.lwlogfit[i]);cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<");

cat(">>>>>>>>>>>>>>","\t","ALK-lykil","\t","<<<<<<<<<<<<")

lapply(tmp1,round,3)

cat(">>>>>>>>>>>>>>","\t","ALD-lykil","\t","<<<<<<<<<<<<")

lapply(tmp2,round,3)

resFun(tmp2, tmp3, new_veida_obj =  new_veida_obj)

sink();sink()

