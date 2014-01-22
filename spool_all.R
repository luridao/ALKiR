sink(paste("alt_",Slag,"_", year_yvirlit, "_", manFun(Arma),"_", paste(Drift,collapse="_"),".txt",sep=""))

cat( "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"        , "\n"
        , "> Dato                                      : "  , date()              , "\n"
        , "> Slag                                      : "  , Slag                , "\n"
        , "> Ár                                        : "  , year_yvirlit        , "\n"
        , "> Skipabólk/-ar                             : "  , Drift               , "\n"
        , "> Byrjanar og enda mánað fyri ALK/ALD       : "  , gsub("_", "-",toupper(manFun(Arma)))               , "\n"
        , "> Skipabólk/-ar, sum longdir eru lántar frá : "  , Drift_Lgd_Borrow    , "\n"
        , "> Skipabólk/-ar, sum vektir eru lántar frá  : "  , Drift_Vekt_Borrow   , "\n"
        , "> Skipabólk/-ar, sum aldrar eru lántar frá  : "  , Drift_Age_Borrow    , "\n"
        , "> ICES-øki                                  : "  , Oeki                , "\n"
        , "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"        )

cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n");for(i in 1:6) cat(get(paste("abr",Slag,manFun(Arma),paste(Drift,collapse="_"),sep="_"))[i], fill=T, labels=lab.lwlogfit[i]);cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<");

cat(">>>>>>>>>>>>>>","\t","ALK-lykil","\t","<<<<<<<<<<<<")

lapply(tmp1,round,3)

cat(">>>>>>>>>>>>>>","\t","ALD-lykil","\t","<<<<<<<<<<<<")

lapply(tmp2,round,3)

resFun(tmp2, tmp3, new_veida_obj =  new_veida_obj)

sink();sink()

