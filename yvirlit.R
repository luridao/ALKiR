#------------------------------------------------------
######		YVIRLITSTØLINI		#######
#------------------------------------------------------

#------------------------------------------------------
######		Length Summary		#######
#------------------------------------------------------
# ---- Monthly summary ----
#yvirlit_lgd     <- with(lgdAll, lgdAll[slag %in% Slag & oeki %in% Oeki,]) # some samples have no 'Oeki' registered
yvirlit_lgd     <- with(lgdAll, lgdAll[slag %in% Slag,])
yvirlit_lgd$ls  <- as.character(bdrift[match(yvirlit_lgd$drift, bdrift$drift), 'lysing_stutt'])
yvirlit_lgd     <- with(yvirlit_lgd, yvirlit_lgd[order(slag, arma, drift),])

# summary by 'drift'
yvirlit_lgd_mand <- with(yvirlit_lgd, tapply(sum_matad, list(arma=arma, drift=drift),sum))
yvirlit_lgd_mand[is.na(yvirlit_lgd_mand)] <- 0

yvirlit_lgd_mand <- raetRow(yvirlit_lgd_mand)

# summary by 'lysing_stutt'
yvirlit_lgd_man <- with(yvirlit_lgd, tapply(sum_matad, list(arma=arma, ls=ls),sum))
yvirlit_lgd_man[is.na(yvirlit_lgd_man)] <- 0
ncol_lgd        <- ncol(yvirlit_lgd_man)
yvirlit_lgd_man

yvirlit_lgd_man <- raetRow(yvirlit_lgd_man)

# ---- Quarter summary ----
yvirlit_lgd_qrt           <- data.frame(yvirlit_lgd_man)
#colnames(yvirlit_lgd_qrt) <- gsub('X', '', colnames(yvirlit_lgd_qrt))
yvirlit_lgd_qrt$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_lgd_qrt           <- aggregate(yvirlit_lgd_qrt[,1:ncol_lgd], list(Qrt = yvirlit_lgd_qrt$extra), sum)
colnames(yvirlit_lgd_qrt) <- c('Qrt',colnames(yvirlit_lgd_man))
yvirlit_lgd_qrt

#------------------------------------------------------
######		Weight Summary		#######
#------------------------------------------------------
# ---- Monthly summary ----
#yvirlit_wei     <- with(lgdvektAll, lgdvektAll[slag %in% Slag & oeki %in% Oeki,]) # some samples have no 'Oeki' registered
yvirlit_wei     <- with(lgdvektAll, lgdvektAll[slag %in% Slag ,])
yvirlit_wei$ls  <- as.character(bdrift[match(yvirlit_wei$drift, bdrift$drift), 'lysing_stutt'])
yvirlit_wei     <- with(yvirlit_wei, yvirlit_wei[order(slag, arma, drift),])

# summary by 'drift' 
yvirlit_wei_mand <- with(yvirlit_wei, tapply(vekt, list(arma=arma, drift=drift),function(x) sum(!is.na(x))))
yvirlit_wei_mand[is.na(yvirlit_wei_mand)] <- 0

yvirlit_wei_mand <- raetRow(yvirlit_wei_mand)

# summary by 'lysing_stutt'
yvirlit_wei_man <- with(yvirlit_wei, tapply(vekt, list(arma=arma, ls=ls),function(x) sum(!is.na(x))))
yvirlit_wei_man[is.na(yvirlit_wei_man)] <- 0
ncol_wei        <- ncol(yvirlit_wei_man)
yvirlit_wei_man

yvirlit_wei_man <- raetRow(yvirlit_wei_man)

# ---- Quarter summary ----
yvirlit_wei_qrt           <- data.frame(yvirlit_wei_man)
#colnames(yvirlit_wei_qrt) <- gsub('X', '', colnames(yvirlit_wei_qrt))
yvirlit_wei_qrt$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_wei_qrt           <- aggregate(yvirlit_wei_qrt[,1:ncol_wei], list(Qrt = yvirlit_wei_qrt$extra), sum)
colnames(yvirlit_wei_qrt) <- c('Qrt', colnames(yvirlit_wei_man))
yvirlit_wei_qrt

#------------------------------------------------------
######		Age Summary		#######
#------------------------------------------------------
# ---- Monthly summary ----
#yvirlit_age     <- with(alkAll, alkAll[slag %in% Slag & oeki %in% Oeki,])
yvirlit_age     <- with(alkAll, alkAll[slag %in% Slag,])
yvirlit_age$ls  <- as.character(bdrift[match(yvirlit_age$drift, bdrift$drift), 'lysing_stutt'])
yvirlit_age     <- with(yvirlit_age, yvirlit_age[order(slag, arma, drift, aldur),])

# summary by 'drift' 
yvirlit_age_mand <- with(yvirlit_age, tapply(tal, list(arma=arma, drift=drift),sum))
yvirlit_age_mand[is.na(yvirlit_age_mand)] <- 0

yvirlit_age_mand <- raetRow(yvirlit_age_mand)

# summary by 'lysing_stutt'
yvirlit_age_man <- with(yvirlit_age, tapply(tal, list(arma=arma, ls=ls),sum))
yvirlit_age_man[is.na(yvirlit_age_man)] <- 0
ncol_age        <- ncol(yvirlit_age_man)
yvirlit_age_man

yvirlit_age_man <- raetRow(yvirlit_age_man)

# ---- Quarter summary ----
yvirlit_age_qrt           <- data.frame(yvirlit_age_man)
#colnames(yvirlit_age_qrt) <- gsub('X', '', colnames(yvirlit_age_qrt))
yvirlit_age_qrt$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_age_qrt           <- aggregate(yvirlit_age_qrt[,1:ncol_age], list(Qrt = yvirlit_age_qrt$extra), sum)
colnames(yvirlit_age_qrt) <- c('Qrt', colnames(yvirlit_age_man))
yvirlit_age_qrt

#------------------------------------------------------
######		Landings Summary		#######
#------------------------------------------------------
# ---- Monthly summary ----
yvirlit_lan     <- with(veidaDataAll, veidaDataAll[slag %in% Slag & oeki %in% Oeki,])
yvirlit_lan$ls  <- as.character(bdrift[match(yvirlit_lan$drift, bdrift$drift), 'lysing_stutt'])
yvirlit_lan     <- with(yvirlit_lan, yvirlit_lan[order(slag, arma, drift),])

# summary by 'drift' 
yvirlit_lan_mand <- with(yvirlit_lan, tapply(vekt,list(arma=arma, drift=drift),sum))
yvirlit_lan_mand[is.na(yvirlit_lan_mand)] <- 0

yvirlit_lan_mand <- raetRow(yvirlit_lan_mand)

# summary by 'lysing_stutt'
yvirlit_lan_man <- with(yvirlit_lan, tapply(vekt,list(arma=arma, ls=ls),sum))
yvirlit_lan_man[is.na(yvirlit_lan_man)] <- 0
ncol_lan        <- ncol(yvirlit_lan_man)
yvirlit_lan_man

yvirlit_lan_man <- raetRow(yvirlit_lan_man)

# ---- Quarter summary ----
yvirlit_lan_qrt           <- data.frame(yvirlit_lan_man)
#colnames(yvirlit_lan_qrt) <- gsub('X', '', colnames(yvirlit_lan_qrt))
yvirlit_lan_qrt$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_lan_qrt           <- aggregate(yvirlit_lan_qrt[,1:ncol_lan], list(Qrt = yvirlit_lan_qrt$extra), sum)
colnames(yvirlit_lan_qrt) <- c('Qrt', colnames(yvirlit_lan_man))
yvirlit_lan_qrt


######  A handy summary of fleets and their numerical identification
drifts.stutt.all <- with(bdrift, tapply(drift, list(lysing_stutt), function(x) return(x)) )
drifts.stutt.all <- sapply(drifts.stutt.all, function(x) unique(x))
drifts.stutt.sma <- unique(c(colnames(yvirlit_lgd_man),colnames(yvirlit_age_man),colnames(yvirlit_wei_man),colnames(yvirlit_lan_man)))
###############	###############	###############	###############

# ------------------------------------------------------------------------
#######	 SINK ALL THE SUMMARY RESULTS TO A FILE		##############
# ------------------------------------------------------------------------

sink(paste("Yvirlit", "_",Slag,"_",year_yvirlit, ".txt", sep=""))

cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
cat("\n", "Dato            : ", date(), '\n', "Slag            : ", Slag, '\n',"Tíðarskeið      : " , year_yvirlit , '\n')
cat(" <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n\n")

drifts.stutt_ind  <-  names(drifts.stutt.all) %in% drifts.stutt.sma
drifts.stutt   <- drifts.stutt.all[drifts.stutt_ind]
print(drifts.stutt)

cat('------------- Lengths-summary ------------- \n')
print(yvirlit_lgd_mand)
cat('\n')
print(yvirlit_lgd_man)
cat('\n')
print(yvirlit_lgd_qrt)
cat('\n ------------- Weight-summary  ------------- \n')
print(yvirlit_wei_mand)
cat('\n')
print(yvirlit_wei_man)
cat('\n')
print(yvirlit_wei_qrt)
cat('\n ------------- Age-summary     ------------- \n')
print(yvirlit_age_mand)
cat('\n')
print(yvirlit_age_man)
cat('\n')
print(yvirlit_age_qrt)
cat('\n ------------- Landings-summary ------------- \n')
print(yvirlit_lan_mand)
cat('\n')
print(yvirlit_lan_man)
cat('\n')
print(yvirlit_lan_qrt)

sink()




















