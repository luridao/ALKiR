#------------------------------------------------------
######		YVIRLITST?LINI		#######
#------------------------------------------------------

# Fleet categories for saithe
Snella           <- data.frame(fleet="Snella"           , drift=c(2,9,24))
Partroplus1000   <- data.frame(fleet="Partroplus1000"   , drift=c(19,20,21))
Partrominus1000  <- data.frame(fleet="Partrominus1000"  , drift=c(16,17,18))
Lemmatroplus1000 <- data.frame(fleet="Lemmatroplus1000" , drift=c(13,14,15))
Annad1           <- data.frame(fleet="Annad1"           , drift=c(1,4,5,6,7,8))
Annad2           <- data.frame(fleet="Annad2"           , drift=c(10,11,12,22,23,28))

fleetCategory_UP <- rbind(Snella, Partroplus1000, Partrominus1000, Lemmatroplus1000, Annad1, Annad2)

#- if there exists a fleetCategory object then do the yvirlit according to it
fleetCategory_question = exists(paste("fleetCategory", Slag, sep="_"))

#------------------------------------------------------
######		Length Summary		#######
#------------------------------------------------------
# ---- Monthly summary ----
#yvirlit_lgd      <- with(lgdAll, lgdAll[slag %in% Slag & oeki %in% Oeki,]) # some samples have no 'Oeki' registered
yvirlit_lgd       <- with(lgdAll, lgdAll[slag %in% Slag,])
yvirlit_lgd$ls    <- as.character(bdrift[match(yvirlit_lgd$drift, bdrift$drift), 'lysing_stutt'])
yvirlit_lgd       <- with(yvirlit_lgd, yvirlit_lgd[order(slag, arma, drift),])
if(fleetCategory_question)
yvirlit_lgd$ls1 <- fleetCategory_UP[match(yvirlit_lgd$drift, fleetCategory_UP$drift), 'fleet'] else {}

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

# summary by 'ls1' fleetCategory_UP
yvirlit_lgd_man1 <- with(yvirlit_lgd, tapply(sum_matad, list(arma=arma, ls=ls1),sum))
yvirlit_lgd_man1[is.na(yvirlit_lgd_man1)] <- 0
ncol_lgd1        <- ncol(yvirlit_lgd_man1)
yvirlit_lgd_man1

yvirlit_lgd_man1 <- raetRow(yvirlit_lgd_man1)

# ---- Quarter summary ---- by ls
yvirlit_lgd_qrt           <- data.frame(yvirlit_lgd_man)
#colnames(yvirlit_lgd_qrt) <- gsub('X', '', colnames(yvirlit_lgd_qrt))
yvirlit_lgd_qrt$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_lgd_qrt           <- aggregate(yvirlit_lgd_qrt[,1:ncol_lgd], list(Qrt = yvirlit_lgd_qrt$extra), sum)
colnames(yvirlit_lgd_qrt) <- c('Qrt',colnames(yvirlit_lgd_man))
yvirlit_lgd_qrt

# ---- Quarter summary ---- by ls1
yvirlit_lgd_qrt1           <- data.frame(yvirlit_lgd_man1)
#colnames(yvirlit_lgd_qrt) <- gsub('X', '', colnames(yvirlit_lgd_qrt))
yvirlit_lgd_qrt1$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_lgd_qrt1           <- aggregate(yvirlit_lgd_qrt1[,1:ncol_lgd1], list(Qrt = yvirlit_lgd_qrt1$extra), sum)
colnames(yvirlit_lgd_qrt1) <- c('Qrt',colnames(yvirlit_lgd_man1))
yvirlit_lgd_qrt1

#------------------------------------------------------
######		Weight Summary		#######
#------------------------------------------------------
# ---- Monthly summary ----
#yvirlit_wei     <- with(lgdvektAll, lgdvektAll[slag %in% Slag & oeki %in% Oeki,]) # some samples have no 'Oeki' registered
yvirlit_wei     <- with(lgdvektAll, lgdvektAll[slag %in% Slag ,])
yvirlit_wei$ls  <- as.character(bdrift[match(yvirlit_wei$drift, bdrift$drift), 'lysing_stutt'])
yvirlit_wei     <- with(yvirlit_wei, yvirlit_wei[order(slag, arma, drift),])
if(fleetCategory_question)
yvirlit_wei$ls1 <- fleetCategory_UP[match(yvirlit_wei$drift, fleetCategory_UP$drift), 'fleet'] else {}

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

# summary by 'ls1' fleetCategory_UP
yvirlit_wei_man1 <- with(yvirlit_wei, tapply(vekt, list(arma=arma, ls=ls1),function(x) sum(!is.na(x))))
yvirlit_wei_man1[is.na(yvirlit_wei_man1)] <- 0
ncol_wei1        <- ncol(yvirlit_wei_man1)
yvirlit_wei_man1

yvirlit_wei_man1 <- raetRow(yvirlit_wei_man1)

# ---- Quarter summary ---- by ls
yvirlit_wei_qrt           <- data.frame(yvirlit_wei_man)
#colnames(yvirlit_wei_qrt) <- gsub('X', '', colnames(yvirlit_wei_qrt))
yvirlit_wei_qrt$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_wei_qrt           <- aggregate(yvirlit_wei_qrt[,1:ncol_wei], list(Qrt = yvirlit_wei_qrt$extra), sum)
colnames(yvirlit_wei_qrt) <- c('Qrt', colnames(yvirlit_wei_man))
yvirlit_wei_qrt

# ---- Quarter summary ---- by ls1
yvirlit_wei_qrt1           <- data.frame(yvirlit_wei_man1)
#colnames(yvirlit_lgd_qrt) <- gsub('X', '', colnames(yvirlit_lgd_qrt))
yvirlit_wei_qrt1$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_wei_qrt1           <- aggregate(yvirlit_wei_qrt1[,1:ncol_wei1], list(Qrt = yvirlit_wei_qrt1$extra), sum)
colnames(yvirlit_wei_qrt1) <- c('Qrt',colnames(yvirlit_wei_man1))
yvirlit_wei_qrt1

#------------------------------------------------------
######		Age Summary		#######
#------------------------------------------------------
# ---- Monthly summary ----
#yvirlit_age     <- with(alkAll, alkAll[slag %in% Slag & oeki %in% Oeki,])
yvirlit_age     <- with(alkAll, alkAll[slag %in% Slag,])
yvirlit_age$ls  <- as.character(bdrift[match(yvirlit_age$drift, bdrift$drift), 'lysing_stutt'])
yvirlit_age     <- with(yvirlit_age, yvirlit_age[order(slag, arma, drift, aldur),])
if(fleetCategory_question)
yvirlit_age$ls1 <- fleetCategory_UP[match(yvirlit_age$drift, fleetCategory_UP$drift), 'fleet'] else {}

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

# summary by 'ls1' fleetCategory_UP
yvirlit_age_man1 <- with(yvirlit_age, tapply(tal, list(arma=arma, ls=ls1),sum))
yvirlit_age_man1[is.na(yvirlit_age_man1)] <- 0
ncol_age1        <- ncol(yvirlit_age_man1)
yvirlit_age_man1

yvirlit_age_man1 <- raetRow(yvirlit_age_man1)

# ---- Quarter summary ---- by ls
yvirlit_age_qrt           <- data.frame(yvirlit_age_man)
#colnames(yvirlit_age_qrt) <- gsub('X', '', colnames(yvirlit_age_qrt))
yvirlit_age_qrt$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_age_qrt           <- aggregate(yvirlit_age_qrt[,1:ncol_age], list(Qrt = yvirlit_age_qrt$extra), sum)
colnames(yvirlit_age_qrt) <- c('Qrt', colnames(yvirlit_age_man))
yvirlit_age_qrt

# ---- Quarter summary ---- by ls1
yvirlit_age_qrt1           <- data.frame(yvirlit_age_man1)
#colnames(yvirlit_lgd_qrt) <- gsub('X', '', colnames(yvirlit_lgd_qrt))
yvirlit_age_qrt1$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_age_qrt1           <- aggregate(yvirlit_age_qrt1[,1:ncol_age1], list(Qrt = yvirlit_age_qrt1$extra), sum)
colnames(yvirlit_age_qrt1) <- c('Qrt',colnames(yvirlit_age_man1))
yvirlit_age_qrt1


if(dim(veidaDataAll)[1]!=0)
{
#------------------------------------------------------
######		Landings Summary		#######
#------------------------------------------------------
# ---- Monthly summary ----
yvirlit_lan     <- with(veidaDataAll, veidaDataAll[slag %in% Slag & oeki %in% Oeki,])
yvirlit_lan$ls  <- as.character(bdrift[match(yvirlit_lan$drift, bdrift$drift), 'lysing_stutt'])
yvirlit_lan     <- with(yvirlit_lan, yvirlit_lan[order(slag, arma, drift),])
if(fleetCategory_question)
yvirlit_lan$ls1 <- fleetCategory_UP[match(yvirlit_lan$drift, fleetCategory_UP$drift), 'fleet'] else {}

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

# summary by 'ls1' fleetCategory_UP
yvirlit_lan_man1 <- with(yvirlit_lan, tapply(vekt,list(arma=arma, ls=ls1),sum))
yvirlit_lan_man1[is.na(yvirlit_lan_man1)] <- 0
ncol_lan1        <- ncol(yvirlit_lan_man1)
yvirlit_lan_man1

yvirlit_lan_man1 <- raetRow(yvirlit_lan_man1)


# ---- Quarter summary ---- by ls
yvirlit_lan_qrt           <- data.frame(yvirlit_lan_man)
#colnames(yvirlit_lan_qrt) <- gsub('X', '', colnames(yvirlit_lan_qrt))
yvirlit_lan_qrt$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_lan_qrt           <- aggregate(yvirlit_lan_qrt[,1:ncol_lan], list(Qrt = yvirlit_lan_qrt$extra), sum)
colnames(yvirlit_lan_qrt) <- c('Qrt', colnames(yvirlit_lan_man))
yvirlit_lan_qrt

# ---- Quarter summary ---- by ls1
yvirlit_lan_qrt1           <- data.frame(yvirlit_lan_man1)
#colnames(yvirlit_lgd_qrt) <- gsub('X', '', colnames(yvirlit_lgd_qrt))
yvirlit_lan_qrt1$extra     <- rep(1:qrt, each = qrt.l)
yvirlit_lan_qrt1           <- aggregate(yvirlit_lan_qrt1[,1:ncol_lan1], list(Qrt = yvirlit_lan_qrt1$extra), sum)
colnames(yvirlit_lan_qrt1) <- c('Qrt',colnames(yvirlit_lan_man1))
yvirlit_lan_qrt1
}

######  A handy summary of fleets and their numerical identification
drifts.stutt.all <- with(bdrift, tapply(drift, list(lysing_stutt), function(x) return(x)) )
drifts.stutt.all <- sapply(drifts.stutt.all, function(x) unique(x))
if(dim(veidaDataAll)[1]!=0)
{ drifts.stutt.sma <- unique(c(colnames(yvirlit_lgd_man),colnames(yvirlit_age_man),colnames(yvirlit_wei_man),colnames(yvirlit_lan_man))) } else 
{ drifts.stutt.sma <- unique(c(colnames(yvirlit_lgd_man),colnames(yvirlit_age_man),colnames(yvirlit_wei_man))) }
###############	###############	###############	###############

# ------------------------------------------------------------------------
#######	 SINK ALL THE SUMMARY RESULTS TO A FILE		##############
# ------------------------------------------------------------------------
if(dim(veidaDataAll)[1]!=0)
{
sink(paste("Yvirlit", "_",Slag,"_",year_yvirlit, ".txt", sep=""))

cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
cat("\n", "Dato            : ", date(), '\n', "Slag            : ", Slag, '\n',"T??arskei?      : " , year_yvirlit , '\n')
cat(" <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n\n")

if(fleetCategory_question)
{
cat("Snella          : "         ,c(Snella[,2]          ),'\n')
cat("Partroplus1000  : "         ,c(Partroplus1000[,2]  ),'\n')
cat("Partrominus1000 : "         ,c(Partrominus1000[,2] ),'\n')
cat("Lemmatroplus1000: "         ,c(Lemmatroplus1000[,2]),'\n')
cat("Annad1          : "         ,c(Annad1[,2]          ),'\n')
cat("Annad2          : "         ,c(Annad2[,2]          ),'\n')
} else { 
drifts.stutt_ind  <-  names(drifts.stutt.all) %in% drifts.stutt.sma
drifts.stutt   <- drifts.stutt.all[drifts.stutt_ind]
print(drifts.stutt) }

cat('\n\n-------------------------- Lengths-summary -------------------------- \n')
print(yvirlit_lgd_mand)
cat('\n')
print(yvirlit_lgd_man1)
cat('\n')
print(yvirlit_lgd_qrt1)
cat('\n-------------------------- Weight-summary  -------------------------- \n')
print(yvirlit_wei_mand)
cat('\n')
print(yvirlit_wei_man1)
cat('\n')
print(yvirlit_wei_qrt1)
cat('\n-------------------------- Age-summary     --------------------------- \n')
print(yvirlit_age_mand)
cat('\n')
print(yvirlit_age_man1)
cat('\n')
print(yvirlit_age_qrt1)
cat('\n-------------------------- Landings-summary -------------------------- \n')
print(yvirlit_lan_mand)
cat('\n')
print(yvirlit_lan_man1)
cat('\n')
print(yvirlit_lan_qrt1)

sink()
} else {

sink(paste("Yvirlit", "_",Slag,"_",year_yvirlit, ".txt", sep=""))

cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
cat("\n", "Dato            : ", date(), '\n', "Slag            : ", Slag, '\n',"T??arskei?      : " , year_yvirlit , '\n')
cat(" <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n\n")

if(fleetCategory_question)
{
cat("Snella          : "         ,c(Snella[,2]          ),'\n')
cat("Partroplus1000  : "         ,c(Partroplus1000[,2]  ),'\n')
cat("Partrominus1000 : "         ,c(Partrominus1000[,2] ),'\n')
cat("Lemmatroplus1000: "         ,c(Lemmatroplus1000[,2]),'\n')
cat("Annad1          : "         ,c(Annad1[,2]          ),'\n')
cat("Annad2          : "         ,c(Annad2[,2]          ),'\n')
} else { 
drifts.stutt_ind  <-  names(drifts.stutt.all) %in% drifts.stutt.sma
drifts.stutt   <- drifts.stutt.all[drifts.stutt_ind]
print(drifts.stutt) }


cat('\n\n-------------------------- Lengths-summary -------------------------- \n')
print(yvirlit_lgd_mand)
cat('\n')
print(yvirlit_lgd_man1)
cat('\n')
print(yvirlit_lgd_qrt1)
cat('\n-------------------------- Weight-summary  -------------------------- \n')
print(yvirlit_wei_mand)
cat('\n')
print(yvirlit_wei_man1)
cat('\n')
print(yvirlit_wei_qrt1)
cat('\n-------------------------- Age-summary     -------------------------- \n')
print(yvirlit_age_mand)
cat('\n')
print(yvirlit_age_man1)
cat('\n')
print(yvirlit_age_qrt1)
sink()

}



















