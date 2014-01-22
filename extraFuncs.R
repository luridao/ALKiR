
## list of registered species in database  ##
slag.list = c('AV','BL','BR','GF','GL','HB','HU','HV','HY','JS','KA','LD','LO','MA','ME','MG','RE','RO','SI','SK','SM','SV','TO','UP','VI')

## list of years in database  ##
year.list = seq(1974, as.integer(format(Sys.Date(),'%Y')))

## labels for printing out lgd_vekt regression  ##
lab.lwlogfit <- c('> Alpha\t =','> Beta\t =','> R^2\t =', '> N\t =', '> Veida (kg.)\t  =', '> Rundveida (kg.) =')

## 'mean' and 'sum' functions ignoring NAs ##
meanNA <- function(x) mean(x, na.rm=TRUE)
sumNA  <- function(x) sum(x,  na.rm=TRUE)

## character-function ##
ac <- function(x) as.character(x)

## function to return fleets borrowed ##
lanDrifts <- function() 
{
landrifts <- sort(unique(c(Drift_Age_Borrow, Drift_Lgd_Borrow, Drift_Vekt_Borrow)))
return(landrifts)
}

# -------- START function to correct 'Yvirlit' data when lack of sampling months  -------------------------

raetRow <- function(yvirl_mat)
{
rn             <- row.names(yvirl_mat)
rn             <- formatC(as.integer(row.names(yvirl_mat)), width=4, flag="0")
miss.row       <- yymm[-(match(rn, yymm))]	# missing sampling rows(months) 
miss.row.l     <- length(miss.row)		# length of missing sampling rows(months)
miss.row.m     <- matrix(0, nrow = miss.row.l, ncol = ncol(yvirl_mat), dimnames=list(miss.row, colnames(yvirl_mat)))
tmp            <- rbind(yvirl_mat, miss.row.m)	# create empty data with months missing
row.names(tmp) <- formatC(as.integer(row.names(tmp)), width=4, flag="0")
tmp            <- tmp[order(row.names(tmp)),]		# and 'rbind' with oroginal data
return(tmp)
}
#raetRow(yvirlit_lgd_man)

# -------- END function to correct 'Yvirlit' data when lack of sampling months  -------------------------

# -------- START function to determine months  -------------------------

manFun <- function(arma)
{
arma <- formatC(arma,width=4,flag="0")
manarA <- ifelse(substring(arma,3,4)=='01','jan',
ifelse(substring(arma,3,4)=='02','feb',
ifelse(substring(arma,3,4)=='03','mar',
ifelse(substring(arma,3,4)=='04','apr',
ifelse(substring(arma,3,4)=='05','may',
ifelse(substring(arma,3,4)=='06','jun',
ifelse(substring(arma,3,4)=='07','jul',
ifelse(substring(arma,3,4)=='08','aug',
ifelse(substring(arma,3,4)=='09','sep',
ifelse(substring(arma,3,4)=='10','oct',
ifelse(substring(arma,3,4)=='11','nov',
ifelse(substring(arma,3,4)=='12','dec','Wrong_Month'))))))))))))
manarA.l <- length(manarA)						# length of months chosen
manarQ   <- paste(manarA[1], manarA[manarA.l], sep="_")	# months in quarter fashion

return(manarQ)
}
#manFun(Arma)

# -------- END function to determine months  -------------------------

# ------- START intermediate/temporary function to combine final resulst --------------

combTmpFun <- function(tmpObj)
{
t.matad  <- lapply(tmpObj , function(x) {x[,'tal_matad']})
t.fiskad <- lapply(tmpObj , function(x) {x[,'tal_fiskad']})
t.veida  <- lapply(tmpObj , function(x) {x[,'roknad_veida']})
t.mlongd <- lapply(tmpObj , function(x) {x[,'midal_longd']})

t.matad  <- matrix(unlist(t.matad) , ncol = length(tmpObj))
t.fiskad <- matrix(unlist(t.fiskad), ncol = length(tmpObj))
t.veida  <- matrix(unlist(t.veida) , ncol = length(tmpObj))
t.mlongd <- matrix(unlist(t.mlongd), ncol = length(tmpObj))

t.mlongd[t.mlongd==0] <- NA

t.matad  <- apply(t.matad,  1, sumNA)
t.fiskad <- apply(t.fiskad, 1, sumNA)
t.veida  <- apply(t.veida,  1, sumNA)
t.mlongd <- apply(t.mlongd, 1, meanNA)

res0[,'tal_matad']    <- t.matad
res0[,'tal_fiskad']   <- t.fiskad
res0[,'roknad_veida'] <- t.veida
res0[,'midal_vekt']   <- res0[,'roknad_veida'] / res0[,'tal_fiskad']
res0[,'midal_longd']  <- t.mlongd
res0[is.na(res0)]     <- 0

return(res0)
}
#combTmpFun(tmp)
#combTmpFun(test)

# ------- END intermediate/temporary function to combine final resulst ----------------

## Various utilities ##

#------------------------------------------------------
# Quarters to choose for 'Yvirlitstølini'  ------------
qrt   = 3
qrt.l = 12 / qrt
#------------------------------------------------------

#------------------------------------------------------
ages   = 0:15	; ages.l =length(ages)
lens   = 1:150	; lens.l =length(lens)
#------------------------------------------------------

## 'lgd' data with 0's ---------------------------------
lgd0           <- data.frame(array(0, dim=c(lens.l, 2)))
colnames(lgd0) <- c("longd", "sum_matad")
lgd0[,'longd'] <- lens

## 'alk' data with 0's ---------------------------------
alk0           <- data.frame(array(0, dim=c(lens.l, ages.l+1)))
colnames(alk0) <- c("longd", ages)
alk0[,'longd'] <- lens

#-------------------------------------------------------

## Final Result data 'res' data with 0's ---------------
res0       <- array(NA, dim = c(ages.l + 1, 6), dimnames=list(NULL, c("aldur", "tal_matad", "tal_fiskad", "midal_longd", "midal_vekt", "roknad_veida")))
res0       <- data.frame(res0)
res0$aldur <- c(ages, 'Sum')

#-------------------------------------------------------





