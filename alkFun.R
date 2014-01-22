# ------- START function to calculate ALK -------------------
alkFun <- function(alkall, lgdall, slag, drift, drift_age_borrow, drift_lgd_borrow, arma, oeki)
{
driftA <- paste(drift, collapse="_")	# drifts concatenated

manarQ <- manFun(Arma)			# months in quarter fashion

# ------------------ age-length frecuencies ------------------------ #
if(Drift_Age_Borrow!=0)
{
alk1 <- with(alkall, alkall[slag %in% Slag & drift %in% c(Drift, Drift_Age_Borrow) & arma %in% Arma & oeki %in% Oeki, ])
}
else
alk1 <- with(alkall, alkall[slag %in% Slag & drift %in% Drift & arma %in% Arma & oeki %in% Oeki, ])

alk2 <- with(alk1,tapply(tal, list(bolknr,aldur),sum))	# ALK in lgd(row) vs. aldur(column)

# determine which ages are missing from alk2
ind <- ages[!(ages %in% colnames(alk2))]

# empty matrix with lgd (row) and aldur (column) with ages missing from alk2
arr_empty <- array(NA, dim=c(dim(alk2)[1], length(ind)), dimnames = list(NULL, ind))

# combine by column alk2 with empty matrix
alk2                <- cbind(alk2, arr_empty)
# sort alk2 so that columns are in numerical order 0,1,2,3,,,
alk2			  <- alk2[,ac(ages)]	
alk2                <- data.frame(alk2)
alk2[is.na(alk2)]   <- 0
dimnames(alk2)[[2]] <- ages
alk2$longd          <- as.integer(dimnames(alk2)[[1]])
# sort again alk2 so that column 'longd' precedes 0,1,2,3,,,
alk2                <- alk2[,c('longd', ages)]

# combine by row alk2 with empty matrix
alk2             <- rbind(alk2, alk0)
alk2             <- data.frame(aggregate(alk2[,ac(ages)], list(longd=alk2$longd), sum))
colnames(alk2)   <- c('longd', ages)
alk2[is.na(alk2)]<- 0
alk2$Sum <- apply(alk2[,ac(ages)], 1, sum)

# ------------------- length frecuencies ------------------------- #
if(Drift_Lgd_Borrow!=0)
{
lgd1 <- with(lgdall, lgdall[slag %in% Slag & drift %in% c(Drift, Drift_Lgd_Borrow) & arma %in% Arma & oeki %in% Oeki, ])
}
else
lgd1 <- with(lgdall, lgdall[slag %in% Slag & drift %in% Drift & arma %in% Arma & oeki %in% Oeki, ])

lgd2 <- with(lgd1, tapply(sum_matad, list(longd), sum))	# 'lgd1' in lgd(row) vs. sum_matad(column)

lgd2 <- data.frame(longd = as.integer(names(lgd2)), sum_matad = lgd2)
lgd2 <- rbind(lgd2, lgd0) # merge 'lgd2' and 'empty_matad' by row and group by lgdcm
lgd2 <- data.frame(aggregate(lgd2[,2], list(longd=lgd2$longd), sum))
colnames(lgd2)<- c('longd','sum_matad')

# ------ age-length frecuency and length frecuency together -------- #
ALK <- cbind(alk2, Matad=lgd2[,'sum_matad'])
#assign(paste("alk", slag, manarQ, driftA, sep="_"), ALK, env=.GlobalEnv)
#return(ALK)

# ------------------ start SMOOTH ALK ------------------------------- #
alk2.j <- alk0

alk2_tl <- apply(alk2[,ac(ages)], 2, function(x) x * lens)         #=> total length
alk2_tl <- apply(alk2_tl, 2, sum)                                                 #=> total length
alk2_tn <- apply(alk2[,ac(ages)], 2, function(x) sum(x))                  #=> total number
alk2_ml <- alk2_tl/alk2_tn                                                         #=> ALK mean length by age group                  # - the  - #
alk2_ml <- apply(alk2[,ac(ages)], 2, function(x) if(sum(x)==0) return(0) else mean(rep(lens,times=x)))  # - same - #
alk2_sd <- apply(alk2[,ac(ages)], 2, function(x) if(sum(x)<=1) return(0) else sd(rep(lens,times=x)))

alk2_ml_mat <- matrix(alk2_ml, nrow=lens.l, ncol=length(alk2_ml), byrow=T)       # ALK mean length in matrix format
alk2_sd_mat <- matrix(alk2_sd, nrow=lens.l, ncol=length(alk2_sd), byrow=T)       # ALK sd          in matrix format
dimnames(alk2_ml_mat) <- list(longd=lens, age=ages) ; alk2_ml_mat[is.na(alk2_ml_mat)] <- 0
dimnames(alk2_sd_mat) <- list(longd=lens, age=ages) ; alk2_sd_mat[is.na(alk2_sd_mat)] <- 0

for(i in ages)
{
if(alk2_tn[ac(i)] > 1)
alk2.j[, ac(i)] <- alk2_tn[ac(i)]*dnorm(lens, alk2_ml_mat[1, ac(i)], alk2_sd_mat[1, ac(i)])
else
alk2.j[, ac(i)] <- alk2[, ac(i)]
}

# sum across of length mesurements
alk2.j$Sum <- apply(alk2.j[, ac(ages)], 1, sum)

# ----- age-length frecuency and length frecuency together ------- #
ALK.j <- cbind(alk2.j, Matad=lgd2[,'sum_matad'])

# ---------------- end SMOOTH ALK ---------------------------------- #

# both ALK's (raw and smoothed) into a list 
ALK.b <- list(ALK = ALK, ALK.j = ALK.j)

assign(paste("alk", slag, manarQ, driftA, sep="_"), ALK.b, env=.GlobalEnv)
return(ALK.b)

}
#alkFun(alkAll, lgdAll, Slag, Drift, Drift_Age_Borrow, Drift_Lgd_Borrow, Arma, Oeki)

