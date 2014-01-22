aldFun <- function(alkframe, slag, drift, drift_age_borrow, drift_lgd_borrow, drift_vekt_borrow, arma, oeki)
{
driftA <- paste(drift, collapse="_")	# drifts concatenated

manarQ <- manFun(Arma)			# months in quarter fashion

ald             <- apply(alkframe[[1]][, ac(ages)], 2, function(x) (x * alkframe[[1]][,'Matad'])/alkframe[[1]][,'Sum'])
ald[is.na(ald)] <- 0
#ald             <- cbind.data.frame(longd = lens, ald)
#ald             <- data.frame(ald)

# mean lengths, sd of lengths
ald_tl    <- apply(ald[,ac(ages)], 2, function(x) x * lens)         #=> total length
ald_tl    <- apply(ald_tl, 2, sum)                                                 #=> total length
ald_tn    <- apply(ald[,ac(ages)], 2, function(x) sum(x))                  #=> total number
ald_ml    <- ald_tl/ald_tn                                                         #=> ALD mean length by age group                  # - the  - #
ald_ml    <- apply(ald[,ac(ages)], 2, function(x) if(sum(x)==0) return(0) else mean(rep(lens, times=x)))  # - same - #
ald_sd    <- apply(ald[,ac(ages)], 2, function(x) if(sum(x)<=1) return(0) else sd(rep(lens, times=x)))
#ald_matad <- apply(ald[,ac(ages)], 2, sum)                                 #=> ALD total number of fishes measured by age group
tot_ml    <- sum(ald_tl)/sum(ald_tn)								#=> mean length in the entire sample

# Slope & Intercept derived from length-weight relationship
A = lgdvektFun(lgdvektAll, Slag, Drift, Drift_Lgd_Borrow, Drift_Vekt_Borrow, Arma, Oeki)['a']
B = lgdvektFun(lgdvektAll, Slag, Drift, Drift_Lgd_Borrow, Drift_Vekt_Borrow, Arma, Oeki)['b']

# mean weights derived from length-weight relationship
tw     <- apply(ald[,ac(ages)],2,function(x) x*(A * lens ^ B)) #=> total weight
tw	 <- apply(tw,2,sum)
tn     <- apply(ald[,ac(ages)],2,sum)                                            #=> total number
ald_mw <- (tw/tn)/1000                                                            #=> ALD mean weights by age
ald_mw[is.na(ald_mw)] <- 0
ald_pa <- apply(ald[,ac(ages)], 2, sum)/sum(ald[, ac(ages)])  #=> ALD proportion of fish by age
tot_mw <- sum(tw)/sum(tn)         							          #=> mean weight in the total sample cat

ald             <- data.frame(ald)
colnames(ald)   <- ages

ald$Matad       <- alkframe[[1]]$Matad
ald$Sum         <- apply(ald[, ac(ages)], 1, sum)
colSum          <- apply(ald,2, sum)

# adding 0's to 'Sum', 'ald_mw', 'ald_ml' , 'ald_pa' 
# in order to match its length with that of 'ald'
ald <- rbind.data.frame(ald, c(colSum, 0, 0), c(ald_mw, 0, 0), c(ald_ml, 0, 0), c(ald_pa, 0, 0), tot_mw, tot_ml)
rownames(ald) <- c(lens, 'Sum', 'mW', 'mL', 'pA', 'tot.mW', 'tot.mL')

#ald             <- cbind.data.frame(longd = lens, ald)

# ------------------ start SMOOTH ALD ------------------------------- #

ald.j             <- apply(alkframe[[2]][, ac(ages)], 2, function(x) (x * alkframe[[2]][,'Matad'])/alkframe[[2]][,'Sum'])
ald.j[is.na(ald.j)] <- 0
#ald.j             <- cbind.data.frame(longd = lens, ald.j)
#ald.j             <- data.frame(ald.j)

# mean lengths, sd of lengths
ald.j_tl    <- apply(ald.j[,ac(ages)], 2, function(x) x * lens)         #=> total length
ald.j_tl    <- apply(ald.j_tl, 2, sum)                                                 #=> total length
ald.j_tn    <- apply(ald.j[,ac(ages)], 2, function(x) sum(x))                  #=> total number
ald.j_ml    <- ald.j_tl/ald.j_tn                                                         #=> ALD mean length by age group                  # - the  - #
ald.j_ml    <- apply(ald.j[,ac(ages)], 2, function(x) if(sum(x)==0) return(0) else mean(rep(lens, times=x)))  # - same - #
ald.j_sd    <- apply(ald.j[,ac(ages)], 2, function(x) if(sum(x)<=1) return(0) else sd(rep(lens, times=x)))
#ald.j_matad <- apply(ald.j[,ac(ages)], 2, sum)                                 #=> ALD total number of fishes measured by age group
tot.j_ml    <- sum(ald.j_tl)/sum(ald.j_tn)								#=> mean length in the entire sample

# mean weights derived from length-weight relationship
tw.j     <- apply(ald.j[,ac(ages)],2,function(x) x*(A * lens ^ B)) #=> total weight
tw.j	 <- apply(tw.j,2,sum)
tn.j     <- apply(ald.j[,ac(ages)],2,sum)                                            #=> total number
ald.j_mw <- (tw.j/tn.j)/1000                                                            #=> ALD mean weights by age
ald.j_mw[is.na(ald.j_mw)] <- 0
ald.j_pa <- apply(ald.j[,ac(ages)], 2, sum)/sum(ald.j[, ac(ages)])  #=> ALD proportion of fish by age
tot.j_mw <- sum(tw.j)/sum(tn.j)         							          #=> mean weight in the total sample cat

ald.j             <- data.frame(ald.j)
colnames(ald.j)   <- ages

ald.j$Matad       <- alkframe[[2]]$Matad
ald.j$Sum         <- apply(ald.j[, ac(ages)], 1, sum)
colSum.j          <- apply(ald.j, 2, sum)

# adding 0's to 'Sum', 'ald.j_mw', 'ald.j_ml' , 'ald.j_pa' 
# in order to match its length with that of 'ald.j'
ald.j <- rbind.data.frame(ald.j, c(colSum.j, 0, 0), c(ald.j_mw, 0, 0), c(ald.j_ml, 0, 0), c(ald.j_pa, 0, 0), tot.j_mw, tot.j_ml)
rownames(ald.j) <- c(lens, 'Sum', 'mW', 'mL', 'pA', 'tot.mW', 'tot.mL')

#ald.j             <- cbind.data.frame(longd = lens, ald.j)

# ------------------ end SMOOTH ALD ------------------------------- #

# both ALD's (raw and smoothed) into a list 
ALD.b <- list(ALD = ald, ALD.j = ald.j)

assign(paste("ald", slag, manarQ, driftA, sep="_"), ALD.b, env=.GlobalEnv)
return(ALD.b)

}
#aldFun( alkFun(alkAll, lgdAll, Slag, Drift, Drift_Age_Borrow, Drift_Lgd_Borrow, Arma, Oeki), Slag, Drift, Drift_Age_Borrow, Drift_Lgd_Borrow, Drift_Vekt_Borrow, Arma, Oeki ) 






