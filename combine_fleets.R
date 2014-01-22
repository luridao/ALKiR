# ------- START function to combine final result -----------------

combFun <- function(slag, drift)
{

#files <- ls(pattern=sprintf('res_%s_.*%s',slag,paste(driftV,collapse="_")), pos=1)
files <- ls(pattern=paste('res_',slag,'_.*',paste(drift,collapse="_"),sep=''), pos=1)

files.l <- length(files)
files.list <- as.list(numeric(files.l))
names(files.list) <- files

for(i in 1:files.l) files.list[[i]] <- get(files[i])

tmp    <- as.list(numeric(files.l))
tmpj   <- as.list(numeric(files.l))
tmp.l  <- length(tmp)
tmpj.l <- length(tmpj)

for(i in 1:files.l)
{
tmp[[i]]   <- files.list[[i]][[1]]	# raw      numbers
tmpj[[i]]  <- files.list[[i]][[2]]	# smoothed numbers
}

#------------------------------------------------
### here we are calling function 'combTmpFun' ###
#------------------------------------------------

# ---------- Start - Raw result ---------- #

# 'R'aw results
comb.R <- combTmpFun(tmp)
comb.R[,'midal_longd'] <- round(comb.R[,'midal_longd'], 2)
comb.R[,'midal_vekt']  <- round(comb.R[,'midal_vekt'], 3)

assign(paste("combR", slag, paste(drift, collapse="_"), sep="_"), comb.R, env=.GlobalEnv)

# ---------- End - Raw result ---------- #

# ---------- Start - Smooth result ---------- #

# 'S'mooth results
comb.S <- combTmpFun(tmpj)
comb.S[,'midal_longd'] <- round(comb.S[,'midal_longd'], 2)
comb.S[,'midal_vekt']  <- round(comb.S[,'midal_vekt'], 3)

assign(paste("combS", slag, paste(drift, collapse="_"), sep="_"), comb.S, env=.GlobalEnv)

# ---------- End - Smooth result ---------- #

# both res's (raw and smoothed) into a list 
comb.b <- list(Urslitid = comb.R, Urslitid_Javnad = comb.S)

assign(paste("combB", slag, paste(drift, collapse="_"), sep="_"), comb.b, env=.GlobalEnv)
return(comb.b)

}
combFun(Slag, DriftV)



# ------- END function to combine final result -----------------
