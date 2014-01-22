
file.names.no.res <- unlist(lapply(strsplit(res.files.choose, "res"), function(x) x[[2]]))
file.names.ja.res <- paste("res", file.names.no.res, sep="")

# ------- START function to combine final result -----------------

combFunInt <- function(files)
{

#files <- ls(pattern=sprintf('res_%s_.*%s',slag,paste(driftV,collapse="_")), pos=1)
#files <- ls(pattern=paste('res_',slag,'_.*',paste(drift,collapse="_"),sep=''), pos=1)

files   <- gsub(".txt", "", files)
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

#assign(paste("combR", slag, paste(drift, collapse="_"), sep="_"), comb.R, env=.GlobalEnv)

# ---------- End - Raw result ---------- #

# ---------- Start - Smooth result ---------- #

# 'S'mooth results
comb.S <- combTmpFun(tmpj)
comb.S[,'midal_longd'] <- round(comb.S[,'midal_longd'], 2)
comb.S[,'midal_vekt']  <- round(comb.S[,'midal_vekt'], 3)

#assign(paste("combS", slag, paste(drift, collapse="_"), sep="_"), comb.S, env=.GlobalEnv)

# ---------- End - Smooth result ---------- #

# both res's (raw and smoothed) into a list 
comb.b <- list(Urslitid = comb.R, Urslitid_Javnad = comb.S)

# get the name of the combined quarters (jan_apr, may_aug, etc ,,,)
qrts <- substring(file.names.ja.res,8,14)
qrts <- toupper(qrts)

# get the fish species name 
slag.again <- substring(files,5,6)[1]

# get the name of the combined files
temp        <- gsub("[^0-9]", "-", files)		# substitute characters with "-" from strings
temp1       <- gsub("--"   , "" , temp)[1]	# substitute "--" with "-" from strings & take the first element
drift.again <- gsub("-","_",temp1)			# substitute "-" with "_" from string

assign(paste("combB_", slag.again, drift.again, sep=""), comb.b, env=.GlobalEnv)

cat("\n\n Fílar lagdar saman í : ")
cat("*",paste("combB_", Slag, drift.again, ".txt", sep=""),"*",'\n')
cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n")
cat(" > Slag            : ", Slag                      , '\n' 
, "> Ár              : "  , year_yvirlit              , '\n'
, "> Quarters/periods: "  , paste(qrts,collapse=", ") , '\n'
, "> Combined fleets :"   , gsub("_"," ",drift.again) , '\n')
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n\n")

###	sinking the combined result to a data file	###
sink(paste("combB_", Slag, drift.again, ".txt", sep=""))
cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n")
cat(" > Dato            : " , date()                    , '\n' 
, "> Slag            : "    , Slag                      , '\n' 
, "> Ár              : "    , year_yvirlit              , '\n'
, "> Tíðarskeið      : "    , paste(qrts,collapse=", ") , '\n'
, "> Combined fleets :"    , gsub("_"," ",drift.again) , '\n')
cat(" <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n\n")

return(comb.b)
sink()

return(comb.b)

}
combFunInt(file.names.ja.res)
sink(file=NULL)

