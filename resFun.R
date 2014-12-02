# ## ----- READ-IN fleets to borrow weights ----- ##
# cat("Inntasta veida", readline("Inntasta veida (press enter if no value): "), file="new_veida_obj.in", sep="\n")
# new_veida_obj <- scan("new_veida_obj.in", skip = 1, quiet= TRUE)

if (length(new_veida_obj)==0) new_veida_obj=NULL

resFun <- function(alk_ald_obj, veida_obj, new_veida_obj = new_veida_obj)
{
ald_matad <- alk_ald_obj[[1]]['Sum', ac(ages)]
tot_ml    <- alk_ald_obj[[1]]["tot.mL", 1]	         # total mean-length from 'aldFun'
tot_mw    <- alk_ald_obj[[1]]["tot.mW", 1]	         # total mean-weight from 'aldFun'
ald_pa    <- alk_ald_obj[[1]]["pA", ac(ages)] # proportion of catch by age from 'aldFun'
ald_mw    <- alk_ald_obj[[1]]["mW", ac(ages)] # mean-weight by age from 'aldFun'
ald_ml    <- alk_ald_obj[[1]]["mL", ac(ages)] # mean-length by age from 'aldFun'

# ---------------- Catch numbers -----------
Cy			<- veida_obj / tot_mw	
# ---------------- Catch numbers-at-age ----
Ca			<- Cy * ald_pa * 1000	
# --- Catch (in kg) corresponding to the catch-numbers ---
roknad_veida	<- Ca * ald_mw		

# --- scale catch-numbers if new catch data are available ---
if (!is.null(new_veida_obj))
{
Ca              <- Ca * (new_veida_obj / veida_obj)	#=> catch-numbers-at-age
roknad_veida    <- Ca * ald_mw				#=> roknad veida
}

# ------------ all in one data frame ------------------------
urslitid <- data.frame(aldur       = ages
                    , tal_matad    = t(ald_matad)
                    , tal_fiskad   = t(Ca)
                    , midal_longd  = t(ald_ml)
                    , midal_vekt   = t(ald_mw)
                    , roknad_veida = t(roknad_veida))

colnames(urslitid) <- c('aldur', 'tal_matad', 'tal_fiskad', 'midal_longd', 'midal_vekt', 'roknad_veida')

# pretty printing
urslitid[is.na(urslitid)]  <- 0
urslitid[,'tal_matad']     <- round(urslitid[,'tal_matad'],    0)
urslitid[,'midal_longd']   <- round(urslitid[,'midal_longd'],  2)
urslitid[,'midal_vekt']    <- round(urslitid[,'midal_vekt'],   3)
urslitid[,'tal_fiskad']    <- round(urslitid[,'tal_fiskad'],    0)
urslitid[,'roknad_veida']  <- round(urslitid[,'roknad_veida'], 0)

# Sum-line at the bottom of the final table "urslitid"
bot.line                <- apply(urslitid, 2, sum)		
bot.line['aldur']       <- 99
bot.line['midal_longd'] <- round(tot_ml, 2)
bot.line['midal_vekt']  <- round(tot_mw/1000, 3)

urslitid <- rbind(urslitid, bot.line)

# pretty printing for the bottom line
ind                     <- urslitid[,'aldur'] == 99
urslitid[ind, 'aldur']  <- "Sum"

# ------------------ start SMOOTH  ------------------------------- #

ald.j_matad <- alk_ald_obj[[2]]['Sum', ac(ages)]
tot.j_ml    <- alk_ald_obj[[2]]["tot.mL", 1]	         # total mean-length from 'aldFun'
tot.j_mw    <- alk_ald_obj[[2]]["tot.mW", 1]	         # total mean-weight from 'aldFun'
ald.j_pa    <- alk_ald_obj[[2]]["pA", ac(ages)] # proportion of catch by age from 'aldFun'
ald.j_mw    <- alk_ald_obj[[2]]["mW", ac(ages)] # mean-weight by age from 'aldFun'
ald.j_ml    <- alk_ald_obj[[2]]["mL", ac(ages)] # mean-length by age from 'aldFun'

# ---------------- Catch numbers -----------
Cy.j			<- veida_obj / tot.j_mw	
# ---------------- Catch numbers-at-age ----
Ca.j			<- Cy.j * ald.j_pa * 1000	
# --- Catch (in kg) corresponding to the catch-numbers ---
roknad_veida.j	<- Ca.j * ald.j_mw		

# ------------ all in one data frame ------------------------
urslitid.j <- data.frame(aldur       = ages
                    , tal_matad    = t(ald.j_matad)
                    , tal_fiskad   = t(Ca.j)
                    , midal_longd  = t(ald.j_ml)
                    , midal_vekt   = t(ald.j_mw)
                    , roknad_veida = t(roknad_veida.j))

colnames(urslitid.j) <- c('aldur', 'tal_matad', 'tal_fiskad', 'midal_longd', 'midal_vekt', 'roknad_veida')

# pretty printing
urslitid.j[is.na(urslitid.j)]  <- 0
urslitid.j[,'tal_matad']     <- round(urslitid.j[,'tal_matad'],    0)
urslitid.j[,'midal_longd']   <- round(urslitid.j[,'midal_longd'],  2)
urslitid.j[,'midal_vekt']    <- round(urslitid.j[,'midal_vekt'],   3)
urslitid.j[,'tal_fiskad']    <- round(urslitid.j[,'tal_fiskad'],    0)
urslitid.j[,'roknad_veida']  <- round(urslitid.j[,'roknad_veida'], 0)

# Sum-line at the bottom of the final table "urslitid"
bot.line.j                <- apply(urslitid.j, 2, sum)		
bot.line.j['aldur']       <- 99
bot.line.j['midal_longd'] <- round(tot.j_ml, 2)
bot.line.j['midal_vekt']  <- round(tot.j_mw/1000, 3)

urslitid.j <- rbind(urslitid.j, bot.line.j)

# pretty printing for the bottom line
ind.j                     <- urslitid.j[,'aldur'] == 99
urslitid.j[ind.j, 'aldur']  <- "Sum"

# ------------------ end SMOOTH  ------------------------------- #

# both results (raw and smoothed) into a list 
urslitid.b <- list(URSLITID = urslitid, URSLITID.j = urslitid.j)

# assign the result list to the workspace
assign(paste("res", Slag, manFun(Arma), paste(DriftV, collapse="_"), sep="_"), urslitid.b, env=.GlobalEnv)
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n")
cat(" > Dato                : ", date(), '\n' 
, "> Slag                : ", Slag, '\n'
, "> ?r                  : ", year_yvirlit, '\n' 
, "> Ti?arskei?          : ", gsub("_", "-",toupper(manFun(Arma))), '\n'
, "> Skipab?lk/-ar       : ", paste(DriftV, collapse=" "), '\n'  
, "> Skipab?lk/-ar (l?n) : ", lanDrifts(), '\n'
, "> ICES-?ki            : ", Oeki, '\n')
cat(" <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n\n")
return(urslitid.b)

}

#resFun(
#aldFun(  alkFun(alkAll, lgdAll, Slag, Drift,  Arma, Oeki), Slag, Drift, Arma, Oeki ),
#veidaFun(veidaDataAll         , Slag, DriftV, Arma, Oeki),
#new_veida_obj = NULL )

#--------------------------->  SLAG  DRIFT   DRIFT_AGE_BORROW  DRIFT_LGD_BORROW  DRIFT_VEKT_BORROW  ARMA  OEKI
#--------------------------->  ^^^^  ^^^^^^  ^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^  ^^^^  ^^^^
tmp1 <- alkFun(alkAll, lgdAll, Slag, Drift , Drift_Age_Borrow, Drift_Lgd_Borrow                   , Arma, Oeki)
tmp2 <- aldFun(tmp1          , Slag, Drift , Drift_Age_Borrow, Drift_Lgd_Borrow, Drift_Vekt_Borrow, Arma, Oeki)
if(dim(veidaDataAll)[1]!=0) 
	{
	#print("") 
	tmp3 <- veidaFun(veidaDataAll, Slag, DriftV                                                       , Arma, Oeki)
} else tmp3 <- veidaFun(new_veida_obj, Slag, DriftV                                                       , Arma, Oeki)

sink(paste("res_", Slag, "_", year_yvirlit, "_", manFun(Arma), "_", paste(DriftV, collapse="_"), ".txt", sep=""))
resFun(tmp2, tmp3, new_veida_obj = new_veida_obj)
sink()

#test <- noquote(paste("res_", Slag,"_", year_yvirlit, "_", manFun(Arma), "_", paste(DriftV, collapse="_"), ".txt", sep=""))

#system("sed 's/[[:space:]][[:punct:]][[:punct:]][[:punct:]][[:punct:]][[:space:]][[:punct:]]TRUNCATED[[:punct:]]//g' test > foo1.txt")


