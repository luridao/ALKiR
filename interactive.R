## --------	APPLICATION TO COMPILE CATCH-AT-AGE NUMBERS -------------
## The script starts out by asking for species & area & year to work on.
##
## After this a sampling-summary for the species % area & year is produced.
## This sampling-summary is an overview of the length, weight, age and catch samples
## of the species collected in the specified year and area in a monthly and 
## quarter fashion (saved in file "Yvirlit.txt")
##
## The next step is to calculate the age-length key to get the fleet-based catch-numbers (c@a)
## With the help of the sampling-summary ("Yvirlit.txt") work can now start by typing in
## the fleets and time period to calculate c@a. There is the possibility to borrow samples from
## other more-extensive sampled fleets in cases where sampling in the particular period is sparse,
## poor or non-existent at all. These fleet-based catch-numbers are saved in memory and in data files
## having the following name convention: res_"Species"_"period"_"fleet_number"
##             example 1 -----------> "res_UP_may_aug_19_20_21" <----------
##             example 2 -----------> "res_UP_jan_apr_16_17_18" <----------
##
## Finally the user may opt for combining fleets over periods of time
## A dialog will open in the local directory and the user must choose the files
## to combine (e.g., files 'res_UP_may_aug_19_20_21' and 'res_UP_jan_apr_19_20_21'
##
## To terminate any process or stop altogether just press 'ESC'
##

#load("ALKiR.RData")
myencoding = "ISO-8859-14"
#bveida_data = FALSE

source('libraries.R'  )
source('extraFuncs.R' )
source('uid_pwd_script.R')
#source('gui_alk.R'    )
# source('correctInfo.R')

#  if(ora_uid=="uid" | ora_pwd=="pwd")
#  {source('gui_alk.R'    )}


# while(coInfo == 'n')
# {
# source('gui_alk.R'    )
# source('correctInfo.R')
# }

source("menu_option.R")

# cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
# cat("\n" 
#     , "[1] Halda fram (Default)"  , "\n"
#     , "[2] Leggja filur saman?"  , "\n"
#     , "[3] Restore \'res\' fila "  , "\n")
# cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n")

ANSWER <- readline("\nSet inn 1, 2, 3 : ")
if (ANSWER == 2)
{
	source("combine_fleets_interactive_check.R", echo=T)
	while(msg!="") 
	{
		source("combine_fleets_interactive_check.R", echo=T)
	}
	source("combine_fleets_interactive.R", echo=T)
	ANSWER=0
}
if (ANSWER == 3)
{
	more_res <- readline("\nMore files *res*  to restore? (y/n): ")
	while(more_res=="y")
	{
	source("restore_res_files.R", echo=T)
	cat("\n\nYou have restored the following data file: ", "\'", res_file, "\'", " into the workspace as: ", "\'", res_obj, "\'", "\n\n", sep="")
	ANSWER=0
	more_res <- readline("\nMore files *res*  to restore? (y/n): ")
	}
}



## ----- READ-IN 'Slag' ----- ##
#slagQuestion <- readline("Sama fiskaslag? (y/n): ")
#if(slagQuestion=='n') 
#{
cat("Fiskaslag", readline("\nSet inn fiskaslag (up, to, hy ,,,) : "), file="species.in", sep="\n")
Slag <- scan("species.in", skip = 1, quiet= TRUE, what="character")
Slag <- toupper(Slag)
#}

#.- check whether the species is registered or not
while(!(Slag %in% slag.list))
{
cat("\n", Slag,"er ikki skr?set ? databasu","\n\n")
cat("Fiskaslag", readline("Set inn fiskaslag (up, to, hy ,,,) : "), file="species.in", sep="\n")
Slag <- scan("species.in", skip = 1, quiet= TRUE, what="character")
Slag <- toupper(Slag)
}

## ----- READ-IN 'Oeki' ----- ##
#areaQuestion <- readline("Sama ICES-?ki? (y/n): ")
#if(areaQuestion=='n') 
#{
cat("ICES-?ki", readline("\nSet inn ICES-?ki: "), file="driftsOki.in", sep="\n")
Oeki <- scan("driftsOki.in", skip = 1, quiet= TRUE, what="character")
Oeki <- toupper(Oeki)
Oeki1 <- noquote(paste("\'", noquote(paste(unlist(strsplit(Oeki, ",")), collapse = "\',\'")), "\'", sep=""))

#.- check whether the ICES-area is correct
while(!(all(Oeki %in% ICES.areas)))
{
indexO <- Oeki %in% ICES.areas
cat("\n", Oeki[!indexO],"is not a faroese ICES-area. Re-enter ICES-?ki","\n\n")
cat("ICES-?ki", readline("Set inn ICES-?ki aftur: "), file="driftsOki.in", sep="\n")
Oeki <- scan("driftsOki.in", skip = 1, quiet= TRUE, what="character")
Oeki <- toupper(Oeki)
Oeki1 <- noquote(paste("\'", noquote(paste(unlist(strsplit(Oeki, ",")), collapse = "\',\'")), "\'", sep=""))
}
#}

## ----- MAKE sampling-summary ----- ##
#summary <- readline("\nYnskir t? yvirlitst?l? (y/n): ")
#if(summary=='y') 
#{
cat("Ar fyri yvirlistol", readline("\nSet inn ar fyri yvirlitstol: "), file="driftsArmaSmry.in", sep="\n")
year_yvirlit <- scan("driftsArmaSmry.in", skip = 1, quiet= TRUE)

#.- check whether the year is registered or not
while(!(year_yvirlit %in% year.list))
{
cat("\n", year_yvirlit, "er ikki skraset i databasu","\n\n")
cat("Ar fyri yvirlistol", readline("Set inn ar fyri yvirlitstol: "), file="driftsArmaSmry.in", sep="\n")
year_yvirlit <- scan("driftsArmaSmry.in", skip = 1, quiet= TRUE)
}


ArmaS_yvirlit <- as.numeric(paste(substring(year_yvirlit, 3, 4), "00", sep=""))
ArmaE_yvirlit <- as.numeric(paste(substring(year_yvirlit, 3, 4), "13", sep=""))
#}

#################################################################################################
source('alkFun.R')								 #### -----       ####
source('veidaFun.R')							 ##### -----     #####
source('aldFun.R')								 ###### -----   ######
source('lgdvektFun.R')							 ####### ----- #######
#source('resFun.R')							 ####### ----- #######
source('extraFuncs.R')							 ###### -----   ######
source('do_data_import.R')						 ##### -----     #####
source('data_import.R')							 #### -----       ####
#################################################################################################

#.- make the sequence of year/month 
yymm <- paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep="")

#if(summary=='y') 
#{
source('yvirlit.R')								 
#}

cat("\n Filur fyri yvirlitstol gjordur og goymdur her: ",paste(getwd(),'/Yvirlit', "_", Slag, "_", year_yvirlit, ".txt", sep=""),'\n\n')

####################			START->SCRIPT TO LOOP 		######################
repeat{ # START repeat  ----------------------------------------------------------------

## ----- READ-IN fleets ----- ##
fleetQuestion <- readline("\nSama skipabolk/-ar? (y/n): ")

if(fleetQuestion=='n') 
{
cat("Skipabolk/-ar", readline("\nSet inn skipab?lk/-ar (set millumrum millum skipabolkanumrini): "), file="drifts.in", sep="\n")
Drift <- DriftV <- scan("drifts.in", skip = 1, quiet= TRUE)

#.- check whether the fleets are listed or not in the adopted list "bdrift.lst"
while(!(all(Drift %in% unique(bdrift$drift))))
{
indexF <- Drift %in% unique(bdrift$drift)
cat("\n", Drift[!indexF],"is not listed as a fleet. Re-enter fleets","\n\n")
cat("Skipabolk/-ar", readline("Set inn skipabolk/-ar : "), file="drifts.in", sep="\n")
Drift <- DriftV <- scan("drifts.in", skip = 1, quiet= TRUE)
}

}

## ----- READ-IN 'ArmaS' & 'ArmaE' ----- ##
cat('\n', paste("", substring(months(as.Date(paste("01", 1:12, year_yvirlit, sep="-"))), 1, 3), sep=" ")		# YYMM
, '\n', paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep=""), '\n')	# example

cat("Byrjanar og enda ar og manad fyri ALK-ALD (YYMM)", readline("\nSet inn byrjanar ar og manad fyri ALK-ALD (YYMM): "), file="driftsArma.in", sep="\n")
ArmaS <- scan("driftsArma.in", skip = 1, quiet= TRUE)
ArmaS <- formatC(ArmaS, width=4, flag="0")

#.- check whether the Start year&month is correct 
while(!(ArmaS %in% yymm))
{
cat("\n", ArmaS, "is not a proper year&month. Re-enter byrjanar ?r og m?na?: ","\n\n")
cat("Byrjanar og enda ar og manad fyri ALK-ALD (YYMM)", readline("Set inn byrjanar ar og manad fyri ALK-ALD (YYMM): "), file="driftsArma.in", sep="\n")
ArmaS <- scan("driftsArma.in", skip = 1, quiet= TRUE)
ArmaS <- formatC(ArmaS, width=4, flag="0")
}

cat('\n', paste("", substring(months(as.Date(paste("01", 1:12, year_yvirlit, sep="-"))), 1, 3), sep=" ")		# YYMM
, '\n', paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep=""), '\n')	# example

ArmaE <- readline("\nSet inn enda ?ar og manad fyri ALK-ALD (YYMM): ")

#.- check whether the End year&month is correct,  
if(ArmaE %in% yymm)
{
cat(ArmaE, file="driftsArma.in", sep=" ", append=T)
#ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE) 

ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE)
ArmaE <- formatC(ArmaE, width=4, flag="0")} else {
while(!(ArmaE %in% yymm))
{
cat("\n", ArmaE, "is not a proper year&month. Re-enter enda ar og mand?: ")
ArmaE <- readline()
ArmaE <- formatC(ArmaE, width=4, flag="0")
}
cat(ArmaE, file="driftsArma.in", sep=" ", append=T)
ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE)
ArmaE <- formatC(ArmaE, width=4, flag="0")
}
#cat(ArmaE, file="driftsArma.in", sep=" ", append=T)
#ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE)

## ----- MAKE 'Arma' ----- ##
source('armaFun.R')

## ----- READ-IN fleets to borrow lengths ----- ##
cat("Skipabolk/-ar, sum longdir eru lantar fra", readline("\nSet inn skipabolk/-ar, sum longdir skulu lanast fra (skriva '0' um onki skal lanast): "), file="driftsLgd.in", sep="\n")
Drift_Lgd_Borrow <- scan("driftsLgd.in", skip = 1, quiet= TRUE)

## ----- READ-IN fleets to borrow weights ----- ##
cat("Skipabolk/-ar, sum vektir eru lantar fra", readline("\nSet inn skipabolk/-ar, sum vektir skulu lanast fra (skriva '0' um onki skal lanast): "), file="driftsVkt.in", sep="\n")
Drift_Vekt_Borrow <- scan("driftsVkt.in", skip = 1, quiet= TRUE)

## ----- READ-IN fleets to borrow ages ----- ##
cat("Skipabolk/-ar, sum aldrar eru lantar fra", readline("\nSet inn skipabolk/-ar, sum aldrar skulu lanast fra(skriva '0' um onki skal lanast): "), file="driftsAge.in", sep="\n")
Drift_Age_Borrow <- scan("driftsAge.in", skip = 1, quiet= TRUE)

## ----- PRINT INFORMATION on base-calculations ----- ##

cat("\n","Filur goymdur sum: ", paste("res", Slag, manFun(Arma), paste(DriftV, collapse="_"), sep="_"),"\n")
cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
cat("\n", "^ Slag                                      : "  , Slag                , "\n"
        , "^ Skipabolk/-ar                             : "  , Drift               , "\n"
        , "^ Byrjanar ar og manad fyri ALK/ALD         : "  , ArmaS               , "\n"
        , "^ Enda ar og manad fyri ALK/ALD             : "  , ArmaE               , "\n"
        , "^ Skipabolk/-ar, sum longdir eru lantar fra : "  , Drift_Lgd_Borrow    , "\n"
        , "^ Skipabolk/-ar, sum vektir eru lantar fra  : "  , Drift_Vekt_Borrow   , "\n"
        , "^ Skipabolk/-ar, sum aldrar eru lantar fra  : "  , Drift_Age_Borrow    , "\n"
        , "^ ICES-oki                                  : "  , Oeki                , "\n")
cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n")

if(dim(veidaDataAll)[1]==0)
{
	cat("\nTad saer ut sum eingi BVEIDA er logd inni i BASTA-databasuna\n\n")
	cat("Inntasta veida", readline("Inntasta veida i kg. : "), file="new_veida_obj.in", sep="\n")
	new_veida_obj <- scan("new_veida_obj.in", skip = 1, quiet= TRUE)
	
} else {
## ----- READ-IN fleets to borrow weights ----- ##
cat("Inntasta veida", readline("Inntasta veida i kg. (tryst  \'enter\' hvis tu ikki veist): "), file="new_veida_obj.in", sep="\n")
new_veida_obj <- scan("new_veida_obj.in", skip = 1, quiet= TRUE)
}

## ----- CALCULATE the fleet-based C@A ----- ##
source('resFun.R', echo = TRUE, max.deparse.length = 0)

source('spool_all.R', echo=T, max.deparse.length = 0)

cat("\n\n >>>>>>>>>>>>>>> Tryst 'ESC' fyri at enda <<<<<<<<<<<<<<< \n")
cat("\n >>>>>>>>>>>>>>> Um tu ynskir at stedga og halda fram seinni, skriva: source(\"interactive.R\") <<<<< \n\n")

# ## ----- COMBINATE fleets ----- ##
# cat("\n","Vel filar sum starta vid \'res_\', til domis \'res_UP_jan_apr_19_20_21\'","\n")
# 
# YesCombFlt <- readline("\n Ynskir tu at leggja filar saman? (y/n): ")
# #Sys.sleep(1)
# #cat("\n","Vel filar sum starta vid \'res_\', til domis res_UP_jan_apr_19_20_21")
# 
# if(YesCombFlt=='y') 
# {
# #res.files.choose  <- tk_choose.files()
# source("combine_fleets_interactive.R", echo=T)
# }

source("menu_option.R")#, echo=T)

# cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
# cat("\n" 
#     , "[1] Halda fram (Default)"  , "\n"
#     , "[2] Leggja filur saman?"  , "\n"
#     , "[3] Restore \'res\' fila "  , "\n")
# cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n")

ANSWER <- readline("\nSet inn 1, 2, 3 : ")
if (ANSWER == 2)
{
	source("combine_fleets_interactive_check.R", echo=T)
	while(msg!="") 
	{
		source("combine_fleets_interactive_check.R", echo=T)
	}
	source("combine_fleets_interactive.R", echo=T)
	ANSWER=0
}
if (ANSWER == 3)
{
	more_res <- readline("\nMore files *res*  to restore? (y/n): ")
	while(more_res=="y")
	{
		source("restore_res_files.R", echo=T)
		cat("\n\nYou have restored the following data file: ", "\'", res_file, "\'", " into the workspace as: ", "\'", res_obj, "\'", "\n\n", sep="")
		ANSWER=0
		more_res <- readline("\nMore files *res*  to restore? (y/n): ")
	}
}




} # END of repeat ----------------------------------------------------------------------

####################			END->SCRIPT TO LOOP 		######################







