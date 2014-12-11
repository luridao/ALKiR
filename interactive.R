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

source('libraries.R'  , encoding = myencoding)
source('extraFuncs.R' , encoding = myencoding)
source('uid_pwd_script.R', encoding = myencoding)
#source('gui_alk.R'    , encoding = myencoding)
# source('correctInfo.R', encoding = myencoding)

#  if(ora_uid=="uid" | ora_pwd=="pwd")
#  {source('gui_alk.R'    , encoding = myencoding)}


# while(coInfo == 'n')
# {
# source('gui_alk.R'    , encoding = myencoding)
# source('correctInfo.R', encoding = myencoding)
# }

source("menu_option.R")

# cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
# cat("\n" 
#     , "[1] Halda fram (Default)"  , "\n"
#     , "[2] Leggja fílur saman?"  , "\n"
#     , "[3] Restore \'res\' fíla "  , "\n")
# cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n")

ANSWER <- readline("\nSet inn 1, 2, 3 : ")
if (ANSWER == 2)
{
	source("combine_fleets_interactive_check.R", echo=T, encoding=myencoding)
	while(msg!="") 
	{
		source("combine_fleets_interactive_check.R", echo=T, encoding=myencoding)
	}
	source("combine_fleets_interactive.R", echo=T, encoding=myencoding)
	ANSWER=0
}
if (ANSWER == 3)
{
	more_res <- readline("\nMore files *res*  to restore? (y/n): ")
	while(more_res=="y")
	{
	source("restore_res_files.R", echo=T, encoding=myencoding)
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
while(!(all(Oeki %in% c('VB1', 'VB2', 'IIA'))))
{
indexO <- Oeki %in% c('VB1', 'VB2', 'IIA')
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
cat("?r fyri yvirlist?l", readline("\nSet inn ?r fyri yvirlitst?l: "), file="driftsArmaSmry.in", sep="\n")
year_yvirlit <- scan("driftsArmaSmry.in", skip = 1, quiet= TRUE)

#.- check whether the year is registered or not
while(!(year_yvirlit %in% year.list))
{
cat("\n", year_yvirlit, "er ikki skr?set ? databasu","\n\n")
cat("?r fyri yvirlist?l", readline("Set inn ?r fyri yvirlitst?l: "), file="driftsArmaSmry.in", sep="\n")
year_yvirlit <- scan("driftsArmaSmry.in", skip = 1, quiet= TRUE)
}


ArmaS_yvirlit <- as.numeric(paste(substring(year_yvirlit, 3, 4), "00", sep=""))
ArmaE_yvirlit <- as.numeric(paste(substring(year_yvirlit, 3, 4), "13", sep=""))
#}

#################################################################################################
source('alkFun.R', encoding=myencoding)								 #### -----       ####
source('veidaFun.R', encoding=myencoding)							 ##### -----     #####
source('aldFun.R', encoding=myencoding)								 ###### -----   ######
source('lgdvektFun.R', encoding=myencoding)							 ####### ----- #######
#source('resFun.R', encoding=myencoding)							 ####### ----- #######
source('extraFuncs.R', encoding=myencoding)							 ###### -----   ######
source('do_data_import.R', encoding=myencoding)						 ##### -----     #####
source('data_import.R', encoding=myencoding)							 #### -----       ####
#################################################################################################

#.- make the sequence of year/month 
yymm <- paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep="")

#if(summary=='y') 
#{
source('yvirlit.R', encoding=myencoding)								 
#}

cat("\n F?lur fyri yvirlitst?l gj?rdur og goymdur her: ",paste(getwd(),'/Yvirlit', "_", Slag, "_", year_yvirlit, ".txt", sep=""),'\n\n')

####################			START->SCRIPT TO LOOP 		######################
repeat{ # START repeat  ----------------------------------------------------------------

## ----- READ-IN fleets ----- ##
fleetQuestion <- readline("\nSama skipab?lk/-ar? (y/n): ")

if(fleetQuestion=='n') 
{
cat("Skipab?lk/-ar", readline("\nSet inn skipab?lk/-ar (set millumr?m millum skipabolkanumrini): "), file="drifts.in", sep="\n")
Drift <- DriftV <- scan("drifts.in", skip = 1, quiet= TRUE)

#.- check whether the fleets are listed or not in the adopted list "bdrift.lst"
while(!(all(Drift %in% unique(bdrift$drift))))
{
indexF <- Drift %in% unique(bdrift$drift)
cat("\n", Drift[!indexF],"is not listed as a fleet. Re-enter fleets","\n\n")
cat("Skipab?lk/-ar", readline("Set inn skipab?lk/-ar : "), file="drifts.in", sep="\n")
Drift <- DriftV <- scan("drifts.in", skip = 1, quiet= TRUE)
}

}

## ----- READ-IN 'ArmaS' & 'ArmaE' ----- ##
cat('\n', paste("", substring(months(as.Date(paste("01", 1:12, year_yvirlit, sep="-"))), 1, 3), sep=" ")		# YYMM
, '\n', paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep=""), '\n')	# example

cat("Byrjanar og enda ?r og m?na? fyri ALK-ALD (YYMM)", readline("\nSet inn byrjanar ?r og m?na? fyri ALK-ALD (YYMM): "), file="driftsArma.in", sep="\n")
ArmaS <- scan("driftsArma.in", skip = 1, quiet= TRUE)
ArmaS <- formatC(ArmaS, width=4, flag="0")

#.- check whether the Start year&month is correct 
while(!(ArmaS %in% yymm))
{
cat("\n", ArmaS, "is not a proper year&month. Re-enter byrjanar ?r og m?na?: ","\n\n")
cat("Byrjanar og enda ?r og m?na? fyri ALK-ALD (YYMM)", readline("Set inn byrjanar ?r og m?na? fyri ALK-ALD (YYMM): "), file="driftsArma.in", sep="\n")
ArmaS <- scan("driftsArma.in", skip = 1, quiet= TRUE)
ArmaS <- formatC(ArmaS, width=4, flag="0")
}

cat('\n', paste("", substring(months(as.Date(paste("01", 1:12, year_yvirlit, sep="-"))), 1, 3), sep=" ")		# YYMM
, '\n', paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep=""), '\n')	# example

ArmaE <- readline("\nSet inn enda ?r og m?na? fyri ALK-ALD (YYMM): ")

#.- check whether the End year&month is correct,  
if(ArmaE %in% yymm)
{
cat(ArmaE, file="driftsArma.in", sep=" ", append=T)
#ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE) 

ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE)
ArmaE <- formatC(ArmaE, width=4, flag="0")} else {
while(!(ArmaE %in% yymm))
{
cat("\n", ArmaE, "is not a proper year&month. Re-enter enda ?r og m?na?: ")
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
source('armaFun.R', encoding=myencoding)

## ----- READ-IN fleets to borrow lengths ----- ##
cat("Skipab?lk/-ar, sum longdir eru l?ntar fr?", readline("\nSet inn skipab?lk/-ar, sum longdir skulu l?nast fr? (skriva '0' um onki skal l?nast): "), file="driftsLgd.in", sep="\n")
Drift_Lgd_Borrow <- scan("driftsLgd.in", skip = 1, quiet= TRUE)

## ----- READ-IN fleets to borrow weights ----- ##
cat("Skipab?lk/-ar, sum vektir eru l?ntar fr?", readline("\nSet inn skipab?lk/-ar, sum vektir skulu l?nast fr? (skriva '0' um onki skal l?nast): "), file="driftsVkt.in", sep="\n")
Drift_Vekt_Borrow <- scan("driftsVkt.in", skip = 1, quiet= TRUE)

## ----- READ-IN fleets to borrow ages ----- ##
cat("Skipab?lk/-ar, sum aldrar eru l?ntar fr?", readline("\nSet inn skipab?lk/-ar, sum aldrar skulu l?nast fr?(skriva '0' um onki skal l?nast): "), file="driftsAge.in", sep="\n")
Drift_Age_Borrow <- scan("driftsAge.in", skip = 1, quiet= TRUE)

## ----- PRINT INFORMATION on base-calculations ----- ##

cat("\n","F?lur goymdur sum: ", paste("res", Slag, manFun(Arma), paste(DriftV, collapse="_"), sep="_"),"\n")
cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
cat("\n", "^ Slag                                      : "  , Slag                , "\n"
        , "^ Skipab?lk/-ar                             : "  , Drift               , "\n"
        , "^ Byrjanar ?r og m?na? fyri ALK/ALD         : "  , ArmaS               , "\n"
        , "^ Enda ?r og m?na? fyri ALK/ALD             : "  , ArmaE               , "\n"
        , "^ Skipab?lk/-ar, sum longdir eru l?ntar fr? : "  , Drift_Lgd_Borrow    , "\n"
        , "^ Skipab?lk/-ar, sum vektir eru l?ntar fr?  : "  , Drift_Vekt_Borrow   , "\n"
        , "^ Skipab?lk/-ar, sum aldrar eru l?ntar fr?  : "  , Drift_Age_Borrow    , "\n"
        , "^ ICES-?ki                                  : "  , Oeki                , "\n")
cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n")

if(dim(veidaDataAll)[1]==0)
{
	cat("\nTað sær út sum eingi BVEIDA er løgd inni í BASTA-databasuna\n\n")
	cat("Inntasta veida", readline("Inntasta veida í kg. : "), file="new_veida_obj.in", sep="\n")
	new_veida_obj <- scan("new_veida_obj.in", skip = 1, quiet= TRUE)
	
} else {
## ----- READ-IN fleets to borrow weights ----- ##
cat("Inntasta veida", readline("Inntasta veida í kg. (trýst  \'enter\' hvis tú veist ikki): "), file="new_veida_obj.in", sep="\n")
new_veida_obj <- scan("new_veida_obj.in", skip = 1, quiet= TRUE)
}

## ----- CALCULATE the fleet-based C@A ----- ##
source('resFun.R', echo = TRUE, encoding=myencoding, max.deparse.length = 0)

source('spool_all.R', echo=T, encoding=myencoding, max.deparse.length = 0)

cat("\n\n >>>>>>>>>>>>>>> Tr?st 'ESC' fyri at enda <<<<<<<<<<<<<<< \n")
cat("\n >>>>>>>>>>>>>>> Um t? ynskir at ste?ga og halda fram seinni, skriva: source(\"interactive.R\") <<<<< \n\n")

# ## ----- COMBINATE fleets ----- ##
# cat("\n","Vel f?lar sum starta vi? \'res_\', til d?mis \'res_UP_jan_apr_19_20_21\'","\n")
# 
# YesCombFlt <- readline("\n Ynskir t? at leggja f?lar saman? (y/n): ")
# #Sys.sleep(1)
# #cat("\n","Vel f?lar sum starta vi? \'res_\', til d?mis res_UP_jan_apr_19_20_21")
# 
# if(YesCombFlt=='y') 
# {
# #res.files.choose  <- tk_choose.files()
# source("combine_fleets_interactive.R", echo=T, encoding=myencoding)
# }

source("menu_option.R")#, echo=T, encoding=myencoding)

# cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
# cat("\n" 
#     , "[1] Halda fram (Default)"  , "\n"
#     , "[2] Leggja fílur saman?"  , "\n"
#     , "[3] Restore \'res\' fíla "  , "\n")
# cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n")

ANSWER <- readline("\nSet inn 1, 2, 3 : ")
if (ANSWER == 2)
{
	source("combine_fleets_interactive_check.R", echo=T, encoding=myencoding)
	while(msg!="") 
	{
		source("combine_fleets_interactive_check.R", echo=T, encoding=myencoding)
	}
	source("combine_fleets_interactive.R", echo=T, encoding=myencoding)
	ANSWER=0
}
if (ANSWER == 3)
{
	more_res <- readline("\nMore files *res*  to restore? (y/n): ")
	while(more_res=="y")
	{
		source("restore_res_files.R", echo=T, encoding=myencoding)
		cat("\n\nYou have restored the following data file: ", "\'", res_file, "\'", " into the workspace as: ", "\'", res_obj, "\'", "\n\n", sep="")
		ANSWER=0
		more_res <- readline("\nMore files *res*  to restore? (y/n): ")
	}
}




} # END of repeat ----------------------------------------------------------------------

####################			END->SCRIPT TO LOOP 		######################







