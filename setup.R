###########################################
#	THESE COMMANDS ARE TO BE EXECUTED	#
#	ONLY ONCE. NOT NECESSARY TO RUN 	#
#	A SECOND TIME IF R-SESSION IS SAVED	#
#	save.image("MyWorkspace.RData")	#
##################################################################################
											 ##           ##
source('yvirlit_input.R')	# INTRODUCE YEAR IN THIS SCRIPT	 ###		 ###
source('alkFun.R')								 ####       ####
source('veidaFun.R')								 #####     #####
source('aldFun.R')								 ######   ######
source('lgdvektFun.R')								 ####### #######
source('resFun.R')								 ####### #######
source('extraFuncs.R')								 ######   ######
source('do_data_import.R')							 #####     #####
source('data_import.R')								 ####       ####
source('yvirlit.R')								 ###         ###
											 ##           ##
##################################################################################

# ------------------------------------------------------------------------------ #
#	GO TO 'alk_imput.R' and insert the fleets and period to workout ALK/ALD
#	SAVE the file AND then run the following
# ------------------------------------------------------------------------------ #
source('alk_input.R')
source('armaFun.R')

#--------------------------->  SLAG  DRIFT   DRIFT_AGE_BORROW  DRIFT_LGD_BORROW  DRIFT_VEKT_BORROW  ARMA  OEKI
#--------------------------->  ^^^^  ^^^^^^  ^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^  ^^^^  ^^^^
tmp1 <- alkFun(alkAll, lgdAll, Slag, Drift , Drift_Age_Borrow, Drift_Lgd_Borrow                   , Arma, Oeki)
tmp2 <- aldFun(tmp1          , Slag, Drift , Drift_Age_Borrow, Drift_Lgd_Borrow, Drift_Vekt_Borrow, Arma, Oeki)
tmp3 <- veidaFun(veidaDataAll, Slag, DriftV                                                       , Arma, Oeki)

resFun(tmp2, tmp3, new_veida_obj = NULL)

# resFun(
# aldFun(  alkFun(alkAll, lgdAll, Slag, Drift,  Drift_Age_Borrow, Drift_Lgd_Borrow, Arma, Oeki), Slag, Drift, Drift_Age_Borrow, Drift_Lgd_Borrow, Drift_Vekt_Borrow, Arma, Oeki )
# , veidaFun(veidaDataAll       , Slag, DriftV, Arma, Oeki),
# new_veida_obj = NULL )

# ----------------------------------------------------------------------------- #
#	TO COMBINE FLEETS run the following
# ----------------------------------------------------------------------------- #
source("combine_fleets.R", echo=T)


