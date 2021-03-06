#-----------------------------------------------
######		 DATA-IMPORT		 #######
#-----------------------------------------------

##--------------------------------------
######		 DRIFTS		 #######
#---------------------------------------

bdrift           <- read.table("bdrift_alt.lst",header=T)
colnames(bdrift) <- tolower(colnames(bdrift))

#------------------------------------------------------
######			 SQL 			#######
#------------------------------------------------------

library(RODBC)
conn <- odbcConnect("BASTA","your_user_name","your_password")

#------------------------------------------------------
######		 ALK (AGE-LENGTH KEY) 		#######
#------------------------------------------------------

alkAll <- sqlQuery(conn,"select a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, b.aldur, b.tal 
  FROM bpaldur b, bsyni a
  WHERE
  a.synnr=b.synnr and a.arma > 1300 and a.arma < 1313
  and a.slagkoda in ('UP')
  ORDER by a.arma, b.bolknr, b.aldur")

colnames(alkAll)    <- c('slag', 'drift', 'oeki', 'arma', 'bolknr', 'aldur', 'tal')

##------------------------------------------------------
######		 Length frecuency		 #######
#-------------------------------------------------------

lgdAll <- sqlQuery(conn, "SELECT a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, tal
  FROM blongd b, bsyni a
  WHERE
  a.synnr=b.synnr and a.slagkoda in ('UP')
  and a.arma > 1300 and a.arma < 1313
  ORDER BY b.bolknr")

colnames(lgdAll) <- c('slag','drift','oeki','arma','longd','sum_matad')

###-----------------------------------------------------
######		 Length-Weight function		 #######
#-------------------------------------------------------

lgdvektAll <- sqlQuery(conn, "SELECT a.slagkoda, a.drift, a.oeki, a.arma, b.lgd, b.aldur, b.vekt
  FROM bpeinkv b, bsyni a
  WHERE
  a.synnr=b.synnr and a.slagkoda in ('UP')
  and a.arma > 1300 and a.arma < 1313")

colnames(lgdvektAll)    <- tolower(colnames(lgdvektAll))
colnames(lgdvektAll)[1] <- 'slag'

####--------------------------------------------
######		 Catch data		 #######
#-----------------------------------------------

veidaDataAll <- sqlQuery(conn, "SELECT slagkoda, arma, drift, oeki, vekt
  FROM bveida
  WHERE
  arma > 1300 and arma < 1313 and slagkoda in ('UP')
  group by slagkoda, arma, drift, oeki, vekt
  ORDER BY slagkoda, arma, drift, oeki, vekt")

colnames(veidaDataAll)    <- tolower(colnames(veidaDataAll))
colnames(veidaDataAll)[1] <- 'slag'