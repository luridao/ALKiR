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
conn <- odbcConnect("BASTA","luisr","juanayzakarias")

#------------------------------------------------------
######		 ALK (AGE-LENGTH KEY) 		#######
#------------------------------------------------------

alkAll <- sqlQuery(conn,"select a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, b.aldur, b.tal 
  FROM bpaldur b, bsyni a
  WHERE
  a.synnr=b.synnr and a.arma > 1400 and a.arma < 1413
  and a.slagkoda in ('TO') and a.oeki in ('VB1')
  ORDER by a.arma, b.bolknr, b.aldur")

colnames(alkAll)    <- c('slag', 'drift', 'oeki', 'arma', 'bolknr', 'aldur', 'tal')

##------------------------------------------------------
######		 Length frecuency		 #######
#-------------------------------------------------------

lgdAll <- sqlQuery(conn, "SELECT a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, tal
  FROM blongd b, bsyni a
  WHERE
  a.synnr=b.synnr and a.slagkoda in ('TO')
  and a.arma > 1400 and a.arma < 1413 and a.oeki in ('VB1')
  ORDER BY b.bolknr")

colnames(lgdAll) <- c('slag','drift','oeki','arma','longd','sum_matad')

###-----------------------------------------------------
######		 Length-Weight function		 #######
#-------------------------------------------------------

lgdvektAll <- sqlQuery(conn, "SELECT a.slagkoda, a.drift, a.oeki, a.arma, b.lgd, b.aldur, b.vekt
  FROM bpeinkv b, bsyni a
  WHERE
  a.synnr=b.synnr and a.slagkoda in ('TO') and a.oeki in ('VB1')
  and a.arma > 1400 and a.arma < 1413")

colnames(lgdvektAll)    <- tolower(colnames(lgdvektAll))
colnames(lgdvektAll)[1] <- 'slag'

####--------------------------------------------
######		 Catch data		 #######
#-----------------------------------------------

veidaDataAll <- sqlQuery(conn, "SELECT slagkoda, arma, drift, oeki, vekt
  FROM bveida
  WHERE
  arma > 1400 and arma < 1413 and slagkoda in ('TO')
  and oeki in ('VB1')
  group by slagkoda, arma, drift, oeki, vekt
  ORDER BY slagkoda, arma, drift, oeki, vekt")

colnames(veidaDataAll)    <- tolower(colnames(veidaDataAll))
colnames(veidaDataAll)[1] <- 'slag'