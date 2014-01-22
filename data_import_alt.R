#-------------------------------------------------
######	 ALTERNATIVE DATA-IMPORT 	 #######
#-------------------------------------------------


#------------------------------------------------------
######			 SQL 			#######
#------------------------------------------------------

library(RODBC) 
 conn <- odbcConnect("basta", uid="luisr", pwd="juanayzakarias") 

#------------------------------------------------------
######		 ALK (AGE-LENGTH KEY) 		#######
#------------------------------------------------------

alk_alt <- sqlQuery(conn,"select a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, b.aldur, b.tal 
  FROM bpaldur b, bsyni a
  WHERE
  a.synnr=b.synnr and a.arma > 1001 and a.arma < 1004
  and a.slagkoda in ('HY')
  ORDER by a.arma, b.bolknr, b.aldur")

colnames(alk_alt)    <- c('slag', 'drift', 'oeki', 'arma', 'bolknr', 'aldur', 'tal')

##------------------------------------------------------
######		 Length frecuency		 #######
#-------------------------------------------------------

lgd_alt <- sqlQuery(conn, "SELECT a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, tal
  FROM blongd b, bsyni a
  WHERE
  a.synnr=b.synnr and a.slagkoda in ('HY')
  and a.arma > 1001 and a.arma < 1003
  ORDER BY b.bolknr")

colnames(lgd_alt) <- c('slag','drift','oeki','arma','longd','sum_matad')

###-----------------------------------------------------
######		 Length-Weight function		 #######
#-------------------------------------------------------


 GHJ er ikki skráset á databasu 

