
sink("data_import_alt.r")

cat(paste("#-------------------------------------------------
######\t ALTERNATIVE DATA-IMPORT \t #######
#-------------------------------------------------\n\n"))

cat('\n')

cat(paste("#------------------------------------------------------
######\t\t\t SQL \t\t\t#######
#------------------------------------------------------\n\n"))

ora_sid <- scan("USER_PWD.txt",what="character",skip=2,nlines=1)
#ora_uid <- scan("USER_PWD.txt",what="character",skip=5,nlines=1)
#ora_pwd <- scan("USER_PWD.txt",what="character",skip=8,nlines=1)

cat(paste("library(RODBC)",'\n',
"conn <- odbcConnect(\"",ora_sid,"\"",",","\"",ora_uid,"\"",",","\"",ora_pwd,"\"",")",sep=""   ))

cat('\n\n')

#cat(paste("library(RODBC)",'\n',
#"conn <- odbcConnect(\"basta\", uid=\"luisr\", pwd=\"juanayzakarias\")",'\n\n'))

cat(paste("#------------------------------------------------------
######\t\t ALK (AGE-LENGTH KEY) \t\t#######
#------------------------------------------------------\n\n"))

cat(paste("alk_alt <- sqlQuery(conn,\"select a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, b.aldur, b.tal" ," 
  FROM bpaldur b, bsyni a
  WHERE
  a.synnr=b.synnr and a.arma > ", ArmaS_alt_age, " and a.arma < ", ArmaE_alt_age,"
  and a.slagkoda in (\'", Slag, "\')
  ORDER by a.arma, b.bolknr, b.aldur\")", sep=""))

cat('\n\n')

cat(paste("colnames(alk_alt)    <- c('slag', 'drift', 'oeki', 'arma', 'bolknr', 'aldur', 'tal')"))

cat('\n\n')

cat(paste("##------------------------------------------------------
######\t\t Length frecuency\t\t #######
#-------------------------------------------------------\n\n"))

cat(paste("lgd_alt <- sqlQuery(conn, \"SELECT a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, tal
  FROM blongd b, bsyni a
  WHERE
  a.synnr=b.synnr and a.slagkoda in (\'", Slag, "\')
  and a.arma > ", ArmaS_alt_lgd, " and a.arma < ", ArmaE_alt_lgd,"
  ORDER BY b.bolknr\")", sep=""))

cat('\n\n')

cat(paste("colnames(lgd_alt) <- c('slag','drift','oeki','arma','longd','sum_matad')"))

cat('\n\n')

cat(paste("###-----------------------------------------------------
######\t\t Length-Weight function\t\t #######
#-------------------------------------------------------\n\n"))

cat(paste("lgdvekt_alt <- sqlQuery(conn, \"SELECT a.slagkoda, a.drift, a.oeki, a.arma, b.lgd, b.aldur, b.vekt
  FROM bpeinkv b, bsyni a
  WHERE
  a.synnr=b.synnr and a.slagkoda in (\'", Slag, "\')
  and a.arma > ", ArmaS_alt_lgd, " and a.arma < ", ArmaS_alt_lgd,"\
  and a.arma > ", ArmaS_alt_vkt, " and a.arma < ", ArmaS_alt_vkt,"\")", sep=""))

cat('\n\n')

cat(paste("colnames(lgdvekt_alt)    <- tolower(colnames(lgdvekt_alt))"))
cat('\n')
cat(paste("colnames(lgdvekt_alt)[1] <- 'slag'"))

cat('\n\n')

sink()



