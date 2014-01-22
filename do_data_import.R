
sink("data_import.R")

cat(paste("#-----------------------------------------------
######\t\t DATA-IMPORT\t\t #######
#-----------------------------------------------\n\n"))

cat(paste("##--------------------------------------
######\t\t DRIFTS\t\t #######
#---------------------------------------\n\n"))

cat(paste("bdrift           <- read.table(\"bdrift_alt.lst\",header=T)
colnames(bdrift) <- tolower(colnames(bdrift))"))

cat('\n\n')

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

cat(paste("alkAll <- sqlQuery(conn,\"select a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, b.aldur, b.tal" ," 
  FROM bpaldur b, bsyni a
  WHERE
  a.synnr=b.synnr and a.arma > ", ArmaS_yvirlit, " and a.arma < ", ArmaE_yvirlit,"
  and a.slagkoda in (\'", Slag, "\')
  ORDER by a.arma, b.bolknr, b.aldur\")", sep=""))

cat('\n\n')

cat(paste("colnames(alkAll)    <- c('slag', 'drift', 'oeki', 'arma', 'bolknr', 'aldur', 'tal')"))

cat('\n\n')

cat(paste("##------------------------------------------------------
######\t\t Length frecuency\t\t #######
#-------------------------------------------------------\n\n"))

cat(paste("lgdAll <- sqlQuery(conn, \"SELECT a.slagkoda, a.drift, a.oeki, a.arma, b.bolknr, tal
  FROM blongd b, bsyni a
  WHERE
  a.synnr=b.synnr and a.slagkoda in (\'", Slag, "\')
  and a.arma > ", ArmaS_yvirlit, " and a.arma < ", ArmaE_yvirlit,"
  ORDER BY b.bolknr\")", sep=""))

cat('\n\n')

cat(paste("colnames(lgdAll) <- c('slag','drift','oeki','arma','longd','sum_matad')"))

cat('\n\n')

cat(paste("###-----------------------------------------------------
######\t\t Length-Weight function\t\t #######
#-------------------------------------------------------\n\n"))

cat(paste("lgdvektAll <- sqlQuery(conn, \"SELECT a.slagkoda, a.drift, a.oeki, a.arma, b.lgd, b.aldur, b.vekt
  FROM bpeinkv b, bsyni a
  WHERE
  a.synnr=b.synnr and a.slagkoda in (\'", Slag, "\')
  and a.arma > ", ArmaS_yvirlit, " and a.arma < ", ArmaE_yvirlit,"\")", sep=""))

cat('\n\n')

cat(paste("colnames(lgdvektAll)    <- tolower(colnames(lgdvektAll))"))
cat('\n')
cat(paste("colnames(lgdvektAll)[1] <- 'slag'"))

cat('\n\n')

cat(paste("####--------------------------------------------
######\t\t Catch data\t\t #######
#-----------------------------------------------\n\n"))

cat(paste("veidaDataAll <- sqlQuery(conn, \"SELECT slagkoda, arma, drift, oeki, vekt
  FROM bveida
  WHERE
  arma > ", ArmaS_yvirlit, " and arma < ", ArmaE_yvirlit," and slagkoda in (\'", Slag, "\')
  group by slagkoda, arma, drift, oeki, vekt
  ORDER BY slagkoda, arma, drift, oeki, vekt\")", sep=""))

cat('\n\n')

cat(paste("colnames(veidaDataAll)    <- tolower(colnames(veidaDataAll))"))
cat('\n')
cat(paste("colnames(veidaDataAll)[1] <- 'slag'"))

sink()



