ora_sid <- scan("USER_PWD.txt",what="character",skip=1,nlines=1)
ora_uid <- scan("USER_PWD.txt",what="character",skip=3,nlines=1)
ora_pwd <- scan("USER_PWD.txt",what="character",skip=5,nlines=1)

cat(paste("library(RODBC)",'\n',
"conn <- odbcConnect(\"",ora_sid,"\"",",","\"",ora_uid,"\"",",","\"",ora_pwd,"\"",")",sep=""   ))



