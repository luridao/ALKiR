ora_sid <- scan("USER_PWD.txt",what="character",skip=2,nlines=1)
ora_uid <- scan("USER_PWD.txt",what="character",skip=5,nlines=1)
ora_pwd <- scan("USER_PWD.txt",what="character",skip=8,nlines=1)

#cat(paste("library(RODBC)",'\n',
#"conn <- odbcConnect(\"",ora_sid,"\"",",","\"",ora_uid,"\"",",","\"",ora_pwd,"\"",")",sep=""   ))

cat(paste("\nBrúkaranavn og loyniorð importera frá \"USER_PWD.txt\"\n",'\n'))


