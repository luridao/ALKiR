require(tcltk)
tt<-tktoplevel()
UserName <- tclVar("uid")
PwdName <- tclVar("pwd")
SidName <- tclVar("BASTA")

entry.UserName <-tkentry(tt,width="20",textvariable=UserName)
entry.PwdName <-tkentry(tt,width="20",textvariable=PwdName)
rb1 <- tkradiobutton(tt)
tkgrid(tklabel(tt,text="Enter your oracle user name"))
tkgrid(entry.UserName)
tkgrid(tklabel(tt,text="Enter your oracle password"))
tkgrid(entry.PwdName)
#tkgrid(tklabel(tt,text="Database: BASTA"))
#tkconfigure(rb1,variable=SidName,value="BASTA")
tkgrid(tklabel(tt,text="Database: BASTA"),rb1)

OnOK <- function()
{
  UserNameVal <- tclvalue(UserName)
  PwdNameVal <- tclvalue(PwdName)
  SidNameVal <- tclvalue(SidName)
	tkdestroy(tt)
	msg <- paste(UserNameVal,"is logged into BASTA")
	tkmessageBox(message=msg)
  assign("ora_uid", UserNameVal, envir = .GlobalEnv)
  assign("ora_pwd", PwdNameVal, envir = .GlobalEnv)
  assign("ora_sid", SidNameVal, envir = .GlobalEnv)
}
OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkbind(entry.UserName, "<Return>",OnOK)
tkbind(entry.PwdName, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(tt)




