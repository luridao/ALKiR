files2excelFun <- function(files)
{

#files <- ls(pattern=sprintf('res_%s_.*%s',slag,paste(driftV,collapse="_")), pos=1)
#files <- ls(pattern=paste('res_',slag,'_.*',paste(drift,collapse="_"),sep=''), pos=1)

files   <- gsub(".txt", "", files)
files.l <- length(files)
files.list <- as.list(numeric(files.l))
names(files.list) <- files

files_w_res <- strsplit(files,"res")
files_res   <- lapply(files_w_res, function(x) paste("res", x,sep=""))
files_res   <- unlist(lapply(files_res, function(x) x[[2]]))

#file.list <- files_res
for(i in 1:files.l) files.list[[i]] <- get(files_res[i])[[2]]

#tmp    <- as.list(numeric(files.l))
tmpj   <- as.list(numeric(files.l))
#tmp.l  <- length(tmp)
tmpj.l <- length(tmpj)

#for(i in 1:files.l)
#{
##tmp[[i]]   <- files.list[[i]][[1]]	# raw      numbers
#tmpj[[i]]  <- files.list[[i]][[2]]	# smoothed numbers
#}

## get the name of the combined quarters (jan_apr, may_aug, etc ,,,)
#qrts <- substring(file.names.ja.res,8,14)
#qrts <- toupper(qrts)

## get the fish species name 
#slag.again <- substring(files,5,6)

## get the name of the combined files
#temp        <- gsub("[^0-9]", "-", files)		# substitute characters with "-" from strings
#temp1       <- gsub("--"   , "" , temp)[1]	# substitute "--" with "-" from strings & take the first element
#drift.again <- gsub("-","_",temp1)			# substitute "-" with "_" from string

row.excel <- seq(1,221,by=20)
row.excel <- row.excel[1:files.l]

wb <- createWorkbook()
name_sheet <- substring(files_res[1], 16, nchar(files_res[1]))
sheet  <- createSheet(wb, sheetName = name_sheet)

for(i in 1:files.l)
{
#name_sheet <- substring(files[i], 16, 50)
#name_sheet <- substring(files_res[i], 8, 14)
#name_sheet <- substring(files_res[1], 16, nchar(files_res[1]))
#sheet  <- createSheet(wb, sheetName = name_sheet)
addDataFrame(files.list[[i]], sheet, startRow=row.excel[i] , startColumn=1, row.names=FALSE)
#saveWorkbook(wb, "wb.xlsx")
}
saveWorkbook(wb, "wb.xlsx")

}
#files2excelFun(res.files2excel.choose)
files2excelFun(res.files2excel)

## This works ##
#wb <- createWorkbook()
#sheet1  <- createSheet(wb, sheetName="16_17_18")
#sheet2  <- createSheet(wb, sheetName="19_20_21")
#sheet3  <- createSheet(wb, sheetName="lo_que_sea")

#addDataFrame(res_UP_may_dec_16_17_18[[2]], sheet1, startRow=1 , startColumn=1, row.names=FALSE)
#addDataFrame(res_UP_jan_jun_16_17_18[[2]], sheet1, startRow=21 , startColumn=1, row.names=FALSE)
#addDataFrame(res_UP_jun_dec_19_20_21[[2]], sheet2, startRow=1 , startColumn=1, row.names=FALSE)
#addDataFrame(res_UP_jan_may_19_20_21[[2]], sheet2, startRow=21 , startColumn=1, row.names=FALSE)
#addDataFrame(res_UP_jun_dec_19_20_21[[2]], sheet3, startRow=1 , startColumn=1, row.names=FALSE)
#addDataFrame(res_UP_jan_may_19_20_21[[2]], sheet3, startRow=21 , startColumn=1, row.names=FALSE)

#saveWorkbook(wb, "wb.xlsx")

