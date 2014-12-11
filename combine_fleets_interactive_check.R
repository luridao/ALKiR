Filters <- matrix(c("Text", "res_*.txt"), 1, 2, byrow = TRUE)

res.files.choose  <- tk_choose.files(caption = "Select \'res\' files", filters = Filters)

file.names.no.res <- unlist(lapply(strsplit(res.files.choose, "res"), function(x) x[[2]]))
file.names.ja.res <- paste("res", file.names.no.res, sep="")

# ------- START function to check if combined files make sense  -----------------

# --- function that checks whether the species, year and time period are OK
#     when combining fleets, e.g. ; R > file.names.ja.res
#     [1] "res_TO_2014_jan_dec_19_20.txt" "res_TO_2014_jun_dec_19_20.txt"

combine_fleets_interactive_check <- function(files_to_combine)
{
	# - function to compare one element in a vector with the rest
	#compare <- function(v) all(sapply(as.list(v[-1]), FUN=function(z) {identical(z, v[1])}))
	
	slag_ar <- substring(files_to_combine, 5, 11)
	period  <- substring(files_to_combine, 13, 19)
	fleets  <- substring(files_to_combine, 21, 150) ; fleets <- gsub(".txt", "", fleets)
	
	# - check if species and year coincide -> TRUE then it is OK
	slag_ar_boolean <- compare(slag_ar)
	
	# - check if time period (months) overlap -> TRUE then it is NOT OK
	list_seq_months <- lapply(as.list(period), function(x) seq(mon_df[mon_df$mon_substr == substring(x, 1, 3), "mon_tal"]
												    , mon_df[mon_df$mon_substr == substring(x, 5, 7), "mon_tal"]))
	period_boolean <- any(diff(sort(unlist(list_seq_months))) %in% 0)
	
	# - check if fleets coincide -> TRUE then it is  OK
	fleets_boolean <- compare(fleets)
	
	# - both checks together
	vector_boolean <- c("slag_ar_equal"=slag_ar_boolean, "fleets_equal"=fleets_boolean, "period_overlap"=period_boolean)
	#return(vector_boolean)
# 	msg<-ifelse(vector_boolean[1] ,
# 		  ifelse(vector_boolean[2],
# 		  	  ifelse(vector_boolean[3],print("\nOverlap in period chosen\n"),print("")),
# 		  	   print("\nFleets are not identical\n")),print("\nSlag og/ella ár eru ikki eins\n"))

          if(vector_boolean[1])
		{
		if(vector_boolean[2])
		{
		if(vector_boolean[3])
		{
			{msg <- "Overlap in period chosen" ; source("combine_fleets_interactive_check_msg3.R")}#cat("\nOverlap in period chosen\n\n")
		} else msg <- ""
		} else {msg <- "Fleets are not identical" ; source("combine_fleets_interactive_check_msg2.R")}#cat("\nFleets are not identical\n\n")
		} else {msg <- "Slag og/ella ár eru ikki eins" ; source("combine_fleets_interactive_check_msg1.R")}#cat("\nSlag og/ella ár eru ikki eins\n\n")

	assign("msg", msg, envir = .GlobalEnv)

}

#combine_fleets_interactive_check(file.names.ja.res)

comment(combine_fleets_interactive_check) <- "The first two components need to be TRUE. If third (period_overlap) is TRUE then need to check
the chosen period because of overlapping"

combine_fleets_interactive_check(file.names.ja.res)
# combine_fleets_interactive_check(test)
# combine_fleets_interactive_check(test1)
# combine_fleets_interactive_check(test2)

# ------- END function to check if combined files make sense  -----------------
