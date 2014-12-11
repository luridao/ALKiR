# -------- START function to restore 'res' files, e.g. "res_UP_2014_jan_dec_20.txt"  -------------------------

# --- select the "res" file
temp_res_txt <- tk_choose.files(caption = "Select \'res\' files", filters = Filters)

# - read-in column names in tables $URSLITID 
col_names_urslitid <- scan(temp_res_txt, what = "char", sep="", skip = 13, nmax = 6) # 6 number of columns in "$URSLITID"

# --- read-in "$URSLITID" table from file, e.g. "res_UP_2014_jan_dec_20.txt"
urslitid <- read.table(temp_res_txt, header= F, sep="", skip = 14, nrows = ages.l + 1) # 17 = length number of ages (0:15) + 1 (bottom-line=Sum)
urslitid <- urslitid[,-1] # remove first column
colnames(urslitid) <- col_names_urslitid
rownames(urslitid) <- c(ages, ac(ages.l+1))

# --- read-in "$URSLITID.j" table (javnad) from file, e.g. "res_UP_2014_jan_dec_20.txt"
urslitid_j <- read.table(temp_res_txt, header= F, sep="", skip = 34, nrows = ages.l + 1) # 17 = length number of ages (0:15) + 1 (bottom-line=Sum)
urslitid_j <- urslitid_j[,-1] # remove first column
colnames(urslitid_j) <- col_names_urslitid
rownames(urslitid_j) <- c(ages, ac(ages.l+1))

# --- encapsulate both data frames into one list
urslitid_list <- list("URSLITID" = urslitid, "URSLITID.j" = urslitid_j)

# --- get the object name
species_name <- scan(temp_res_txt, what = "char", sep="", skip = 4, nlines = 1)[4]	# UP
year         <- scan(temp_res_txt, what = "char", sep="", skip = 5, nlines = 1)[4]	# 2014
time_period  <- scan(temp_res_txt, what = "char", sep="", skip = 6, nlines = 1)[4]	# jan_dec
time_period  <- tolower(time_period) ; time_period  <- gsub("-","_",time_period)	#  
fleets       <- scan(temp_res_txt, what = "char", sep="", skip = 7, nlines = 1) 	# 19_20_21
fleets       <- fleets[4:length(fleets)] ; fleets    <- paste(fleets, collapse="_")	# 

# --- assign the name of the file as the name of the object
res_file <- paste("res", species_name, year, time_period, fleets, ".txt",collapse="_", sep="_")
res_file <- gsub("_.txt", ".txt", res_file)

res_obj  <- paste("res", species_name, year, time_period, fleets, collapse="_", sep="_") 
assign(res_obj, urslitid_list, env = .GlobalEnv)

#cat("\nYou have restored the following data file: ", res_file, "into the workspace as: ", res_obj,"\n")
# -------- END function to restore 'res' files, e.g., res_UP_2014_jan_dec_20.txt  -------------------------

