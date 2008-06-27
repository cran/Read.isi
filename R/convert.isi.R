`convert.isi` <-
function(input.file, output.file, dat.file) {

conversion.result <- read.codebook.isi(input.file)
codebook <- conversion.result[[1]]
value.labels <- conversion.result[[2]]	

name <- NA
start <- NA
positions <- NA
label.sub <- NA

varname <- NA
value <- NA
label <- NA

temp.var <- NA
temp.label <- NA
temp.value <- NA

varcount <- 0
labelcount <- 0
temp <- 0

k <- 0
a <- 1
filter <- 0


name <- codebook$name
start <- codebook$start
positions <- codebook$positions
missing_a <- codebook$missing_a
missing_b <- codebook$missing_b
variable.description <- codebook$variable.description
label.sub <- codebook$label.sub

varname <- value.labels$varname
value <- value.labels$value
label <- value.labels$label


## Preparing the writing of the data


inter <- matrix(ncol=1,nrow=4)
inter[1] <- "."
inter[2] <- " "
inter[3] <- "EXECUTE ."
inter[4] <- " "

file.header <- matrix(ncol=1,nrow=21)
file.header[1] <- "* * ** *** ***** ********"
file.header[2] <- "* This syntax file has been created using read.isi, version 0.4"
file.header[3] <- "* Read.isi is an R-Project extension package developed by Rense Nieuwenhuis."
file.header[4] <- "* Read.isi is free software and comes with absolutely no warranty. This also applies to this SPSS syntax file."
file.header[5] <- "* For more information regarding the license under which read.isi is distributed," 
file.header[6] <- "* please refer to the original read.isi package."
file.header[7] <- "*"
file.header[8] <- "* Read.isi has been developed and is currently maintained by Rense Nieuwenhuis"
file.header[9] <- "* For more information on read.isi, please visit: http://www.rensenieuwenhuis.nl/r-project/read-isi/"
file.header[10] <-"* or contact the developer at: contact@rensenieuwenhuis.nl"
file.header[11] <-"* * ** *** ***** ********"
file.header[12] <-""
file.header[13] <-""
file.header[14] <- "GET DATA  /TYPE = TXT"
file.header[15] <- paste(c("/FILE= '", dat.file, "'"), collapse="")
file.header[16] <- "/FIXCASE = 1"
file.header[17] <- "/ARRANGEMENT = FIXED"
file.header[18] <- "/FIRSTCASE = 1"
file.header[19] <- "/IMPORTCASE = ALL"
file.header[20] <- "/VARIABLES ="
file.header[21] <- "/1 "

data.positions <- matrix(ncol=7, nrow=length(name))

for (i in 1:length(positions))
	{
		data.positions[i, 1] <- as.character(name[i])
		data.positions[i, 2] <- " "
		data.positions[i, 3] <- start[i]-1
		data.positions[i, 4] <- "-"
		data.positions[i, 5] <- start[i]+positions[i]-2
		data.positions[i, 6] <- " F"
		data.positions[i, 7] <- positions[i]
	}
	
varlabels <- NA	
varlabels[1:3] <- ""
varlabels[4] <- "VARIABLE LABELS"
variable.labels <- matrix(ncol=4,nrow=length(name))

for (i in 1:length(name))
	{
		variable.labels[i, 1] <- as.character(name[i])
		variable.labels[i, 2] <- " '"
		variable.labels[i, 3] <- make.names(clean(as.character(variable.description[i])))
		variable.labels[i, 4] <- "'"
	}
	

## Actual writing of the data


#Header
write.table(file.header, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="")
write.table(data.positions, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="", append=TRUE)

#Variable names and positions
write.table(inter, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="", append=TRUE)

#Variable labels
write.table(varlabels, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="", append=TRUE)
write.table(variable.labels, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="", append=TRUE)
write.table(inter, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="", append=TRUE)

#Value labels

value.labellist <- NA

for (k in 1:length(name))
{
	temp <- which(as.character(varname)==as.character(name[k]))

	if (length(temp) == 0)
		{
		temp <- which(as.character(varname)==as.character(label.sub[k]))
		}
	if (length(temp) > 0) 
		{
					
		temp.label <- as.character(label[temp])
						
		labels <- matrix(ncol=4, nrow=(length(temp)+3))
		labels[1, 1:4] <- c("VALUE LABELS ", as.character(name[k]), " ", " ")
		for (l in 1:length(temp))
			{
				labels[1+l, 1] <- value[temp[l]]
				labels[1+l, 2] <- " '"
				labels[1+l, 3] <- clean(as.character(label[temp[l]]))
				labels[1+l, 4] <- "'"
			}
		labels[length(temp)+2, 1:4] <- c("."," ", " ", " ")
		labels[length(temp)+3, 1:4] <- c(" "," ", " ", " ")

		write.table(labels, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="", append=TRUE)
	
		}	
		
		
		## Missing values
		
		if (is.na(missing_a[k])==TRUE) { missing_a[k] <-" " }
		if (is.na(missing_b[k])==TRUE) { missing_b[k] <-" " }	
			
				mis.values <- matrix(ncol=7, nrow=3, data="")
				mis.values[1,1] <- "MISSING VALUES "
				mis.values[1,2] <- as.character(name[k])
				mis.values[1,3] <- "("
				mis.values[1,4] <- missing_a[k]
				mis.values[1,5] <- ", "
				mis.values[1,6] <- missing_b[k]
				mis.values[1,7] <- ") . "
				mis.values[2,1] <- "EXECUTE ."
				mis.values[3,1] <- " "

				write.table(mis.values, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="", append=TRUE)
		
	
		
	}

write.table(inter, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="", append=TRUE)


## Saving the file

save <- matrix(ncol=5,nrow=4,data="")
save[1,1] <- "SAVE OUTFILE="
save[1,2] <- " '"
save[1,3] <- dat.file
save[1,4] <- ".sav'"
save[2,1] <- "/COMPRESSED."
save[3,1] <- ""

	write.table(save, file=output.file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="", append=TRUE)


cat("\n", "Finished.")


}

