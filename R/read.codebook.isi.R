`read.codebook.isi` <-
function(input.file) {

## Initialisation

letter <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")  

name <- NA
start <- NA
positions <- NA
label <- NA
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
codebook <- NA
codebook.raw <- NA
codebook.row <- NA
filter <- 0
data <- 0
variable.description <- NA
missing_a <- 0
missing_b <- 0

#Reading the codebook (twice)
codebook <- read.fwf(input.file, c(6,3,4,1,2,9,4,1,4,1,30,1,6), sep = "#", comment.char="*", strip.white=FALSE, fill=TRUE, colClasses="character")

data.labels <- read.fwf(input.file, c(34,5,33), sep="#", comment.char="*", strip.white=FALSE, fill=TRUE, colClasses="character")

## Interpreting the codebook



for (i in 1:nrow(codebook)) 
{
		
	if (sum(substr(as.character(codebook[i,1]),1,1) == letter) == 1)
	{
		varcount <- varcount+1
		
		name[varcount] <- as.character(codebook[i, 1])
		# Used for Debugging: cat(name[varcount], "\n") 
		start[varcount] <- as.numeric(codebook[i, 3])
		positions[varcount] <- as.numeric(codebook[i, 5])
		missing_a[varcount] <- as.numeric(codebook[i,7])
		missing_b[varcount] <- as.numeric(codebook[i,9])
		variable.description[varcount] <- as.character(codebook[i, 11])
		label.sub[varcount] <- as.character(codebook[i, 13])
	}
			
	if (substr(as.character(codebook[i,1]),1,1) == " ")
	{	
	
		labelcount <- labelcount+1
		
		varname[labelcount] <- as.character(name[varcount])
		value[labelcount] <- as.character(data.labels[i,2])
		label[labelcount] <- as.character(data.labels[i,3])
					
	}
		
}

## Some cleaning up

name <- as.vector(sapply(name,clean))
varname <- as.vector(sapply(varname,clean))
label.sub <- as.vector(sapply(label.sub,clean)) 

## Creating and returning the data.frames

converted.codebook <- data.frame(name, start, positions, 
	missing_a, missing_b, 
	variable.description, label.sub)

converted.labels <- data.frame(varname, value, label)

converted.result <- list(converted.codebook, converted.labels)

return(converted.result)

}

