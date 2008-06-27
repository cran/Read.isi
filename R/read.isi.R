`read.isi` <-
function(input.file, dat.file, add.missings=TRUE, add.value.labels=TRUE, ...) 
	{
	# Preparation of some variables
	conversion.result <- read.codebook.isi(input.file)
	codebook <- conversion.result[[1]]
	value.labels <- conversion.result[[2]]
	
	temp <- NA
		
	n.var <- dim(codebook)[1]
	n.obs <- dim(codebook)[2]
	
	isi.widths <- codebook$positions
	isi.names <- codebook$name

	# Actual reading of the data, including appending variable names
	isi.data <- read.fwf(dat.file, 
		width = isi.widths, 
		col.names = isi.names, ... )
		
	# Setting the missings
	if (add.missings==TRUE)
		{
		for (i in 1:n.var) 
			{
			isi.data[, i][isi.data[, i] == codebook$missing_a[i]] <- NA
			isi.data[, i][isi.data[, i] == codebook$missing_b[i]] <- NA
			}
		}
			
	# Setting the value labels
	if (add.value.labels==TRUE)
		{
		for (i in 1:n.var)
			{	
			temp <- which(as.character(value.labels[, 1]) == as.character(codebook[i,1]))
			if(length(temp) == 0) 
				{
				temp <- which(as.character(value.labels[, 1]) == as.character(codebook[i,7]))
				}
			if(length(temp) > 0)
				{
				isi.data[, i] <- factor(isi.data[, i])
			
				temp.values <- as.numeric(levels(value.labels[temp, 2]))[value.labels[temp, 2]]
				temp.labels <- sapply(as.character(value.labels[temp, 3]), FUN=clean)
			
				a <- isi.data[, i]
				b <- as.numeric(levels(value.labels[temp, 2]))[value.labels[temp, 2]]
				c <- sapply(as.character(value.labels[temp, 3]), FUN=clean)
				f <- isi.data[, i]
						
				for (k in 1:length(b))
					{
					x <- which(levels(f)==b[k])
					levels(f)[x] <- c[k]
					}
			
				isi.data[, i] <- f

				} 	
			}
		}
	
	return(isi.data)
}

