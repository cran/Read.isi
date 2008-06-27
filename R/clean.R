clean <-
function (x) 
{
	spaces <- nchar(x, type="c")
	start <- 0
	end <- spaces
	
	# Deleting spraces at the start
	for (i in 1:spaces) 
	{
		if(substr(x,i,i) == " ") {start <- start+1}
		else break
	}
	
	#Deleting spaces at the end
	for (i in 0:spaces)
	{
		if(substr(x,spaces-i,spaces-i) == " ") {end <- end-1}
		else break
	}
	
	string <- substr(x,start+1,end)
	
	for (i in 1:nchar(string, type="c"))
	{
		if(substr(string,i,i) == "'") {substr(string,i,i) <- "."}
	}
	return(string)
}

