##Function 1 - pollutantmean

pollutantmean <- function (directory= ".", pollutant, id= 1:332)
	{
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
                      
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
                
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
               
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        #Part 1 - Compile all csv into 1 single data table
       dir_path <- paste(getwd(),"/", directory, "/", sep="")
       
       csv_filename <-list.files(dir_path)
       
       c_comb <-NULL
	      		 
	      		 for (i in seq_along(id)){
		
			ci <- read.csv(paste(dir_path, csv_filename[id[i]], sep = ""), header = TRUE, sep = ",")
                
                	c_comb <- rbind(c_comb, ci)
        				
        					} 
                       
        #Part 2 - calculate mean according to ID range omit NA

	pollutant_mean<- round(mean(c_comb[,pollutant], na.rm=TRUE), digits = 3)
	
	return(pollutant_mean)
	
	}

##Function 2 - complete


	complete <- function(directory, id = 1:332) 
	{
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

	dir_path <- paste(getwd(),"/", directory, "/", sep="")

	csv_filename <-list.files(dir_path)
	
	complete <- NULL
                      
		for(i in seq_along(id))
			
				{

			c_i <- read.csv(paste(dir_path, csv_filename[id[i]], sep = ""), header = TRUE, sep = ",")

			site_i <- c_i$ID[1]

			check_i <- complete.cases(c_i)

			nobs_i <- sum(check_i)

			summary_i <- data.frame(id = site_i, nobs = nobs_i)

			complete <- rbind(complete, summary_i)
				}

	return(complete)

	}

#Part 3 - Corr


corr <- function(directory, threshold = 0) {

dir_path <- paste(getwd(),"/", directory, "/", sep="")
csv_filename <-list.files(dir_path)

id <- c(1:length(csv_filename))

master_summary <- NULL

	for(i in seq_along(id))
	{


	c_i <- read.csv(paste(dir_path, csv_filename[id[i]], sep = ""), header = TRUE, sep = ",")
	
	
	crr_i <- cor(c_i$sulfate, c_i$nitrate, "pairwise.complete.obs")

	comp_i <- complete.cases(c_i)
	
	
	summary_i <- c(crr_i, sum(comp_i))
	
	master_summary <- rbind(master_summary, summary_i)

	}

	
	corr <- na.omit(as.vector(subset(master_summary[,1], master_summary[,2] >= threshold)))


	return(corr)


}

