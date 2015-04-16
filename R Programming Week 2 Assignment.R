pollutantmean <- function (directory= ".", pollutant, id= 1:332){
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
     if(length(id)>1){
        c_comb <- read.csv(paste(dir_path, csv_filename[id[1]], sep = ""), header = TRUE, sep = ",")
	range <- id[2]:id[length(id)]
      	        for (i in seq_along(range)){
	        ci <- read.csv(paste(dir_path, csv_filename[range[i]], sep = ""), header = TRUE, sep = ",")
                c_comb <- rbind(c_comb, ci)}
        } 
        else {
        c_comb<-read.csv(paste(dir_path, csv_filename[id[1]], sep = ""), header = TRUE, sep = ",")}
                       
        #Part 2 - calculate mean according to ID range omit NA

pollutant_mean<- round(mean(c_comb[,pollutant], na.rm=TRUE), digits = 3)
return(pollutant_mean)
}
