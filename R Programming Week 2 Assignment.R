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
        setwd(directory)
        csv_filename <-list.files()
        c1<-read.csv(csv_filename[1], header = TRUE, sep = ",")
        c_comb <- c1
        for (i in 2:length(csv_filename)){
                ci <- read.csv(csv_filename[i], header = TRUE, sep = ",")
                c_comb <- rbind(c_comb, ci)
        }
                
        }
        
        #Part 2 - calculate mean according to ID range omit NA

sf <- subset(c_comb, ID=id, select = sulfate)


mean(sf$sulfate, na.rm = TRUE)

}