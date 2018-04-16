pollutantmean <- function (directory, pollutant, id = 1:332)
{
        filelist <- list.files (path = directory, pattern = ".csv", full.names = TRUE)
        values <- numeric()
        
        for (i in id )
                {
                data <- read.csv(filelist[i])
                values <- c(values, data[[pollutant]])
        }
        mean (values, na.rm= TRUE)
}

complete <- function (directory , id = 1:332){
        filelist <- list.files (path = directory , pattern = ".csv", full.names = TRUE)
        nobs <- numeric()
                for (i in id ){
                        data <- read.csv(filelist[i])
                        nobs <- c(nobs, sum(complete.cases(data)))
                }
data.frame(id, nobs)
}



Corr <- function (directory, threshold =0){
               filelist <- list.files (path = directory, pattern = ".csv", full.names = TRUE)
                 threshold = numeric()
                 for (i in threshold) {
                                 data <- read.csv( filelist[i])
                       threshold <- c(threshold, Corr(data[,"nitrate"], data[, "sulfate"]))
                         }
               return (threshold)
        }
