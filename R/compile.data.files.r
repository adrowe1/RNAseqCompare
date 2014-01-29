#' compile.data.files
#'
#' Function to combine the raw RNAseq result files (.xls format) into data frames we can operate on
#'
#' 
#'
#' @param data.path path to the directory containing all of the data files
#' @param filter.pattern allows one to choose a subset of the files by a common substring in the filename. Default is "Filter" 
#' @param file.type default expects excel (.xls) files, but .csv is preferable if at all possible!
#' 
#' @return a data frame containing the raw data
#' @author Alexander D. Rowe
#' @seealso \code{\link{BoxCox.uniformity}}
#' @examples
#' cov.transform.age(data.frame(analyte=rnorm(20,1), Age=rexp(20,1)*10 ))
#' @export
#' @import car ggplot2 MASS gam


compile.data.files <- function(data.path, filter.pattern="Filter", file.type=".xls") {
    # Begin by making an unambiguous selection of the data files
    ## Trim any whitespace from the filter.pattern variable
    filter.pattern  <- gsub("^\\s+|\\s+$", "", filter.pattern)
    ## get a list of the data files
    data.files <- list.files(data.path, pattern=filter.pattern)
    
    
}



analyte.transform.curvefit <- function(cov.transformed.data, chosen.analyte, chosen.covariates=c("ages")) {
    data <- cov.transformed.data[[1]]
    # only relevant details
    dat <- ddply(data, .(age.cluster), summarize, transformed.age.years=median(transformed.age.years), C16.0=median(C16.0))
    ggplot(data, aes(x=transformed.age.years, y=C16.0, colour=age.cluster)) + 
        geom_point() + 
        scale_y_log10() +
        geom_point(data=dat, aes(x=transformed.age.years, y=C16.0), colour="black") +
        geom_smooth(data=dat, aes(x=transformed.age.years, y=C16.0), colour="black")
    
}


splines  <- gam(log10(C18.0) ~ s(transformed.age.years, 5), data=updated.data)
updated.data$pred<-10^(predict(splines, updated.data))
ggplot(updated.data, aes(x=transformed.age.years, y=C18.0, colour=Gender)) + geom_point() + geom_smooth() + scale_y_log10() + geom_line(data=updated.data, aes(x=transformed.age.years, y=pred), size=3)

# http://people.bath.ac.uk/sw283/mgcv/


knots <- 100

fitM <- gam(CRE~s(transformed.age.years, bs="cc", k=knots), knots=list( seq(min(output$transformed.age.years), max(output$transformed.age.years), length.out =10) ), data=output[output$gender==1,] )
fitF <- gam(CRE~s(transformed.age.years, bs="cc", k=knots), knots=list( seq(min(output$transformed.age.years), max(output$transformed.age.years), length.out =10) ), data=output[output$gender==0,] )

output[output$gender==1,]$pred <- predict(fitM, output[output$gender==1,])
output[output$gender==0,]$pred <- predict(fitF, output[output$gender==0,])      

ggplot(output, aes(x=transformed.age.years, y=CRE, colour=gender)) + 
    geom_point(alpha=0.3) + 
    scale_y_log10() + 
    geom_line(data=output, aes(x=transformed.age.years, y=pred))