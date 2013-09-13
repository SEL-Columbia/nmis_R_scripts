
#* If you would like to merge the data.frame with 774lga population data and get the ratio, run the population function
#* Please be advided that you need to specify the location of population_774.csv like in (L1)
#* Example:
#* (L1): dir="C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/raw_data/Population DATA/"
#* (L2): edu2 <- population(df=edu,target_col="total_public", lga_col="lga_id", work_dir=dir)

population <- function(df, lga_col, target_col, work_dir)
{
    pop <- read.csv(paste(work_dir, "population_774.csv", sep=""))
    df <- merge(df, pop, by.x=lga_col, by.y="lga_id")
    df[,"ratio"] <- df[,target_col]/df[,"Population"]                
    return(df)
}




#* If you missed out the optional output_dir argument, then the chart will be print out to R console
#* If you missed out the optional ratio_col argument, then Only the Pareto chart will be printed
#* The default of percentile is set to 95%
#* Example: 
#* (L1): outdir="C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/raw_data/plot.pdf"
#* (L2): pareto(df=edu2, target_col="total_public", perctl=0.99, ratio_col="ratio", output_dir=outdir)  
#* (L3): pareto(df=edu2, target_col="total_public", perctl=0.99, output_dir=outdir)   
#* (L4): pareto(df=edu2, target_col="total_public", perctl=0.99, ratio_col="ratio")  
#* (L5): pareto(df=edu2, target_col="total_public", perctl=0.99)  

#* run (L1) first,
#* If you woulld like to have ratio and print pdf file go to (L2)
#* If you woulld like no ratio plot but print pdf file go to (L3)
#* If you woulld like to have ratio and print to R Console go to (L4)
#* If you woulld like no ratio plot and print to R Console go to (L5)

#* Please be advised you can fill any variable to ratio argument to have second plot at the bottom of the canvas


pareto <- function(df, target_col, perctl=0.95, ratio_col, output_dir)
{
     
    #Some error Handling here
    if (missing(df)){
        stop("Error, Please assign the input data.frame")
    }
    if (missing(target_col)){
        stop("Please assign the target varaible to be ranked to target_col")
    }
    
    
    #Order the records by the target_col
    df <- df[order(df[,target_col]),]
    df[,"cum_sum"] <- NA
    df[,"cum_sum"] <- cumsum(df[,target_col])    
    
    
#Plot function for generating 2 plots        
    plot2 <- function(df, target_col, perctl, ratio_col)
    {
        #Setting plot margin to (5,4,4,5) and make 2 charts in one graph with 2 rows and 1 col 
        par(mar=c(5,4,4,5)+.1, mfrow=c(2,1))
        #Barplot of Target_col
        barplot(df[,target_col],
                xlab=paste("LGAs are ranked by", target_col, sep=" "),
                axes=T, col = "black", density=20)
        #Setting Y-axis of barplot to the left 
        axis(2)
        #Adding the percentile in to the left axis
        abline(h = quantile(df[,target_col], probs=perctl,na.rm=T), col="blue",lwd=1.2 )
        #Not starting a new graph for the next plot, but overlay on top of the horizontal line & bar plot 
        par(new=T)
        #line plot of cumulative sum in red, and no axes or label printed on the original chart
        plot(x=1:dim(df)[1], y=df[,"cum_sum"], type="l", col="red", xaxt="n", yaxt="n", lty=1, lwd=2, xlab="", ylab="")
        #Setting the Y-axis of previous plot to the right
        axis(side=4, yaxs="s")
        #Adding labels to both left & right sides
        mtext(paste("Cumulative Sum of", target_col, sep=" "), side=4, line=2)
        mtext(paste("Actu ofal number", target_col, sep=" "), side=2, line=2)
        #Setting the margin of legend to 0
        par(mar=c(0, 0, 0, 0))
        ##########plot.new()
        #Setting the legend text color and size
        legend("topleft",col=c("black","red","blue"),lty=1,lwd=3,cex=0.6,
               legend=c(paste("Actual number of", target_col, sep=" "),
                        paste("Cumulative Sum of", target_col, sep=" "),
                        paste(paste(perctl*100, "%", sep=""),"Percentile of", target_col, sep=" ")))
        #Staring a new chart to the bottom(but not starting a new "Canvas"), with marging (5,4,4,5) 
        par(mar=c(5,4,4,5)+.1)
        par(new=F)
        #Scatter plot in grey of the ratio_col
        plot(log(df[,ratio_col], 10), type="p", col="grey")
        #Adding the percentile line in green
        abline(h = quantile(log(df[,ratio_col],10), probs=perctl,na.rm=T), col="green" )
        #Margin of the legend == 0
        par(mar=c(0, 0, 0, 0))
        #Setting the legend
        legend("topleft",col=c("grey","green"),lty=1,lwd=3,cex=0.6,
               legend=c(paste("Log 10 of ratio of", target_col, "vs population", sep=" "),
                        paste(perctl*100, "% Percentile line of Log raito", sep="")))    
    }

#Plot function for printing only the first pareto chart
    plot1 <- function(df, target_col, perctl)
    {
        par(mar=c(5,4,4,5)+.1)
        barplot(df[,target_col],
                xlab=paste("LGAs are ranked by", target_col, sep=" "),
                axes=T, col = "black", density=20
        )
        axis(2)
        abline(h = quantile(df[,target_col], probs=perctl,na.rm=T), col="blue",lwd=1.2 )
        par(new=T)
        plot(x=1:dim(df)[1], y=df[,"cum_sum"], type="l", col="red", xaxt="n", yaxt="n", lty=1, lwd=2, xlab="", ylab="")
        axis(4)
        mtext(paste("Cumulative Sum of", target_col, sep=" "), side=4, line=2)
        mtext(paste("Actu ofal number", target_col, sep=" "), side=2, line=2)
        par(mar=c(0, 0, 0, 0))
        #plot.new()
        legend("topleft",col=c("black","red","blue"),lty=1,lwd=3,cex=0.6,
               legend=c(paste("Actual number of", target_col, sep=" "),
                        paste("Cumulative Sum of", target_col, sep=" "),
                        paste(paste(perctl*100, "%", sep=""),"Percentile of", target_col, sep=" ")))
     
    }
    
    
    if (!missing(output_dir))
    {
        pdf(output_dir)
        if (!missing(ratio_col))
        {
            plot2(df=df,target_col=target_col,ratio_col=ratio_col,perctl=perctl)
            
        }else{
            plot1(df=df,target_col=target_col,perctl=perctl)
        }
        dev.off()
        
    }else{
        if (!missing(ratio_col))
        {
            plot2(df=df,target_col=target_col,ratio_col=ratio_col,perctl=perctl)
            
        }else{
            plot1(df=df,target_col=target_col,perctl=perctl)
        }    
    }
    
    return(df)
}    


    
