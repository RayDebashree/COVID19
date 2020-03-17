##------------------------------------------------ Developer Info ------------------------------------------------
## Code developed by: Debashree Ray, PhD, MStat
## Contact info		: dray@jhu.edu
##----------------------------------------------------------------------------------------------------------------
## This code makes barplots of COVID-19 Confirmed Cases/Deaths to compare growth rate across countries
## Data Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
##----------------------------------------------------------------------------------------------------------------
## Input: 
##	start.date		: starting date of format mm-dd-yyyy (data available from 01-22-2020 but not from all countries)
## 	end.date			: end date of format mm-dd-yyyy
##	plot.variable	: the variable to plot (options: 'Confirmed'/'Deaths'/'Recovered'. Default 'Confirmed') 
##	countries		: a vector of country names (Default: c('India','US'))
## Output:
##	A barplot
## Example use:
##	covid19plot(start.date="02-01-2020", end.date=NULL, plot.variable="Confirmed", countries=c('India','US','Italy))
##----------------------------------------------------------------------------------------------------------------

##--------------------------- Begin: Required functions ---------------------------
# Function to define possible months and days
.posdates <- function(start.date, end.date){
	if(is.null(end.date)){
		end.date <- format(Sys.Date()-1, "%m-%d-%Y")
		message(paste("Input 'end.date' not provided. Using end.date =",end.date))
	}
	months <- as.numeric(unlist(strsplit(start.date,"-"))[1]) : as.numeric(unlist(strsplit(end.date,"-"))[1])
	months <- as.character(months)
	months[which(nchar(months)==1)] <- paste("0",months[which(nchar(months)==1)],sep="")
	days <- as.character(1:31)
	days[which(nchar(days)==1)] <- paste("0",days[which(nchar(days)==1)],sep="")
	
	dates <- NULL
	for(mm in months){
		if(mm %in% c("01","03","05","07","08","10","12")) days1 <- days
		if(mm %in% c("04","06","09","11")) days1 <- days[1:30]
		if(mm=="02") days1 <- days[1:29]
		if(mm==months[1]) days1 <- days1[as.numeric(unlist(strsplit(start.date,"-"))[2]):length(days1)]
		if(mm==months[length(months)]) days1 <- days[1:as.numeric(unlist(strsplit(end.date,"-"))[2])]
		if(mm=="01") days1 <- days1[22:length(days1)]
		
		for(dd in days1){
			dates <- c(dates, paste0(mm,"-",dd,"-2020"))
		}
		rm(days1)
	}
	return(dates)
}
##--------------------------- End: Required functions -----------------------------

#################################################################################
##### PLOT OF CONFIRMED COVID-19 CASES/DEATHS
#################################################################################

require(readr)
require(tidyr)
require(ggplot2)

covid19plot <- function(start.date="02-01-2020", end.date=NULL, plot.variable="Confirmed", countries=c('India','US')){
	# 
	if(plot.variable!='Confirmed' & plot.variable!='Deaths' & plot.variable!='Recovered')
		stop("Input 'plot.variable' can be either 'Confirmed' or 'Deaths' or 'Recovered'." )
		
	# Define possible dates
	dates <- .posdates(start.date, end.date)
	
	# Read the JHU CSSE data on confirmed cases/deaths
	datafull <- list()
	for(i in 1:length(dates)){
		message(paste("Reading date",dates[i]))
		data <- as.data.frame(read_csv(url(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",dates[i],".csv"))))
		colnames(data) <- sapply(1:ncol(data), function(i) strsplit(colnames(data),"/")[[i]][1])
		# subset data for the chosen countries
		if(length(countries)==1){
			data1 <- data[grep(countries,data$Country),]
		}else{
			data1 <- data[grep(paste(countries,collapse="|"),data$Country),]
		}
		# aggregate data for each chosen country
		data2 <- aggregate(data1[,which(colnames(data1)==plot.variable)], by=list(Country=data1$Country), FUN=sum)
		colnames(data2)[2] <- plot.variable
		datafull[[i]] <- data2
		rm(data1, data2)
	}
	
	# Create a longformat data for plotting
	dataf <- as.data.frame(matrix(NA, nrow=length(dates), ncol=length(countries)+1))
	colnames(dataf) <- c("Dates", countries)
	dataf$Dates <- dates
	for(j in 1:length(dates)){
		dat1 <- datafull[[j]]
		for(k in 1:length(countries)){
			dat2 <- dat1[which(dat1$Country==countries[k]),2]
			if(length(dat2)>0) dataf[j,1+k] <- dat2
		}
	}
	if(plot.variable=="Confirmed") dataflong <- gather(dataf, "Country", "Confirmed", all_of(countries))
	if(plot.variable=="Deaths") dataflong <- gather(dataf, "Country", "Deaths", all_of(countries))
	if(plot.variable=="Recovered") dataflong <- gather(dataf, "Country", "Recovered", all_of(countries))
	dataflong$Country <- factor(dataflong$Country, levels=c(countries))
	dataflong$logCount <- log(dataflong[,3])
	dataflong$logCount[which(dataflong$logCount==-Inf)] <- NA
	
	# Barplot using ggplot
	ymax <- max(dataflong$logCount, na.rm=T)
	if(plot.variable=='Confirmed'){
		mytitle <- "COVID-19 Cumulative Confirmed Cases by Day"
		myylab <- "Number of confirmed cases"
		mybreaks <- seq(0,ymax,0.5)
	}else if(plot.variable=='Deaths'){
		mytitle <- "COVID-19 Cumulative Deaths by Day"
		myylab <- "Number of deaths"
		mybreaks <- seq(0,ymax,0.5)
	}else{
		mytitle <- "COVID-19 Cumulative Recovered by Day"
		myylab <- "Number recovered"
		mybreaks <- seq(0,ymax,0.5)
	}
	dev.new()
	p <- ggplot(dataflong, aes(Dates, logCount))
	p + geom_bar(stat = "identity", aes(fill = Country), position = "dodge") +
		xlab("Dates") + ylab(myylab) +  
		labs(title=mytitle, subtitle="Data source: JHU CSSE (https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)",
  		caption=paste0("https://github.com/RayDebashree/COVID19 (",as.POSIXlt(Sys.time(), "GMT")," GMT)")) +
		theme_bw() + 
		scale_y_continuous(breaks=mybreaks, limits=c(0,ymax), labels=round(exp(mybreaks),0)) + 
		coord_flip() 
}


