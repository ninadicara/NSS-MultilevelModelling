##Chapter1 - Intro

  #sector response rate by year
  sector_response_rates <- rename(sector_response_rates, c("Response.Rate...." = "Response"))
  plot(sector_response_rates, ylab="Response Rate (%)", axes = FALSE, col = "blue")
  axis(side = 1, at = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015))
  axis(side = 2, at = c(50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72))
  abline(lm(Response ~ Year, data = sector_response_rates))

  #observations by university
  uni.freq = table(df_agree$Inst_No)
  barplot(sort(uni.freq), ylab = "Institution Observation Frequency", axes = FALSE,xaxt='n')
  axis(side = 2, at = c())
  abline(v=365,col=4,lty=2)
  
  #observations by JACS3
  jacs.freq = table(df_agree$Subject)
  barplot(sort(jacs.freq), ylab = "JACS3/Discipline Observation Frequency", axes = FALSE, xaxt='n')
  axis(side = 2, at = c())

##Chapter 4 - Data 
  #Boxplot for each variable in data exploration
  boxplot(dataset2,xlab = "Question", ylab = "% Agreement", col = c("steelblue1","steelblue1","steelblue1","steelblue1", #Teaching
                                                                  "slateblue1", "slateblue1","slateblue1","slateblue1","slateblue1", #A&F
                                                                  "olivedrab2", "olivedrab2", "olivedrab2", #Academic Support
                                                                  "hotpink1", "hotpink1", "hotpink1", #Org and Mang
                                                                  "tan1", "tan1", "tan1", #Learning Resources
                                                                  "yellow1", "yellow1", "yellow1", #Personal Development
                                                                  "lavender"))
  #PIE CHART Number of JACS codes with available observations
  slices <- c(1462, 108)
  lbls <- c("Data Unavailable: 1462", "Data Available: 108")
  pie(slices, labels = lbls, main="JACS3 Codes with Available Data")
  