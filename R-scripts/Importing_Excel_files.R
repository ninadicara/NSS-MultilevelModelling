setwd("/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/All_Files/")

#read in column names
column.names<- read.csv("Columns.csv", sep=",", header = TRUE)

#set up data frame with correct columns which all the data will be pasted to:
dataset <- data.frame(column.names)

#create a list of all the files in the directory
file_list <- list.files()

#start to read in the data for the years 2006 to 2013 
#Startrow=3, StartCol=1. Last row with data = 34
require(XLConnect)

#read the first set of files up to where the format changes to sheet 2
for (i in 1:319) { 
  temp_dataset <- readWorksheetFromFile(file_list[i], sheet=1, startRow = 3, startCol = 1,endCol=34, header = FALSE)
  colnames(temp_dataset) <- colnames(dataset)
  dataset<-rbind(dataset, temp_dataset)
  rm(temp_dataset)
}
#avoiding the columns.csv file
for (i in 321:962) { 
  temp_dataset <- readWorksheetFromFile(file_list[i], sheet=1, startRow = 3, startCol = 1,endCol=34, header = FALSE)
  colnames(temp_dataset) <- colnames(dataset)
  dataset<-rbind(dataset, temp_dataset)
  rm(temp_dataset)
}
#changed to sheet 2 for 2014/15
for (i in 963:1176) { 
  temp_dataset <- readWorksheetFromFile(file_list[i], sheet=2, startRow = 3, startCol = 1, endCol=34, header = FALSE)
  colnames(temp_dataset) <- colnames(dataset)
  dataset<-rbind(dataset, temp_dataset)
  rm(temp_dataset)
}

#making sure all numeric columns are saved as numbers, not chars
dataset$X1..Staff.are.good.at.explaining.things. <- as.numeric(dataset$X1..Staff.are.good.at.explaining.things.)
dataset$X2..Staff.have.made.the.subject.interesting. <- as.numeric(dataset$X2..Staff.have.made.the.subject.interesting.)
dataset$X3..Staff.are.enthusiastic.about.what.they.are.teaching. <- as.numeric(dataset$X3..Staff.are.enthusiastic.about.what.they.are.teaching.)
dataset$X4..The.course.is.intellectually.stimulating. <- as.numeric(dataset$X4..The.course.is.intellectually.stimulating.)
dataset$X5..The.criteria.used.in.marking.have.been.clear.in.advance. <- as.numeric(dataset$X5..The.criteria.used.in.marking.have.been.clear.in.advance.)
dataset$X6..Assessment.arrangements.and.marking.have.been.fair. <- as.numeric(dataset$X6..Assessment.arrangements.and.marking.have.been.fair.)
dataset$X7..Feedback.on.my.work.has.been.prompt. <- as.numeric(dataset$X7..Feedback.on.my.work.has.been.prompt.)
dataset$X8..I.have.received.detailed.comments.on.my.work. <- as.numeric(dataset$X8..I.have.received.detailed.comments.on.my.work.)
dataset$X9..Feedback.on.my.work.has.helped.me.clarify.things.I.did.not.understand. <- as.numeric(dataset$X9..Feedback.on.my.work.has.helped.me.clarify.things.I.did.not.understand.)
dataset$X10..I.have.received.sufficient.advice.and.support.with.my.studies. <- as.numeric(dataset$X10..I.have.received.sufficient.advice.and.support.with.my.studies.)
dataset$X11..I.have.been.able.to.contact.staff.when.I.needed.to. <- as.numeric(dataset$X11..I.have.been.able.to.contact.staff.when.I.needed.to.)
dataset$X12..Good.advice.was.available.when.I.needed.to.make.study.choices. <- as.numeric(dataset$X12..Good.advice.was.available.when.I.needed.to.make.study.choices.)
dataset$X13..The.timetable.works.efficiently.as.far.as.my.activities.are.concerned. <- as.numeric(dataset$X13..The.timetable.works.efficiently.as.far.as.my.activities.are.concerned.)
dataset$X14..Any.changes.in.the.course.or.teaching.have.been.communicated.effectively. <- as.numeric(dataset$X14..Any.changes.in.the.course.or.teaching.have.been.communicated.effectively.)
dataset$X15..The.course.is.well.organised.and.is.running.smoothly. <- as.numeric(dataset$X15..The.course.is.well.organised.and.is.running.smoothly.)
dataset$X16..The.library.resources.and.services.are.good.enough.for.my.needs. <- as.numeric(dataset$X16..The.library.resources.and.services.are.good.enough.for.my.needs.)
dataset$X17..I.have.been.able.to.access.general.IT.resources.when.I.needed.to. <- as.numeric(dataset$X17..I.have.been.able.to.access.general.IT.resources.when.I.needed.to.)
dataset$X18..I.have.been.able.to.access.specialised.equipment..facilities.or.rooms.when.I.needed.to. <- as.numeric(dataset$X18..I.have.been.able.to.access.specialised.equipment..facilities.or.rooms.when.I.needed.to.)
dataset$X19..The.course.has.helped.me.to.present.myself.with.confidence. <- as.numeric(dataset$X19..The.course.has.helped.me.to.present.myself.with.confidence.)
dataset$X20..My.communication.skills.have.improved. <- as.numeric(dataset$X20..My.communication.skills.have.improved.)
dataset$X21..As.a.result.of.the.course..I.feel.confident.in.tackling.unfamiliar.problems. <- as.numeric(dataset$X21..As.a.result.of.the.course..I.feel.confident.in.tackling.unfamiliar.problems.)
dataset$Overall.Satisfaction <- as.numeric(dataset$Overall.Satisfaction)

#Renaming the columns because output is too much otherwise!
library(plyr)
dataset <- rename(dataset, c("X1..Staff.are.good.at.explaining.things." = "Q1" , 
                                 "X2..Staff.have.made.the.subject.interesting." = "Q2" , 
                                 "X3..Staff.are.enthusiastic.about.what.they.are.teaching." = "Q3" , 
                                 "X4..The.course.is.intellectually.stimulating." = "Q4" ,
                                 "X5..The.criteria.used.in.marking.have.been.clear.in.advance." = "Q5" , 
                                 "X6..Assessment.arrangements.and.marking.have.been.fair." = "Q6" , 
                                 "X7..Feedback.on.my.work.has.been.prompt." = "Q7" , 
                                 "X8..I.have.received.detailed.comments.on.my.work." = "Q8" , 
                                 "X9..Feedback.on.my.work.has.helped.me.clarify.things.I.did.not.understand." = "Q9" , 
                                 "X10..I.have.received.sufficient.advice.and.support.with.my.studies." = "Q10" , 
                                 "X11..I.have.been.able.to.contact.staff.when.I.needed.to." = "Q11" , 
                                 "X12..Good.advice.was.available.when.I.needed.to.make.study.choices." = "Q12" ,
                                 "X13..The.timetable.works.efficiently.as.far.as.my.activities.are.concerned." = "Q13" ,
                                 "X14..Any.changes.in.the.course.or.teaching.have.been.communicated.effectively." = "Q14" ,
                                 "X15..The.course.is.well.organised.and.is.running.smoothly." = "Q15" ,
                                 "X16..The.library.resources.and.services.are.good.enough.for.my.needs." = "Q16" ,
                                 "X17..I.have.been.able.to.access.general.IT.resources.when.I.needed.to." = "Q17" ,
                                 "X18..I.have.been.able.to.access.specialised.equipment..facilities.or.rooms.when.I.needed.to." = "Q18" ,
                                 "X19..The.course.has.helped.me.to.present.myself.with.confidence." = "Q19" ,
                                 "X20..My.communication.skills.have.improved." = "Q20" ,
                                 "X21..As.a.result.of.the.course..I.feel.confident.in.tackling.unfamiliar.problems." = "Q21" ,
                                 "Overall.Satisfaction" = "OS"))

save(dataset,file="fullnssdata.Rdata")
