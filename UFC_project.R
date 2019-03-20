library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

ufc_basics <- read.csv("/Users/VickyWu/Desktop/R datasets/UFC stats/ufcbasics.csv",
                       blank.lines.skip = TRUE, na.strings=TRUE)
str(ufc_basics)

#Strike Accuracy and Takedown Accuracy should be numerics instaead of factors
#Coerce factors to numerics (first coerce to character and replace "%" and "." with " ")
ufc_basics$Total.Strike.Accuracy<-gsub("%", "",as.character(ufc_basics$Total.Strike.Accuracy), fixed=TRUE)
ufc_basics$Total.Strike.Accuracy<-gsub(".", "",as.character(ufc_basics$Total.Strike.Accuracy), fixed=TRUE)
ufc_basics$Total.Strike.Accuracy<-gsub("-", "0", ufc_basics$Total.Strike.Accuracy, fixed=TRUE)
#Check the variable after coercion to character (checl the tail cz that's where - are at)
print(tail(ufc_basics$Total.Strike.Accuracy))
#Now coerce the characters to numerics and divide the number by 1000
ufc_basics$Total.Strike.Accuracy<-as.numeric(ufc_basics$Total.Strike.Accuracy)/10000

#Do the same with takedown accuracy
ufc_basics$Take.Down.Accuracy<-gsub("%", "", as.character(ufc_basics$Take.Down.Accuracy), fixed=TRUE)
ufc_basics$Take.Down.Accuracy<-gsub(".", "", ufc_basics$Take.Down.Accuracy, fixed=TRUE)
ufc_basics$Take.Down.Accuracy<-gsub("-", "0", ufc_basics$Take.Down.Accuracy, fixed=TRUE)
#Check the variable after coercion to character (checl the tail cz that's where - are at)
print(tail(ufc_basics$Take.Down.Accuracy))
#Now coerce the characters to numerics and divide the number by 1000
ufc_basics$Take.Down.Accuracy<-as.numeric(ufc_basics$Take.Down.Accuracy)/10000

#format decimals into percents (to use for later displying, but this turns them into char again)
#sprintf("%1.2f%%", 100*ufc_basics$Total.Strike.Accuracy)
#sprintf("%1.2f%%", 100*ufc_basics$Take.Down.Accuracy)


#Coerce Name, Lastname and Firstname to characters
ufc_basics$Name <- as.character(ufc_basics$Name)
ufc_basics$Last.Name <- as.character(ufc_basics$Last.Name)
ufc_basics$Firstst.Name<-as.character(ufc_basics$Firstst.Name)
str(ufc_basics)
colnames(ufc_basics)[2] <- "Last.Name"
colnames(ufc_basics)[3] <- "First.Name"
colnames(ufc_basics)[10] <- "Pass"
colnames(ufc_basics)[12] <- "Submission"

#####Type in (firstname,lastname) to get fighter stats
get_fight_stats <- function(firstname,lastname){
     if(!any(tolower(firstname)==tolower(ufc_basics$First.Name))){
          stop("Invalid First Name")
     }
     if(!any(tolower(lastname)==tolower(ufc_basics$Last.Name))){
          stop("Invalid Last Name")
     }
     fighter_stats <- ufc_basics[tolower(ufc_basics$First.Name)==tolower(firstname),]
     if(!any(tolower(lastname)==tolower(fighter_stats$Last.Name))){
          stop("The Last Name and the First Name do not match")
     }
     fighter_stats <- fighter_stats[tolower(fighter_stats$Last.Name)==tolower(lastname),]
     fighter_stats
}
get_fight_stats("greg","hardy")

##Type in fighter name to get pie chart and a text msg
get_fight_chart <-function(firstname,lastname){
     if(!any(tolower(firstname)==tolower(ufc_basics$First.Name))){
          stop("Invalid First Name")
     }
     if(!any(tolower(lastname)==tolower(ufc_basics$Last.Name))){
          stop("Invalid Last Name")
     }
     fighter_stats <- ufc_basics[tolower(ufc_basics$First.Name)==tolower(firstname),]
     if(!any(tolower(lastname)==tolower(fighter_stats$Last.Name))){
          stop("The Last Name and the First Name do not match")
     }
     fighter_stats <- fighter_stats[tolower(fighter_stats$Last.Name)==tolower(lastname),]
     slices <- c(fighter_stats$Take.Down, fighter_stats$Knock.Down, fighter_stats$Pass,
                 fighter_stats$Reversal, fighter_stats$Submission)
     lbls <- c("Takedowns:", "Knockdowns:", "Passes:", "Reversals:","Submissions:")
     cnts <- c(fighter_stats$Take.Down, fighter_stats$Knock.Down, fighter_stats$Pass,
              fighter_stats$Reversal, fighter_stats$Submission)
     lbls <- paste(lbls, cnts) # add counts to labels 
     if(sum(cnts)==0){stop("This fighter has no other score yet")}
     else{fighter_piechart <-pie(slices,labels = lbls, col=rainbow(5),
                                 main=paste(fighter_stats$Name,"Scores"))}
     print(paste(fighter_stats$Name,"has had", fighter_stats$Fights,"fights and",
                 fighter_stats$Strikes, "strikes up till now"))
     ## produce a barchart 
     return(fighter_piechart)
}
get_fight_chart("greg","hardy")

#Type in fighter name and get a bar chart of scores
get_bar_chart <-function(firstname,lastname){
     if(!any(tolower(firstname)==tolower(ufc_basics$First.Name))){
          stop("Invalid First Name")
     }
     if(!any(tolower(lastname)==tolower(ufc_basics$Last.Name))){
          stop("Invalid Last Name")
     }
     fighter_stats <- ufc_basics[tolower(ufc_basics$First.Name)==tolower(firstname),]
     if(!any(tolower(lastname)==tolower(fighter_stats$Last.Name))){
          stop("The Last Name and the First Name do not match")
     }
     fighter_stats <- fighter_stats[tolower(fighter_stats$Last.Name)==tolower(lastname),]
     scores<-c(fighter_stats$Take.Down,fighter_stats$Knock.Down,fighter_stats$Pass,
               fighter_stats$Reversal,fighter_stats$Submission)
     barchart<-barplot(scores, names.arg=c("Takedowns","Knockdowns","Passes","Reversals",
                                      "Submissions"), 
                       xlab="score points", ylab="number of scores",
                       col="black", border="red")
     text(x = barchart, y = scores, label = scores, pos = 3, cex = 0.8, col = "black")
     title(paste(fighter_stats$Name,"fight stats"), line=2)
     print(paste(fighter_stats$Name,"has had", fighter_stats$Fights,"fights and",
                 fighter_stats$Strikes, "strikes up till now"))
}
####??how to display labels on top of the long bars
get_bar_chart("zhang","weili")


##Get strike accurcy pie chart 
get_strike_accuracy <- function(firstname,lastname){
     if(!any(tolower(firstname)==tolower(ufc_basics$First.Name))){
          stop("Invalid First Name")
     }
     if(!any(tolower(lastname)==tolower(ufc_basics$Last.Name))){
          stop("Invalid Last Name")
     }
     fighter_stats <- ufc_basics[tolower(ufc_basics$First.Name)==tolower(firstname),]
     if(!any(tolower(lastname)==tolower(fighter_stats$Last.Name))){
          stop("The Last Name and the First Name do not match")
     }
     fighter_stats <- fighter_stats[tolower(fighter_stats$Last.Name)==tolower(lastname),]
     slices <- c(fighter_stats$Total.Strike.Accuracy, 1-fighter_stats$Total.Strike.Accuracy)
     lbls <- c("Accurate Strikes:", "Total Attempts")
     cnts<-c(fighter_stats$Strikes, fighter_stats$Strikes/fighter_stats$Total.Strike.Accuracy)
     lbls <- paste(lbls,cnts ) # add counts to labels 
     if(fighter_stats$Strikes==0){stop("This fighter has no strikes yet")}
     else{fighter_strikes <-pie(slices,labels = lbls, col=c("red","black"),
                                 main=paste(fighter_stats$Name,"Strike Accuracy"))}
     print(paste(fighter_stats$Name,"has scored", fighter_stats$Strikes, "strikes"))
}
get_strike_accuracy("chris","leben")

###Get Takedown Accuracy pie chart 
get_takedown_accuracy <- function(firstname,lastname){
     if(!any(tolower(firstname)==tolower(ufc_basics$First.Name))){
          stop("Invalid First Name")
     }
     if(!any(tolower(lastname)==tolower(ufc_basics$Last.Name))){
          stop("Invalid Last Name")
     }
     fighter_stats <- ufc_basics[tolower(ufc_basics$First.Name)==tolower(firstname),]
     if(!any(tolower(lastname)==tolower(fighter_stats$Last.Name))){
          stop("The Last Name and the First Name do not match")
     }
     fighter_stats <- fighter_stats[tolower(fighter_stats$Last.Name)==tolower(lastname),]
     slices <- c(fighter_stats$Take.Down.Accuracy, 1-fighter_stats$Take.Down.Accuracy)
     lbls <- c("Takedowns:", "Total Attempts:")
     cnts<-c(fighter_stats$Take.Down, fighter_stats$Take.Down/fighter_stats$Take.Down.Accuracy)
     lbls <- paste(lbls,cnts ) # add counts to labels 
     if(fighter_stats$Take.Down==0){stop("This fighter has no takedowns yet")}
     else{fighter_takedowns <-pie(slices,labels = lbls, col=c("red","black"),
                                main=paste(fighter_stats$Name,"Takedown Accuracy"))}
     print(paste(fighter_stats$Name,"has socred", fighter_stats$Take.Down, "takedowns"))
}
get_takedown_accuracy("conor","mcgregor")


### Rank fighters by all varibales and request user input for how many fighters they wanna see for what varibale

number_of_fights<-function(variable,number){
     if(!any(tolower(variable)==tolower(colnames(ufc_basics)))){
          stop("Invalid stats request")}
     if(as.integer(number)== "NA"){
          stop("Second input has to be an integer")}
     if(as.integer(number)>1115){
          stop("Second input has to be an integer less or equal to than 1115")}
     ### lock down chosen variable 
      wanted<-ufc_basics[,c(variable,"Name")]
      #ordered<-wanted[with(wanted, sort(variable)),]
      ordered<-wanted[order(wanted[,variable],decreasing=TRUE),]
      ordered<-ordered[1:number,]
      return(ordered)
}

number_of_fights("Knock.Down",20)
ufc_basics[,2:3]
