install.packages("reshape")
install.packages("varhandle")
library(reshape)
library(varhandle)

readmit_mortality <- read.csv("Readmissions and Deaths - Hospital.csv",stringsAsFactors = FALSE,na.strings = c("","Not Available"));
timely_effective_Care <- read.csv("Timely and Effective Care - Hospital.csv",stringsAsFactors = FALSE,na.strings = c("","Not Available"));
imaging_efficiency <- read.csv("Outpatient Imaging Efficiency - Hospital.csv",stringsAsFactors = FALSE,na.strings = c("","Not Available"));


nrow(readmit_mortality)#67452
nrow(timely_effective_Care)#207174
nrow(imaging_efficiency)#28908

setdiff(unique(readmit_mortality$Provider.ID),unique(timely_effective_Care$Provider.ID))#0
setdiff(unique(readmit_mortality$Provider.ID),unique(imaging_efficiency$Provider.ID))#0
#This implies that the providers in all the datasets are the same

# inorder to merge the datasets we  need to do a column transpose of the 'Measure ID' column
#and then do a vertical concatination or do a rbind and then do a transpose. SInce the number of rows in each dataset
# are different it is better to do a rbind first then a transpose, so that we can avoid NA values
#as a result of column bind.

#Columns necessary for analysis:
#"Provider.ID"  "Hospital.Name"   "Address"  "City"   "State"             
# "ZIP.Code"  "County.Name"  "Measure.ID"  "Measure.Name"      
# "Score" 

columns <- c("Provider.ID","Hospital.Name","Address","City","State","ZIP.Code","County.Name","Measure.Name","Measure.ID","Score") 
data_readmit_mortality <- readmit_mortality[,columns]
data_timely_effective <- timely_effective_Care[,columns]
data_imaging_efficiency <- imaging_efficiency[,columns]


str(data_timely_effective)

x <- data_timely_effective$Score

str(x)

#Check if the score column contains non numeric meaures also, if so we need to identify such values and do binning
#It is easy to check this here itself before transpose
y <- x[which(check.numeric(x, na.rm=FALSE)== FALSE)]

str(y)

y <- as.factor(y)

str(y)# there are 4 levels of factors for which we need to create dummy vars

#So lets remove the unnecessary string from 'data_timely_effective$Score'

data_timely_effective$Score <- gsub(' \\(.*','',data_timely_effective$Score)#remove everythnig after " ("

#check for non numeric scores in data_readmit_mortality
x <- data_readmit_mortality$Score

y <- x[which(check.numeric(x, na.rm=FALSE)== FALSE)]
length(y)#0, no need of string manipulation in the Score variable

#check for non numeric scores in data_imaging_efficiency

x <- data_imaging_efficiency$Score

y <- x[which(check.numeric(x, na.rm=FALSE)== FALSE)]
length(y)#0, no need of string manipulation in the Score variable


#rbind
D1 <- rbind(data_readmit_mortality,data_timely_effective)
D2 <- rbind(D1,data_imaging_efficiency)
nrow(D2)#303534


#Now perform a transpose of the Measure.ID column. Takes around 11 minutes on a 16GB RAM machine.
D_master_data <- cast(D2,Provider.ID+Hospital.Name+Address+City+State+ZIP.Code+County.Name~Measure.ID,value = "Score")
D_master_data <- as.data.frame(D_master_data)

length(unique(D_master_data$Provider.ID))#4818
str(D_master_data)
length(colnames(D_master_data))#71
length(unique(colnames(D_master_data)))#71 so no duplicate columns

colnames(D_master_data)#measure variables start from column 8

#removing measures which do not have data from atleast 100 hospitals


column_cleaning <- function(x) 
{
 j<-1 
 cols <- vector("numeric", 70)
 count<-0
  for(i in 8:70)
  {
     y <- x[,i]
     
     count <- sum(!is.na(y))
     if(count < 100)
     {
        cols[j] <- i
        j<- j+1
     }
  }
 return(cols)
 
}

D_master_data_process <- D_master_data
col_names <- column_cleaning(D_master_data_process)
col_names#8,23,29 columns ie AMI_7a,OP_1,OP_2 measures
ncol(D_master_data_process)
colnames(D_master_data_process)
D_master_data_process <- D_master_data_process[,-col_names]

ncol(D_master_data_process)#67 measures

'D1 <- readmit[,c(1,2,9,10,13)]
View(D1)
D2<-D1[c(1,2,20,21,50,51),]
View(D2)
D3 <- cast(D2,Provider.ID+Hospital.Name+Measure.Name~Measure.ID,value = "Score")
View(D3)'

