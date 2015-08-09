# analyze the Monthly Expense dataset

# Include libraries
require("reshape2")   # for transposing data with 'cast' and 'melt' functions
require("calibrate") # to add data labels on graphs
require("plyr")     # to do ply functions
require("forecast")  # to perform moving avarages analysis
require("zoo")  # to perform moving avarages analysis
require("psych")  # to perform exploratory factor analysis
require("graphics")
require("segmented")
require("openxlsx")
require("stats")

# enable viewing of warning messages
options(warn=1)

# Clear all objects from R memory
rm(list=ls())

#read the csv file
setwd("C:/Abhijay/Analytics use cases/Monthly expense analysis/Monthly expense analysis/Input files")
Monthly_Expense <- read.xlsx("Monthly Expense Tracking - ORIGINAL.xlsx",sheet=3,startRow=2,colNames=TRUE)
Monthly_Expense <- Monthly_Expense [!is.na(Monthly_Expense$Date),1:9]
Summation_Categories <- read.csv("Summation Categories.csv",header=FALSE)

#convert month name to month number
Monthly_Expense$Month<-match(Monthly_Expense$Month,month.name)
Monthly_Expense$Full_Date<-as.Date(paste(Monthly_Expense$Year,Monthly_Expense$Month,Monthly_Expense$Date,sep = "." ),format="%Y.%m.%d")
Itemized_Monthly_Expense <- Monthly_Expense[,c('Full_Date','Item','Category','Amount')]
#Daily_Monthly_Expense<-ddply(Monthly_Expense,c("Full_Date"),summarize,Amount=sum(Amount))


#transpose Monthly_Expense
TMonthly_Expense_1<-Monthly_Expense[,-7]
TMonthly_Expense_2<-dcast(Monthly_Expense,Date+Month+Year+Day+Week+Salary+Full_Date~Category,sum,value.var='Amount')
TMonthly_Expense<-TMonthly_Expense_2
rm(TMonthly_Expense_1)
rm(TMonthly_Expense_2)
TMonthly_Expense<-TMonthly_Expense[,-c(1,4:5,7)]
TMonthly_Expense[is.na(TMonthly_Expense)] <- 0
TMonthly_Expense<-ddply(TMonthly_Expense,.(Year,Month),colwise(sum))
rownames(TMonthly_Expense) <-NULL

#merging Year and Month to one column
TMonthly_Expense$Year_Month<-paste(TMonthly_Expense$Year,TMonthly_Expense$Month,sep="-")
col_idx<-grep("Year_Month",names(TMonthly_Expense))
TMonthly_Expense<-TMonthly_Expense[, c(col_idx, (1:ncol(TMonthly_Expense))[-col_idx])]
TMonthly_Expense<-TMonthly_Expense[,-c(2,3)]

#remove spaces from columns
col_idx<-grep("Rent",names(TMonthly_Expense))
names(TMonthly_Expense)[col_idx]<-"Rent_Maintenance"
col_idx<-grep("beauty",names(TMonthly_Expense))
names(TMonthly_Expense)[col_idx]<-"A_Beauty_Expenses"
names(TMonthly_Expense)<-sub(" ","",names(TMonthly_Expense))
names(TMonthly_Expense)<-sub(" ","",names(TMonthly_Expense))
names(TMonthly_Expense)<-sub(" ","",names(TMonthly_Expense))
names(TMonthly_Expense)<-sub("'","",names(TMonthly_Expense))
rm(col_idx)


###### MEAN, SD and COEFFT OF VARIATION ##########
TMonthly_Expense_NA<-TMonthly_Expense
TMonthly_Expense_NA[TMonthly_Expense_NA==0]<-NA

##based on full datset
SD<-round(sapply(TMonthly_Expense_NA[,(2:ncol(TMonthly_Expense_NA))],sd,na.rm=TRUE),2)
MEAN<-colMeans(TMonthly_Expense_NA[,(2:ncol(TMonthly_Expense_NA))],na.rm=TRUE)
CV<-data.frame(Coefft_Of_Variation=round((SD/MEAN)*100,2))
CV<-cbind(Category=rownames(CV),CV);rownames(CV)<-NULL
CV<-arrange(CV,Coefft_Of_Variation)
pl1<-ggplot(CV,aes(x=reorder(Category,Coefft_Of_Variation),y=Coefft_Of_Variation))+geom_bar(stat="identity",fill="blue")+ theme(axis.text.x = element_text(angle = 60, hjust = 1))+geom_hline(yintercept=c(50,100,150),linetype="longdash")

##based on last 15 months
cut<-15
SD<-round(sapply(TMonthly_Expense_NA[(nrow(TMonthly_Expense_NA):(nrow(TMonthly_Expense_NA)-cut)),(2:ncol(TMonthly_Expense_NA))],sd,na.rm=TRUE),2)
MEAN<-colMeans(TMonthly_Expense_NA[(nrow(TMonthly_Expense_NA):(nrow(TMonthly_Expense_NA)-cut)),(2:ncol(TMonthly_Expense_NA))],na.rm=TRUE)
CV<-data.frame(Coefft_Of_Variation=round((SD/MEAN)*100,2))
CV<-cbind(Category=rownames(CV),CV);rownames(CV)<-NULL
CV<-arrange(CV,Coefft_Of_Variation)
pl2<-ggplot(CV,aes(x=reorder(Category,Coefft_Of_Variation),y=Coefft_Of_Variation))+geom_bar(stat="identity",fill="blue")+ theme(axis.text.x = element_text(angle = 60, hjust = 1))+geom_hline(yintercept=c(50,100,150),linetype="longdash")

rm(TMonthly_Expense_NA)

###### SIMPLE GRAPHS WITH GGPLOT ############
#Food
ggplot(TMonthly_Expense,aes(x=factor(Year_Month,levels=Year_Month),y=Food,group=1))+geom_bar(stat="identity",fill="blue")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

fit_l<-glm(Food~as.numeric(rownames((TMonthly_Expense[as.numeric(rownames(TMonthly_Expense))<37,]))),data=TMonthly_Expense[as.numeric(rownames(TMonthly_Expense))<37,])
fit_l1<-glm(Food~as.numeric(rownames((TMonthly_Expense[(as.numeric(rownames(TMonthly_Expense))>=37&as.numeric(rownames(TMonthly_Expense))<=48),]))),data=TMonthly_Expense[(as.numeric(rownames(TMonthly_Expense))>=37&as.numeric(rownames(TMonthly_Expense))<=48),])
fit_l2<-glm(Food~as.numeric(rownames((TMonthly_Expense[(as.numeric(rownames(TMonthly_Expense))>48&as.numeric(rownames(TMonthly_Expense))<=64),]))),data=TMonthly_Expense[(as.numeric(rownames(TMonthly_Expense))>48&as.numeric(rownames(TMonthly_Expense))<=64),])
pred_l<-predict(fit_l)
pred_l1<-predict(fit_l1)
pred_l2<-predict(fit_l2)
ggplot(TMonthly_Expense[as.numeric(rownames(TMonthly_Expense))<=64,],aes(x=factor(Year_Month,levels=Year_Month),y=Food,group=1))+geom_bar(stat="identity",fill="blue")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_line(aes(y=c(pred_l,pred_l1,pred_l2),group=1),size=1)

#Car Fuel
ggplot(TMonthly_Expense,aes(x=factor(Year_Month,levels=Year_Month),y=CarFuel,group=1))+geom_bar(stat="identity",fill="blue")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Aparrel
ggplot(TMonthly_Expense,aes(x=factor(Year_Month,levels=Year_Month),y=Aparrel,group=1))+geom_bar(stat="identity",fill="blue")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Income Tax
ggplot(TMonthly_Expense,aes(x=factor(Year_Month,levels=Year_Month),y=IncomeTax,group=1))+geom_bar(stat="identity",fill="blue")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Domestic Help
ggplot(TMonthly_Expense,aes(x=factor(Year_Month,levels=Year_Month),y=DomesticHelp,group=1))+geom_bar(stat="identity",fill="blue")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))




###### CREATE FINANCIAL YEAR WISE BREAK UP ################

Number_of_FYs <-floor(nrow(TMonthly_Expense) / 12)
x<-c(1,12)
i <- 1
Summary_Sheet <- data.frame(Categories=Summation_Categories[,1], stringsAsFactors=FALSE)

for (i in 1:Number_of_FYs)
{
  assign(paste("FY_",substr(TMonthly_Expense[x[1],1],1,4),sep=""),TMonthly_Expense[c(x[1]:x[2]),])
  df_name<-paste("FY_",substr(TMonthly_Expense[x[1],1],1,4),sep="")
  temp<-get(df_name)
  rownames(temp)<-NULL
  temp[13,(1:ncol(TMonthly_Expense))] <- c("Total",colSums(temp[,2:ncol(TMonthly_Expense)], na.rm=TRUE))
  assign(paste("FY_",substr(TMonthly_Expense[x[1],1],1,4),sep=""),temp)
  Summary_Sheet[df_name]<-0
  j<-1
  for (j in 1:nrow(Summation_Categories))
  {
    y<-unlist(strsplit(as.character(Summation_Categories[j,2]),","))
    k<-1
    for (k in 1:length(y))
    {
      col_idx<-which(names(temp)==y[k])
      if (length(col_idx)>0) {Summary_Sheet[j,(i+1)]<-as.numeric(temp[nrow(temp),col_idx]) + Summary_Sheet[j,(i+1)]}
      ##Summary_Sheet[j,(i+1)]<-as.numeric(temp[nrow(temp),col_idx]) + Summary_Sheet[j,(i+1)]
      k<-k+1
    }
    j<-j+1
  }
  i <- i+1
  x<-x+12
}
rm(i,j,k,x,y,temp,df_name,col_idx)
Summary_Sheet[,1]<-sapply(Summary_Sheet[,1],as.character)
Summary_Sheet[(nrow(Summary_Sheet)+1),]<-c("Savings",Summary_Sheet[1,c(2:ncol(Summary_Sheet))] - colSums(Summary_Sheet[c(2:nrow(Summary_Sheet)),c(2:ncol(Summary_Sheet))]))

#write to csv files
setwd("C:/Abhijay/Analytics use cases/Monthly expense analysis/Monthly expense analysis/Output_Files")
write.csv(TMonthly_Expense,"Transposed_Expense_Sheet.csv")
write.csv(Summary_Sheet,"Yearly_Expense_Summary.csv")

######## DRAW TRENDLINES AND PREDICT (LINEAR + EXPONENTIAL) ##########

Trendline <- function(Category) {
  expense<-TMonthly_Expense[,c(1,which(names(TMonthly_Expense)==Category))]
  expense$index<-seq(1,nrow(expense),1)
  Break<-28
  before_ramky<-expense[expense$index<=Break,]
  after_ramky<-expense[expense$index>=Break&expense$index<=nrow(TMonthly_Expense),]
  plot(expense[,2],type="l",lwd=2,xaxt="n",main=Category,ylab="Monthly Expense",xlab="Month")
  axis(1,at=1:nrow(expense),labels=expense[,1],las=2)
  fit1<-glm(before_ramky[,2]~before_ramky[,3])
  fit2<-glm(after_ramky[,2]~after_ramky[,3])
  pred1<-predict(fit1)
  pred2<-predict(fit2)
  x<-seq(Break,nrow(TMonthly_Expense),1)
  abline(v=Break)
  lines(pred1, col="red", lwd=2)
  lines(x,pred2, col="red", lwd=2)
  return()
}

Trendline(Category="Food")
Trendline(Category="Rent_Maintenance")
Trendline(Category="CarFuel")
Trendline(Category="Electricity")
Trendline(Category="Familyresponsibilities")
Trendline(Category="Aparrel")
Trendline(Category="Householditems")
Trendline(Category="Phone")
Trendline(Category="DomesticHelp")

### TEMP LAB AREA FOR MODEL SELECTION ###
### WHEN BREAK IS USED ###
expense<-TMonthly_Expense[,c(1,which(names(TMonthly_Expense)=="Food"))]
expense$t<-seq(1,nrow(expense),1)
Break<-28
before_ramky<-expense[expense$t<=Break,]
after_ramky<-expense[expense$t>Break&expense$t<=nrow(TMonthly_Expense),]
plot(expense[,2],type="l",lwd=2,xaxt="n",main=Category,ylab="Monthly Expense",xlab="Month",xlim=c(1,nrow(expense)+12),ylim=c(0,2*max(expense$Food)))
axis(1,at=1:nrow(expense),labels=expense[,1],las=2)
fit1<-glm(Food~t,data=before_ramky)
fit2<-glm(Food~t,data=after_ramky)
pred1<-predict(fit1)
pred2<-predict(fit2)
x<-seq(Break,nrow(TMonthly_Expense),1)
abline(v=Break)
lines(pred1, col="red", lwd=2)
lines(x,pred2, col="red", lwd=2)

### WHEN BREAK IS NOT USED ###
expense<-TMonthly_Expense[,c(1,which(names(TMonthly_Expense)=="Food"))]
expense$t<-seq(1,nrow(expense),1)
expense$t2<-expense$t^2
expense$sin.t<-sin(2*pi*expense$t)
expense$cos.t<-cos(2*pi*expense$t)
plot(expense[,2],type="l",lwd=2,xaxt="n",main=Category,ylab="Monthly Expense",xlab="Month",xlim=c(1,nrow(expense)+12),ylim=c(0,2*max(expense$Food)))
axis(1,at=1:nrow(expense),labels=expense[,1],las=2)

##break into test and training set
training_size<- floor(.85*nrow(expense))
train_ind<-sample(seq_len(nrow(expense)), size = training_size)
expense_train<-expense[train_ind,]
expense_test<-expense[-train_ind,]

##linear model
fit_l<-glm(Food~t,data=expense_train)
sqrt(mean((expense$Food-predict(fit_l,expense))[-train_ind]^2))

##2nd degree polynomial model
fit_ex<-glm(Food~t+t2,data=expense_train)
sqrt(mean((expense$Food-predict(fit_ex,expense))[-train_ind]^2))

##seasonal model
fit_seas<-glm(Food~t+t2+sin.t+cos.t,data=expense_train)
sqrt(mean((expense$Food-predict(fit_seas,expense))[-train_ind]^2))

## after final model selected - the fit is regenerated with all data and graphed and forecast done
## for next 12 months

##if linear
fit_l<-glm(Food~t,data=expense)

##if exponential
fit_ex<-glm(Food~t+t2,data=expense)

##generate next 12 rows in expense sheet
Current_Year<-as.numeric(substr(Sys.Date(),1,4))
Current_Month<-as.numeric(substr(Sys.Date(),6,7))+1
Year_Month<-rep("",12)
i<-1
for (i in 1:12)
{
  if (Current_Month < 12)
    {
      Year_Month[i]= paste(Current_Year,Current_Month,sep="-")
    }
  else if (Current_Month == 12)
    {
      Current_Year<-Current_Year+1
      Current_Month<-1
      Year_Month[i]= paste(Current_Year,Current_Month,sep="-")
    }
  Current_Month<-Current_Month+1
  i<-i+1
}
forecast_expense<-data.frame(Year_Month)
forecast_expense$t<-seq(nrow(expense)+1,nrow(expense)+12,1)
forecast_expense$t2<-forecast_expense$t^2
forecast_expense$sin.t<-sin(2*pi*forecast_expense$t)
forecast_expense$cos.t<-cos(2*pi*forecast_expense$t)
forecast_expense$Food<-predict(fit_ex,newdata=forecast_expense)
forecast_expense<-forecast_expense[,c(1,6,2,3,4,5)]

expense<-rbind(expense,forecast_expense)
plot(expense[,2],type="l",lwd=2,xaxt="n",main=Category,ylab="Monthly Expense",xlab="Month")
axis(1,at=1:nrow(expense),labels=expense[,1],las=2)

#graph with linear fit
pred_l<-predict(fit_l)
lines(pred_l, col="red", lwd=2)

#graph with exponential fit
pred_ex<-predict(fit_ex)
lines(pred_ex, col="blue", lwd=2)
pred_ex_f<-predict(fit_ex,newdata=forecast_expense)
x<-seq(63,74,1)
lines(x,pred_ex_f, col="blue", lwd=2,lty=2)

return()

########### TRY TIME SERIES "TS" CLASS FUNCTIONS ##############

TS_Category<-ts(TMonthly_Expense$Phone,start=c(2010,4),freq=12)
frequency(TS_Category)
time(TS_Category)
summary(TS_Category)
aggregate(TS_Category)
aggregate(TS_Category,FUN=mean) ; aggregate(TS_Category,FUN=sd)
cycle(TS_Category)
TS_CAtegory.July<-window(TS_Category,start=c(2010,7),freq=TRUE)
plot(TS_Category)
plot(aggregate(TS_Category))
boxplot(TS_Category~cycle(TS_Category))
abline(h=10000)

##decompose a time series into trend,seasonal and random portions
plot(decompose(TS_Category))

##Auto-correlation function
acf(TS_Category)

##forecasting with HoltWinters function
plot(HoltWinters(TS_Category))
TS_Category.hw<-HoltWinters(TS_Category)
TS_Category.predict<-predict(TS_Category.hw,n.ahead=2*12)
ts.plot(TS_Category,TS_Category.predict,lty=1:2)

##random walks
diff(TS_Category)
acf(diff(TS_Category))

pacf(TS_Category)

######   MOVING AVERAGE ANALYSIS  #########################
MA_Window <- 10
f=rep(1/MA_Window,MA_Window)

Moving_Average <- function(Category) {
  expense<-TMonthly_Expense[,c(1,which(names(TMonthly_Expense)==Category))]
  y_lag1 <- filter(expense[,2], f, sides=1)
  y_lag2 <- filter(expense[,2], f, sides=2)
  plot(expense[,2],type="l",lwd=0.5,xaxt="n",main=Category,ylab="Monthly Expense",xlab="Month")
  axis(1,at=1:nrow(expense),labels=expense[,1],las=2)
  lines(y_lag1, col="red",lwd=2)
  lines(y_lag2, col="blue",lwd=2)
  return()
}
  
Moving_Average("Food")
Moving_Average(Category="Rent_Maintenance")
Moving_Average(Category="CarFuel")
Moving_Average(Category="Electricity")
Moving_Average(Category="Familyresponsibilities")
Moving_Average(Category="Aparrel")
Moving_Average(Category="Householditems")
Moving_Average(Category="Phone")
Moving_Average(Category="DomesticHelp")

########## END OF CODE ###################

#plot each category for all months
for (i in 4:ncol(TMonthly_Expense)){
  plot(TMonthly_Expense[,i],xlab="time", ylab = names(TMonthly_Expense)[i], pch=19, type="l")
  windows()
  cat(i," ")
}
dev.off

#plot graphs of important categories - UNFINISHED
plot(factor(TMonthly_Expense$Year_Month,as.character(TMonthly_Expense$Year_Month)),TMonthly_Expense$Pregnancy)
boxplot(TMonthly_Expense$Food~TMonthly_Expense$Year) #Food
boxplot(TMonthly_Expense$Rent_Maintenance~TMonthly_Expense$Year) #Rent Maintenance
boxplot(TMonthly_Expense$Salary~TMonthly_Expense$Year) #Salary


###  FACTOR ANALYIS BETWEEN EXPENSE CATEGORIES


#correlation between expense categories
Correlations_between_Categories<-cor(TMonthly_Expense[,c(4:37)],method="pearson")
#Correlations_between_Categories<-round(Correlations_between_Categories*100,digits=2)
write.csv(Correlations_between_Categories,"Correlations_between_Expense_Categories.csv")

#factor analysis
Monthly_Expense.fa<-fa(Correlations_between_Categories, nfactors=12, rotate="promax",fm="ml",scores="regression")
print.psych(Monthly_Expense.fa,digits=2,sort=TRUE,cut=0.40,lower=TRUE)


######   MOVING AVERAGE ANALYSIS  #########################


#set gobal constants
Moving_Average_window <- 200

#extract the expense categories
Expense_Categories<-ddply(Monthly_Expense,.(Category),summarise,Freq=length(Amount),Total_Expense=sum(Amount))
Expense_Categories$Percent_Spend<-round((Expense_Categories$Total_Expense/sum(Expense_Categories$Total_Expense))*100,2)
Expense_Categories_1<-Expense_Categories[order(Expense_Categories$Freq,decreasing=TRUE),]
Expense_Categories_2<-Expense_Categories[order(Expense_Categories$Total_Expense,decreasing=TRUE),]
rm(Expense_Categories)

rownames(Expense_Categories_1) <-NULL
rownames(Expense_Categories_2) <-NULL

#extract all the unique dates
All_Dates<-as.data.frame(unique(Monthly_Expense$Full_Date))
colnames(All_Dates)<-"Full_Date"

######################  Generic  #########################################

Category_Index<-which((Expense_Categories_2$Category=="Rent + Maintenance"
               |Expense_Categories_2$Category=="Family responsibilities"
               |Expense_Categories_2$Category=="Food"
               |Expense_Categories_2$Category=="Household items"
               |Expense_Categories_2$Category=="Aparrel"
               |Expense_Categories_2$Category=="Car Fuel"
               |Expense_Categories_2$Category=="Baby"
               |Expense_Categories_2$Category=="Health"
               |Expense_Categories_2$Category=="Internet"
               |Expense_Categories_2$Category=="Phone"
               |Expense_Categories_2$Category=="Electricity "))

for (i in Category_Index)
{
  #extract category expenses
  temp<-Monthly_Expense[(Monthly_Expense$Category==Expense_Categories_2$Category[i]),]
  
  #order category expenses by date
  temp<-temp[order(temp$Full_Date,decreasing=FALSE),]
  
  #group by Full_Date and merge with All_Dates to create similar sized expense arrays
  Ctemp<-ddply(temp,.(Full_Date),summarise,Total_Amount=sum(Amount))
  Ctemp<-merge(All_Dates,Ctemp,by="Full_Date",all.x=TRUE)
  Ctemp[is.na(Ctemp)] <- 0
  
  #compute moving averages
  ma<-rollmean(Ctemp$Total_Amount,Moving_Average_window)
  Moving_Average_200<-as.data.frame(ma)
  colnames(Moving_Average_200)<-"Amount"
  
  index<-nrow(All_Dates)
  index<-c(Moving_Average_window:index)
  Moving_Average_200$Time<-All_Dates[index,]
  
  x<-Moving_Average_200$Time
  y<-Moving_Average_200$Amount
  windows()
  plot(x,y,xlab="time", ylab = Expense_Categories_2$Category[i], pch=19)
}
rm(temp)
rm(Ctemp)
rm(Moving_Average_200)

################# END OF CODE #################








######################  TOTAL  #########################################
Monthly_Expense<-Monthly_Expense[order(Monthly_Expense$Full_Date,decreasing=FALSE),]

CMonthly_Expense<-ddply(Monthly_Expense,.(Full_Date),summarise,Total_Amount=sum(Amount))
CMonthly_Expense<-merge(All_Dates,CMonthly_Expense,by="Full_Date",all.x=TRUE)
CMonthly_Expense[is.na(CMonthly_Expense)] <- 0

#compute moving averages
ma<-rollmean(CMonthly_Expense$Total_Amount,Moving_Average_window)
Moving_Average_200<-as.data.frame(ma)
colnames(Moving_Average_200)<-"Amount"

index<-nrow(All_Dates)
index<-c(Moving_Average_window:index)
Moving_Average_200$Time<-All_Dates[index,]

x<-Moving_Average_200$Time
y<-Moving_Average_200$Amount

windows()
plot(x,y,xlab="time", ylab = "TOTAL_Expenses", pch=19)

rm(CMonthly_Expense)

######################  Salary  #########################################

#extract Salary expenses
Salary_Expenses<-Monthly_Expense[(Monthly_Expense$Salary > 0),]

#view top 5 expenses of Salary
head(Salary_Expenses[order(Salary_Expenses$Amount,decreasing=TRUE),])

#order Salary expenses by date
Salary_Expenses<-Salary_Expenses[order(Salary_Expenses$Full_Date,decreasing=FALSE),]

#group by Full_Date and merge with All_Dates to create similar sized expense arrays
CSalary_Expenses<-ddply(Salary_Expenses,.(Full_Date),summarise,Total_Amount=sum(Salary))
CSalary_Expenses<-merge(All_Dates,CSalary_Expenses,by="Full_Date",all.x=TRUE)
CSalary_Expenses[is.na(CSalary_Expenses)] <- 0

#compute moving averages
ma<-rollmean(CSalary_Expenses$Total_Amount,Moving_Average_window)
Moving_Average_200<-as.data.frame(ma)
colnames(Moving_Average_200)<-"Amount"

index<-nrow(All_Dates)
index<-c(Moving_Average_window:index)
Moving_Average_200$Time<-All_Dates[index,]

x<-Moving_Average_200$Time
y<-Moving_Average_200$Amount
windows()
plot(x,y,xlab="time", ylab = "Salary", pch=19)
