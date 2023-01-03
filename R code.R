#Data import
library(ggplot2)
library(dplyr)
library(readxl)
SHE<-read_excel("C:/Users/HP/Desktop/statewise higher education not merge ordered year.xlsx")
View(SHE)

#Line chart of categorised grand total of West Bengal
Only_WB<-SHE[SHE$State=="West Bengal",]
Only_WBtotal<-Only_WB%>%transmute(Male_Total=Only_WB$`Grand Total Male`,Female_Total=Only_WB$`Grand Total Female`,Year=Only_WB$Year)
Only_WB_male<-Only_WBtotal%>%transmute(Total=Only_WBtotal$Male_Total,Year=Only_WBtotal$Year,Category="Male Total")
Only_WB_female<-Only_WBtotal%>%transmute(Total=Only_WBtotal$Female_Total,Year=Only_WBtotal$Year,Category="Female Total")
WB_male_female<-rbind(Only_WB_male,Only_WB_female)
ggplot(WB_male_female,aes(x=`Year`,y=`Total`,colour=`Category`,group=`Category`))+geom_line()+ggtitle("Line diagram for total male-female enrollment growth in higher studies over West Bengal")
###Both male and female students are increased from session 2011-12 to 2015-16


#WestBengal Ph.d gender comparison

WBphd<-SHE%>%filter(State=="West Bengal")%>%select(`Ph.D. Male`|`Ph.D. Female`|`Year`|`Ph.D. Total`)
WBphd.proportion<-WBphd%>%transmute(Male.proportion=WBphd$`Ph.D. Male`/WBphd$`Ph.D. Total`,Female.proportion=WBphd$`Ph.D. Female`/WBphd$`Ph.D. Total`)
WBphd.proportion<-as.matrix(WBphd.proportion)
WBphd.proportion<-t(WBphd.proportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WBphd.proportion)<-b 
WBphd.proportion<-as.data.frame(WBphd.proportion)
barplot.default(as.matrix(WBphd.proportion),beside = T,ylim=c(0,1.2),col=c('#1E90FF','#FF1493'),main = 'West Bengal PHD Enrollment  ',ylab = 'Male -Female proportion',xlab = "Year")
legend('topright',c('Male prop','Female prop'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()###Male proportion is higher in the year 2012-13 than other years

#WestBengal undergraduate gender comparison

WBug<-SHE%>%filter(State=="West Bengal")%>%select(`Under Graduate Male`|`Under Graduate Female`|`Year`|`Under Graduate Total`)
WB.proportion<-WBug%>%transmute(Male.proportion=WBug$`Under Graduate Male`/WBug$`Under Graduate Total`,Female.proportion=WBug$`Under Graduate Female`/WBug$`Under Graduate Total`)
WB.proportion<-as.matrix(WB.proportion)
WB.proportion<-t(WB.proportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WB.proportion)<-b 
WB.proportion<-as.data.frame(WB.proportion)
barplot.default(as.matrix(WB.proportion),beside = T,ylim=c(0,1.2),col=c('#1E90FF','#FF1493'),main = 'West Bengal UnderGraduate Enrollment',ylab = 'Male -Female proportion',xlab = "Year")
legend('topright',c('Male prop','Female prop'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()                              ###Female proportion is getting higher by year

#WestBengal M.phil. Gender Comparison

WBmphil<-SHE%>%filter(State=="West Bengal")%>%select(`M.Phil. Male`|`M.Phil. Female`|`Year`|`M.Phil. Total`)
WB_Mphil.proportion<-WBmphil%>%transmute(Male.proportion=WBmphil$`M.Phil. Male`/WBmphil$`M.Phil. Total`,Female.proportion=WBmphil$`M.Phil. Female`/WBmphil$`M.Phil. Total`)
WB_Mphil.proportion<-as.matrix(WB_Mphil.proportion)
WB_Mphil.proportion<-t(WB_Mphil.proportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WB_Mphil.proportion)<-b 
WB_Mphil.proportion<-as.data.frame(WB_Mphil.proportion)
barplot.default(as.matrix(WB_Mphil.proportion),beside = T,ylim=c(0,1.2),col=c('#1E90FF','#FF1493'),main = 'West Bengal M.Phil Enrollment',ylab = 'Male -Female proportion',xlab = "Year")
legend('topright',c('Male prop','Female prop'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()                        ##Female proportion at first declining by year and again move upwards in 2015-16

#Line chart of categorised M.Phil total of West Bengal(optional)
WBmphil_male<-WBmphil%>%transmute(Total=WBmphil$`M.Phil. Male`,Year=WBmphil$Year,Category="M.Phil Male Total")
WBmphil_female<-WBmphil%>%transmute(Total=WBmphil$`M.Phil. Female`,Year=WBmphil$Year,Category="M.Phil Female Total")
WBmphil_total<-rbind(WBmphil_male,WBmphil_female)
ggplot(WBmphil_total,aes(x=`Year`,y=`Total`,colour=`Category`,group=`Category`))+geom_line()+ggtitle("line diagram of total M.Phil male and female enrollment in West Bengal")
                                                             ###Justification of M.phil bar diagram 

#WestBengal Postgraduate gender comparison

WBpg<-SHE%>%filter(State=="West Bengal")%>%select(`Post Graduate Male`|`Post Graduate Female`|`Year`|`Post Graduate Total`)
WB.pgproportion<-WBpg%>%transmute(Male.proportion=WBpg$`Post Graduate Male`/WBpg$`Post Graduate Total`,Female.proportion=WBpg$`Post Graduate Female`/WBpg$`Post Graduate Total`)
WB.pgproportion<-as.matrix(WB.pgproportion)
WB.pgproportion<-t(WB.pgproportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WB.pgproportion)<-b 
WB.pgproportion<-as.data.frame(WB.pgproportion)
barplot.default(as.matrix(WB.pgproportion),beside = T,ylim=c(0,1.2),col=c('#1E90FF','#FF1493'),main = 'West Bengal Post-Graduate Enrollment',ylab = 'Male -Female proportion',xlab = "Year")
legend('topright',c('Male prop','Female prop'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()                                       ###female proportion is always higher throughout the time period                  

#Line chart of categorised Post-Graduate total of West Bengal(optional)
WBpg_male<-WBpg%>%transmute(Total=WBpg$`Post Graduate Male`,Year=WBpg$Year,Category="PostGraduated Male Total")
WBpg_female<-WBpg%>%transmute(Total=WBpg$`Post Graduate Female`,Year=WBpg$Year,Category="PostGraduated Female Total")
WBpg_total<-rbind(WBpg_male,WBpg_female)
ggplot(WBpg_total,aes(x=`Year`,y=`Total`,colour=`Category`,group=`Category`))+geom_line()+ggtitle("line diagram of total PostGraduate male and female enrollment in West Bengal")
                                            ###justification of postgraduate bar diagram

#WestBengal Diploma gender comparison

WBdip<-SHE%>%filter(State=="West Bengal")%>%select(`Diploma Male`|`Diploma Female`|`Year`|`Diploma Total`)
WB.dipproportion<-WBdip%>%transmute(Male.proportion=WBdip$`Diploma Male`/WBdip$`Diploma Total`,Female.proportion=WBdip$`Diploma Female`/WBdip$`Diploma Total`)
WB.dipproportion<-as.matrix(WB.dipproportion)
WB.dipproportion<-t(WB.dipproportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WB.dipproportion)<-b 
WB.dipproportion<-as.data.frame(WB.dipproportion)
barplot.default(as.matrix(WB.dipproportion),beside = T,ylim=c(0,1.2),col=c('#1E90FF','#FF1493'),main = 'West Bengal Diploma Enrollment',ylab = 'Male -Female proportion',xlab = "Year")
legend('topright',c('Male prop','Female prop'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()                                            ###Female proportion declining upto 2012-12 then increased again

#WestBengal P.G.Diploma gender comparison

WBpgdip<-SHE%>%filter(State=="West Bengal")%>%select(`PG Diploma Male`|`PG Diploma Female`|`Year`|`PG Diploma Total`)
WB.pgdipproportion<-WBpgdip%>%transmute(Male.proportion=WBpgdip$`PG Diploma Male`/WBpgdip$`PG Diploma Total`,Female.proportion=WBpgdip$`PG Diploma Female`/WBpgdip$`PG Diploma Total`)
WB.pgdipproportion<-as.matrix(WB.pgdipproportion)
WB.pgdipproportion<-t(WB.pgdipproportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WB.pgdipproportion)<-b 
WB.pgdipproportion<-as.data.frame(WB.pgdipproportion)
barplot.default(as.matrix(WB.pgdipproportion),beside = T,ylim=c(0,1.2),col=c('#1E90FF','#FF1493'),main = 'West Bengal PG.Diploma Enrollment',ylab = 'Male -Female proportion',xlab = "Year")
legend('topright',c('Male prop','Female prop'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()                    #Female proportion almost inclined through the time period

#WestBengal Certificate gender comparison

WBcerti<-SHE%>%filter(State=="West Bengal")%>%select(`Certificate Male`|`Certificate Female`|`Year`|`Certificate Total`)
WB.certiproportion<-WBcerti%>%transmute(Male.proportion=WBcerti$`Certificate Male`/WBcerti$`Certificate Total`,Female.proportion=WBcerti$`Certificate Female`/WBcerti$`Certificate Total`)
WB.certiproportion<-as.matrix(WB.certiproportion)
WB.certiproportion<-t(WB.certiproportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WB.certiproportion)<-b 
WB.certiproportion<-as.data.frame(WB.certiproportion)
barplot.default(as.matrix(WB.certiproportion),beside = T,ylim=c(0,1.2),col=c('#1E90FF','#FF1493'),main = 'West Bengal Certificate Enrollment',ylab = 'Male -Female proportion',xlab = "Year")
legend('topright',c('Male prop','Female prop'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()                                  #Female proportion first decreased upto 2013-14 then increased 

#Line chart of categorised Certificate total of West Bengal(optional)
WBcerti_male<-WBcerti%>%transmute(Total=WBcerti$`Certificate Male`,Year=WBcerti$Year,Category="Certificate Male Total")
WBcerti_female<-WBcerti%>%transmute(Total=WBcerti$`Certificate Female`,Year=WBcerti$Year,Category="Certificate Female Total")
WBcerti_total<-rbind(WBcerti_male,WBcerti_female)
ggplot(WBcerti_total,aes(x=`Year`,y=`Total`,colour=`Category`,group=`Category`))+geom_line()+ggtitle("line diagram of total Certificate male and female enrollment in West Bengal")
                                       ##Justification of Certificate course bar diagram

#WestBengal Integrated gender comparison

WBinte<-SHE%>%filter(State=="West Bengal")%>%select(`Integrated Male`|`Integrated Female`|`Year`|`Integrated Total`)
WB.inteproportion<-WBinte%>%transmute(Male.proportion=WBinte$`Integrated Male`/WBinte$`Integrated Total`,Female.proportion=WBinte$`Integrated Female`/WBinte$`Integrated Total`)
WB.inteproportion<-as.matrix(WB.inteproportion)
WB.inteproportion<-t(WB.inteproportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WB.inteproportion)<-b 
WB.inteproportion<-as.data.frame(WB.inteproportion)
barplot.default(as.matrix(WB.inteproportion),beside = T,ylim=c(0,1.2),col=c('#1E90FF','#FF1493'),main = 'West Bengal Integrated Course Enrollment',ylab = 'Male -Female proportion',xlab = "Year")
legend('topright',c('Male prop','Female prop'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()


#By year  course proportion West Bengal
Last_WB<-SHE[SHE$State=="West Bengal",]
Last_WB_Total<-Last_WB[,c(5,8,11,14,17,20,23,26,29)]
LastWB_course_proportion<-Last_WB_Total%>%transmute(Ph.D_proportion=Last_WB_Total$`Ph.D. Total`/Last_WB_Total$`Grand Total (Total)`,M.Phil_proportion=Last_WB_Total$`M.Phil. Total`/Last_WB_Total$`Grand Total (Total)`,P.G_proportion=Last_WB_Total$`Post Graduate Total`/Last_WB_Total$`Grand Total (Total)`,U.G_proportion=Last_WB_Total$`Under Graduate Total`/Last_WB_Total$`Grand Total (Total)`,PG.Diploma_proportion=Last_WB_Total$`PG Diploma Total`/Last_WB_Total$`Grand Total (Total)`,Diploma_proportion=Last_WB_Total$`Diploma Total`/Last_WB_Total$`Grand Total (Total)`,certificate_proportion=Last_WB_Total$`Certificate Total`/Last_WB_Total$`Grand Total (Total)`,integrated_proportion=Last_WB_Total$`Integrated Total`/Last_WB_Total$`Grand Total (Total)`)
LastWB_course_proportion<-as.matrix(LastWB_course_proportion)
LastWB_course_proportion<-t(LastWB_course_proportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(LastWB_course_proportion)<-b
LastWB_course_proportion<-as.data.frame(LastWB_course_proportion)
barplot.default(as.matrix(LastWB_course_proportion),beside = T,col=c('red2','dodgerblue3','chocolate3','darkorchid2','aquamarine3','lavenderblush2','darkolivegreen2','goldenrod3'),ylim=c(0,1.5),xlim=c(0,50),main = 'West Bengal Higher Education Course proportion',ylab = 'Course proportion',xlab = "Year")
grid(nx=NULL,ny=NULL)
legend('topright',c('Ph.D_proportion','M.Phil_proportion','P.G_proportion','U.G_proportion','PG.Diploma_proportion','Diploma_proportion','certificate_proportion','integrated_proportion'),fill =c('red2','dodgerblue3','chocolate3','darkorchid2','aquamarine3','lavenderblush2','darkolivegreen2','goldenrod3') )
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()


#male course proportion by year West Bengal
WB<-SHE%>%filter(SHE$State=="West Bengal")
WB_male_proportion<-WB%>%transmute(Ph.D_mprop=WB$`Ph.D. Male`/WB$`Ph.D. Total`,M.Phil_mprop=WB$`M.Phil. Male`/WB$`M.Phil. Total`,P.G_mprop=WB$`Post Graduate Male`/WB$`Post Graduate Total`,U.G_mprop=WB$`Under Graduate Male`/WB$`Under Graduate Total`,PG.Diploma_mprop=WB$`PG Diploma Male`/WB$`PG Diploma Total`,Diploma_mprop=WB$`Diploma Male`/WB$`Diploma Total`,certificate_mprop=WB$`Certificate Male`/WB$`Certificate Total`,integrated_mprop=WB$`Integrated Male`/WB$`Integrated Total`)
WB_male_proportion<-as.matrix(WB_male_proportion)
WB_male_proportion<-t(WB_male_proportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WB_male_proportion)<-b
WB_male_proportion<-t(WB_male_proportion)
WB_male_proportion<-as.data.frame(WB_male_proportion)
WB_male_proportion
barplot.default(as.matrix(WB_male_proportion),beside = T,col=c('#F0FFFF','#00FFFF','#89CFF0','#0000FF','#7393B3'),ylim=c(0,1.2),xlim=c(0,50),main = 'West Bengal Higher EducationYear wise Male Proportion of different course',ylab = 'Male  proportion',xlab = "Course")
grid(nx=NULL,ny=NULL,lty = 4,lwd = 1)
legend('topright',c("2011-12","2012-13","2013-14","2014-15","2015-16"),fill =c('#F0FFFF','#00FFFF','#89CFF0','#0000FF','#7393B3') )
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()


#Female course proportion by year West Bengal
WB<-SHE%>%filter(SHE$State=="West Bengal")
WB_female_proportion<-WB%>%transmute(Ph.D_fprop=WB$`Ph.D. Female`/WB$`Ph.D. Total`,M.Phil_fprop=WB$`M.Phil. Female`/WB$`M.Phil. Total`,P.G_fprop=WB$`Post Graduate Female`/WB$`Post Graduate Total`,U.G_fprop=WB$`Under Graduate Female`/WB$`Under Graduate Total`,PG.Diploma_fprop=WB$`PG Diploma Female`/WB$`PG Diploma Total`,Diploma_fprop=WB$`Diploma Female`/WB$`Diploma Total`,certificate_fprop=WB$`Certificate Female`/WB$`Certificate Total`,integrated_fprop=WB$`Integrated Female`/WB$`Integrated Total`)
WB_female_proportion<-as.matrix(WB_female_proportion)
WB_female_proportion<-t(WB_female_proportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(WB_female_proportion)<-b
WB_female_proportion<-t(WB_female_proportion)
WB_female_proportion<-as.data.frame(WB_female_proportion)
WB_female_proportion
barplot.default(as.matrix(WB_female_proportion),beside = T,col=c('#FFC0CB','#FF69B4','#FF1493','#DB7093','#C71585'),ylim=c(0,1.2),xlim=c(0,50),main = 'West Bengal Higher Education Year wise Female Proportion of different course',ylab = 'Female proportion',xlab = "Course")
grid(nx=NULL,ny=NULL,lty = 4,lwd = 1)
legend('topright',c("2011-12","2012-13","2013-14","2014-15","2015-16"),fill =c('#FFC0CB','#FF69B4','#FF1493','#DB7093','#C71585') )
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()

######Analysis of India

#Line chart of categorised grand total of India
Last_India<-SHE[SHE$State=="All India",]
Indiatotal_male<-Last_India%>%transmute(Male_Total=Last_India$`Grand Total Male`,Female_Total=Last_India$`Grand Total Female`,Year=Last_India$Year)
India_male<-Indiatotal_male%>%transmute(Total=Indiatotal_male$Male_Total,Year=Indiatotal_male$Year,Category="Male Total")
India_female<-Indiatotal_male%>%transmute(Total=Indiatotal_male$Female_Total,Year=Indiatotal_male$Year,Category="Female Total")
India_total<-rbind(India_male,India_female)
ggplot(India_total,aes(x=`Year`,y=`Total`,colour=`Category`,group=`Category`))+geom_line()+ggtitle("Line diagram for total male-female enrollment growth in higher studies all over India")
                                 ###Both male and female students are increased from session 2011-12 to 2015-16


#By year  course proportion India
Last_India<-SHE[SHE$State=="All India",]
Last_India_Total<-Last_India[,c(5,8,11,14,17,20,23,26,29)]
LastIndia_course_proportion<-Last_India_Total%>%transmute(Ph.D_proportion=Last_India_Total$`Ph.D. Total`/Last_India_Total$`Grand Total (Total)`,M.Phil_proportion=Last_India_Total$`M.Phil. Total`/Last_India_Total$`Grand Total (Total)`,P.G_proportion=Last_India_Total$`Post Graduate Total`/Last_India_Total$`Grand Total (Total)`,U.G_proportion=Last_India_Total$`Under Graduate Total`/Last_India_Total$`Grand Total (Total)`,PG.Diploma_proportion=Last_India_Total$`PG Diploma Total`/Last_India_Total$`Grand Total (Total)`,Diploma_proportion=Last_India_Total$`Diploma Total`/Last_India_Total$`Grand Total (Total)`,certificate_proportion=Last_India_Total$`Certificate Total`/Last_India_Total$`Grand Total (Total)`,integrated_proportion=Last_India_Total$`Integrated Total`/Last_India_Total$`Grand Total (Total)`)
LastIndia_course_proportion<-as.matrix(LastIndia_course_proportion)
LastIndia_course_proportion<-t(LastIndia_course_proportion)
b<-c("2011-12","2012-13","2013-14","2014-15","2015-16")
colnames(LastIndia_course_proportion)<-b
LastIndia_course_proportion<-as.data.frame(LastIndia_course_proportion)
barplot.default(as.matrix(LastIndia_course_proportion),beside = T,col=c('red2','dodgerblue3','chocolate3','darkorchid2','aquamarine3','lavenderblush2','darkolivegreen2','goldenrod3'),ylim=c(0,1.5),xlim=c(0,50),main = 'All India Higher Education Coursewise proportion',ylab = 'course proportion',xlab = "Year")
legend('topright',c('Ph.D_proportion','M.Phil_proportion','P.G_proportion','U.G_proportion','PG.Diploma_proportion','Diploma_proportion','certificate_proportion','integrated_proportion'),fill =c('red2','dodgerblue3','chocolate3','darkorchid2','aquamarine3','lavenderblush2','darkolivegreen2','goldenrod3') )
grid(nx=NULL,ny=NULL)
box()


#Top 5 undergraduate states in 2015-16

No_India<-SHE[SHE$State!="All India",]
Last<-No_India%>%filter(No_India$Year=="2015-16")
SHEorder<-order(Last$`Grand Total (Total)`,decreasing = T)
ordered_table<-SHE[SHEorder,]
ordered_table<-head(ordered_table,5)
ordered_table
top5<-Last%>%filter(Last$State=="Uttar Pradesh"|Last$State=="Maharashtra"|Last$State=="Tamil Nadu"|Last$State=="West Bengal"|Last$State=="Karnataka")
top5.proportion<-top5%>%transmute(Male.proportion=top5$`Under Graduate Male`/top5$`Under Graduate Total`,Female.proportion=top5$`Under Graduate Female`/top5$`Under Graduate Total`)
top5.proportion<-as.matrix(top5.proportion)
top5.15proportion<-t(top5.proportion)
d<-c("Uttar Pradesh","Maharashtra","Tamil Nadu","West Bengal","Karnataka")
colnames(top5.15proportion)<-d
barplot.default(as.matrix(top5.15proportion),beside = T,ylim=c(0,1.0),col=c('#1E90FF','#FF1493'),main = '2015-16 UnderGraduate Enrollment of 5 states',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',c('Male Prop','Female Prop'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL)
box()                 ###tamilnadu has the higher female proportion in undergraduate course


#piechart

library(ggplot2)
No_India<-SHE[SHE$State!="All India",]
Last<-No_India%>%filter(No_India$Year=="2015-16")
SHEorder<-order(Last$`Grand Total (Total)`,decreasing = T)
ordered_table<-Last[SHEorder,]
ordered_table<-head(ordered_table,5)
ordered_table
top5<-Last%>%filter(Last$State=="Uttar Pradesh"|Last$State=="Maharashtra"|Last$State=="Tamil Nadu"|Last$State=="West Bengal"|Last$State=="Karnataka")
top5.proportion<-top5%>%transmute(Male.proportion=top5$`Under Graduate Male`/top5$`Under Graduate Total`,Female.proportion=top5$`Under Graduate Female`/top5$`Under Graduate Total`,top5$State)
ggplot(top5.proportion,aes(x="",y=top5.proportion$Female.proportion,fill=`top5$State`))+geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  geom_text(aes(label = paste0(round(top5.proportion$Female.proportion/sum(top5.proportion$Female.proportion)*100,2), "%")), position = position_stack(vjust=0.5))+
  scale_fill_ordinal()+ theme_dark()                      ####piechart of top 5 state undergraduate female proportion


#Histogram
No_India<-SHE[SHE$State!="All India",]
Last<-No_India%>%filter(No_India$Year=="2015-16")
Last.proportion<-Last%>%transmute(Male.proportion=Last$`Under Graduate Male`/Last$`Under Graduate Total`,Female.proportion=Last$`Under Graduate Female`/Last$`Under Graduate Total`)
hist(Last.proportion$Female.proportion,xlim = c(.30,.80),ylim = c(0,10),freq = F,col = "#FF1493",xlab ="Female Proportion",main="Histogram of female proportion in 2015-16" )
lines(density(Last.proportion$Female.proportion),type = "l")
                                           ###Positively skewed distrbution

##EastZone total yearwise analysis
Last<-SHE%>%filter(SHE$Year=="2015-16")
East_Zone_15_16<-Last%>%filter(Last$State=="Bihar"|Last$State=="Odisha"|Last$State=="Jharkhand"|Last$State=="West Bengal")
East_Zone_15_16<-East_Zone_15_16%>%transmute(Male.proportion=East_Zone_15_16$`Grand Total Male`/East_Zone_15_16$`Grand Total (Total)`,East_Zone_15_16$`Grand Total Female`/East_Zone_15_16$`Grand Total (Total)`)
East_Zone_15_16<-as.matrix(East_Zone_15_16)
East_Zone_15_16<-t(East_Zone_15_16)
colnames(East_Zone_15_16)<-c("Bihar","Odisha","Jharkhand","West Bengal")

Last<-SHE%>%filter(SHE$Year=="2014-15")
East_Zone_14_15<-Last%>%filter(Last$State=="Bihar"|Last$State=="Odisha"|Last$State=="Jharkhand"|Last$State=="West Bengal")
East_Zone_14_15<-East_Zone_14_15%>%transmute(Male.proportion=East_Zone_14_15$`Grand Total Male`/East_Zone_14_15$`Grand Total (Total)`,East_Zone_14_15$`Grand Total Female`/East_Zone_14_15$`Grand Total (Total)`)
East_Zone_14_15<-as.matrix(East_Zone_14_15)
East_Zone_14_15<-t(East_Zone_14_15)
colnames(East_Zone_14_15)<-c("Bihar","Odisha","Jharkhand","West Bengal")

Last<-SHE%>%filter(SHE$Year=="2013-14")
East_Zone_13_14<-Last%>%filter(Last$State=="Bihar"|Last$State=="Odisha"|Last$State=="Jharkhand"|Last$State=="West Bengal")
East_Zone_13_14<-East_Zone_13_14%>%transmute(Male.proportion=East_Zone_13_14$`Grand Total Male`/East_Zone_13_14$`Grand Total (Total)`,East_Zone_13_14$`Grand Total Female`/East_Zone_13_14$`Grand Total (Total)`)
East_Zone_13_14<-as.matrix(East_Zone_13_14)
East_Zone_13_14<-t(East_Zone_13_14)
colnames(East_Zone_13_14)<-c("Bihar","Odisha","Jharkhand","West Bengal")

Last<-SHE%>%filter(SHE$Year=="2012-13")
East_Zone_12_13<-Last%>%filter(Last$State=="Bihar"|Last$State=="Odisha"|Last$State=="Jharkhand"|Last$State=="West Bengal")
East_Zone_12_13<-East_Zone_12_13%>%transmute(Male.proportion=East_Zone_12_13$`Grand Total Male`/East_Zone_12_13$`Grand Total (Total)`,East_Zone_12_13$`Grand Total Female`/East_Zone_12_13$`Grand Total (Total)`)
East_Zone_12_13<-as.matrix(East_Zone_12_13)
East_Zone_12_13<-t(East_Zone_12_13)
colnames(East_Zone_12_13)<-c("Bihar","Odisha","Jharkhand","West Bengal")

Last<-SHE%>%filter(SHE$Year=="2011-12")
East_Zone_11_12<-Last%>%filter(Last$State=="Bihar"|Last$State=="Odisha"|Last$State=="Jharkhand"|Last$State=="West Bengal")
East_Zone_11_12<-East_Zone_11_12%>%transmute(Male.proportion=East_Zone_11_12$`Grand Total Male`/East_Zone_11_12$`Grand Total (Total)`,East_Zone_11_12$`Grand Total Female`/East_Zone_11_12$`Grand Total (Total)`)
East_Zone_11_12<-as.matrix(East_Zone_11_12)
East_Zone_11_12<-t(East_Zone_11_12)
colnames(East_Zone_11_12)<-c("Bihar","Odisha","Jharkhand","West Bengal")

par(mfrow=c(3,2))

barplot.default(as.matrix(East_Zone_15_16),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2015-16 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(East_Zone_14_15),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2014-15 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(East_Zone_13_14),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2013-14 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(East_Zone_12_13),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2012-13 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(East_Zone_11_12),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2011-12 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()

par(mfrow=c(1,1))                        ###No state in east zone has higher female proportion than male proportion

##North_eastZone total yearwise analysis
Last<-SHE%>%filter(SHE$Year=="2015-16")
NE_Zone_15_16<-Last%>%filter(Last$State=="Arunachal Pradesh"|Last$State=="Assam"|Last$State=="Manipur"|Last$State=="Meghalaya"|Last$State=="Mizoram"|Last$State=="Nagaland"|Last$State=="Tripura"|Last$State=="Sikkim")
NE_Zone_15_16<-NE_Zone_15_16%>%transmute(Male.proportion=NE_Zone_15_16$`Grand Total Male`/NE_Zone_15_16$`Grand Total (Total)`,NE_Zone_15_16$`Grand Total Female`/NE_Zone_15_16$`Grand Total (Total)`)
NE_Zone_15_16<-as.matrix(NE_Zone_15_16)
NE_Zone_15_16<-t(NE_Zone_15_16)
colnames(NE_Zone_15_16)<-c("Arunachal","Assam","Manipur","Meghalaya","Mizoram","Nagaland","Tripura","Sikkim")

Last<-SHE%>%filter(SHE$Year=="2014-15")
NE_Zone_14_15<-Last%>%filter(Last$State=="Arunachal Pradesh"|Last$State=="Assam"|Last$State=="Manipur"|Last$State=="Meghalaya"|Last$State=="Mizoram"|Last$State=="Nagaland"|Last$State=="Tripura"|Last$State=="Sikkim")
NE_Zone_14_15<-NE_Zone_14_15%>%transmute(Male.proportion=NE_Zone_14_15$`Grand Total Male`/NE_Zone_14_15$`Grand Total (Total)`,NE_Zone_14_15$`Grand Total Female`/NE_Zone_14_15$`Grand Total (Total)`)
NE_Zone_14_15<-as.matrix(NE_Zone_14_15)
NE_Zone_14_15<-t(NE_Zone_14_15)
colnames(NE_Zone_14_15)<-c("Arunachal","Assam","Manipur","Meghalaya","Mizoram","Nagaland","Tripura","Sikkim")

Last<-SHE%>%filter(SHE$Year=="2013-14")
NE_Zone_13_14<-Last%>%filter(Last$State=="Arunachal Pradesh"|Last$State=="Assam"|Last$State=="Manipur"|Last$State=="Meghalaya"|Last$State=="Mizoram"|Last$State=="Nagaland"|Last$State=="Tripura"|Last$State=="Sikkim")
NE_Zone_13_14<-NE_Zone_13_14%>%transmute(Male.proportion=NE_Zone_13_14$`Grand Total Male`/NE_Zone_13_14$`Grand Total (Total)`,NE_Zone_13_14$`Grand Total Female`/NE_Zone_13_14$`Grand Total (Total)`)
NE_Zone_13_14<-as.matrix(NE_Zone_13_14)
NE_Zone_13_14<-t(NE_Zone_13_14)
colnames(NE_Zone_13_14)<-c("Arunachal","Assam","Manipur","Meghalaya","Mizoram","Nagaland","Tripura","Sikkim")

Last<-SHE%>%filter(SHE$Year=="2012-13")
NE_Zone_12_13<-Last%>%filter(Last$State=="Arunachal Pradesh"|Last$State=="Assam"|Last$State=="Manipur"|Last$State=="Meghalaya"|Last$State=="Mizoram"|Last$State=="Nagaland"|Last$State=="Tripura"|Last$State=="Sikkim")
NE_Zone_12_13<-NE_Zone_12_13%>%transmute(Male.proportion=NE_Zone_12_13$`Grand Total Male`/NE_Zone_12_13$`Grand Total (Total)`,NE_Zone_12_13$`Grand Total Female`/NE_Zone_12_13$`Grand Total (Total)`)
NE_Zone_12_13<-as.matrix(NE_Zone_12_13)
NE_Zone_12_13<-t(NE_Zone_12_13)
colnames(NE_Zone_12_13)<-c("Arunachal","Assam","Manipur","Meghalaya","Mizoram","Nagaland","Tripura","Sikkim")

Last<-SHE%>%filter(SHE$Year=="2011-12")
NE_Zone_11_12<-Last%>%filter(Last$State=="Arunachal Pradesh"|Last$State=="Assam"|Last$State=="Manipur"|Last$State=="Meghalaya"|Last$State=="Mizoram"|Last$State=="Nagaland"|Last$State=="Tripura"|Last$State=="Sikkim")
NE_Zone_11_12<-NE_Zone_11_12%>%transmute(Male.proportion=NE_Zone_11_12$`Grand Total Male`/NE_Zone_11_12$`Grand Total (Total)`,NE_Zone_11_12$`Grand Total Female`/NE_Zone_11_12$`Grand Total (Total)`)
NE_Zone_11_12<-as.matrix(NE_Zone_11_12)
NE_Zone_11_12<-t(NE_Zone_11_12)
colnames(NE_Zone_11_12)<-c("Arunachal","Assam","Manipur","Meghalaya","Mizoram","Nagaland","Tripura","Sikkim")

par(mfrow=c(3,2))

barplot.default(as.matrix(NE_Zone_15_16),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2015-16 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(NE_Zone_14_15),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2014-15 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(NE_Zone_13_14),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2013-14 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(NE_Zone_12_13),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2012-13 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(NE_Zone_11_12),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2011-12 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()

par(mfrow=c(1,1))           ###Meghalaya has female proportion than male proportion alomost throughout the period and in Nagaland Female proportion increased throughout the time period uniformly   

##Centralzone total yearwise analysis
Last<-SHE%>%filter(SHE$Year=="2015-16")
Central_Zone_15_16<-Last%>%filter(Last$State=="Madhya Pradesh"|Last$State=="Chhatisgarh"|Last$State=="Uttar Pradesh")
Central_Zone_15_16<-Central_Zone_15_16%>%transmute(Male.proportion=Central_Zone_15_16$`Grand Total Male`/Central_Zone_15_16$`Grand Total (Total)`,Central_Zone_15_16$`Grand Total Female`/Central_Zone_15_16$`Grand Total (Total)`)
Central_Zone_15_16<-as.matrix(Central_Zone_15_16)
Central_Zone_15_16<-t(Central_Zone_15_16)
colnames(Central_Zone_15_16)<-c("Madhya Pradesh","Chhatisgarh","Uttar Pradesh")

Last<-SHE%>%filter(SHE$Year=="2014-15")
Central_Zone_14_15<-Last%>%filter(Last$State=="Madhya Pradesh"|Last$State=="Chhatisgarh"|Last$State=="Uttar Pradesh")
Central_Zone_14_15<-Central_Zone_14_15%>%transmute(Male.proportion=Central_Zone_14_15$`Grand Total Male`/Central_Zone_14_15$`Grand Total (Total)`,Central_Zone_14_15$`Grand Total Female`/Central_Zone_14_15$`Grand Total (Total)`)
Central_Zone_14_15<-as.matrix(Central_Zone_14_15)
Central_Zone_14_15<-t(Central_Zone_14_15)
colnames(Central_Zone_14_15)<-c("Madhya Pradesh","Chhatisgarh","Uttar Pradesh")

Last<-SHE%>%filter(SHE$Year=="2013-14")
Central_Zone_13_14<-Last%>%filter(Last$State=="Madhya Pradesh"|Last$State=="Chhatisgarh"|Last$State=="Uttar Pradesh")
Central_Zone_13_14<-Central_Zone_13_14%>%transmute(Male.proportion=Central_Zone_13_14$`Grand Total Male`/Central_Zone_13_14$`Grand Total (Total)`,Central_Zone_13_14$`Grand Total Female`/Central_Zone_13_14$`Grand Total (Total)`)
Central_Zone_13_14<-as.matrix(Central_Zone_13_14)
Central_Zone_13_14<-t(Central_Zone_13_14)
colnames(Central_Zone_13_14)<-c("Madhya Pradesh","Chhatisgarh","Uttar Pradesh")

Last<-SHE%>%filter(SHE$Year=="2012-13")
Central_Zone_12_13<-Last%>%filter(Last$State=="Madhya Pradesh"|Last$State=="Chhatisgarh"|Last$State=="Uttar Pradesh")
Central_Zone_12_13<-Central_Zone_12_13%>%transmute(Male.proportion=Central_Zone_12_13$`Grand Total Male`/Central_Zone_12_13$`Grand Total (Total)`,Central_Zone_12_13$`Grand Total Female`/Central_Zone_12_13$`Grand Total (Total)`)
Central_Zone_12_13<-as.matrix(Central_Zone_12_13)
Central_Zone_12_13<-t(Central_Zone_12_13)
colnames(Central_Zone_12_13)<-c("Madhya Pradesh","Chhattisgarh","Uttar Pradesh")

Last<-SHE%>%filter(SHE$Year=="2011-12")
Central_Zone_11_12<-Last%>%filter(Last$State=="Madhya Pradesh"|Last$State=="Chhatisgarh"|Last$State=="Uttar Pradesh")
Central_Zone_11_12<-Central_Zone_11_12%>%transmute(Male.proportion=Central_Zone_11_12$`Grand Total Male`/Central_Zone_11_12$`Grand Total (Total)`,Central_Zone_11_12$`Grand Total Female`/Central_Zone_11_12$`Grand Total (Total)`)
Central_Zone_11_12<-as.matrix(Central_Zone_11_12)
Central_Zone_11_12<-t(Central_Zone_11_12)
colnames(Central_Zone_11_12)<-c("Madhya Pradesh","Chhatisgarh","Uttar Pradesh")

par(mfrow=c(3,2))

barplot.default(as.matrix(Central_Zone_15_16),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2015-16 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(Central_Zone_14_15),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2014-15 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(Central_Zone_13_14),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2013-14 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(Central_Zone_12_13),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2012-13 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(Central_Zone_11_12),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2011-12 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()

par(mfrow=c(1,1))            ###no state has higher female proportion than malr proportion

##Northzone total yearwise analysis
Last<-SHE%>%filter(SHE$Year=="2015-16")
North_Zone_15_16<-Last%>%filter(Last$State=="Jammu and Kashmir"|Last$State=="Punjab"|Last$State=="Haryana"|Last$State=="Himachal Pradesh"|Last$State=="Uttrakhand")
North_Zone_15_16<-North_Zone_15_16%>%transmute(Male.proportion=North_Zone_15_16$`Grand Total Male`/North_Zone_15_16$`Grand Total (Total)`,North_Zone_15_16$`Grand Total Female`/North_Zone_15_16$`Grand Total (Total)`)
North_Zone_15_16<-as.matrix(North_Zone_15_16)
North_Zone_15_16<-t(North_Zone_15_16)
colnames(North_Zone_15_16)<-c("Jammu and Kashmir","Punjab","Haryana","Himachal Pradesh","Uttrakhand")

Last<-SHE%>%filter(SHE$Year=="2014-15")
North_Zone_14_15<-Last%>%filter(Last$State=="Jammu and Kashmir"|Last$State=="Punjab"|Last$State=="Haryana"|Last$State=="Himachal Pradesh"|Last$State=="Uttrakhand")
North_Zone_14_15<-North_Zone_14_15%>%transmute(Male.proportion=North_Zone_14_15$`Grand Total Male`/North_Zone_14_15$`Grand Total (Total)`,North_Zone_14_15$`Grand Total Female`/North_Zone_14_15$`Grand Total (Total)`)
North_Zone_14_15<-as.matrix(North_Zone_14_15)
North_Zone_14_15<-t(North_Zone_14_15)
colnames(North_Zone_14_15)<-c("Jammu and Kashmir","Punjab","Haryana","Himachal Pradesh","Uttrakhand")

Last<-SHE%>%filter(SHE$Year=="2013-14")
North_Zone_13_14<-Last%>%filter(Last$State=="Jammu and Kashmir"|Last$State=="Punjab"|Last$State=="Haryana"|Last$State=="Himachal Pradesh"|Last$State=="Uttrakhand")
North_Zone_13_14<-North_Zone_13_14%>%transmute(Male.proportion=North_Zone_13_14$`Grand Total Male`/North_Zone_13_14$`Grand Total (Total)`,North_Zone_13_14$`Grand Total Female`/North_Zone_13_14$`Grand Total (Total)`)
North_Zone_13_14<-as.matrix(North_Zone_13_14)
North_Zone_13_14<-t(North_Zone_13_14)
colnames(North_Zone_13_14)<-c("Jammu and Kashmir","Punjab","Haryana","Himachal Pradesh","Uttrakhand")

Last<-SHE%>%filter(SHE$Year=="2012-13")
North_Zone_12_13<-Last%>%filter(Last$State=="Jammu and Kashmir"|Last$State=="Punjab"|Last$State=="Haryana"|Last$State=="Himachal Pradesh"|Last$State=="Uttrakhand")
North_Zone_12_13<-North_Zone_12_13%>%transmute(Male.proportion=North_Zone_12_13$`Grand Total Male`/North_Zone_12_13$`Grand Total (Total)`,North_Zone_12_13$`Grand Total Female`/North_Zone_12_13$`Grand Total (Total)`)
North_Zone_12_13<-as.matrix(North_Zone_12_13)
North_Zone_12_13<-t(North_Zone_12_13)
colnames(North_Zone_12_13)<-c("Jammu and Kashmir","Punjab","Haryana","Himachal Pradesh","Uttrakhand")

Last<-SHE%>%filter(SHE$Year=="2011-12")
North_Zone_11_12<-Last%>%filter(Last$State=="Jammu and Kashmir"|Last$State=="Punjab"|Last$State=="Haryana"|Last$State=="Himachal Pradesh"|Last$State=="Uttrakhand")
North_Zone_11_12<-North_Zone_11_12%>%transmute(Male.proportion=North_Zone_11_12$`Grand Total Male`/North_Zone_11_12$`Grand Total (Total)`,North_Zone_11_12$`Grand Total Female`/North_Zone_11_12$`Grand Total (Total)`)
North_Zone_11_12<-as.matrix(North_Zone_11_12)
North_Zone_11_12<-t(North_Zone_11_12)
colnames(North_Zone_11_12)<-c("Jammu and Kashmir","Punjab","Haryana","Himachal Pradesh","Uttrakhand")

par(mfrow=c(3,2))

barplot.default(as.matrix(North_Zone_15_16),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2015-16 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(North_Zone_14_15),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2014-15 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(North_Zone_13_14),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2013-14 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(North_Zone_12_13),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2012-13 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(North_Zone_11_12),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2011-12 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()

par(mfrow=c(1,1))               ## in Punjab and haryana female proportion sometimes increased and sometimes decreased

##Westzone total yearwise analysis
Last<-SHE%>%filter(SHE$Year=="2015-16")
West_Zone_15_16<-Last%>%filter(Last$State=="Gujarat"|Last$State=="Goa"|Last$State=="Maharashtra"|Last$State=="Rajasthan")
West_Zone_15_16<-West_Zone_15_16%>%transmute(Male.proportion=West_Zone_15_16$`Grand Total Male`/West_Zone_15_16$`Grand Total (Total)`,West_Zone_15_16$`Grand Total Female`/West_Zone_15_16$`Grand Total (Total)`)
West_Zone_15_16<-as.matrix(West_Zone_15_16)
West_Zone_15_16<-t(West_Zone_15_16)
colnames(West_Zone_15_16)<-c("Gujarat","Goa","Maharashtra","Rajasthan")

Last<-SHE%>%filter(SHE$Year=="2014-15")
West_Zone_14_15<-Last%>%filter(Last$State=="Gujarat"|Last$State=="Goa"|Last$State=="Maharashtra"|Last$State=="Rajasthan")
West_Zone_14_15<-West_Zone_14_15%>%transmute(Male.proportion=West_Zone_14_15$`Grand Total Male`/West_Zone_14_15$`Grand Total (Total)`,West_Zone_14_15$`Grand Total Female`/West_Zone_14_15$`Grand Total (Total)`)
West_Zone_14_15<-as.matrix(West_Zone_14_15)
West_Zone_14_15<-t(West_Zone_14_15)
colnames(West_Zone_14_15)<-c("Gujarat","Goa","Maharashtra","Rajasthan")

Last<-SHE%>%filter(SHE$Year=="2013-14")
West_Zone_13_14<-Last%>%filter(Last$State=="Gujarat"|Last$State=="Goa"|Last$State=="Maharashtra"|Last$State=="Rajasthan")
West_Zone_13_14<-West_Zone_13_14%>%transmute(Male.proportion=West_Zone_13_14$`Grand Total Male`/West_Zone_13_14$`Grand Total (Total)`,West_Zone_13_14$`Grand Total Female`/West_Zone_13_14$`Grand Total (Total)`)
West_Zone_13_14<-as.matrix(West_Zone_13_14)
West_Zone_13_14<-t(West_Zone_13_14)
colnames(West_Zone_13_14)<-c("Gujarat","Goa","Maharashtra","Rajasthan")

Last<-SHE%>%filter(SHE$Year=="2012-13")
West_Zone_12_13<-Last%>%filter(Last$State=="Gujarat"|Last$State=="Goa"|Last$State=="Maharashtra"|Last$State=="Rajasthan")
West_Zone_12_13<-West_Zone_12_13%>%transmute(Male.proportion=West_Zone_12_13$`Grand Total Male`/West_Zone_12_13$`Grand Total (Total)`,West_Zone_12_13$`Grand Total Female`/West_Zone_12_13$`Grand Total (Total)`)
West_Zone_12_13<-as.matrix(West_Zone_12_13)
West_Zone_12_13<-t(West_Zone_12_13)
colnames(West_Zone_12_13)<-c("Gujarat","Goa","Maharashtra","Rajasthan")

Last<-SHE%>%filter(SHE$Year=="2011-12")
West_Zone_11_12<-Last%>%filter(Last$State=="Gujarat"|Last$State=="Goa"|Last$State=="Maharashtra"|Last$State=="Rajasthan")
West_Zone_11_12<-West_Zone_11_12%>%transmute(Male.proportion=West_Zone_11_12$`Grand Total Male`/West_Zone_11_12$`Grand Total (Total)`,West_Zone_11_12$`Grand Total Female`/West_Zone_11_12$`Grand Total (Total)`)
West_Zone_11_12<-as.matrix(West_Zone_11_12)
West_Zone_11_12<-t(West_Zone_11_12)
colnames(West_Zone_11_12)<-c("Gujarat","Goa","Maharashtra","Rajasthan")

par(mfrow=c(3,2))

barplot.default(as.matrix(West_Zone_15_16),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2015-16 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(West_Zone_14_15),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2014-15 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(West_Zone_13_14),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2013-14 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(West_Zone_12_13),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2012-13 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(West_Zone_11_12),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2011-12 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()

par(mfrow=c(1,1))            ##in gujrat female proportion sometimes increased and sometimes decreased

##Southzone total yearwise analysis
Last<-SHE%>%filter(SHE$Year=="2015-16")
South_Zone_15_16<-Last%>%filter(Last$State=="Andhra Pradesh"|Last$State=="Karnataka"|Last$State=="Kerala"|Last$State=="Tamil Nadu"|Last$State=="Telangana")
South_Zone_15_16<-South_Zone_15_16%>%transmute(Male.proportion=South_Zone_15_16$`Grand Total Male`/South_Zone_15_16$`Grand Total (Total)`,South_Zone_15_16$`Grand Total Female`/South_Zone_15_16$`Grand Total (Total)`)
South_Zone_15_16<-as.matrix(South_Zone_15_16)
South_Zone_15_16<-t(South_Zone_15_16)
colnames(South_Zone_15_16)<-c("Andhra Pradesh","Karnataka","Kerala","Tamil Nadu","Telangana")

Last<-SHE%>%filter(SHE$Year=="2014-15")
South_Zone_14_15<-Last%>%filter(Last$State=="Andhra Pradesh"|Last$State=="Karnataka"|Last$State=="Kerala"|Last$State=="Tamil Nadu"|Last$State=="Telangana")
South_Zone_14_15<-South_Zone_14_15%>%transmute(Male.proportion=South_Zone_14_15$`Grand Total Male`/South_Zone_14_15$`Grand Total (Total)`,South_Zone_14_15$`Grand Total Female`/South_Zone_14_15$`Grand Total (Total)`)
South_Zone_14_15<-as.matrix(South_Zone_14_15)
South_Zone_14_15<-t(South_Zone_14_15)
colnames(South_Zone_14_15)<-c("Andhra Pradesh","Karnataka","Kerala","Tamil Nadu","Telangana")

Last<-SHE%>%filter(SHE$Year=="2013-14")
South_Zone_13_14<-Last%>%filter(Last$State=="Andhra Pradesh"|Last$State=="Karnataka"|Last$State=="Kerala"|Last$State=="Tamil Nadu"|Last$State=="Telangana")
South_Zone_13_14<-South_Zone_13_14%>%transmute(Male.proportion=South_Zone_13_14$`Grand Total Male`/South_Zone_13_14$`Grand Total (Total)`,South_Zone_13_14$`Grand Total Female`/South_Zone_13_14$`Grand Total (Total)`)
South_Zone_13_14<-as.matrix(South_Zone_13_14)
South_Zone_13_14<-t(South_Zone_13_14)
colnames(South_Zone_13_14)<-c("Andhra Pradesh","Karnataka","Kerala","Tamil Nadu","Telangana")

Last<-SHE%>%filter(SHE$Year=="2012-13")
South_Zone_12_13<-Last%>%filter(Last$State=="Andhra Pradesh"|Last$State=="Karnataka"|Last$State=="Kerala"|Last$State=="Tamil Nadu"|Last$State=="Telangana")
South_Zone_12_13<-South_Zone_12_13%>%transmute(Male.proportion=South_Zone_12_13$`Grand Total Male`/South_Zone_12_13$`Grand Total (Total)`,South_Zone_12_13$`Grand Total Female`/South_Zone_12_13$`Grand Total (Total)`)
South_Zone_12_13<-as.matrix(South_Zone_12_13)
South_Zone_12_13<-t(South_Zone_12_13)
colnames(South_Zone_12_13)<-c("Andhra Pradesh","Karnataka","Kerala","Tamil Nadu","Telangana")

Last<-SHE%>%filter(SHE$Year=="2011-12")
South_Zone_11_12<-Last%>%filter(Last$State=="Andhra Pradesh"|Last$State=="Karnataka"|Last$State=="Kerala"|Last$State=="Tamil Nadu"|Last$State=="Telangana")
South_Zone_11_12<-South_Zone_11_12%>%transmute(Male.proportion=South_Zone_11_12$`Grand Total Male`/South_Zone_11_12$`Grand Total (Total)`,South_Zone_11_12$`Grand Total Female`/South_Zone_11_12$`Grand Total (Total)`)
South_Zone_11_12<-as.matrix(South_Zone_11_12)
South_Zone_11_12<-t(South_Zone_11_12)
colnames(South_Zone_11_12)<-c("Andhra Pradesh","Karnataka","Kerala","Tamil Nadu")

par(mfrow=c(3,2))

barplot.default(as.matrix(South_Zone_15_16),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2015-16 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(South_Zone_14_15),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2014-15 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(South_Zone_13_14),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2013-14 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(South_Zone_12_13),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2012-13 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(South_Zone_11_12),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2011-12 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()

par(mfrow=c(1,1))             ##in kerala female proportion is always higher than male proportion


##Union Territories total yearwise analysis
Last<-SHE%>%filter(SHE$Year=="2015-16")
UT_15_16<-Last%>%filter(Last$State=="Andaman & Nicobar Islands"|Last$State=="Chandigarh"|Last$State=="Dadra & Nagar Haveli"|Last$State=="Daman & Diu"|Last$State=="Delhi"|Last$State=="Lakshadweep"|Last$State=="Puducherry")
UT_15_16<-UT_15_16%>%transmute(Male.proportion=UT_15_16$`Grand Total Male`/UT_15_16$`Grand Total (Total)`,UT_15_16$`Grand Total Female`/UT_15_16$`Grand Total (Total)`)
UT_15_16<-as.matrix(UT_15_16)
UT_15_16<-t(UT_15_16)
colnames(UT_15_16)<-c("Andaman","Chandigarh","D N H","Daman & Diu","Delhi","Lakshadweep","Puducherry")

Last<-SHE%>%filter(SHE$Year=="2014-15")
UT_14_15<-Last%>%filter(Last$State=="Andaman & Nicobar Islands"|Last$State=="Chandigarh"|Last$State=="Dadra & Nagar Haveli"|Last$State=="Daman & Diu"|Last$State=="Delhi"|Last$State=="Lakshadweep"|Last$State=="Puducherry")
UT_14_15<-UT_14_15%>%transmute(Male.proportion=UT_14_15$`Grand Total Male`/UT_14_15$`Grand Total (Total)`,UT_14_15$`Grand Total Female`/UT_14_15$`Grand Total (Total)`)
UT_14_15<-as.matrix(UT_14_15)
UT_14_15<-t(UT_14_15)
colnames(UT_14_15)<-c("Andaman","Chandigarh","D N H","Daman & Diu","Delhi","Lakshadweep","Puducherry")

Last<-SHE%>%filter(SHE$Year=="2013-14")
UT_13_14<-Last%>%filter(Last$State=="Andaman & Nicobar Islands"|Last$State=="Chandigarh"|Last$State=="Dadra & Nagar Haveli"|Last$State=="Daman & Diu"|Last$State=="Delhi"|Last$State=="Lakshadweep"|Last$State=="Puducherry")
UT_13_14<-UT_13_14%>%transmute(Male.proportion=UT_13_14$`Grand Total Male`/UT_13_14$`Grand Total (Total)`,UT_13_14$`Grand Total Female`/UT_13_14$`Grand Total (Total)`)
UT_13_14<-as.matrix(UT_13_14)
UT_13_14<-t(UT_13_14)
colnames(UT_13_14)<-c("Andaman","Chandigarh","D N H","Daman & Diu","Delhi","Lakshadweep","Puducherry")

Last<-SHE%>%filter(SHE$Year=="2012-13")
UT_12_13<-Last%>%filter(Last$State=="Andaman & Nicobar Islands"|Last$State=="Chandigarh"|Last$State=="Dadra & Nagar Haveli"|Last$State=="Daman & Diu"|Last$State=="Delhi"|Last$State=="Lakshadweep"|Last$State=="Puducherry")
UT_12_13<-UT_12_13%>%transmute(Male.proportion=UT_12_13$`Grand Total Male`/UT_12_13$`Grand Total (Total)`,UT_12_13$`Grand Total Female`/UT_12_13$`Grand Total (Total)`)
UT_12_13<-as.matrix(UT_12_13)
UT_12_13<-t(UT_12_13)
colnames(UT_12_13)<-c("Andaman","Chandigarh","D N H","Daman & Diu","Delhi","Lakshadweep","Puducherry")

Last<-SHE%>%filter(SHE$Year=="2011-12")
UT_11_12<-Last%>%filter(Last$State=="Andaman & Nicobar Islands"|Last$State=="Chandigarh"|Last$State=="Dadra & Nagar Haveli"|Last$State=="Daman & Diu"|Last$State=="Delhi"|Last$State=="Lakshadweep"|Last$State=="Puducherry")
UT_11_12<-UT_11_12%>%transmute(Male.proportion=UT_11_12$`Grand Total Male`/UT_11_12$`Grand Total (Total)`,UT_11_12$`Grand Total Female`/UT_11_12$`Grand Total (Total)`)
UT_11_12<-as.matrix(UT_11_12)
UT_11_12<-t(UT_11_12)
colnames(UT_11_12)<-c("Andaman","Chandigarh","D N H","Daman & Diu","Delhi","Lakshadweep","Puducherry")

par(mfrow=c(3,2))

barplot.default(as.matrix(UT_15_16),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2015-16 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(UT_14_15),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2014-15 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(UT_13_14),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2013-14 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(UT_12_13),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2012-13 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()
barplot.default(as.matrix(UT_11_12),beside=T,ylim=c(0,0.8),col=c('#1E90FF','#FF1493'),main = '2011-12 Total Enrollment',ylab = 'Male -Female proportion',xlab = "State")
legend('topright',cex = 0.7,c('Male proportion','Female proportion'),fill = c('#1E90FF','#FF1493'))
grid(nx=NULL,ny=NULL,lty = 1,lwd = 0.2)
box()

par(mfrow=c(1,1))              ##Lakshadweep and andaman has higher female proportions

#Line chart of kerala
kerala<-SHE%>%filter(State=="Kerala")%>%select(`Grand Total Male`|`Grand Total Female`|`Grand Total (Total)`|`Year`)
kerala_male<-kerala%>%transmute(Total=kerala$`Grand Total Male`,Year=kerala$Year,Category="Kerala male Total")
kerala_female<-kerala%>%transmute(Total=kerala$`Grand Total Female`,Year=kerala$Year,Category="Kerala female Total")
kerala_total<-rbind(kerala_male,kerala_female)
ggplot(kerala_total,aes(x=`Year`,y=`Total`,colour=`Category`,group=`Category`))+geom_line()+ggtitle("line diagram of total male and female enrollment in Kerala")
#Line chart of Meghalaya
Meghalaya<-SHE%>%filter(State=="Meghalaya")%>%select(`Grand Total Male`|`Grand Total Female`|`Grand Total (Total)`|`Year`)
Meghalaya_male<-Meghalaya%>%transmute(Total=Meghalaya$`Grand Total Male`,Year=Meghalaya$Year,Category="Meghalaya male Total")
Meghalaya_female<-Meghalaya%>%transmute(Total=Meghalaya$`Grand Total Female`,Year=Meghalaya$Year,Category="Meghalaya female Total")
Meghalaya_total<-rbind(Meghalaya_male,Meghalaya_female)
ggplot(Meghalaya_total,aes(x=`Year`,y=`Total`,colour=`Category`,group=`Category`))+geom_line()+ggtitle("line diagram of total male and female enrollment in Meghalaya")
#Line chart of Nagaland
Nagaland<-SHE%>%filter(State=="Nagaland")%>%select(`Grand Total Male`|`Grand Total Female`|`Grand Total (Total)`|`Year`)
Nagaland_male<-Nagaland%>%transmute(Total=Nagaland$`Grand Total Male`,Year=Nagaland$Year,Category="Nagaland male Total")
Nagaland_female<-Nagaland%>%transmute(Total=Nagaland$`Grand Total Female`,Year=Nagaland$Year,Category="Nagaland female Total")
Nagaland_total<-rbind(Nagaland_male,Nagaland_female)
ggplot(Nagaland_total,aes(x=`Year`,y=`Total`,colour=`Category`,group=`Category`))+geom_line()+ggtitle("line diagram of total male and female enrollment in Nagaland")

