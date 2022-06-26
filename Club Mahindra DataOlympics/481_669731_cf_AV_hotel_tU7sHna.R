Hotel_tr<-read.csv("file:///C:/Users/hp/Desktop/train.csv",stringsAsFactors = F)
Hotel_te<-read.csv("file:///C:/Users/hp/Desktop/test.csv",stringsAsFactors = F)

library(lubridate)
library(randomForest)
library(tree)
library(gbm)
library(ggplot2)
library(gplots)
library(ModelMetrics)
library(dplyr)
library(class)
library(car)
Hotel_te$amount_spent_per_room_night_scaled<-NA
Hotel=rbind(Hotel_tr,Hotel_te)
View(Hotel)
Hotel$booking_date=as.Date(Hotel$booking_date,format="%d/%m/%y")
Hotel$checkin_date=as.Date(Hotel$checkin_date,format="%d/%m/%y")
Hotel$checkout_date=as.Date(Hotel$checkout_date,format="%d/%m/%y")
Hotel$Booking_day=wday(Hotel$booking_date)
Hotel$Booking_month=month(Hotel$booking_date)
Hotel$Booking_year=year(Hotel$booking_date)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

plotmeans(amount_spent_per_room_night_scaled ~ Booking_year , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)

plotmeans(amount_spent_per_room_night_scaled ~ Booking_month , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)

plotmeans(amount_spent_per_room_night_scaled ~ Booking_day , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)

Hotel$duration_of_stay=as.integer(Hotel$checkout_date-Hotel$checkin_date)
plotmeans(amount_spent_per_room_night_scaled ~ duration_of_stay , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)

Hotel$duration_of_stay=ifelse(Hotel$duration_of_stay>5 & Hotel$duration_of_stay <=10,10,ifelse(Hotel$duration_of_stay>10,20,Hotel$duration_of_stay))

Hotel$Booking_year=ifelse(Hotel$Booking_year %in% c(2015,2016),1,ifelse(Hotel$Booking_year %in% c(2017,2018,2019),2,3))

Hotel$period_of_booking=as.integer(Hotel$checkin_date-Hotel$booking_date)

plotmeans(amount_spent_per_room_night_scaled ~ period_of_booking , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled) & (Hotel$period_of_booking>20 & Hotel$period_of_booking <=40)  ,], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
hist(Hotel[abs(Hotel$period_of_booking)<200,"period_of_booking"] )

Hotel$period_of_booking=ifelse(Hotel$period_of_booking==0,1,ifelse(Hotel$period_of_booking>0 & Hotel$period_of_booking<50,2,3))
Hotel$check_in_day= wday(Hotel$checkin_date)
Hotel$check_in_month=month(Hotel$checkin_date)
Hotel$check_in_year=year(Hotel$checkin_date)   


plotmeans(amount_spent_per_room_night_scaled ~ check_in_year , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)

plotmeans(amount_spent_per_room_night_scaled ~ check_in_month , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)

plotmeans(amount_spent_per_room_night_scaled ~ check_in_day , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)

Hotel$check_in_year=as.numeric(Hotel$check_in_year==2012)
Hotel$check_in_day=ifelse(Hotel$check_in_day %in% c(1,5),1,ifelse(Hotel$check_in_day %in% c(2,3,4),2,3))
Hotel$check_in_month=ifelse(Hotel$check_in_month %in% c(1,7,11),1,ifelse(Hotel$check_in_month %in% c(3,9),2,ifelse(Hotel$check_in_month %in% c(4,6,8,10),3,ifelse(Hotel$check_in_month %in% c(5,12),4,5))))

Hotel$booking_date=NULL
Hotel$checkin_date=NULL
Hotel$checkout_date=NULL
Hotel$Booking_day=NULL
Hotel$Booking_year=NULL
Hotel$Booking_month=NULL
Hotel$Check_in_day=NULL
Hotel$Check_in_month=NULL

plotmeans(amount_spent_per_room_night_scaled ~ channel_code , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
#Hotel$channel_code=as.factor(Hotel$channel_code)

plotmeans(amount_spent_per_room_night_scaled ~ main_product_code , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
#Hotel$main_product_code=as.factor(Hotel$main_product_code)


Hotel$family=Hotel$numberofchildren+Hotel$numberofadults
Hotel$numberofadults=NULL
Hotel$numberofchildren=NULL
Hotel$family_pax=Hotel$family+Hotel$total_pax
Hotel$family=NULL
Hotel$total_pax=NULL
plotmeans(amount_spent_per_room_night_scaled ~ family_pax , data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled) & Hotel$family < 15, ], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)

#Hotel$family_pax=ifelse(Hotel$family_pax>10,11,Hotel$family_pax)
#Hotel$family_pax=as.factor(Hotel$family_pax)
#table(Hotel$memberid)
#Hotel$memberid2=substr(Hotel$memberid,1,1)
#table(Hotel$memberid2)

#plotmeans(amount_spent_per_room_night_scaled ~ memberid2, data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
#Hotel$memberid2=NULL
Hotel$memberid=NULL

plotmeans(amount_spent_per_room_night_scaled ~ cluster_code, data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
Hotel$cluster_code=ifelse(Hotel$cluster_code %in% c("B","D","F"),1,ifelse(Hotel$cluster_code %in% c("A"),2,ifelse(Hotel$cluster_code %in% c("C"),3,4)))
#Hotel$cluster_code=as.factor(Hotel$cluster_code)
plotmeans(amount_spent_per_room_night_scaled ~ reservationstatusid_code, data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
#Hotel$reservationstatusid_code=as.factor(as.numeric(as.factor(Hotel$reservationstatusid_code)))


length(unique(Hotel$resort_id))
Hotel$resort_id=substr(Hotel$resort_id,1,3)
length(unique(Hotel$resort_id))
Hotel$resort_id2=NULL

plotmeans(amount_spent_per_room_night_scaled ~ resort_id, data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
sort(tapply(Hotel$amount_spent_per_room_night_scaled, Hotel$resort_id,mean,na.rm=T))

Hotel$resort_id=ifelse(Hotel$resort_id %in% c("3fd","790","ef2"),1,ifelse(Hotel$resort_id %in% c("7f2","98a","e7f"),2,ifelse(Hotel$resort_id %in% c("f5c","670","c6f","0b9","da4","ff5"),3,ifelse(Hotel$resort_id %in% c("9f1","39f","a68","484","4e0","81b","620"),4,ifelse(Hotel$resort_id %in% c("d47","4ec","3e1","6b8"),5,ifelse(Hotel$resort_id %in% c("e29","b17","624","c75"),6,7))))))
str(Hotel)
#Hotel$duration_of_stay=as.factor(Hotel$duration_of_stay)
#Hotel$period_of_booking=as.factor(Hotel$period_of_booking)
#Hotel$check_in_day=as.factor(Hotel$check_in_day)
#Hotel$check_in_month=as.factor(Hotel$check_in_month)
#Hotel$check_in_year=as.factor(Hotel$check_in_year)

#View(Hotel[is.na(Hotel$season_holidayed_code), ])

table((Hotel$season_holidayed_code))


Hotel$season_holidayed_code=as.factor(Hotel$season_holidayed_code)
#Hotel$resort_id=as.factor(Hotel$resort_id)
#Hotel$member_age_buckets=as.factor(as.numeric(as.factor(Hotel$member_age_buckets)))
Hotel$roomnights=NULL
#Hotel$season_holidayed_code=ifelse(is.na(Hotel$season_holidayed_code),Hotel$season_holidayed_code_pred,Hotel$season_holidayed_code)
names(Hotel)

str(Hotel)
lapply(Hotel,function(x) length(unique(x)))

Hotel=CreateDummies(Hotel,"channel_code")
Hotel=CreateDummies(Hotel,"main_product_code")
Hotel=CreateDummies(Hotel,"persontravellingid")
Hotel=CreateDummies(Hotel,"resort_region_code")
Hotel=CreateDummies(Hotel,"resort_type_code")
Hotel=CreateDummies(Hotel,"room_type_booked_code")
Hotel=CreateDummies(Hotel,"season_holidayed_code")
Hotel$state_code_resort=as.factor(Hotel$state_code_resort)
plotmeans(amount_spent_per_room_night_scaled ~ member_age_buckets, data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
Hotel$member_age_buckets=ifelse(Hotel$member_age_buckets %in% c("A","B","G","H"),1,ifelse(Hotel$member_age_buckets %in% c("C","F"),2,ifelse(Hotel$member_age_buckets %in% c("I","J"),3,4)))
Hotel=CreateDummies(Hotel,"member_age_buckets")
Hotel$booking_type_code=as.factor(Hotel$booking_type_code)
Hotel=CreateDummies(Hotel,"cluster_code")
Hotel=CreateDummies(Hotel,"reservationstatusid_code")
Hotel$resort_id=as.factor(Hotel$resort_id)
Hotel=CreateDummies(Hotel,"duration_of_stay")
Hotel=CreateDummies(Hotel,"period_of_booking")
Hotel=CreateDummies(Hotel,"check_in_day")
Hotel=CreateDummies(Hotel,"check_in_month")
Hotel$check_in_year=as.factor(Hotel$check_in_year)
str(Hotel)

holiday_r_fit=randomForest(season_holidayed_code~.,data=Hotel[!is.na(Hotel$season_holidayed_code),c(2:58)],do.trace=T,ntree=4)
print(holiday_r_fit)
plot(holiday_r_fit)
Hotel$season_holidayed_code_pred2=predict(holiday_r_fit,newdata=Hotel,type='class')
table(Hotel$season_holidayed_code_pred2)
table(Hotel$season_holidayed_code)
Hotel$season_holidayed_code=ifelse(is.na(Hotel$season_holidayed_code),Hotel$season_holidayed_code_pred2,Hotel$season_holidayed_code)
Hotel$season_holidayed_code_pred2=NULL
Hotel$season_holidayed_code=as.factor(Hotel$season_holidayed_code)
View(Hotel)
plotmeans(amount_spent_per_room_night_scaled ~ state_code_residence, data = Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled) & Hotel$state_code_residence<20,], frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
lapply(Hotel, function(x) sum(is.na(x)))

Hotel$state_code_residence=ifelse(Hotel$state_code_residence %in% c(1,2,3,10,16),1,ifelse(Hotel$state_code_residence %in% c(4,5,6,8,19),2,ifelse(Hotel$state_code_residence %in% c(7,13),3,ifelse(Hotel$state_code_residence %in% c(9,14,18),4,ifelse(Hotel$state_code_residence %in% c(11,12),5,6)))))
Hotel$booking_type_code=as.factor(Hotel$booking_type_code)

Hotel$state_code_residence=as.factor(Hotel$state_code_residence)
names(Hotel)

str(Hotel)
lapply(Hotel,function(x) length(unique(x)))

names(Hotel)



model=tree(amount_spent_per_room_night_scaled~.,data=Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled) & !is.na(Hotel$state_code_residence),c(2:58)])

plot(model)
text(model)
rmse(predict(model,newdata=Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),]),Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),"amount_spent_per_room_night_scaled"])
p=predict(model,newdata=Hotel)

g_fit=gbm(amount_spent_per_room_night_scaled~.,data=Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled)& !is.na(Hotel$state_code_residence),c(2:58)],distribution = "gaussian",interaction.depth =3,shrinkage=0.9,n.trees = 150)
RMSE=rmse(predict(g_fit,newdata=Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),],n.trees = 50),Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),"amount_spent_per_room_night_scaled"])
print(RMSE)

for(i in 1:20){
  print(plot(g_fit,i,label_both=T))
}

#r_fit=randomForest(amount_spent_per_room_night_scaled~.,data=Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),c(2:9,12:24)],ntree=5,do.trace=T)
p1=predict(g_fit,newdata=Hotel,n.trees=50)
names(Hotel)


l_fit=lm(amount_spent_per_room_night_scaled~.,data=Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled) & !is.na(Hotel$state_code_residence),c(2:58)])
summary(l_fit)
formula(l_fit)
l_fit=lm(amount_spent_per_room_night_scaled ~ state_code_residence + state_code_resort + 
           booking_type_code + resort_id + check_in_year + family_pax + 
           channel_code_3 + channel_code_1 + main_product_code_4 + main_product_code_1 + 
           main_product_code_3 + main_product_code_2 + persontravellingid_4753 + 
           persontravellingid_4752 + persontravellingid_46 + persontravellingid_47 + 
           persontravellingid_45 + resort_region_code_2 + resort_region_code_1 + 
           resort_type_code_7 + resort_type_code_4 + resort_type_code_5 + 
           resort_type_code_3 + resort_type_code_2 + resort_type_code_1 + 
           room_type_booked_code_5 + room_type_booked_code_1 + room_type_booked_code_4 + 
           room_type_booked_code_2 + room_type_booked_code_3 + season_holidayed_code_4 + 
           season_holidayed_code_3 + season_holidayed_code_2 + member_age_buckets_2 + 
           member_age_buckets_1 + member_age_buckets_4 + cluster_code_3 + 
           cluster_code_4 + cluster_code_1 + reservationstatusid_code_C + 
           reservationstatusid_code_B + reservationstatusid_code_A + 
           duration_of_stay_10 + duration_of_stay_5 + duration_of_stay_4 + 
           duration_of_stay_1 + duration_of_stay_3 + duration_of_stay_2 + 
           period_of_booking_3 + period_of_booking_2 + check_in_day_3 + 
           check_in_day_2 + check_in_month_2 + check_in_month_4 + check_in_month_1 + 
           check_in_month_3,data=Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled) & !is.na(Hotel$state_code_residence),c(2:58)]) 

p2=predict(l_fit,newdata=Hotel)
summary(l_fit)
Hotel$p2=p2
Hotel$p=p
Hotel$p1=p1
f_fit=lm(amount_spent_per_room_night_scaled~p+p1,data=Hotel[!is.na(Hotel$amount_spent_per_room_night_scaled),])
summary(f_fit)
p3=predict(f_fit,newdata = Hotel)
Hotel$p3=p3
Hotel2=Hotel[is.na(Hotel$amount_spent_per_room_night_scaled),] %>% select(reservation_id,p2)
#View(Hotel2)
names(Hotel2)[2]="amount_spent_per_room_night_scaled"
write.csv(Hotel2,"file:///C:/Users/hp/Desktop/dataframe.csv",row.names=F)
