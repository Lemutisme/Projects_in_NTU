#hpg_store_info.csv <- hst
#air_store_info.csv <- ast

#hpg_reserve.csv <- hre
#air_reserve.csv <- are

#air_visit_data.csv <- avi

#store_id_relation <- id
#date_info.csv <- date

#sample_submission.csv <- ss

hst <- read.csv('hpg_store_info.csv')
str(hst)
factor(hst$hpg_genre_name)
factor(hst$hpg_area_name)
sum(is.na(hst))

ast <- read.csv('air_store_info.csv')
str(ast)
factor(ast$air_genre_name)
sum(is.na(ast))

hre<- read.csv('hpg_reserve.csv')
are<- read.csv('air_reserve.csv')
str(hre)
str(are)
sum(is.na(hre))
sum(is.na(are))
summary(hre)

head(are)

avi <- read.csv('air_visit_data.csv')
str(avi)
sum(is.na(avi))

head(avi)

which.max(avi$visitors)
avi[85315,c('air_store_id','visitors')]

stid <- read.csv('store_id_relation.csv')
str(stid)
sum(is.na(stid))

date<- read.csv('date_info.csv')
str(date)
sum(is.na(date))
head(date)

ss<- read.csv('sample_submission.csv')
str(ss)
sum(is.na(ss))

head(ss)

store1<-merge(hst,stid,'hpg_store_id')
store2<-merge(ast,stid,'air_store_id')
store3<-merge(store1,store2,'hpg_store_id')
store4<-store3[c(1,6,2,8,3,9,4,5,10,11)]
head(store4)

serve1<-merge(hre,stid,'hpg_store_id')
serve2<-merge(are,stid,'air_store_id')
serve3<-merge(serve1,serve2,'hpg_store_id')
serve4<-serve3[-5]
head(serve4)
tail(serve4)


