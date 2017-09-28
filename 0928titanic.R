#讀資料
data1 <- read.csv("C:/Users/Lenovo/Desktop/titanic.csv", header=T)
View(data1)

#將乘客及員工分開，%in%表示前者內是否有後者的值
cus <- data1[data1$class %in% c(1,2,3),]
boa <- data1[!data1$class %in% c(1,2,3),]
View(boa)

#剃除資料中為9999的值
cus <- cus[!cus$fare == 9999,]
cus <- cus[!cus$age == 9999,]
boa <- boa[!boa$age == 9999,]

#船員及乘客的基本敘述統計
summary(cus$age)
summary(boa$age)
table(cus$gender)
table(boa$gender) #0為男性？

#船員及乘客存活及性別列聯表
ag <- table(cus$survival,cus$gender)
ag
ag[1,1] / (ag[1,1] + ag[2,1]) #男性乘客死亡比例：0.794
ag[1,2] / (ag[1,2] + ag[2,2]) #女性乘客死亡比例：0.312
bg <- table(boa$survival,boa$gender)
bg
bg[1,1] / (bg[1,1] + bg[2,1]) #男性船員死亡比例：0.900
bg[1,2] / (bg[1,2] + bg[2,2]) #女性船員死亡比例：0.870



#救生艇及存活關係
allb <- rbind(cus,boa)
table(allb$boat)
sort(table(allb$boat),decreasing = T) #依照救生艇載的人數多寡排序
table(cus$survival,cus$boat)
table(boa$survival,boa$boat)

#建立乘客存活及年齡列聯表
ad <-table(cus$survival,cut(cus$age,seq(1,71,10)))
t1 <- round(ad[2,] / (ad[1,]+ad[2,]),2)
barplot(t1)

#建立船員存活及年齡列聯表
ad2 <-table(boa$survival,cut(boa$age,seq(1,71,10)))
t2 <- round(ad2[2,] / (ad2[1,]+ad2[2,]),2)
barplot(t2)

#存活者的票價分佈
sur <- cus[cus$survival == 1,]
surf <- table(cut(sur$fare,seq(1,530,20)))
barplot(table(sur$fare))
barplot(surf)
barplot(table(cut(cus$fare,seq(1,530,20))))


par(2,1)

#
group_by(cus,joined) %>%
  summarise(fare = mean(fare))

#
table(boa$survival,boa$class)
table(boa$job,boa$class)

#
ste <- boa %>%
  filter(grepl('Steward',job))
table(ste$survival,ste$job)

#
as.character()
nam <- strsplit(as.character(cus$name),",",fixed = T)
a1 <- unlist(nam)
a2 <- a1[seq(1,length(a1)-2,2)]
fam <- names(table(a2)[table(a2)>1])
cus$lastname <- a2
cus$singal <- rep(1,1285)
cus$singal[cus$lastname %in% fam] = 0
View(cus)
af <- table(cus$survival,cus$singal)
af[1,1] / (af[1,1]+af[2,1]) #家庭搭船死亡率
af[1,2] / (af[1,2]+af[2,2]) #單獨搭船死亡率

