a<-10
x1<-10
5->x2
x5<-x2
b=2
b=x10

y=c(1,10,100,1000)
1:100

x<-"a"
x
x<-"y"
x
x=10
x="10"
x
a=10
a+2
a="10"
a+2

set.seed(123)
sample(1:45,6)

x=c("인천광역시 서구 무슨동",
    "인천광역시 동구 무슨동")
y=substr(x,7,8)
y
x=c("2025-06-09","2025-06-10")

map()
library(maps)



library(magick)
library(image.libfacedetection)

image <- image_read("https://cdnimage.dailian.co.kr/news/201911/news_1573518511_842387_m_1.jpg")
image

faces <- image_detect_faces(image)
faces

plot(faces, image, border = "red", lwd = 7, col = "white")

xy=function(x,y){
  return(print(x+y))
}
xy(1,10)

y=10:110
y[1:10]
y[20]
y[c(10,20)]
y[y<20]

data[관측치조건,변수조건]
airquality
head(airquality)
air=airquality

#변수선택
air[,"Temp"]
air[,4]
air[,c("Month","Temp")]
air[,c(5,4)]
air[,-6]
air[,-c(1,2)]
air$Month

#관측치선택
air[2,]
air[1:10,]
air[air$Temp>=70,]
air[air[,"Temp"]>=70,]
air[air[,5]==6,]

table(air[air$Wind>=10,"Month"])
mean(air[air$Temp>=70,"Temp"])
mean(air$Temp)

x=air[air$Wind>=10,"Month"]
table(x)

air[,"Temp"][70]
air[air$Wind>=10,][,"Month"]

air[air$Temp>=70 &
      air$Month%in%6,]->x

air[air$Month%in%6 | 
      air$month%in%9,]
air[air$Month%in%c(6,9),]

#데이터명[관측치선택,변수선택]


stn <- c( "서울", "부산","대전","대구","인천","광주")
temp <- c(22.7,25.3,23.4,24.9,22.9,24.8)
prec <- c(0,18.5,4,10.4,0,0)
ws <- c(0.5,1.2,0.9,1.2,0.7,2.4)
rh <- c(89,97,96,97,85,84)
obs <- data.frame(stn,temp,prec,ws,rh)

library(readxl)
exam<-read_excel("C:/Users/PC00/Desktop/EHRD-2025-day1-main/excel_exam.xlsx")
exam2=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day1-main/csv_exam.csv")

head(exam2)
class2=exam2[exam2$class%in%2,]
write.csv(class2,"C:/Users/PC00/Desktop/EHRD-2025-day1-main/c2.csv")



head(exam2)
tail(exam2)
View(exam2)
str(exam2)

head(airquality)
air=airquality

air[!is.na(air$Ozone),]

mean(air$Temp)
max(air$Temp)

mean(air$Ozone,na.rm=TRUE)
max(air$Ozone,na.rm=T)


p<-boxplot(air$Ozone)
p

df=air[!air$Ozone%in%p$out,]
boxplot(df$Ozone)


# 1.openair 패키지를 설치
install.packages("openair")
library(openair)
head(mydata)
# 2.mydata에서 o3이 10이상인 자료 df1 생성
step1=mydata$o3>=10
mydata[step1,]->df1

df1=mydata[mydata$o3>=10,]
# 3.pm10가 20이상일때의 pm10의 평균
mydata=as.data.frame(mydata)
step1=mydata$pm10>=20
step2=mydata[step1,]
mean(step2$pm10,na.rm=T)

mydata[mydata$pm10>=20,]->ex
mean(ex$pm10,na.rm=T)

mean(mydata[mydata$pm10>=20,"pm10"])
# 4.pm10이 pm10의 평균 이상일때의 자료 df2 생성
mydata[mydata$pm10>=mean(mydata$pm10,na.rm=T),]->df2

step1=mean(mydata$pm10,na.rm=T)
step2=mydata$pm10>=step1
df2=mydata[step2,]

# 5.ws가 10이상일때의 pm10의 평균
mean(mydata[mydata$ws>=10,"pm10"],na.rm=T)

step1=mydata$ws>=10
step2=mydata[step1,]
mean(step2$pm10,na.rm=T)

# 6.pm25의 결측치를 제거했을때 pm10의 평균
mean(mydata[!is.na(mydata$pm25),"pm10"],na.rm=T)

step1=is.na(mydata$pm25)
step2=mydata[!step1,]
mean(step2$pm10,na.rm=T)


# 데이터명[관측치선택,변수선택]
a=air$Wind>=10
b=air[a,]
mean(b$Temp)





