# 1. csv_exam.csv 파일을 불러와서 'data'로 저장
data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day1-main/csv_exam.csv")
head(data)
tail(data)
str(data)

# 2. class가 2인 학생들만 'class2'로 저장
class2=data[data$class%in%2,]

# 3. math 점수가 평균보다 높은 학생들의 id 출력
data[data$math>=mean(data$math),"id"]

# 4. english가 최고점인 학생의 id 출력
data[data$english%in%max(data$english),"id"]

# 5. math와 science가 모두 80점 이상인 학생 id 출력
data[data$math>=80 & data$science>=80,"id"]

# 6. english가 80점 이상인 학생들의 math 평균
mean(data[data$english>=80,"math"])

# 7. class가 4인 학생들의 science 평균
mean(data[data$class%in%4,"science"])

# 8. class가 3인 학생 중 science가 50점 이상인 학생 id
data[data$class%in%3 & data$science>=50,"id"]

# 9. class가 1반과 3반 학생들의 math 평균
mean(data[data$class%in%c(1,3),"math"])

# 10. class가 5인 학생 중 english 최고점 학생 id
class5=data[data$class%in%5,]
class5[class5$english%in%max(class5$english),"id"]

data[data$class%in%5 & 
       data$english%in%max(data[data$class%in%5,"english"]),"id"]

data=airquality
head(data)
str(data)

table(is.na(data$Ozone))
ozone=data[!is.na(data$Ozone),]
data[!is.na(data$Ozone)&!is.na(data$Solar.R),]
data2=data[complete.cases(data),]

# 선형보간 na.approx()
install.packages("zoo")
library(zoo)
na.approx(data$Ozone)

df=data.frame(data$Ozone,
           na.approx(data$Ozone,na.rm=F))
View(df)

ex=boxplot(data$Ozone)
ex

data[data$Ozone%in%ex$out,"Ozone"]=
  max(data[!data$Ozone%in%ex$out,"Ozone"],na.rm=T)
#62,117
data[c(62,117),]

data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day1-main/csv_exam.csv")
head(data)

data$total=data$math+data$english+data$science
head(data)
data[,"avg"]=data[,"total"]/3
head(data)
data$test=ifelse(data$avg>=70,"합격","재시험")
View(data)
table(data$test)

data$col=ifelse(data$avg>=70,"blue","red")
plot(data$math,data$english,col=data$col)

# 90점이상 : A, 90미만 80이상 : B, 80미만 : C
data$grade=ifelse(data$avg>=80,"A",
                  ifelse(data$avg>=70,"B","C"))
table(data$grade)

a=1000
a
as.character(a)
a="1000"
as.numeric(a)+10
wind=c("12.5","13")
mean(as.numeric(wind))
c = "1,000"
c ="서울"
as.numeric(c)
gsub(",","",c)
e = "2020-10-10"
Sys.Date()-as.Date(e)

# airquality 를 data로 저장하여 문제 해결
data=airquality
# 1. 일사량의 평균
head(data)
mean(data$Solar.R,na.rm=T)

# 2. 풍속이 10 이상일때 온도의 평균
mean(data[data$Wind>=10,"Temp"])

# 3. 오존의 결측치를 제거한 data2 생성
data2=data[!is.na(data$Ozone),]

# data2를 이용하여 문제해결
# 4. Ozone이 가장 높은 수치를 보인 월은?
data2[data2$Ozone%in%max(data2$Ozone),"Month"]
# 5. 4번에서 확인된 월의 평균온도는?
mean(data2[data2$Month%in%8,"Temp"])
# 6. Ozone의 값이 40이상이면 위험
#    30이상 40미만은 보통, 30미만은 안전인
#    grade 변수 생성
data2$grade=ifelse(data2$Ozone>=40,"위험",
                   ifelse(data2$Ozone>=30,"보통","안전"))
table(data2$grade)

# 7. grade가 위험일때의 Ozone 평균
mean(data2[data2$grade%in%"위험","Ozone"])

# 수학점수가 80이상인 학생의 id
data[data$math>=80,"id"]

data %>% 
  filter(math>=80) %>% 
  select(id)

library(dplyr)
data=airquality
#오존이 40이상일때
data %>% 
  filter(Ozone>=40)
#8월에 오존이 40일때
data %>% 
  filter(Ozone>=40, Month%in%8)
#풍속이 10이상일때
data %>% 
  filter(Wind>=10)

data[관측치조건,]
data %>% 
  filter(관측치조건)

# 오존만 선택
data %>% 
  select(Ozone,Month)

data[,"Ozone"]
data[,c("Ozone","Month")]
data[,-1]

data %>% 
  select(-1,-3)
data %>% 
  select(-Ozone)

#풍속이 10 이상일때의 온도
data[data$Wind>=10,"Temp"]

data %>% 
  filter(Wind>=10) %>% 
  select(Temp)

data %>% 
  arrange(-Ozone) %>% 
  head(5)

data %>% 
  filter(Wind>=10) %>% 
  select(Temp) %>% 
  arrange(-Temp) %>% 
  head(5)

data$total=data$math+data$eng+data$scie

data %>% 
  mutate(total=math+eng+scie)

head(data)
data %>% 
  mutate(test=Ozone+Temp)->data2

data %>% 
  mutate(grade=ifelse(Ozone>=40,"위험","안전")) %>% 
  filter(grade%in%"위험") %>% 
  head()
  
#풍속이 10이상일때 온도의 평균
mean(data[data$Wind>=10,"Temp"])
data %>% 
  filter(Wind>=10) %>% 
  summarise(mean(Temp))

data %>% 
  group_by(Month) %>% 
  summarise(n=n())

summary(data)
  
library(openair)
data=as.data.frame(mydata)
head(data)
# 1. pm25의 결측치 제거
data %>% 
  filter(!is.na(pm25))

# 2. pm10이 30이상
data %>% 
  filter(!is.na(pm25),pm10>=30)

# 3. ws의 평균
data %>% 
  filter(!is.na(pm25),pm10>=30) %>% 
  summarise(mean(ws,na.rm=T))

mean(data[!is.na(data$pm25)&
            data$pm10>=30,"ws"],na.rm=T)

# 4. 연도별 pm10의 평균
View(data)
substr(data$date,1,4)

data %>% 
  mutate(year=substr(date,1,4)) %>% 
  group_by(year) %>% 
  summarise(m_pm10=mean(pm10,na.rm=T)) %>% 
  arrange(-m_pm10)

# 5. pm10이 80이상이면 위험
#    80미만 50이상이면 보통
#    50미만이면 좋음인 grade 생성
# 5. 연도별 grade별 pm10의 평균
data %>% 
  mutate(year=substr(date,1,4),
         grade=ifelse(pm10>=80,"위험",
                      ifelse(pm10>=50,"보통","좋음"))) %>% 
  group_by(year,grade) %>% 
  summarise(m_pm10=mean(pm10,na.rm=T)) %>% 
  data.frame()

data$year=substr(data$date,1,4)
data$grade=ifelse(data$pm10>=80,"위험",
                  ifelse(data$pm10>=50,"보통","좋음"))
mean(data[data$year%in%2000 & 
            data$grade%in%"위험","pm10"],na.rm=T)

data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day2-main/부산광역시 영도구_생활쓰레기 배출량 현황_20231231.csv",
              fileEncoding = 'cp949',encoding = 'utf-8')
head(data)

# 1.3월의 평균 배출량은 얼마인가요?
data %>% 
  summarise(mean(X3월))

# 2.6월의 일반쓰레기 평균 배출량은 얼마인가요?
data %>% 
  filter(구분%in%"일반쓰레기") %>% 
  summarise(mean(X6월))

# 3.9월의 재활용품 배출량 최댓값은 얼마인가요?
data %>% 
  filter(구분%in%"재활용품") %>% 
  summarise(max(X9월))

# 4.년도별 12월 배출량의 평균과 표준편차를 구하세요.
data %>% 
  group_by(년도) %>% 
  summarise(mean(X12월),sd(X12월)) %>% 
  data.frame()

# 5.구분별 10월 배출량의 최댓값은 몇년도인가요?
data %>% 
  group_by(구분) %>% 
  summarise(max(X10월))

data1=data.frame(
  date=c("2025-01","2025-02"),
  pm10=c(50,30)
)
data2=data.frame(
  date=c("2025-03","2025-04"),
  pm10=c(20,40)
)
data1
data2
data=rbind(data1,data2)
data

data1=data.frame(
  측정소코드=c(11111,11122),
  pm10=c(50,60)
)
data2=data.frame(
  측정소=c(11111,11122),
  위도=c(38,37),
  경도=c(128,129)
)
data1
data2
data=merge(data1,data2,by="측정소코드")
data
plot(data$위도,data$경도)

names(data2)=c("측정소코드",names(data2)[2:3])
names(data2)


data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day2-main/2022년 미세먼지 경보,주의보 발령 이력.csv",
              fileEncoding = 'cp949',encoding = 'utf-8')
head(data)
#주의보와 경보는 총 몇번 발령됬나요?
data %>% 
  group_by(경보.단계) %>% 
  summarise(n=n())

#경보가 발령된 지역은 어디인가요?
data %>% 
  filter(경보.단계%in%"경보") %>% 
  select(지역명)

#지역별 항목별 발령 건수와 평균 발령농도는 어떻게 되나요?
data %>% 
  group_by(지역명,항목_코드) %>% 
  summarise(n=n(),m=mean(발령농도)) %>% 
  arrange(-m)
  
#권역별 항목별 발령농도 최대수치는 몇인가요?
data %>% 
  group_by(권역명,항목_코드) %>% 
  summarise(m=max(발령농도)) %>% 
  arrange(-m)->df
View(df)  









