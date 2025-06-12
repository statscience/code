pororo=data.frame(
  name=c("뽀로로","크롱","에디","포비","해리","루피"),
  math=c(81,86,98,62,86,79),
  eng=c(89,63,75,65,95,93),
  kor=c(92,73,86,78,93,89)
)

name=c("뽀로로","크롱","에디","포비","해리","루피")
math=c(81,86,98,62,86,79)
eng=c(89,63,75,65,95,93)
kor=c(92,73,86,78,93,89)
pororo=data.frame(name,math,eng,kor)
#1.pororo라는 data.frame를 생성하세요.
#2.total=math+eng+kor 입니다. 총점을 생성하세요.
#3.avg = total/3입니다. 평균을 생성하세요.
library(dplyr)
pororo %>% 
  mutate(total=math+eng+kor,
         avg=total/3)->pororo2
pororo$total=pororo$math+pororo$eng+pororo$kor
#4.평균이 80점을 넘으면 합격, 그렇지 않으면 불합격이라고 합니다. 합격유무를 pass로 만드세요.
pororo2 %>% 
  mutate(pass=ifelse(avg>=80,"합격","불합격"))
  
#5.max()함수를 이용하여 총점이 높은 친구이름과 min()함수를 이용하여 총점이 낮은 친구이름을 찾으세요
pororo %>% 
  mutate(total=math+eng+kor) %>% 
  filter(total%in%min(total)) %>% 
  select(name)

pororo[pororo$total%in%max(pororo$total),"name"]

data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day3-main/diet.csv")
head(data)
data %>% 
  filter(Height>=170 & gender%in%0)

data %>% 
  filter(Age<30,gender%in%1)

data %>% 
  filter(Diet%in%2,pre.weight>=70)

data %>% 
  arrange(Age) %>% 
  head()

data %>% 
  arrange(Diet,-Age)

data %>% 
  select(gender,Age,pre.weight) %>% 
  head()
data %>% 
  select(2,5) %>% 
  head()

data %>% 
  select(-gender) %>% 
  head()

data %>% 
  mutate(BMI=pre.weight/(Height**2)*10000,
         BMI2=ifelse(BMI<=18.5,"저체중",
                     ifelse(BMI<=23,"정상",
                            ifelse(BMI<=25,"과체중","비만"))))->data2
table(data2$BMI2)

#Diet별로 연령의 평균 최고연령
data2 %>% 
  group_by(Diet) %>% 
  summarise(mean(Age),max(Age))
  
#성별에따라 이전몸무게의 평균
data2 %>% 
  group_by(gender) %>% 
  summarise(mean(pre.weight))

#Diet별 성별에따라 회원수

data2 %>% 
  group_by(Diet,gender) %>% 
  summarise(n())

# 1. gender의 결측치 및 20살미만 제거
data %>% 
  filter(!is.na(gender),Age>=20)->data2
# 2. 20대, 30대, 40대, 50대이상 연령대 생성
data2 %>% 
  mutate(연령대=ifelse(Age<=29,"20대",
                    ifelse(Age<=39,"30대",
                           ifelse(Age<=49,"40대","50대이상"))))->data3
table(data3$연령대)
# 3. 연령대별 몸무게 평균
data3 %>% 
  group_by(연령대) %>% 
  summarise(mean(pre.weight))
# 4. Diet별 연령대 회원수
data3 %>% 
  group_by(Diet,연령대) %>% 
  summarise()
# 5. 키가 가장큰 회원의 Diet프로그램과 나이
data3 %>% 
  filter(Height%in%max(Height)) %>% 
  select(Diet,Age)
data3 %>% 
  arrange(-Height) %>% 
  select(Diet,Age) %>% 
  head(1)

# 6. Diet별 이전몸무게평균과 6주후몸무게평균
data3 %>% 
  group_by(Diet) %>% 
  summarise(mean(pre.weight),mean(weight6weeks)) %>% 
  data.frame()
###

data=read.csv("C:/Users/PC00/Desktop/아파트(매매)_실거래가_20250612084724.csv",
              fileEncoding = 'cp949',encoding = 'utf-8',skip=15)
head(data)
data %>% 
  mutate(거래금액=gsub(",","",거래금액.만원.),
         지역=substr(시군구,1,2))->data2
data2 %>% 
  select(지역,거래금액) %>% 
  head()
mean(data2$거래금액,na.rm=T)
str(data2)
data2 %>% 
  mutate(거래금액=as.numeric(거래금액))->data2
mean(data2$거래금액)

names(data2)
# 1. 계약이 가장 많았던 날은?
data2 %>% 
  group_by(계약일) %>% 
  summarise(n=n()) %>% 
  filter(n%in%max(n))
# 2. 거래금액의 평균이 가장 높은 지역은?
data2 %>% 
  group_by(지역) %>% 
  summarise(m=mean(거래금액)) %>% 
  arrange(-m)
# 3. 건축일로부터 가장 오래된 건물이 
#    거래된 지역과 금액은?
names(data2)
str(data2$건축년도)
data2 %>% 
  mutate(건물나이=2025-건축년도) %>% 
  filter(건물나이%in%min(건물나이)) %>% 
  select(지역,거래금액,건물나이)
names(data2)
# 4. 거래금액이 가장 큰 건물
data2 %>% 
  arrange(-거래금액) %>% 
  select(지역,단지명) %>% 
  head(5)

data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day3-main/data_day3 (2)/환경부 국립환경과학원_대기오염도 확정자료_20221231.csv",fileEncoding = 'cp949')
head(data)
summary(data)
# -999를 제거
#1.오존의 평균을 얼마인가요?
library(dplyr)
data %>% 
  filter(오존>=0) %>% 
  summarise(mean(오존))
#2.오존의 최댓값이 관측된 측정소는 어디인가요?
data %>% 
  filter(오존>=0,오존%in%max(오존)) %>% 
  select(시도,측정소명,오존)
#3.서울의 평균 미세먼지 농도는 몇인가요?
unique(data$시도)
data %>% 
  filter(미세먼지>=0,시도%in%"서울") %>% 
  summarise(mean(미세먼지))
#4.도시별 초미세먼지의 평균과 분산을 구하세요.
data %>% 
  filter(초미세먼지>=0) %>% 
  group_by(도시) %>% 
  summarise(mean(초미세먼지),var(초미세먼지)) %>% 
  data.frame()
#5.시도별 측정소별 미세먼지 평균의 상위10개 확인
data %>%
  filter(미세먼지>=0) %>% 
  group_by(시도,측정소명) %>% 
  summarise(pm10=mean(미세먼지)) %>% 
  arrange(-pm10) %>% 
  head(10)
#6.월별 아황산가스의 평균
head(data)
data$측정일시[1]
data %>% 
  mutate(month=substr(측정일시,6,7)) %>% 
  group_by(month) %>% 
  filter(아황산가스>=0) %>% 
  summarise(m=mean(아황산가스)) %>% 
  arrange(-m)

data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day3-main/환경부 국립환경과학원_시간단위 수온관측 자료_20141231.csv",
              fileEncoding = 'cp949')
head(data)
summary(data)
#1.수온의 결측치를 제거한 data2 생성
data %>% 
  filter(!is.na(수온))->data2
summary(data2)
#2.년도별 월별 수온의 평균
data2 %>% 
  mutate(year=substr(측정일시,1,4),
         month=substr(측정일시,6,7)) %>% 
  group_by(year,month) %>% 
  summarise(m=mean(수온)) %>% 
  arrange(-m)
#3.수온의 결측치를 선형보간한 data3 생성
library(zoo)
data %>% 
  mutate(수온=na.approx(수온))->data3
summary(data3)
#4.시간별 수온의 평균
data3 %>% 
  mutate(hour=substr(측정일시,12,13)) %>% 
  group_by(hour) %>% 
  summarise(수온=mean(수온)) %>% 
  arrange(-수온)



library(readxl)
data=read_excel("C:/Users/PC00/Desktop/EHRD-2025-day3-main/data_day3 (2)/한국환경공단_에어코리아_최종확정 측정자료_20181231.xlsx")
summary(data)
head(data)
names(data)
#1.지역별 PM10의 평균을 구하세요.
data %>% 
  group_by(지역) %>% 
  summarise(pm10=mean(PM10,na.rm=T)) %>% 
  data.frame() %>% 
  head
#2.월별 O3의 평균을 구하세요.
data %>% 
  mutate(month=substr(측정일시,5,6)) %>% 
  group_by(month) %>% 
  summarise(o3=mean(O3,na.rm=T))
#3.PM25의 수치가 가장 높게나온 일시와 측정소
data %>% 
  filter(PM25%in%max(PM25,na.rm=T)) %>% 
  select(측정일시,측정소명)

data[data$PM25%in%max(data$PM25,na.rm=T),
     c("측정일시","측정소명")]
#4.PM10이 50이상이면 경보, 아니면 안전
#  발령 변수 생성. 경보의 발령 수
data %>% 
  mutate(발령=ifelse(PM10>=50,"경보","안전")) %>% 
  group_by(발령) %>% 
  summarise(n())

#5.계절별 pm25의 평균
data %>% 
  mutate(month=substr(측정일시,5,6),
         계절=ifelse(month%in%c("03","04","05"),"봄",
                   ifelse(month%in%paste0("0",6:8),"여름",
                          ifelse(month%in%c("09","10","11"),"가을","겨울")))) %>% 
  group_by(계절) %>% 
  summarise(mean(PM25,na.rm=T))
           
data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day3-main/인천광역시기온.csv",
              fileEncoding = 'cp949',encoding = 'utf-8')
head(data)
data %>% 
  group_by(지점) %>% 
  summarise(기온=mean(기온..C.))->data2
data2

st=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day3-main/관측지점.csv",
            fileEncoding = 'cp949',encoding = 'utf-8')
head(st)
st %>% 
  select(지점,위도,경도)->st2

data2
st2
data3=merge(data2,st2,by="지점")
data3
plot(data3$경도,data3$위도,
     col=as.factor(data3$지점),pch=16)

#1.ASOS_sample을 불러와서
# 지점별 평균기온을 구하세요
data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day3-main/ASOS_sample.csv",
              fileEncoding = 'cp949')
head(data)
data %>% 
  group_by(지점) %>% 
  summarise(평균기온=mean(평균기온..C.,na.rm=T))->data2
#2.stninfo를 불러와서
# 지점과 위도, 경도를 선택하세요
st=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day3-main/stnInfo_20171018091530.csv",
            fileEncoding = 'cp949')
head(st)
st %>% 
  select(지점,위도,경도)->st2
#3.두 결과 데이터를 합치세요.
data2
st2
data=merge(data2,st2,by="지점")
head(data)
#4.평균기온이 5이상이면 red
#  2.5이상이면 orange, 0이상이면 yellow
#  이외는 green의 col 변수생성
data %>% 
  mutate(col=ifelse(평균기온>=5,"red",
                    ifelse(평균기온>=2.5,"orange",
                           ifelse(평균기온>=0,"yellow","green"))))->data
head(data)
plot(data$경도,data$위도,
     col=data$col,pch=16)

library(maps)
map('world')
map('world','South Korea',add=T)

tc.data2=data
names(tc.data2)=c("지점","avg","lat","lon","col")
library(sp)
library(raster)
library(gstat)

coordinates(tc.data2) <-c("lon","lat")
grd <- as.data.frame(spsample(tc.data2, "regular", n=50000))
names(grd)<- c("lon","lat")
coordinates(grd) <- c("lon","lat")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

tc.idw <- gstat::idw(avg ~ 1, tc.data2, newdata=grd, idp=2.0)
r <- raster(tc.idw)
plot(r)
map('world','South Korea',add=T)
contour(r, add=T, lty=2)









