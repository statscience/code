library(maps)
library(ggmap)

map("world","south korea")
df=map_data("world","south korea")
head(df)
wifi=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day4-main/wifi.csv")
plot(wifi$lon,wifi$lat,col=as.factor(wifi$company),pch=16,cex=0.2)
map('world','South Korea',add=T)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

world = ne_countries(scale = "medium", returnclass = "sf")
ggplot(world) +
  geom_sf(fill = "lightgray", color = "white") +
  coord_sf(expand = FALSE) +
  labs(title = "World Map (rnaturalearth + ggplot2)") +
  theme_minimal()
head(world)

library(geodata)
kor0=gadm('KOR',level=3,path=tempdir())
plot(kor0)

data=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day3-main/data_day3 (2)/환경부 국립환경과학원_대기오염도 확정자료_20221231.csv",
              fileEncoding = 'cp949')

summary(data)
# 시도별 오존 농도의 평균
library(dplyr)
data %>% 
  filter(오존>=0) %>% 
  group_by(시도) %>% 
  summarise(오존=mean(오존))->data2

data2 %>% 
  mutate(col=ifelse(오존<=0.03,"blue",
                    ifelse(오존<=0.09,"green",
                           ifelse(오존<=0.15,"orange","red"))))->data2

data2
kor1=gadm('KOR',level=1,path=tempdir())
kor1$NAME_1

kor1$시도=c("부산","충북","충남","대구","대전",
          "강원","광주","경기","경북","경남","인천","제주",
          "전북","전남","세종","서울","울산")

data3=merge(kor1,data2,by="시도")
head(data3)
plot(data3,col=data3$col)
data2
head(data)

kor2=gadm('KOR',level=2,path=tempdir())
df=subset(kor2,kor2$NAME_1%in%
            c("Seoul","Gyeonggi-do","Incheon"))
plot(df)
df=subset(kor2,kor2$NAME_1%in%"Seoul")
plot(df)
df$NAME_2

head(data)
data %>% 
  filter(시도%in%"서울",미세먼지>=0) %>% 
  group_by(군구) %>% 
  summarise(미세먼지=mean(미세먼지))->data2
data2 %>% 
  mutate(
    col=ifelse(미세먼지>=30,"red","blue")
    )->data2

data2$군구
df$NAME_2
df$군구=c("도봉구","동대문구",rep(NA,23))
data3=merge(df,data2,by="군구",all.x=T)
plot(data3,col=data3$col)


library(leaflet)
m <- leaflet() %>% 
  addTiles()
m
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
devtools::install_github("ropensci/rnaturalearthhires")
kr = ne_states(country = "South Korea", returnclass = "sf")
# interactive leaflet map
leaflet(kr) %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(color = "blue",
              weight = 1,
              fillOpacity = 0.3,
              popup = ~name) %>%
  setView(lng = 127.8, lat = 35.8, zoom = 6)


wifi=read.csv("C:/Users/PC00/Desktop/EHRD-2025-day4-main/wifi.csv")
library(dplyr)
unique(wifi$company)
wifi %>% 
  mutate(col=ifelse(company%in%"KT","red",
                    ifelse(company%in%"LGU+","blue","green")))->wifi
head(wifi)
library(leaflet)
leaflet(wifi) %>% 
  addTiles() %>% 
  addCircles(lng = ~ lon, lat = ~ lat, color = ~col)


library(geodata)
kor2=gadm('KOR',level=2,path=tempdir())
plet(kor2,border="white")


library(XML)
library(RCurl)

url2<-getURL("https://www.weather.go.kr/w/observation/land/city-obs.do", .encoding="euc-kr")
tables<-as.data.frame(readHTMLTable(url2,encoding='UTF-8'))
names(tables)<-
  c("id","current","vis","cloud","l.cloud","Tcurrent","dew","sensible","prec","rh","dir","ws","hpa")
head(tables)


library("openair")
summaryPlot(mydata)


a<-2
b=10
a2<-3
a=c(10,25,11)
x="a"
x=a
x
data$col="red"

data[관측치,변수]
data[data$wind>=10,]
data[!is.na(data$pm10),]
data[,"pm10"]
data[,c("wind","pm10")]
data[,-c(2,5,6)]
data$wind
paste0("x",1:10)
substr(date,1,4)

library(dplyr)
data %>% 
  select(변수선택) %>% 
  filter(관측치조건) %>% 
  arrange(정렬) %>% 
  mutate(변수생성) %>% 
  group_by(집단별 통계량) %>% 
  summarise(통계값)

data %>% 
  select(-wind,-pm10) %>% 
  arrange(-wind,pm10) %>% 
  mutate(year=substr(date,1,4),
         col=ifelse(pm10>=50,"red",ifelse(조건,참,거짓))) %>% 
  filter(year%in%"2000") %>% 
  group_by(집단) %>% 
  summarise(평균오존=mean(오존,na.rm=T),max(),n(),var())

data$year=substr(data$date,1,4)

data=rbind(data1,data2)
data=merge(data1,data2,by="키")















