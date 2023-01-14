##----------------------------------------------
## This program is for step2_mergedata_solar_V1_20191012_466920
##
## 2020.04.19   
##   set function : solar.direction
##  1.add (Az-AW) < 90. &  (Az-AW)  >-90   
##  2.fix elseif function
##
##  reference:
##  1.Principles of Solar Engineering, Third Edition P.69
##  
##  2020.04.19
##  1.add Pta degree
##
##  2020.05.04
##  1.add each direction month sum solar  
##  
##  2020.05.06
##  add
##  4.1 each hour output 
##  4.2 each month output 
##  4.3 average all month  output per title angle  
##  4.3 average select month  output per title angle  
##  4.4 For TMY month  output per title angle  
##
##  2020.05.07 
##  output sum & percent by S value 
##
##  2020.05.14
##  set title angle is by 0.1 
## 
##  2022.09.20 
##  change 
##                         by Lin,yung-ching  V1.2
##----------------------------------------------------------------      

## set path 
## ex:
##setwd("F:/研究論文/00.測站比值/466920台北-1080315")
#"F:/研究論文/00.測站比值/466920台北-1080315"
getwd()
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)
getwd()

#DPath="C:/Users/FENG CHUAN/Desktop/論文/database/YANG/"
#Ddir="467990-馬祖"
#stno="467990"


#DPath="E:/Google_drive/@program/ASHRAE_R/"
#DPath="D:/Google_drive/@program/ASHRAE_R/"
#Ddir="466920台北-1080315"
Ddir=getwd()
stno="467990"
#stno="467410"

# datapath=paste0(DPath,'/',Ddir) 

# setwd(datapath)
# getwd()

#####  ste1. read merge data 
inputname=paste0(stno,"_solar_data")
MyData_solar <- read.csv(paste0(inputname,".csv"),check.names = F)
#head(MyData_solar)
#summary(MyData_solar)
#####  ste2. read function   


solar.direction <- function(SH,DN,alpha,Az,BetaW=90,AW=0.,Ro=0.2){
  #-----------------------------------------------------------------------
  # SH:Global Horizontal Irradiance (GHI)
  # DN:Direct Normal Irradiance (DNI)
  # α:Solar altitude angle
  # Az:Solar azimuth angle
  # BetaW(β)  : Panel tilt angle
  # AW        : Panel azimuth angle
  # i        : Incident angle, is the angle of incidence of the beam radiation on the tilted surface
  #
  # IC: Solar Radiation on a Tilted Surface
  #  IC=IBC+IDC+IRC
  # IBC:the sum of components consisting of beam (Ib,c)
  # IDC:sky diffuse (Id,c)
  # IRC:ground reflected solar radiation (Ir,c)
  # IRC=Ro*(SH*sin(α) +DN )*sin( Az/2 )^2
  #  Ro=0.2  ---> ordinary ground or grass
  #  Ro=0.8  ---> snow-covered ground
  # reference:
  # Book: Principles of Solar Engineering, Third Edition P.68-69
  
  # da is the difference azimuth angle: Az-AW
  
  # Az=0
  # AW=seq(-180,180,by=60)
  
  #  IBc = DN*cos(i)
  #  cos(i)=cos(α*(pi/180))*cos((Az-AW)*(pi/180))*sin(BetaW*(pi/180))+sin(α*(pi/180))*cos(BetaW*(pi/180))   
  
  
  IBCh <- ifelse( !is.na(DN) & !is.na(SH) & alpha >=0 & (Az-AW) < 90. &  (Az-AW)  > -90  ,
                  DN*(cos(alpha*(pi/180))*cos((Az-AW)*(pi/180))*sin(BetaW*(pi/180)) ),
                  0.  )        
  #  IBCh <- ifelse( IBCh >= 0 , IBCh, 0.)  
  
  IBCv <- ifelse( !is.na(DN) & !is.na(SH) & alpha >=0 & BetaW <= 90.  ,
                  DN*(sin(alpha*(pi/180))*cos(BetaW*(pi/180))),
                  0.  )        
  #  IBCv <- ifelse( IBCv >= 0 , IBCv, 0.)  # let 
  
  IBC <- IBCh+IBCv
  
  
  
  IDC=SH*cos((BetaW/2)*(pi/180))^2
  
  IRC=Ro*(DN*sin(alpha*(pi/180)) +SH )*sin( (Az/2) *(pi/180) )^2
  
  
  IC=IBC+IDC+IRC
  
  my_list <- list("IC" = IC, "IBC" = IBC, "IDC" = IDC, "IRC" = IRC ,"AZ"= Az ,"AW"= AW, "da"= Az-AW ,
                  "BetaW"=BetaW ,"alpha"= alpha )
  
  # my_list <- list( "IC" = IC, "IBC" = IBC, "IDC" = IDC, "IRC" = IRC )
  return(my_list)   
  
  
}


#####  ste3. caculation direction    

dir=seq(0,360-1,1)-180
# dir=seq(0,360-30,30)-180

#cdir <-  c("N","NNE","NE","ENE",
#           "E","ESE","SE","SSE",
#           "S","SSW","SW","WSW",
#           "W","WNW","NW","NNW")

cdir <-  paste0("d_",   sprintf("%03d", seq(0,360-1,1) )  )

Panel_dir <- data.frame(cdir, dir)
MyData <- MyData_solar[,c("yyyymmddhh","solar","SH","DN","alpha","Az")]


################################################################# 
##BetaW(β): Panel tilt angle

# Pta=seq(0,90,by=5)
#Pta=seq(0,165,by=15)
#Pta=seq(15,30,by=1)
Pta=seq(10,30,by=1) #...最佳角度
#sprintf("%03d", Pta )...傾角到個位數by=1
#sprintf("%5.1f", Pta )...傾角到小數點by=0.1

MyData$Pta <- Pta[1]
MyData.total <- MyData

#j=2
#length(Pta)

if (length(Pta) >= 2 ){
  for ( j in 2:length(Pta) ) {
    print(paste0(" N: " , j," angle:", Pta[j] ))
    MyData.add <- MyData
    MyData.add$Pta <- Pta[j]
    # head(MyData.add)
    # Adding Rows
    
    MyData.total <- rbind(MyData.total, MyData.add )
    
    #  head(MyData.total)
    #  summary(MyData.total)
    #  factor(MyData.total$Pta) 
  }
}

for( i in c(1:dim(Panel_dir)[1]) ){
  #  print(i)
  nline=dim(MyData.total)[2]+1
  print(paste("M: ",i," DIR:",Panel_dir[i,2]))
  
  sd<-solar.direction(SH=MyData.total$SH ,
                      DN=MyData.total$DN ,
                      alpha=MyData.total$alpha ,
                      Az=(MyData.total$Az-180) ,
                      AW=Panel_dir[i,2],   #dir[i],   # Panel_dir[i,2]
                      BetaW=MyData.total$Pta,
                      Ro=0.0)
  
  MyData.total <- data.frame(MyData.total, sd$IC )
  colnames(MyData.total)[nline] = cdir[i]
  print(dim(MyData.total))
}

#install.packages("dplyr")
library(plyr)       # for rename function 
#install.packages("plyr")    # alternative installation of the %>%
library(dplyr) 
library(ggplot2)

MyData.total_sum_df <-
  MyData.total %>%
  subset(select=c(Pta:d_359) ) %>%
  #  head()
  group_by( Pta  ) %>%
  summarise_all(list(sum), na.rm = T)


MyData.total_sum_df
sort(MyData.total_sum_df$Pta)
outfile=paste0( Ddir,"/", stno,"_MyData.total_V1_d10.Rdata"    )
save(MyData.total_sum_df, file = outfile )

library(tidyr)      # tidyr  gather function 


outtime=paste0( format( min( substr(MyData.total$yyyymmddhh,start=1,stop=8)  ) )," to ",
                format( max(  substr(MyData.total$yyyymmddhh,start=1,stop=8)) ) ) 

Panel_dir$limits = sprintf("%03d", seq(0,360-1,1) )
Panel_dir$labels = ifelse( seq(1,dim(Panel_dir)[1] ) %% 3 ==  1 ,
                           Panel_dir$limits , "" )

MyData.total_sum_df %>%
  gather( key = "var", value = "value", -Pta ) %>%
  mutate( D    = matrix(unlist( strsplit( var,split="_",fixed=T) ),ncol=2,byrow=T)[,2]) %>%
  #  str()
  # head()
  ggplot()+
  geom_raster(aes(x = D, y = Pta, fill = value) ) + 
  scale_fill_gradientn(colours=c("yellow","red")) +
  labs(x = 'Direction', y = 'Panel tilt angle' , title = paste0( "stno: ", stno,' Direction VS Pta \n ',outtime) ) +
  theme(plot.title=element_text(hjust = 0.5,face="bold",size=20)  , #改標題字體位置  +
        axis.text=element_text(angle = 0,size=10,color="black",hjust=0.5),
        axis.title=element_text(size=20,face="bold") )+
  scale_x_discrete(
    limits = Panel_dir$limits,
    labels = Panel_dir$labels
  )  
# --------------
MyData.total_sum_df %>%
  gather( key = "var", value = "value", -Pta ) %>%
  mutate( D = matrix(unlist( strsplit( var,split="_",fixed=T) ),ncol=2,byrow=T)[,2]) %>%
  mutate( D = as.numeric(D) ) %>%
  max()



MyData.total_sum_df %>%
  gather( key = "var", value = "value", -Pta ) %>%
  mutate( D = matrix(unlist( strsplit( var,split="_",fixed=T) ),ncol=2,byrow=T)[,2]) %>%
  mutate( D = as.numeric(D) ) %>%
  # str()
  ggplot()+
  geom_raster(aes(x = D, y = Pta, fill = value) ) + 
  scale_fill_gradientn(colours=c("yellow","red")) +
  labs(x = 'Direction', y = 'Panel tilt angle' , title = paste0( "stno: ", stno,' Direction VS Pta \n ',outtime) ) +
  theme(plot.title=element_text(hjust = 0.5,face="bold",size=20)  , #改標題字體位置  +
        axis.text=element_text(angle = 0,size=14,color="black",hjust=0.5),
        axis.title=element_text(size=20,face="bold") ) +
  scale_x_continuous(breaks = seq(from = 0, to =360, by = 60))

max(MyData.total_sum_df)
max(MyData.total_sum_df)/12



#     End  #####################################################################















































#####  ste3-1. Set col order in R

MyData.hour <- MyData.total[,c("yyyymmddhh","solar","Pta",
                               "S","SSW","SW","WSW",
                               "W","WNW","NW","NNW",
                               "N","NNE","NE","ENE",
                               "E","ESE", "SE","SSE" )]


MyData.hour$mm <- substr(MyData.hour$yyyymmddhh, 5, 6)
MyData.hour$dd <- substr(MyData.hour$yyyymmddhh, 7, 8)
MyData.hour$hh <- substr(MyData.hour$yyyymmddhh, 9,10)

###############################   For each month  one direction ,different tilt angle
# BetaW(β)  : Panel tilt angle
# BetaW=0.
sdir="S"
idir<-match(sdir,colnames(MyData.hour))
idir
#cindex = (MyData.hour$Pta == BetaW )
# cindex = (MyData.hour$Pta == 0 & MyData.hour$mm >=10 & MyData.hour$mm <= 17 )
#table(cindex)/sum( table(cindex) )
#table(cindex)["TRUE"]

#summary(MyData.hour[cindex,idir])

mo.Pta <-as.data.frame( with(MyData.hour, tapply(MyData.hour[,idir], list(mm, Pta), sum, na.rm = TRUE)) )
mo.Pta$solar <- with(MyData.hour, tapply(solar, list(mm, Pta), sum, na.rm = TRUE))[,1]

mo.Pta$mm=rownames(mo.Pta)
mo.Pta$so_0=mo.Pta$solar-mo.Pta$`0`
mo.Pta$so.0=mo.Pta$so_0/mo.Pta$solar

library("RColorBrewer")

#plot( 0,xlim = c(1,12), ylim=c( 0, max(mo.Pta[,1:(length(Pta)+1) ] ) ) ,
#      xlab="Month", ylab="solar" ,
#      main = paste(stno," vs month in ",sdir," direction  ")  ,
#      type = "n") #, type="n"
#color = brewer.pal(n = (length(Pta)+1), name = "RdBu")

#for(i in 1:(length(Pta)+1) ){
#  lines( mo.Pta$mm, mo.Pta[,i], type = "b", col=color[i], lty = i  , lwd = 3 )
#}

# Add a legend
#legend("topright", legend= colnames( mo.Pta[,1:( length(Pta)+1 )])   ,
#       col= color , lty=1:(  length(Pta)+1 ), lwd = 3 ) # , lty=1:2, cex=0.8

#cormo.Pta$`0`,mo.Pta$solar)
#length(Pta)

write.csv(mo.Pta, file = paste0(stno,"_",sdir,".mo.Pta.csv"), row.names=FALSE)


#####  ste4. output data    

output=paste0(stno,"_Tilted_Surface")

##  4.1 each hour output 

# saveRDS(MyData,paste0(output,"_hour.Rdata"))
# write.csv(MyData, file = paste0(output,"_hour.csv"), row.names=FALSE)

##  4.2 each month output 
# sprintf("%03d", MyData.hour$Pta)
# sprintf("%05.1f", MyData.hour$Pta)
#MyData.hour$yymmPta <- paste0(substr(MyData.hour$yyyymmddhh, 1, 6),"_",MyData.hour$Pta)

#MyData.hour$yymmPta <- paste0(substr(MyData.hour$yyyymmddhh, 1, 6),"_",sprintf("%03d", MyData.hour$Pta) )
MyData.hour$yymmPta <- paste0(substr(MyData.hour$yyyymmddhh, 1, 6),"_",sprintf("%05.1f", MyData.hour$Pta) )

solar = tapply(MyData.hour$solar, MyData.hour$yymmPta , sum ,na.rm=T )
S   = tapply(MyData.hour$S,   MyData.hour$yymmPta , sum ,na.rm=T )
SSW = tapply(MyData.hour$SSW, MyData.hour$yymmPta , sum ,na.rm=T )
SW  = tapply(MyData.hour$SW,  MyData.hour$yymmPta , sum ,na.rm=T )
WSW = tapply(MyData.hour$WSW, MyData.hour$yymmPta , sum ,na.rm=T )
W   = tapply(MyData.hour$W,   MyData.hour$yymmPta , sum ,na.rm=T )
WNW = tapply(MyData.hour$WNW, MyData.hour$yymmPta , sum ,na.rm=T )
NW  = tapply(MyData.hour$NW,  MyData.hour$yymmPta , sum ,na.rm=T )
NNW = tapply(MyData.hour$NNW, MyData.hour$yymmPta , sum ,na.rm=T )
N   = tapply(MyData.hour$N,   MyData.hour$yymmPta , sum ,na.rm=T )
NNE = tapply(MyData.hour$NNE, MyData.hour$yymmPta , sum ,na.rm=T )
NE  = tapply(MyData.hour$NE,  MyData.hour$yymmPta , sum ,na.rm=T )
ENE = tapply(MyData.hour$ENE, MyData.hour$yymmPta , sum ,na.rm=T )
E   = tapply(MyData.hour$E,   MyData.hour$yymmPta , sum ,na.rm=T )
ESE = tapply(MyData.hour$ESE, MyData.hour$yymmPta , sum ,na.rm=T )
SE  = tapply(MyData.hour$SE,  MyData.hour$yymmPta , sum ,na.rm=T )
SSE = tapply(MyData.hour$SSE, MyData.hour$yymmPta , sum ,na.rm=T )


MyData.Month <-data.frame( solar , 
                           S,SSW,SW,WSW,
                           W,WNW,NW,NNW,
                           N,NNE,NE,ENE,
                           E,ESE,SE,SSE )
#head(MyData.Month)

MyData.Month$yyyymm <- substr(rownames(MyData.Month), 1, 6)
MyData.Month$mm <- substr(rownames(MyData.Month), 5, 6)
#MyData.Month$Pta <- as.numeric(substr(rownames(MyData.Month), 8, 10))
MyData.Month$Pta <- as.numeric(substr(rownames(MyData.Month), 8, 12))
#head(MyData.Month)
str(MyData.Month)

MyData.Month$stno <- stno

MyData.Month <- MyData.Month[,c("stno","yyyymm","solar","Pta",
                                "S","SSW","SW","WSW",
                                "W","WNW","NW","NNW",
                                "N","NNE","NE","ENE",
                                "E","ESE", "SE","SSE" )]


saveRDS(MyData.Month,paste0(output,"_month.Rdata"))
write.csv(MyData.Month, file = paste0(output,"_month.csv"), row.names=FALSE)

##############  This is for each month 

summary(MyData.Month)


##  4.3 average all month  output per title angle  
#apply(MyData.Month$s,1,length)

#str(MyData.Month)

setave=c("solar",
         "S","SSW","SW","WSW",
         "W","WNW","NW","NNW",
         "N","NNE","NE","ENE",
         "E","ESE", "SE","SSE" )
setave

Ptafactor<-as.factor(MyData.Month$Pta)
#levels(Ptafactor)[1:5]
#length(levels(Ptafactor))

my.row <-c("Pta" ,setave)
my.array <- array(NA, dim=c(length(levels(Ptafactor)),length(my.row)))
# my.array <- matrix(NA, nrow = length(levels(Ptafactor)), ncol = length(my.row) )
# my.dataframe <- data.frame(Pta=levels(Ptafactor))

#levels(Ptafactor)
#length(levels(Ptafactor))

colnames(my.array)<-my.row
my.array.per<-my.array
#str(my.array.per)


for( i in 1:length(levels(Ptafactor)) ){
  
  print(paste0(i," ->",levels(Ptafactor)[i])  )
  datad <- colSums (MyData.Month[ MyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  # rownames(datad) <- levels(Ptafactor)[i]
  # print(datad)
  
  my.array[i,1]=levels(Ptafactor)[i]
  my.array[i,2:length(my.row)]=colSums (MyData.Month[ MyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  my.array.per[i,1]=levels(Ptafactor)[i]
  
  # my.array.per[i,2:length(my.row)]=( my.array[i,2:length(my.row)] / my.array[i,3] )
  my.array.per[i,2:length(my.row)]=as.character( as.numeric(my.array[i,2:length(my.row)]) / as.numeric(my.array[i,3]) )
  
}  

my.array <- cbind(my.array,c(stno))
colnames(my.array)[length(my.row)+1]="stno"

my.array.per <- cbind(my.array.per,c(stno))
colnames(my.array.per)[length(my.row)+1]="stno"


write.csv(my.array.per, file = paste0(output,"_all_month_percent.csv"), row.names=FALSE)
write.csv(rbind(my.array,my.array.per), file = paste0(output,"_all_month_sum&percent.csv"), row.names=FALSE)

##  4.3 average select month  output per title angle  

MyData.Month$mm <- as.numeric(substr(rownames(MyData.Month), 5, 6))

# select month start and end month
smo=4
emo=11

# MyData.Month$mm >= smo & MyData.Month$mm <= emo 
# MyData.Month[MyData.Month$mm >= smo & MyData.Month$mm <= emo,]
sMyData.Month <- MyData.Month[MyData.Month$mm >= smo & MyData.Month$mm <= emo,]
#dim(sMyData.Month)
#str(sMyData.Month)
#head(sMyData.Month)

for( i in 1:length(levels(Ptafactor)) ){
  
  print(paste0(i," ->",levels(Ptafactor)[i])  )
  datad <- colSums (sMyData.Month[ sMyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  my.array[i,1]=levels(Ptafactor)[i]
  my.array[i,2:length(my.row)]=colSums (sMyData.Month[ sMyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  my.array.per[i,1]=levels(Ptafactor)[i]
  my.array.per[i,2:length(my.row)]=as.character( as.numeric(my.array[i,2:length(my.row)]) / as.numeric(my.array[i,3]) )
  
}  


write.csv(my.array.per, file = paste0(output,"_",smo,"to",emo,"_month_percent.csv"), row.names=FALSE)
write.csv(rbind(my.array,my.array.per), file = paste0(output,"_",smo,"to",emo,"_month_sum&percent.csv"), row.names=FALSE)




# select month start and end month
smo=5
emo=10

sMyData.Month <- MyData.Month[MyData.Month$mm >= smo & MyData.Month$mm <= emo,]
for( i in 1:length(levels(Ptafactor)) ){
  
  print(paste0(i," ->",levels(Ptafactor)[i])  )
  datad <- colSums (sMyData.Month[ sMyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  my.array[i,1]=levels(Ptafactor)[i]
  my.array[i,2:length(my.row)]=colSums (sMyData.Month[ sMyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  my.array.per[i,1]=levels(Ptafactor)[i]
  my.array.per[i,2:length(my.row)]=as.character( as.numeric(my.array[i,2:length(my.row)]) / as.numeric(my.array[i,3]) )
  
}  
write.csv(my.array.per, file = paste0(output,"_",smo,"to",emo,"_month_percent.csv"), row.names=FALSE)
write.csv(rbind(my.array,my.array.per), file = paste0(output,"_",smo,"to",emo,"_month_sum&percent.csv"), row.names=FALSE)

##  4.4 For TMY month  output per title angle  
# exam:  466920_TMYYYYMM.csv
# data from step4_TMY3YYMM_format20200305V4.R

MyData_TMY <- read.csv(file=paste0(stno,"_TMYYYYMM.csv"), header=T, sep=",",skip = 0)  
TMYyyyymm<-paste0(MyData_TMY$yyyy, sprintf("%02d", MyData_TMY$mm))


# select TMY month
# ex :c("a","b","c","d") %in% c("a","b","c")
# MyDataMonth$yyyymm %in% TMYyyyymm

#MyDataMonth$yyyymm >= smo & MyDataMonth$mm <= emo
sMyData.Month <- MyData.Month[(MyData.Month$yyyymm %in% TMYyyyymm) , ]


for( i in 1:length(levels(Ptafactor)) ){
  
  print(paste0(i," ->",levels(Ptafactor)[i])  )
  datad <- colSums (sMyData.Month[ sMyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  my.array[i,1]=levels(Ptafactor)[i]
  my.array[i,2:length(my.row)]=colSums (sMyData.Month[ sMyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  my.array.per[i,1]=levels(Ptafactor)[i]
  my.array.per[i,2:length(my.row)]=as.character( as.numeric(my.array[i,2:length(my.row)]) / as.numeric(my.array[i,3]) )
  
}  

write.csv(my.array.per, file = paste0(output,"_TMY_month_percent.csv"), row.names=FALSE)
write.csv(rbind(my.array,my.array.per), file = paste0(output,"_TMY_month_sum&percent.csv"), row.names=FALSE)


# select month start and end month
smo=5
emo=10

TMyData.Month <- sMyData.Month[sMyData.Month$mm >= smo & sMyData.Month$mm <= emo,]
for( i in 1:length(levels(Ptafactor)) ){
  
  print(paste0(i," ->",levels(Ptafactor)[i])  )
  datad <- colSums (TMyData.Month[ TMyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  my.array[i,1]=levels(Ptafactor)[i]
  my.array[i,2:length(my.row)]=colSums (TMyData.Month[ TMyData.Month$Pta == levels(Ptafactor)[i] ,setave], na.rm = FALSE, dims = 1)
  my.array.per[i,1]=levels(Ptafactor)[i]
  my.array.per[i,2:length(my.row)]=as.character( as.numeric(my.array[i,2:length(my.row)]) / as.numeric(my.array[i,3]) )
  
}  
write.csv(my.array.per, file = paste0(output,"_",smo,"to",emo,"_TMY_month_percent.csv"), row.names=FALSE)
write.csv(rbind(my.array,my.array.per), file = paste0(output,"_",smo,"to",emo,"_TMY_month_sum&percent.csv"), row.names=FALSE)





















