############### 統計諮詢EDA ################

getwd()
setwd("C:/Users/jessie/Downloads")

library(readxl)
data1223 <- read_xlsx('newdata1223.xlsx')

data1223

# data1223 <- data1223[complete.cases(data1223),]
data1223
colSums(is.na(data1223))

dim(data1223) ##### 19073 18
table(data1223$判決結果) #### 1: 18318,2:755


colnames(data1223)

class(data1223$判決結果)
# 
# lm_model <- glm(factor(判決結果)~.,data = data1223,family = "binomial")
# summary(lm_model)



data1223 <- data.frame(data1223)

names(data1223)[18] <- c("有無罪")
colnames(data1223)
full <- data1223
table(full$有無罪)

#### 1:有罪,2:無罪

##### 有無酒駕

# table(data1223$有無罪)
# levels(factor(data1223$有無罪))

library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(有無酒駕),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

library(dplyr)
for_show = full %>% group_by(有無酒駕) %>% summarise(length(有無酒駕))

for_show
###### 
library(ggplot2)
ggplot(ess2,aes(x = 有無酒駕,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無酒駕",y = "比例") +
  ggtitle("在有無酒駕下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(full$有無酒駕) ## 0:7322,1:11751
nrow(full[full$有無酒駕 == 1&full$有無罪 == 1,]) ## 11510
nrow(full[full$有無酒駕 == 0&full$有無罪 == 2,]) ## 514
11510/11751
514/7322

###### glm model
# glm_model <- glm(有無罪~.,data = full,family = "binomial")
# summary(glm_model)



##### 有無駕照

library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(有無駕照),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(有無駕照) %>% summarise(length(有無駕照))

for_show
###### 

ggplot(ess2,aes(x = 有無駕照,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無駕照",y = "比例") +
  ggtitle("在有無駕照下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))


table(full$有無駕照) ## 0:2968,1:16105
nrow(full[full$有無駕照 == 1&full$有無罪 == 1,]) ## 15412
nrow(full[full$有無駕照 == 0&full$有無罪 == 1,]) ## 2906
15412/16105
2906/2968

##### 有無證據

library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(有無證據),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(有無證據) %>% summarise(length(有無證據))

for_show

ggplot(ess2,aes(x = 有無證據,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無證據",y = "比例") +
  ggtitle("在有無證據下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(full$有無證據) ## 0:758,1:18315
nrow(full[full$有無證據 == 1&full$有無罪 == 1,]) ## 18290
nrow(full[full$有無證據 == 0&full$有無罪 == 1,]) ## 28
nrow(full[full$有無證據 == 0&full$有無罪 == 2,]) ## 730
18290/18315
28/758
730/758

##### 有無肇事逃逸
full$有無肇事逃逸
library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(有無肇事逃逸),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(有無肇事逃逸) %>% summarise(length(有無肇事逃逸))

for_show

ggplot(ess2,aes(x = 有無肇事逃逸,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無肇事逃逸",y = "比例") +
  ggtitle("在有無肇事逃逸下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(data1223$有無肇事逃逸)
table(full$有無肇事逃逸) ## 0:16842,1:2231
nrow(full[full$有無肇事逃逸 == 1&full$有無罪 == 1,]) ## 2042
nrow(full[full$有無肇事逃逸 == 0&full$有無罪 == 2,]) ## 566
2042/2231
566/16842


##### 有無累犯
full$是否累犯
library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(是否累犯),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(是否累犯) %>% summarise(length(是否累犯))

for_show

ggplot(ess2,aes(x = 是否累犯,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無累犯",y = "比例") +
  ggtitle("在有無累犯下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))


table(full$是否累犯) ## 0:14948,1:4125
nrow(full[full$是否累犯 == 1&full$有無罪 == 1,]) ## 4122
nrow(full[full$是否累犯 == 0&full$有無罪 == 1,]) ## 14196
nrow(full[full$是否累犯 == 0&full$有無罪 == 2,]) ##752
4122/4125
14196/14948
752/14948


##### 有無自首
full$是否自首
library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(是否自首),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(是否自首) %>% summarise(length(是否自首))

for_show

ggplot(ess2,aes(x = 是否自首,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無自首",y = "比例") +
  ggtitle("在有無自首下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(full$是否自首) ## 0:1221,1:17852
nrow(full[full$是否自首 == 1&full$有無罪 == 1,]) ## 17407
nrow(full[full$是否自首 == 0&full$有無罪 == 1,]) ## 911
nrow(full[full$是否自首 == 0&full$有無罪 == 2,]) ## 310
17407/17852
911/1221
310/1221


##### 有無服用非法藥物
full$是否使用藥物
library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(是否使用藥物),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(是否使用藥物) %>% summarise(length(是否使用藥物))

for_show

ggplot(ess2,aes(x = 是否使用藥物,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('否', '是'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "是否服用非法藥物",y = "比例") +
  ggtitle("在是否服用非法藥物下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(full$是否使用藥物) ## 0:9569,1:9504
nrow(full[full$是否使用藥物 == 1&full$有無罪 == 1,]) ## 9452
nrow(full[full$是否使用藥物 == 0&full$有無罪 == 1,]) ## 8866
nrow(full[full$是否使用藥物 == 0&full$有無罪 == 2,]) ## 703
9452/9504
8866/9569
703/9569

##### 有無超速
full$有無超速
library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(有無超速),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(有無超速) %>% summarise(length(有無超速))

for_show

ggplot(ess2,aes(x = 有無超速,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無超速",y = "比例") +
  ggtitle("在有無超速下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(full$有無超速) ## 0:18088,1:985
nrow(full[full$有無超速 == 1&full$有無罪 == 1,]) ## 881
nrow(full[full$有無超速 == 0&full$有無罪 == 1,]) ## 17437
nrow(full[full$有無超速 == 0&full$有無罪 == 2,]) ## 651
881/985
17437/18088
651/18088


##### 有無精神疾病
full$有無精神疾病
library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(有無精神疾病),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(有無精神疾病) %>% summarise(length(有無精神疾病))

for_show

ggplot(ess2,aes(x = 有無精神疾病,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無精神疾病",y = "比例") +
  ggtitle("在有無精神疾病下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(full$有無精神疾病) ## 0:19050,1:23
nrow(full[full$有無精神疾病 == 1&full$有無罪 == 1,]) ## 19
nrow(full[full$有無精神疾病 == 0&full$有無罪 == 1,]) ## 18299
nrow(full[full$有無精神疾病 == 0&full$有無罪 == 2,]) ## 751
19/23
18299/19050
751/19050


##### 有無減速
full$有無減速
library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(有無減速),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(有無減速) %>% summarise(length(有無減速))

for_show

ggplot(ess2,aes(x = 有無減速,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無減速",y = "比例") +
  ggtitle("在有無減速下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))


table(full$有無減速) ## 0:723,1:18350
nrow(full[full$有無減速 == 1&full$有無罪 == 1,]) ## 17661
nrow(full[full$有無減速 == 0&full$有無罪 == 1,]) ## 657
nrow(full[full$有無減速 == 0&full$有無罪 == 2,]) ## 66
17661/18350
657/723
66/723




###### glm model
# glm_model <- glm(有無罪~.,data = full,family = "binomial")
# summary(glm_model)






######## 有無罪 圓餅圖

guity_ornot <- data.frame(table(full$有無罪)) 
colnames(guity_ornot) <- c("guity","freq")
guity_ornot
ggplot(guity_ornot, aes (x="", y = freq, fill = factor(guity))) + 
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste(round(freq / sum(freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "guilty",
       x = NULL,
       y = NULL,
       title = "有無罪圓餅圖") + 
  scale_fill_discrete(name="有無罪",labels=c("有罪","無罪"))+
  coord_polar("y")

nrow(full) # 19073
table(full$有無罪)
755/19073


###### 有無過失傷害
full$有無過失傷害

full
library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(有無過失傷害),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

library(dplyr)
for_show = full %>% group_by(有無過失傷害) %>% 
  summarise(length(有無過失傷害))

for_show
###### 
library(ggplot2)
ggplot(ess2,aes(x = 有無過失傷害,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無過失傷害",y = "比例") +
  ggtitle("在有無過失傷害下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(full$有無過失傷害) ## 0:2674,1:16399
nrow(full[full$有無過失傷害 == 1&full$有無罪 == 1,]) ## 15690
nrow(full[full$有無過失傷害 == 0&full$有無罪 == 1,]) ## 2628
nrow(full[full$有無過失傷害 == 0&full$有無罪 == 2,]) ## 46

15690/16399
2628/2674
46/2674

###### 有無過失致死
library(plyr)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(有無過失致死),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

library(dplyr)
for_show = full %>% group_by(有無過失致死) %>% 
  summarise(length(有無過失致死))

for_show
###### 
library(ggplot2)
ggplot(ess2,aes(x = 有無過失致死,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "有無過失致死",y = "比例") +
  ggtitle("在有無過失致死下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(full$有無過失致死) ## 0:16404,1:2669
nrow(full[full$有無過失致死 == 1&full$有無罪 == 1,]) ## 2576
nrow(full[full$有無過失致死 == 0&full$有無罪 == 1,]) ## 15742
nrow(full[full$有無過失致死 == 0&full$有無罪 == 2,]) ## 662

2576/2669
15742/16404


###### 是否有小客車:19073(全部)

##### 是否有貨車或大客車
full$是否有貨車或大客車
library(plyr)
table(full$是否有貨車或大客車)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(是否有貨車或大客車),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(是否有貨車或大客車) %>% summarise(length(是否有貨車或大客車))

for_show

ggplot(ess2,aes(x = 是否有貨車或大客車,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "是否有貨車或大客車",y = "比例") +
  ggtitle("在是否有貨車或大客車下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(data1223$是否有貨車或大客車) ## 0:17668, 1:1405
table(full$是否有貨車或大客車) ## 0:17668,1:1405
nrow(full[full$是否有貨車或大客車 == 1&full$有無罪 == 1,]) ## 1263
nrow(full[full$是否有貨車或大客車 == 0&full$有無罪 == 2,]) ## 613
1263/1405
613/17668



###### 是否有小客車:19073(全部)

##### 是否有機車
full$是否有機車
library(plyr)
table(full$是否有機車)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(是否有機車),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(是否有機車) %>% summarise(length(是否有機車))

for_show

ggplot(ess2,aes(x = 是否有機車,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "是否有機車",y = "比例") +
  ggtitle("在是否有機車下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(data1223$是否有機車) ## 0:10395, 1:8678
table(full$是否有機車) ## 0:10395,1:8678
nrow(full[full$是否有機車 == 1&full$有無罪 == 1,]) ## 8099
nrow(full[full$是否有機車 == 0&full$有無罪 == 2,]) ## 176
8099/8678
176/10395


##### 是否有腳踏車
full$是否有腳踏車
library(plyr)
table(full$是否有腳踏車)
#----統計起來為了做長條圖----#
ess2 = ddply(full,.(是否有腳踏車),function(.){
  res = prop.table(table(factor(.$有無罪)))
  res2 = table(factor(.$有無罪))
  data.frame(lab=names(res), y=c(res),yy =c(res2))
})
detach("package:plyr", unload=TRUE)

for_show = full %>% group_by(是否有腳踏車) %>% 
  summarise(length(是否有腳踏車))

for_show

ggplot(ess2,aes(x = 是否有腳踏車,y=y,fill = lab))+
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels=c('無', '有'))+
  annotate("text", x = c(0,1), y = -0.1, label = c('無', '有'))+
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=20,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "是否有腳踏車",y = "比例") +
  ggtitle("在是否有腳踏車下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

table(data1223$是否有腳踏車) ## 0:17910, 1:1163
table(full$是否有腳踏車) ## 0:17910,1:1163
nrow(full[full$是否有腳踏車 == 1&full$有無罪 == 1,]) ## 1105
nrow(full[full$是否有腳踏車 == 0&full$有無罪 == 2,]) ## 697
1105/1163
697/17910


##### 車輛類型
# full$車輛類型
# library(plyr)
# table(full$車輛類型)
# #----統計起來為了做長條圖----#
# ess2 = ddply(full,.(車輛類型),function(.){
#   res = prop.table(table(factor(.$有無罪)))
#   res2 = table(factor(.$有無罪))
#   data.frame(lab=names(res), y=c(res),yy =c(res2))
# })
# detach("package:plyr", unload=TRUE)
# 
# for_show = full %>% group_by(車輛類型) %>% 
#   summarise(length(車輛類型))
# 
# for_show
# 
ggplot(ess2,aes(x = 車輛類型,y=y,fill = lab))+
  geom_bar(stat = "identity") +
  theme_classic(base_size = 16)+
  geom_text(mapping = aes(label = sprintf("%.2f%%",y*100)),
            size = 4, colour = 'black', position = position_stack(vjust = 0.5))+
  scale_x_continuous(breaks=seq(0,5,1)) +
  theme(axis.text.y=element_text(face="bold",size=15,color="#333333"))+##調整y軸字型
  theme(axis.text.x=element_text(face="bold",size=10,angle=360,color="#333333"))+#x軸字型(筆數的數字)
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1) ,labels =c("0%","25%","50%","75%","100%"))+
  scale_fill_discrete(name="判決結果",labels=c("有罪","無罪"))+
  labs( x= "車輛類型",y = "比例") +
  ggtitle("在不同車輛類型下有無罪相對比例長條圖") +
  theme(plot.title = element_text(hjust = 0.5))

# table(data1223$是否有腳踏車) ## 0:17910, 1:1163
# table(full$是否有腳踏車) ## 0:17910,1:1163
# nrow(full[full$是否有腳踏車 == 1&full$有無罪 == 1,]) ## 1105
# nrow(full[full$是否有腳踏車 == 0&full$有無罪 == 2,]) ## 697
# 1105/1163
# 697/17910




################# 有無罪甜甜圈圖
nrow(full)
########
# # friends_palette <- c("cyan", "deeppink")
# guilty_ornot <- data.frame(table(full$有無罪))
# guilty_ornot
# colnames(guilty_ornot) <- c("guilty","freq")
# class(guilty_ornot$guilty)
# colnames(guilty_ornot)
# 
# guilty_ornot %>%
#   group_by(guilty) %>%
#   summarize(n_lines = sum(freq)) %>%
#   mutate(prop_lines = (n_lines/sum(n_lines))*100) %>%
#   ggplot(aes(x = 2, y = prop_lines, fill = guilty)) +
#   geom_bar(stat = "identity", color = "white") + 
#   coord_polar(theta = "y", start = 0) + 
#   geom_text(aes(label = paste0(round(prop_lines, 1), "%")), color = "black", 
#             position = position_stack(vjust = 0.5), fontface = "bold") +
#   labs(fill = "") +
#   scale_fill_discrete(name="判斷結果",labels=c("有罪","無罪"))+
#   annotate("text", x = 0, y = 0, label = "有無罪甜甜圈圖",
#            size = 5) +
#   theme_void() + 
#   xlim(0, 2.5)

# ?annotate



######### 有無酒駕
full$有無酒駕
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$有無酒駕,dnn = c("有無罪","有無酒駕")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無酒駕) <- list("酒駕" = "1","無" = "0")

car_type
class(car_type$有無酒駕)
PD = car_type %>% group_by(有無罪, 有無酒駕) 
PD
factor(levels(PD$有無酒駕)) 
PD

####### 依有無罪比例
PieDonut(PD, aes(有無罪, 有無酒駕, count=Freq), 
         title = "有無罪 vs. 有無酒駕(依有無罪比例)",
         showRatioDonut = TRUE,
         labelposition = 0,
         showRatioThreshold = 0.0001,
         labelpositionThreshold=0.5,
         explode = 2,
         ratioByGroup = TRUE,
         start = 2*pi/3,
         explodeDonut=TRUE,selected = c(3,4),r1 = 1.0)

####### 全部比例
PieDonut(PD, aes(有無罪, 有無酒駕, count=Freq), 
         title = "有無罪 vs. 有無酒駕(所有比例)",
         showRatioDonut = TRUE,start=3*pi/2,
         labelposition = 0.05,
         showRatioThreshold = 0.0001,
         labelpositionThreshold=0.2,
         explode = 1,
         ratioByGroup = FALSE)


######### 有無駕照

full$有無駕照
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$有無駕照,dnn = c("有無罪","有無駕照")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無駕照) <- list("駕照" = "1","無" = "0")

car_type

PieDonut(car_type, aes(有無罪, 有無駕照, count=Freq), 
         title = "有無罪 vs. 有無駕照(依有無罪比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0.05,
         showRatioThreshold = 0.0001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 有無駕照, count=Freq), 
         title = "有無罪 vs. 有無駕照(所有比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0.05,
         showRatioThreshold = 0.0001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)


######### 有無證據

full$有無證據
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$有無證據,dnn = c("有無罪","有無證據")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無證據) <- list("有" = "1","無" = "0")

car_type

PieDonut(car_type, aes(有無罪, 有無證據, count=Freq), 
         title = "有無罪 vs. 有無證據(依有無罪比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0.05,
         showRatioThreshold = 0.01,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 有無證據, count=Freq), 
         title = "有無罪 vs. 有無證據(所有比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0.05,
         showRatioThreshold = 0.01,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)


######### 有無肇事逃逸

full$有無肇事逃逸
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$有無肇事逃逸,dnn = c("有無罪","有無肇事逃逸")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無肇事逃逸) <- list("肇逃" = "1","無" = "0")

car_type
# PD = car_type %>% group_by(有無罪, 有無肇事逃逸) %>% summarise(n = sum(Freq))
# PD
# factor(levels(PD$有無酒駕)) 
# PD
PieDonut(car_type, aes(有無罪, 有無肇事逃逸, count=Freq), 
         title = "有無罪 vs. 有無肇事逃逸(依有無罪比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.05,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 有無肇事逃逸, count=Freq), 
         title = "有無罪 vs. 有無肇事逃逸(所有比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.05,
         showRatioThreshold = 0.00000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)




######### 有無累犯

full$有無累犯
library(ggplot2)
library(webr)
library(dplyr)

car_type <- as.data.frame(table(full$有無罪,full$是否累犯,dnn = c("有無罪","有無累犯")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無累犯) <- list("累犯" = "1","否" = "0")

car_type
# PD = car_type %>% group_by(有無罪, 有無累犯) %>% summarise(n = sum(Freq))
# PD
# factor(levels(PD$有無酒駕)) 
# PD
PieDonut(car_type, aes(有無罪, 有無累犯, count=Freq), 
         title = "有無罪 vs. 是否累犯(依有無罪比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0.0,
         showRatioThreshold = 0.01,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 有無累犯, count=Freq), 
         title = "有無罪 vs. 是否累犯(所有比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0,
         showRatioThreshold = 0.01,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)


######### 有無自首

full$有無自首
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$是否自首,dnn = c("有無罪","是否自首")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$是否自首) <- list("自首" = "1","否" = "0")

car_type
# PD = car_type %>% group_by(有無罪, 有無自首) %>% summarise(n = sum(Freq))
# PD
# factor(levels(PD$有無酒駕)) 
# PD
PieDonut(car_type, aes(有無罪, 是否自首, count=Freq), 
         title = "有無罪 vs. 是否自首(依有無罪比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 是否自首, count=Freq), 
         title = "有無罪 vs. 是否自首(所有比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)


######### 有無服用非法藥物

full$是否使用藥物
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$是否使用藥物,dnn = c("有無罪","是否使用藥物")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$是否使用藥物) <- list("藥物" = "1","否" = "0")

car_type
# PD = car_type %>% group_by(有無罪, 有無服用非法藥物) %>% summarise(n = sum(Freq))
# PD
# factor(levels(PD$有無酒駕)) 
# PD
PieDonut(car_type, aes(有無罪, 是否使用藥物, count=Freq), 
         title = "有無罪 vs. 是否服用非法藥物(依有無罪比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 是否使用藥物, count=Freq), 
         title = "有無罪 vs. 是否服用非法藥物(所有比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)



######### 有無超速

full$有無超速
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$有無超速,dnn = c("有無罪","有無超速")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無超速) <- list("超速" = "1","無" = "0")

car_type
# PD = car_type %>% group_by(有無罪, 有無超速) %>% summarise(n = sum(Freq))
# PD
# factor(levels(PD$有無酒駕)) 
# PD
PieDonut(car_type, aes(有無罪, 有無超速, count=Freq), 
         title = "有無罪 vs. 有無超速(依有無罪比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0.001,
         showRatioThreshold = 0.001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 有無超速, count=Freq), 
         title = "有無罪 vs. 有無超速(所有比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0.01,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)




######### 有無精神疾病

full$有無精神疾病
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$有無精神疾病,dnn = c("有無罪","有無精神疾病")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無精神疾病) <- list("疾病" = "1","無" = "0")

car_type
# PD = car_type %>% group_by(有無罪, 有無精神疾病) %>% summarise(n = sum(Freq))
# PD
# factor(levels(PD$有無酒駕)) 
# PD
PieDonut(car_type, aes(有無罪, 有無精神疾病, count=Freq), 
         title = "有無罪 vs. 有無精神疾病(依有無罪比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0.001,
         showRatioThreshold = 0.000001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 有無精神疾病, count=Freq), 
         title = "有無罪 vs. 有無精神疾病(所有比例)",
         showRatioDonut = TRUE,start=pi/4,
         labelposition = 0.01,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)


######### 有無減速

full$有無減速
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$有無減速,dnn = c("有無罪","有無減速")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無減速) <- list("減速" = "1","無" = "0")

car_type
# PD = car_type %>% group_by(有無罪, 有無減速) %>% summarise(n = sum(Freq))
# PD
# factor(levels(PD$有無酒駕)) 
# PD
PieDonut(car_type, aes(有無罪, 有無減速, count=Freq), 
         title = "有無罪 vs. 有無減速(依有無罪比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.1,
         showRatioThreshold = 0.000001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 有無減速, count=Freq), 
         title = "有無罪 vs. 有無減速(所有比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)


######### 有無過失傷害

full$有無過失傷害
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$有無過失傷害,dnn = c("有無罪","有無過失傷害")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無過失傷害) <- list("過失傷害" = "1","無" = "0")

car_type
PieDonut(car_type, aes(有無罪, 有無過失傷害, count=Freq), 
         title = "有無罪 vs. 有無過失傷害(依有無罪比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.1,
         showRatioThreshold = 0.000001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 有無過失傷害, count=Freq), 
         title = "有無罪 vs. 有無過失傷害(所有比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)


######### 有無過失致死

full$有無過失致死
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$有無過失致死,dnn = c("有無罪","有無過失致死")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$有無過失致死) <- list("過失致死" = "1","無" = "0")

car_type
PieDonut(car_type, aes(有無罪, 有無過失致死, count=Freq), 
         title = "有無罪 vs. 有無過失致死(依有無罪比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.1,
         showRatioThreshold = 0.000001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 有無過失致死, count=Freq), 
         title = "有無罪 vs. 有無過失致死(所有比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)


######### 是否有貨車或大客車

full$是否有貨車或大客車
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$是否有貨車或大客車,
                                dnn = c("有無罪","是否有貨車或大客車")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$是否有貨車或大客車) <- list("是" = "1","否" = "0")

car_type
PieDonut(car_type, aes(有無罪, 是否有貨車或大客車, count=Freq), 
         title = "有無罪 vs. 是否有貨車或大客車(依有無罪比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.1,
         showRatioThreshold = 0.000001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 是否有貨車或大客車, count=Freq), 
         title = "有無罪 vs. 是否有貨車或大客車(所有比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)


######### 是否有機車

full$是否有機車
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$是否有機車,
                                dnn = c("有無罪","是否有機車")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$是否有機車) <- list("機車" = "1","否" = "0")

car_type
PieDonut(car_type, aes(有無罪, 是否有機車, count=Freq), 
         title = "有無罪 vs. 是否有機車(依有無罪比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.1,
         showRatioThreshold = 0.000001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 是否有機車, count=Freq), 
         title = "有無罪 vs. 是否有機車(所有比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)

######### 是否有腳踏車

full$是否有腳踏車
library(ggplot2)
library(webr)
library(dplyr)
car_type <- as.data.frame(table(full$有無罪,full$是否有腳踏車,
                                dnn = c("有無罪","是否有腳踏車")))
car_type
levels(car_type$有無罪) <- list("有罪" = "1","無罪" = "2")
levels(car_type$是否有腳踏車) <- list("腳踏車" = "1","否" = "0")

car_type
PieDonut(car_type, aes(有無罪, 是否有腳踏車, count=Freq), 
         title = "有無罪 vs. 是否有腳踏車(依有無罪比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.1,
         showRatioThreshold = 0.000001,
         labelpositionThreshold=0.2,
         explode = 2)
PieDonut(car_type, aes(有無罪, 是否有腳踏車, count=Freq), 
         title = "有無罪 vs. 是否有腳踏車(所有比例)",
         showRatioDonut = TRUE,start=2*pi/3,
         labelposition = 0.01,
         showRatioThreshold = 0.000000001,
         labelpositionThreshold=0.2,
         explode = 2,ratioByGroup = FALSE)



######## 有無罪
my.df <- data.frame(table(full$有無罪))
my.df
colnames(my.df) <- c("判定結果","案件數")
my.df
levels(my.df$判定結果) <- list("有罪" = "1","無罪" = "2")

p <- ggplot(data = my.df, aes(x = 判定結果, y = 案件數,fill = 判定結果)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = 案件數), vjust = -0.2, size = 5,color = "black") +
  ggtitle("有無罪長條圖") +
  theme(plot.title = element_text(hjust = 0.5))
p





