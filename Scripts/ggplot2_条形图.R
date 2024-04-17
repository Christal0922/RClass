
# 数据准备 --------------------------------------------------------------------

data3_1 <- read.csv("Input/mydata/chap03/data3_1.csv",
                    fileEncoding = "GBK",
                    stringsAsFactors = T)
c1 <- c("D","E")
d2 <- diamonds[diamonds$color %in% c1,]
library(ggplot2)
library(RColorBrewer)
library(dplyr)
# 简单条形图添加注释文本 -------------------------------------------------------------

ggplot(diamonds,aes(x=cut,fill=cut))+
  geom_bar()+
  stat_count(aes(label=..count..),geom = 'text',vjust=-0.5)
------------------------------------------------
ggplot(data3_1,aes(x=性别,fill=性别))+
  geom_bar()+
  stat_count(aes(label=..count..),geom = 'text',vjust=-0.5)
----------------------------------------------
ggplot(diamonds,aes(x=cut))+
  geom_bar(stat = 'count')+
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5)
------------------
mytable <- with(diamonds,table(cut))
df <- as.data.frame(mytable)
ggplot(df,aes(x=cut,y=Freq,fill=cut))+
  geom_bar(stat = 'identity')
# 并列条形图添加注释文本 -------------------------------------------------------------------

ggplot(data3_1,aes(x=网购次数,fill=性别))+
  geom_bar(width = 0.5,position = position_dodge2(width = 0.6))+
  stat_count(aes(label=..count..),geom = 'text',
             position = position_dodge2(width = 0.6),
             vjust=-0.5)


ggplot(d2,aes(x=cut,fill=color))+
  geom_bar(width = 0.5,position = position_dodge2(width = 0.6))+
  stat_count(aes(label=..count..),geom = "text",
             position = position_dodge2(width = 0.6),
             vjust=-0.5,size=2)


# 堆叠条形图添加注释文本 -------------------------------------------------------------------

ggplot(data3_1,aes(x=网购次数,fill=性别))+
  geom_bar(width = 0.5,position = position_stack(vjust = 0.5))+
  stat_count(aes(label=..count..),geom = 'text',
             position = position_stack(vjust = 0.5),
             vjust=-0.5)   

ggplot(d2,aes(x=cut,fill=color))+
  geom_bar(width = 0.5,position = position_stack(vjust = 0.5))+
  stat_count(aes(label=..count..),geom = "text",
             position = position_stack(vjust = 0.5),
             vjust=-0.5,size=2)+
  scale_fill_brewer(palette = "Set3")


# 百分比堆叠图 ------------------------------------------------------------------
t1 <- table(data3_1$性别,data3_1$网购次数)
t2 <- as.data.frame(prop.table(t1,margin = 2))

ggplot()+
  geom_bar(data=data3_1,
           mapping=aes(x=网购次数,fill=性别,group=性别),
           width = 0.5,position = "fill")+
  geom_text(data = t2,mapping = aes(x=Var2,y=Freq,label=scales::percent(Freq),group=Var1),
            position = position_fill(0.5))+
  scale_y_continuous(labels=scales::percent)+
  labs(x="网购次数",y="百分比")
---------------------------------------

d4 <-  diamonds %>% 
  filter(color %in% c("D","E")) %>% 
  group_by(cut,color) %>% 
  summarise(Freq=n(),.groups = "drop_last") %>% 
  mutate(perc=Freq/sum(Freq))

ggplot(d2,aes(x=cut,fill=color))+
  geom_bar(stat = 'count',width = 0.5,position = 'fill')+
  scale_fill_manual(values = c('#999999','#E69F00'))+
  geom_text(data = d4,
            aes(x=cut,y=perc,group=color,label=scales::percent(perc)),
            position = position_fill(0.5),
            size=2.5)+
  scale_y_continuous(labels = scales::percent)
# 条形图有正负 ----------------------------------------------------------------------
dat <- data.frame(
  name=LETTERS[1:5],
  value=c(10,8,4,3,-5)
)
ggplot(data = dat,aes(x = name, y = value)) + 
  geom_bar(stat = 'identity', 
           fill = ifelse(dat$value>0,'gold','gray40'), # 根据y值的正负设置颜色
           width = 0.9)


# 修改坐标轴显示 -----------------------------------------------------------------

ggplot(diamonds,aes(x=cut,fill=color))+
  geom_bar(width = 0.5)+
  scale_fill_brewer(palette = "Blues",direction = -1)+
  theme(
    axis.text.x = element_text(angle = 90,hjust = 1) #横坐标文字旋转90度
  )


# 修改条形图的颜色 ----------------------------------------------------------------

p <- ggplot(diamonds,aes(x=cut,fill=cut))+
  geom_bar()
p+scale_fill_manual(values = c("#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0"))


# 修改图例的位置 -----------------------------------------------------------------

# 修改图例的位置，通过theme(legend.position=) 来实现，默认的位置是right，有效值是right、top、bottom、left和none，其中none是指移除图例。
p+theme(legend.position = "top")
p+theme(legend.position = "none")


# 修改条形图的顺序 ----------------------------------------------------------------

p+scale_x_discrete(limits=c("Ideal","Premium","Very Good","Good","Fair"))
