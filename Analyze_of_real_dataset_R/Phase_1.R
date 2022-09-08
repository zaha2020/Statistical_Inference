#--------------------------QO--------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------
df2<-read.csv("C:\\Users\\z_hab\\OneDrive\\Desktop\\prozheestenbat\\StudentsPerformance.csv")
View(df2)
data1 <- df2[ , ! names(df2) %in% c("X")]
View(data1)
#--------------------------B----------------------------------
ncol(data1)
nrow(data1)
#--------------------------C----------------------------------
is.null(data1)  
any(is.na(data1))
data1[!complete.cases(data1),] 
summary(data1)

#--------------------------D----------------------------------
data1[duplicated(data1), ]


View(data1)


#--------------------------Q1--------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------  
#I Choosed G1 numerical variable
library(ggplot2)
best_binwidth=ceiling(density(data1$G1)[["bw"]])
best_binwidth

ggplot(data1, aes(x=G1)) +
  geom_histogram(aes(y=..density..),color="black",fill="gray", binwidth=1)+
  geom_density(col = "blue",lwd=1.2)+
  labs(title = "Histogram & curve density for G1",
       x="G1",y="Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.6))

skewness<-(mean(data1$G1)-median(data1$G1))/sd(data1$G1)
skewness
#--------------------------B----------------------------------  
#histogram 
hist(data1$G1,freq = FALSE , xlab = "G1 scores")
lines(density(data1$G1), lwd = 2, col = "Blue", lty=1)

x <- seq(min(data1$G1), max(data1$G1), length.out=length(data1$G1))

lines(x, dnorm(x,mean = mean(data1$G1),sd=sd(data1$G1)), lwd = 2, col = "red", lty=2)
legend("topright", c("G1_dist","normal_dist")
       ,col=c("blue", "red"), lty=1:2, cex=0.9)


#qqplot TWO_WAY
qqnorm(data1$G1,main="qqplot")

qqline(data1$G1,col="Blue") ## adds the line to the plot

#qq_plot:
ggplot(data1, aes(sample = G1))+
  stat_qq()+
  stat_qq_line()+
  labs(title = "QQ plot of G1",y="G1")+
  theme(plot.title = element_text(hjust = 0.5))
#--------------------------C---------------------------------  
  

skewness<-(mean(data1$G1)-median(data1$G1))/sd(data1$G1)
skewness

#--------------------------D----------------------------------  
  
ggplot(data = data1, aes(x= "",y = G1))+
  geom_boxplot(size=1)+
  labs(title = "G1 score box plot ",x="",y="G1 score")+
  theme(plot.title = element_text(hjust = 0.6))


#SECOND WAY
s=data1$G1
boxplot(s ,main="boxplot ",
        ylab="score",
      col="darkgray")
#--------------------------E----------------------------------  
  
mean(data1$G1)
median(data1$G1)
var(data1$G1)
sd(data1$G1)

#--------------------------F----------------------------------  

best_binwidth2=ceiling(density(data1$G1)[["bw"]])
best_binwidth2  

h=ggplot(data1, aes(x=G1))+
  geom_histogram(aes(y=..density..),color="black",fill="darkgray", binwidth=1)+
  geom_density(col = "blue",lwd=1.2)+
  labs(title = "Histogram & curve density for G1",
       x="G1",y="Density")+  theme_classic()+
  theme(plot.title = element_text(hjust = 0.6))+
  geom_vline(xintercept=mean(data1$G1), colour='green',linetype="dashed",size=1)+
  geom_vline(xintercept=median(data1$G1), colour='red',linetype="dashed",size=1)

h+annotate(x=mean(data1$G1),y=0.15,label="mean",size = 4,color="green",vjust=1.5,hjust=1,geom="label")+
  annotate(x=median(data1$G1),y=.15,label = "median", size = 4,color="red",vjust=1.5,hjust=-0.01,geom="label")


#--------------------------G----------------------------------  
  
G1=data1$G1

up  = G1[G1 > mean(G1) ]
down= G1[G1 < mean(G1) ]

very_high = up[up > mean(up)]
high      = up[up < mean(up)]
low       = down[down > mean(down)]
very_low  = down[down < mean(down)]

S2<-c(length(very_high),length(high),length(low),length(very_low)
      )
percentage  <- round(100*S2/sum(S2), 2)
pie(S2,labels = percentage , main="Pie Chart of G1",
    col = rainbow(length(S2 )))
legend("topright", c("very high","high","low","very low")
       , cex = 0.8,  fill = rainbow(length(S2 )))

#--------------------------H----------------------------------  
  
#quantile,IQR,Wisker


Q1 = quantile(data1$G1)[[2]]
Q1
Q3 = quantile(data1$G1)[[4]]
Q3
#OR
quantile(data1$G1, prob=c(.25,.5,.75))
IQR(data1$G1)

Z1=boxplot(data1$G1)$stats[c(1, 5), ]

#other way for cacute wisker

lower_whisker = quantile(data1$G1)[[1]]
lower_whisker
upper_whisker = quantile(data1$G1)[[5]]
upper_whisker

#exact ZERO outlier
outlier<- data1$G1[data1$G1<Z1[1] | data1$G1>Z1[2]]
outlier

ggplot(data = data1, aes(x= "",y = G1))+
  geom_boxplot(size=1)+
  labs(title = "G1  box plot ",x="",y="G1")+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("segment", x = 0.45, xend = 0.6, y = Q1, yend = Q1,colour = "red",alpha=0.5, size=.4,arrow=arrow())+  
  annotate("segment", x = 0.45, xend = 0.6, y = Q3, yend = Q3,colour = "red",alpha=0.5, size=.4,arrow=arrow())+ 
  annotate("segment", x = 0.55, xend = 1, y = lower_whisker, yend = lower_whisker,colour = "red",alpha=0.5, size=.4,arrow=arrow())+ 
  annotate("segment", x = 0.55, xend = 1, y = upper_whisker, yend = upper_whisker,colour = "red",alpha=0.5, size=.4,arrow=arrow())+ 
  annotate("text", x = 0.5, y = Q1,label = "Q1", size = 5,color="black")+
  annotate("text", x = 0.5, y = Q3,label = "Q3", size = 5,color="black")+
  annotate("text", x = 0.7, y = lower_whisker,label = "lower_whisker", size = 5,color="black")+
  annotate("text", x = 0.7, y = upper_whisker,label = "upper_whisker", size = 5,color="black")


#--------------------------Q2--------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------  
#fatherjop
freqency2   = table(data1$Fjob)
freqency2
percentage2 = prop.table(table(data1$Fjob))
percentage2
#--------------------------B----------------------------------  
  
ggplot(data1, aes(Fjob))+
  geom_bar(fill = c("blue","red",'yellow','green','purple'))+
  geom_text(size=5,aes(label=after_stat(paste(round(percentage2*100,2),'%'))
                       ,vjust=-0.04),stat='count')+
  labs(title = "Bar plot of father job",y="Percent(%)")+
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------C----------------------------------  
  
freqency2  = sort(freqency2 ,decreasing = TRUE)

ggplot(data1, aes(x = reorder(Fjob,Fjob,function(x)-length(x))))+
  geom_bar(fill = c("yellow","red",'blue','green','purple'))+
  geom_text(size=5,aes(label=after_stat(freqency2)),stat='count',hjust=-0.06)+
  labs(title = "Horizontal bar plot for Father job",x="fjob")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()


#-------------------------D----------------------------------  
 
ggplot(data = data1, aes(x= Fjob,y = G1,fill=Fjob))+
  geom_violin()+
  labs(title = "student G1 score per Fjob (violin plot)",x="Fjob",y="G1")+
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------Q3--------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------  
  
# G2 $ G3
#____________________________  A  ______________________________
#they have realitionship if we have n't outliers
#--------------------------B----------------------------------  
  
ggplot(data1, aes(x = G2, y = G3,color=G2)) +
  geom_point() +
  stat_smooth(method = 'loess',
              col = "#C42126",
              se = FALSE,
              size = 1,formula = 'y ~ x')+
  labs(title = "scatter plot for G2&G3 ")+
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------C----------------------------------  
  
G2 <- data1$G2
G3 <- data1$G3
correlation = cor(G2,G3)
correlation

#OR


correlation1 = (sum((G2-mean(G2))*(G3-mean(G3)))/(sd(G2)*sd(G3)))/(length(G3)-1)
correlation1 

#--------------------------D----------------------------------  
#  there is  correlation between G2 & G3 score of students
# but correlation does not imply causation

#--------------------------E----------------------------------  
#we want to analysing pair data
#convert 2 variable to one variable & calcute the diff


# we assume that mean of diff must be 0 as Null hypothesis
#  (we have 2 side Test)
# H0 : u_diff  = 0
# HA : u_diff != 0

cor.test(data1$G3, data1$G2)


#SECOND ANALIZE
diff = data1$G3-data1$G2
s_3= sd(diff)
s_3

x_bar_3 = (mean(diff))
x_bar_3

n_3<- length(diff)
n_3
SE_3 = s_3/sqrt(n_3)
SE_3

T_3 = (x_bar_3-0)/SE_3
T_3

pValue_3 = 2*pt(T_3,df=n_3-1,lower.tail = FALSE)
pValue_3
#pValue_3<0.05

#pvalue is lower than of alpha so we can reject ho 
#--------------------------F----------------------------------  
# Categorical=romantic
ggplot(data1, aes(x = G2, y = G3,color=romantic,shape=romantic)) +
  geom_point() +
  labs(title = "scatter plot for G2&G3 ")+
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------G----------------------------------  
  
#install.packages("ggExtra")
library(ggExtra)


hplot = ggplot(data1, aes(x=G2, y=G3) ) +
  geom_hex(bins=12)+
  geom_point(alpha = 0) +  
  geom_smooth(method = 'loess',formula = 'y ~ x',color="red")+
  theme_bw() +scale_fill_continuous(type = "viridis") +
  theme(legend.position = "left")+
  labs(title = "hexbin plot")+
  theme(plot.title = element_text(hjust = 0.5))

ggExtra::ggMarginal(hplot,fill = "slateblue", type = "histogram")


#--------------------------H----------------------------------  
  
ggplot(data1, aes(x=G2, y=G3) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  scale_fill_continuous(type = "viridis")+
  labs(title = "2D density plot")+
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------Q4-------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------
# install.packages("GGally")
# install.packages("ggforce")
library(GGally)


data2=data1
data2$sex <- as.numeric(factor(data1$sex))
data2$school = as.numeric(factor(data1$school))
data2$Fjob = as.numeric(factor(data1$Fjob))
data2$Mjob = as.numeric(factor(data1$Mjob))
data2$internet = as.numeric(factor(data1$internet ))
data2$romantic=as.numeric(factor(data1$romantic ))


View(data2)

ggpairs(
  data2, columns=c(15,14,13,11,6,5,4,3,2),
  upper = list(continuous =  wrap("density", alpha = 0.5,color="blue"), combo = "box_no_facet"),
  diag  = list(continuous=   wrap("densityDiag", alpha = 0.5,color="black")),
  lower = list(continuous =  wrap("points", alpha = 0.5,color="red"), combo = "dot_no_facet")
  )


#--------------------------B----------------------------------
# install.packages("ggcorrplot")
library(ggcorrplot)

corr <- round(cor(data2), 1)
p.mat <- cor_pmat(data2)
head(corr[, 1:15])
head(p.mat[, 1:15])
ggcorrplot(corr,hc.order = TRUE,type = "lower",lab = TRUE, p.mat = p.mat )
        
           
           

          
#--------------------------c----------------------------------
#«G1»,«G2»,«G3» as numerical variables &  «internet» as categorical  

#install.packages("scatterplot3d") 
library("scatterplot3d") 
#data1$internet
colors <- c("red", "green")
colors <- colors[as.numeric(factor(data1$internet))]
colors
scatterplot3d(data1$G1,data1$G2,data1$G3,pch = 16, color=colors,
              xlab = "G1 Score",ylab = "G2 Score",zlab = "G3 Score")
legend("right", legend = c("Yes","No"), pch = 16,
       col =  c("green", "red"))

#--------------------------Q5-------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------
#install.packages("grid")
#install.packages("gridExtra")
library(grid)
library(gridExtra)


#with(data1, table(sex, internet))
Table <- table(data1$sex, data1$internet)
Table
rownames(Table) = c("Female","Male")
theme <- ttheme_minimal(core=list(bg_params =list(fill=c("#E8F3DE","#D3E8BB","#E8F3DE"), col=NA)))
grid.table(addmargins(Table),theme=theme)
#--------------------------B----------------------------------
ggplot(data1, aes(internet, fill = sex))+
  geom_bar(position = 'dodge')+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.8),vjust=-0.1)+
  scale_fill_discrete(labels=c("female", "male"))+
  labs(title = "Grouped bar chart",y="Count")+
  theme(plot.title = element_text(hjust = 0.5))
  
  
#--------------------------c----------------------------------
ggplot(data1,aes(internet,fill=sex))+
  geom_bar()+
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))+
   theme(plot.title = element_text(hjust = 0.5))+
   scale_fill_discrete(labels=c("female", "male"))+
   labs(title = "Segmented bar plot",y="Count")
  
#--------------------------D----------------------------------
Table2= table(sex=data1$sex,internet=data1$internet)
data.Proportion = data.frame(prop.table(Table2))
data.Proportion$Freq = round(data.Proportion$Freq,4)

#install.packages("ggmosaic")
library(ggmosaic)




ggplot(data = data.Proportion) +
  geom_mosaic(aes(weight = Freq,x = product(internet), fill = sex)) +
  scale_y_continuous(limits =c(0,1))+
  annotate("text", x = 0.10, y = 0.30,label=paste(data.Proportion$Freq[[1]]*100,"%"),fontface = "bold" ,size=4,color="black")+
  annotate("text", x = 0.10, y = 0.8,label=paste(data.Proportion$Freq[[2]]*100,"%"), fontface = "bold",size = 4,color="black")+
  annotate("text", x = 0.62, y = 0.25,label=paste(data.Proportion$Freq[[3]]*100,"%"),fontface = "bold" ,size = 4,color="black")+
  annotate("text", x = 0.62, y = 0.75,label=paste(data.Proportion$Freq[[4]]*100,"%"), fontface = "bold",size = 4,color="black")+
  labs(title = "Mosaic plot",y="Proportion",x="internet")+
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------Q6-------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------

x_bar6<- mean(data1$G3)
x_bar6
s6 <- sd(data1$G3)
n6 <- length(data1$G3)
SE6 <- s6/sqrt(n6)
SE6
#calcute zstar for alpha =0.05
#two way
qnorm(1-0.025)
#OR
z_star6=qnorm(0.95+(1-0.95)/2)
z_star6
#
CI6= c(x_bar6-z_star6*SE6,x_bar6+z_star6*SE6)
CI6
#--------------------------B----------------------------------
# 95% of our Mean are in the confidence interval
#--------------------------c---------------------------------
best_binwidth1=ceiling(density(data1$G3)[["bw"]])
best_binwidth1
xbarG3=mean(data1$G3)

g=ggplot(data1, aes(x=G3))+
  geom_histogram(aes(y=..count..),color="black",fill="darkgray", binwidth=2)+
  labs(title =  "Histogram & CI of G3 "
       ,x="G3 score",y="Count")+  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(data1$G3), colour='red',linetype="dashed",size=1)+
  geom_vline(xintercept=CI6[1], colour='blue',linetype="dashed",size=1)+
  geom_vline(xintercept=CI6[2], colour='green',linetype="dashed",size=1)
  
g+annotate(x=xbarG3,y=65,label="u",size = 4,color="red",vjust=0.5,hjust=0,geom="label")+
  annotate(x=CI6[1],y=75,label = "CI1", size = 4,color="blue",vjust=0.5,hjust=1,geom="label")+
  annotate(x=CI6[2],y=75,label = "CI2", size = 4,color="green",vjust=0.5,hjust=0,geom="label")
  
#--------------------------D---------------------------------
# we assume that mean must be 13 As Null hypothesis
#  (we have 2 side Test)

# H0 : u  = 13
# HA : u != 13


z6 <- (x_bar6-13)/SE6
z6
pvalue6 = 2*pnorm(z6 , lower.tail = TRUE)
pvalue6
#pvalue6>0.05

# p_val =0.16 & it is upper than alpha so we can't reject HO
#--------------------------E--------------------------------
#yes the CI is between [12.13339,13.14813] so we know 13 is in of range
#--------------------------F--------------------------------
library(dplyr)
sample6 = sample_n(data1,100)

#ACTUAL MEAN
ua =mean(data1$G3)
ua
# MEAN OF SAMPLE
u0 =mean(sample6$G3)
u0

s6_2 <- sd(sample6$G3)
n6_2 <- length(sample6$G3)
SE6_2 <- s6_2/sqrt(n6_2)
SE6_2


#because ua>u0 so the z star become +1.95

xbar2<-((1.95*SE6_2) + u0)
xbar2
z2 <- (xbar2-ua)/SE6_2
z2
B<-pnorm(z2 , lower.tail = TRUE)
B

#--------------------------G--------------------------------
POWER<-pnorm(z2 , lower.tail = FALSE)
POWER
Power<-1-B
Power
#
#--------------------------Q7-------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------

# a) 
# I choose G1 , G3  as numerical variables
# n=25 <30 so we use t_test
# n1=25,n2=25 so we must use t_test




# -------
# b) 
#The sample_n function selects random rows from a data frame (or table).

library(dplyr)
sample7=sample_n(data1,25)[c("G1","G3")]

#we want to analysing pair data
#convert 2 variable to one variable & calcute the diff
# we assume that mean of diff must be 0 as Null hypothesis
#  (we have 2 side Test)

# H0 : u_diff  = 0
# HA : u_diff != 0

sample7$diff = sample7$G1-sample7$G3


s_7 = sd(sample7$diff)
s_7
x_bar_7 = (mean(sample7$diff))
x_bar_7
n_7 = nrow(sample7)

SE_7 = s_7/sqrt(n_7)
SE_7

T_7 = (x_bar_7-0)/SE_7
T_7
#T_7<0 SO lowe.tail will be TRUE 
#calcute pvalue
pValue_7 = 2*pt(T_7,df=n_7-1,lower.tail = TRUE)
pValue_7

#pValue_7<0.05

#pvalue is LOWER than of alpha so we can reject ho 
#--------------------------B----------------------------------

# H0 : uG1 =uG3 ( uG1 -uG2 =0)there isn't any significant  difference 
#between the  mean  values  of  these  two  variables

# HA : u G1 u != uG2 (uG1 -uG2 != 0)there is any significant  difference 
#between the  mean  values  of  these  two  variables

#draw  100 independent samples  from  the dataset for eachof these two variables  


give200sample = sample_n(data1,200)
give200sample
sample1 = give200sample[0:99,][["G1"]]
sample2 = give200sample[100:200,][["G3"]]

#skewness

skewness1<-(mean(sample1)-median(sample1))/sd(sample1)
skewness1
skewness2<-(mean(sample2)-median(sample2))/sd(sample2)
skewness2

sample1_u <- mean(sample1 )
sample2_u <- mean(sample2)

xbar7<-(sample1_u-sample2_u)
xbar7
n7 = 100
sdn7 = sd(sample1)
sdm7 = sd(sample2)
SE7 <- sqrt((sdn7^2/n7)+(sdm7^2/n7))
SE7
#df = min(n_1-1 , n_2-1) = min(99,99) = 99
df<-99
#alpha=0.05

#b:p-value
T24<-(xbar7-0)/SE7
T24
Pvalue7<-2*pt(T24,df=99,lower.tail=TRUE)
Pvalue7
#Pvalue7<0.05
# we can reject ho so there is  any significant  difference 
#between the  mean  values  of  these  two  variables.


#(sample1_u-sample2_u)+-t*df SE
#CI

t_star7<-qt(0.025,df=99,lower.tail = FALSE)
t_star7
CI7<-c(xbar7-t_star7*SE7,xbar7+t_star7*SE7)
CI7




#--------------------------Q8-------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------

s2=data1$G2
boxplot(s2 ,main="boxplot ",
        ylab="G2",
        col="darkgray")
#outlier of G2
Z2=boxplot(data1$G2)$stats[c(1, 5), ]

#exact ZERO outlier
outlier<- data1$G2[data1$G2<Z2[1] | data1$G2>Z2[2]]
outlier


library(dplyr)
orginal_sample = sample_n(data1,20)$G2

#make resample with 500 repeat
boott = NULL
for(j in 1:500){
  i <- sample(1:20,20, replace = TRUE)
  re_sample <- orginal_sample[i]
  mean = mean(re_sample)
  boott = append(boott, mean)
}
boott
n_boot= length(boott)
n_boot
i= ceiling((n_boot*0.05)/2)
i
#i= ceiling((n_boot-(n_boot*0.95))/2)

j=n_boot-i
j

sort_g2=boott[order(boott)]

CI_1 <- sort_g2[i]
CI_2 <- sort_g2[j]

CI_pm = c(CI_1,CI_2)
CI_pm

#without boot
i1= ceiling((395*0.05)/2)
i1
#i= ceiling((395-(395*0.95))/2)

j1=395-i1
j1

sort_g22=boott[order(boott)]

CI_1_2 <- sort_g22[i1]
CI_2_2 <- sort_g22[j1]

CI_pm_2 = c(CI_1_2,CI_2_2)
CI_pm_2
#--------------------------B----------------------------------
#calcute CI for bootstrap sample using the se method


#CI=sample mean +- t*SEboot
SEboot= sd(boott)
t_star8=qt(0.025,499,lower.tail = FALSE)
CI_sem = c(mean(boott)-t_star8*SEboot , mean(boott)+t_star8*SEboot)
CI_sem

#C
#SE IS abit litle compse

#--------------------------Q9-------------------------------
#-------------------------------------------------------------
# we assume that means must be equal   as Null hypothesis
#  (we have 2 side Test)

# H0 : u1 = u2 = u3 = u4
# HA : at least two of them (u) are not equal  

#processing(add new column)

data1$sumofscores =  data1$G1+data1$G2+data1$G3
View(data1)

#WE check three ANOVA condition  

#1.independece(true)

#2.approximately normal
#we draw qqplot for each 4 type of failures

# 
x=unique(data1$failures)

#0 failure
ggplot(data1[data1$failures==x[1],], aes(sample = sumofscores))+
  stat_qq()+
  stat_qq_line()+
  labs(title = " 0 failure",y="sumofscores")+
  theme(plot.title = element_text(hjust = 0.5))

# 1 failure
ggplot(data1[data1$failures==x[2],], aes(sample = sumofscores))+
  stat_qq()+
  stat_qq_line()+
  labs(title = " 1 failure",y="sumofscores")+
  theme(plot.title = element_text(hjust = 0.5))


# 2 failures
ggplot(data1[data1$failures==x[3],], aes(sample = sumofscores))+
  stat_qq()+
  stat_qq_line()+
  labs(title = " 2 failures",y="sumofscores")+
  theme(plot.title = element_text(hjust = 0.5))

#3 failures
ggplot(data1[data1$failures==x[4],], aes(sample = sumofscores))+
  stat_qq()+
  stat_qq_line()+
  labs(title = " 3 failures",y="sumofscores")+
  theme(plot.title = element_text(hjust = 0.5))


#3.Constant variance
sd(data1[data1$failures==x[1],]$sumofscores)
sd(data1[data1$failures==x[2],]$sumofscores)
sd(data1[data1$failures==x[3],]$sumofscores)
sd(data1[data1$failures==x[4],]$sumofscores)


#firstdway
ybar<-mean(data1$sumofscores)
k9= length(unique(data1$failures))
k9
n9 = nrow(data1)


#-------------------- degree of freedom------------------------

dfT = n9-1
dfG = k9-1
dfE = dfT-dfG
dfT
dfG
dfE
#--------------------sum_square------------------------------------
#SSG TWO WAY

#1.
SSG1 = 0
for (i in unique(data1$failures)) {
  j = data1[data1$failures==i,]
  SSG1 = SSG1 + nrow(j)*(mean(j$sumofscores)-mean(data1$sumofscores))^2
}
SSG1

#2.
library(dplyr)
failergroup = ddply(data1  , c("failures"),count=length(failures),
                    meanevery_group = mean(sumofscores) ,summarize)
failergroup
SSG <- sum(failergroup$count*(failergroup$meanevery_group-ybar)**2)
SSG

#SST
SST <-sum((data1$sumofscores-ybar)**2)
SST

#SSE
SSE = SST-SSG
SSE
#--------------------mean_SQ----------------------------------------

MSG = SSG/dfG
MSG
MSE = SSE/dfE
MSE
#--------------------- F_statistic----------------------------------

F = MSG/MSE
F
#-------------------------P_value-----------------------------------

pf(F , dfG , dfE ,lower.tail = FALSE)

#  p value <0.05 we can significantly reject H0 
# so therE is at least two group of  with different mean



#secondway
data1$failures=factor(data1$failures)
res.aov <- aov(sumofscores ~ failures, data = data1)
summary(res.aov)



alpha = 0.05
K = k9*(k9-1)/2
K
alpha_star = alpha/K
alpha_star

TukeyHSD(res.aov)