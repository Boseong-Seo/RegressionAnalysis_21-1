#################################### hw6 #######################################

setwd("C:/2021s")
getwd()

library(ggplot2)
library(dplyr)
library(nls2)
library(segmented)

########################## 0. Set Functions to get MSE ###########################


getRsq <- function(y, yhat){
  Rsq <- 1 - (sum((y-yhat)^2) / sum((y-mean(y))^2))
  return(Rsq)
}

getMSE_lin<-function(y,yhat){
  MSE <- sum((y-yhat)^2)/(length(y)-2)
  # linear model�� df = n-2
  return(MSE)
}

getMSE <- function(y, yhat){
  MSE <- sum((y-yhat)^2)/(length(y)-3)
  # �츮�� ����� 3���� model�� ���� 3���� parameter => df = n-3
  return(MSE)
}

# length(y)�� parameter ������ ���� �� ũ�� ������ length(y)�� �����൵ ū ���̴� ���� ������ .... �׷��� df=n-k�ϱ�
# ���� ���� getMSE �Լ� �ۼ�


########################## 1. Data Importing & Preprocessing ########################


# Read
cases<-read.csv("global_confirmed_cases_210420.csv")
nld_cases <- cases %>% filter(CountryCode == "NLD")


# Date formatting
nld_cases$Date <- sapply(nld_cases$Date, function(d) {
  vec <- strsplit(d, split = ".", fixed = T) %>%
    unlist %>% as.character() 
  vec_ch <- sapply(vec, function(i) {
    ifelse(nchar(i) < 2, paste0("0",i), i)
  })
  paste(vec_ch, collapse = "-")
})


# ù case�� �߻��� ��¥�� �����ͺ��� �̿�
start_date <- nld_cases$Date[min(which(nld_cases$Cases > 0))]

# Days_after_Start column �߰�
nld_cases_fin <- mutate(nld_cases,Days_after_Start = as.integer(as.Date(Date) - as.Date(start_date))) %>% 
  filter(Days_after_Start >= 0)


# 3�� 20�� �ڷ����
nld_cases_320 <- nld_cases_fin %>% filter(Days_after_Start<=387)



############################# 2. Model Fitting #############################


# data visualization
nld_cases_fin %>% ggplot(aes(x=Days_after_Start, y=Cases)) +
  theme_bw() + geom_point()


# fitting�� �����ϸ� fit_iterate_~~~���� NULL �� �����ϰ� ��
fit_iterate_logi<-NULL
fit_iterate_bert<-NULL
fit_iterate_gomp<-NULL


####### Usig Linear Regression #######
fo_lin <- Cases ~ Days_after_Start
lin.test<-lm(fo_lin,data=nld_cases_320)
summary(lin.test)

# Days_after_Start�� 418�� ���� 2021.04.20��
predict(lin.test,data.frame(Days_after_Start=418))
nld_predict_lin<-data.frame(x=nld_cases_fin$Days_after_Start,
                             predict=predict(lin.test,nld_cases_fin))

day_start <- as.character(sort(nld_cases_fin$Date)[1])
### linear model
cumulated_data_lin <- ggplot(data=nld_cases_fin,aes(x=Days_after_Start,y=Cases))+
  geom_point(color='gray', shape =1, size=3)+ theme_bw() +
  labs(title = paste0("COVID-19 Cases"), 
       subtitle = paste0("Netherland", " / ", "Cumulated"), 
       x=paste0('Days Since ', as.character(day_start)),
       y='Number of Cases')+
  geom_line(data=nld_predict_logi,
            aes(x=x,y=predict), color="maroon", linetype="dashed",size=1.2) +
  theme(plot.title=element_text(size=14, hjust=0.5, face="bold", colour="black", vjust=2),
        plot.subtitle=element_text(size=10.5, hjust=0.5, face="italic", color="maroon", vjust=2),
        axis.text=element_text(size=10, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 10, hjust = 0),
        axis.title=element_text(size=10, colour = "black"))

cumulated_data_lin


### evaluate mse
nld_predict_lin_321420 <- nld_predict_lin %>% filter(x>387)
nld_cases_321420 <- nld_cases_fin %>% filter(Days_after_Start>387)

(MSE_lin <- getMSE_lin(nld_cases_321420$Cases,nld_predict_lin_321420$predict))





########  Using Logistic Model #######
fo_logi<- Cases ~ a / (1 + exp(b-c*Days_after_Start))


### Set grid
grid_logi<- data.frame(a=c(0,max(nld_cases_320$Cases)),b=c(0,100),c=c(0,1))

### find initial value with brute-force algorithm
rough_fit_logi<- nls2(fo_logi, data=nld_cases_320, start=grid_logi, algorithm="brute-force")
coef(rough_fit_logi)


### apply gauss-newton algorithm to find better coefficients
fit_iterate_logi<- nls2(fo_logi,data=nld_cases_320,start=coef(rough_fit_logi))


### check Rsq and estimate coefficients
(Rsq_logi<-getRsq(nld_cases_320$Cases,predict(fit_iterate_logi,nld_cases_320)))
summary(fit_iterate_logi)
coef(fit_iterate_logi)


# Days_after_Start�� 418�� ���� 2021.04.20��
predict(fit_iterate_logi,data.frame(Days_after_Start=418))
nld_predict_logi<-data.frame(x=nld_cases_fin$Days_after_Start,
                             predict=predict(fit_iterate_logi,nld_cases_fin))


day_start <- as.character(sort(nld_cases_fin$Date)[1])
### logistic model
cumulated_data_logi <- ggplot(data=nld_cases_fin,aes(x=Days_after_Start,y=Cases))+
  geom_point(color='gray', shape =1, size=3)+ theme_bw() +
  labs(title = paste0("COVID-19 Cases"), 
       subtitle = paste0("Netherland", " / ", "Cumulated"), 
       x=paste0('Days Since ', as.character(day_start)),
       y='Number of Cases')+
  geom_line(data=nld_predict_logi,
            aes(x=x,y=predict), color="maroon", linetype="dashed",size=1.2) +
  theme(plot.title=element_text(size=14, hjust=0.5, face="bold", colour="black", vjust=2),
        plot.subtitle=element_text(size=10.5, hjust=0.5, face="italic", color="maroon", vjust=2),
        axis.text=element_text(size=10, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 10, hjust = 0),
        axis.title=element_text(size=10, colour = "black"))

cumulated_data_logi


nld_predict_logi_321420 <- nld_predict_logi %>% filter(x>387)
nld_cases_321420 <- nld_cases_fin %>% filter(Days_after_Start>387)

(MSE_logi <- getMSE(nld_cases_321420$Cases,nld_predict_logi_321420$predict))




########## Using Gompertz Model #########
fo_gomp <- Cases ~ a * exp(b * (-1) * exp((-1) * c * Days_after_Start))


### Set grid
grid_gomp <- expand.grid(a = max(nld_cases_320$Cases), b = seq(1, 10, 1), c = seq(0, .1, 0.001))


### find initial values with brute_force algorithm
rough_fit_gomp = nls2(fo_gomp, data = nld_cases_320, start = grid_gomp,
                      algorithm = "brute-force")
rough_fit_gomp


### apply gauss-newton algorithm to find better coefficients
fit_iterate_gomp<-nls2(fo_gomp,data=nld_cases_320,start=coef(rough_fit_gomp))
summary(fit_iterate_gomp)


### result of estimate the coefficients
(Rsq_gomp<-getRsq(nld_cases_320$Cases,predict(fit_iterate_gomp,nld_cases_320)))
coef(fit_iterate_gomp)


# Days_after_Start�� 418�� ���� 2021.04.20��
nld_predict_gomp<-data.frame(x=nld_cases_fin$Days_after_Start,
                             predict=predict(fit_iterate_gomp,nld_cases_fin))
nld_predict_gomp %>% filter(x==418)

day_start <- as.character(sort(nld_cases_fin$Date)[1])
### gompertz model
cumulated_data_gomp <- ggplot(data=nld_cases_fin,aes(x=Days_after_Start,y=Cases))+
  geom_point(color='gray', shape =1, size=3)+ theme_bw() +
  labs(title = paste0("COVID-19 Cases"), 
       subtitle = paste0("Netherland", " / ", "Cumulated"), 
       x=paste0('Days Since ', as.character(day_start)),
       y='Number of Cases')+
  geom_line(data=nld_predict_gomp,
            aes(x=x,y=predict), color="maroon", linetype="dashed",size=1.2) +
  theme(plot.title=element_text(size=14, hjust=0.5, face="bold", colour="black", vjust=2),
        plot.subtitle=element_text(size=10.5, hjust=0.5, face="italic", color="maroon", vjust=2),
        axis.text=element_text(size=10, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 10, hjust = 0),
        axis.title=element_text(size=10, colour = "black"))

cumulated_data_gomp


nld_predict_gomp_321420 <- nld_predict_gomp %>% filter(x>387)
nld_cases_321420 <- nld_cases_fin %>% filter(Days_after_Start>387)

(MSE_gomp <- getMSE(nld_cases_321420$Cases,nld_predict_gomp_321420$predict))





######################### 2. segmented poisson regression model ################
vac<-read.csv("covid_vaccine.csv")
vac_nld<-vac %>% filter(CountryCode=="NLD")


# Date formatting
vac_nld$Date <- sapply(vac_nld$Date, function(d) {
  vec <- strsplit(d, split = ".", fixed = T) %>%
    unlist %>% as.character() 
  vec_ch <- sapply(vec, function(i) {
    ifelse(nchar(i) < 2, paste0("0",i), i)
  })
  paste(vec_ch, collapse = "-")
})


# ù case �߻�
start_date <- "2020-02-27"

# Days_after_Start column �߰�
vac_nld_fin <- mutate(vac_nld,Days_after_Start = as.integer(as.Date(Date) - as.Date(start_date))) %>% 
  filter(Days_after_Start >= 0)


# 3�� 20�� �ڷ����
vac_nld_320 <- vac_nld_fin %>% filter(Days_after_Start<=387)


# data �ð�ȭ
ggplot(vac_nld_fin, aes(x = Days_after_Start, y = Difference)) +
  geom_point() + theme_bw()

 # ___________________________

# �Ϻ� Ȯ���ڼ��� ���� �����Ƿ� days_after_Start �̿�
pois_fit <- glm(Difference~Days_after_Start, data = vac_nld_320, family=poisson)
summary(pois_fit)

set.seed(777)
seg_fit <- NULL
try(seg_fit <- segmented(pois_fit, seg.Z = ~ Days_after_Start, psi=c(350,370),
                     npsi = 2, control = seg.control(it.max = 100000, n.boot = 50)))
summary(seg_fit)
(psi_1<-seg_fit$psi[3])
(psi_2<-seg_fit$psi[4])

# ���յ� �𵨷� fitting

nld_predict_seg<-data.frame(Days_after_Start=vac_nld_fin$Days_after_Start,
                            predict=exp(predict(seg_fit,vac_nld_fin)))

nld_predict_seg2<-data.frame(Days_after_Start=vac_nld_320$Days_after_Start,
                            predict=seg_fit$fitted.values)




### segmented model with days parameter

p_1 <- ggplot(vac_nld_fin, aes(x = Days_after_Start, y = Difference)) +
  geom_point() +
  geom_line(mapping = aes(x = Days_after_Start, y = predict),
            data = nld_predict_seg, color = "tomato") +
  geom_vline(xintercept = psi_1, color = "grey", size = 1.5, linetype="dashed") +
  geom_vline(xintercept = psi_2, color = "grey", size = 1.5, linetype="dashed") +
  theme_bw() +
  scale_y_log10()

p_1


# evaluate mse

nld_predict_seg_321420<-nld_predict_seg %>% filter(Days_after_Start>387)
vac_nld_321420 <- vac_nld_fin %>% filter(Days_after_Start>387)

# ���⼭ mse�� n-6�ε�, getMSE �Լ� ��ü�� n-3�� �ϴ� ������ �� �����Ƿ�,
# �Ʒ��� ���� �� ������ ���־���.
n<-nrow(nld_predict_seg_321420)
(MSE_seg1<-getMSE(vac_nld_321420$Difference,nld_predict_seg_321420$predict)*((n-3)/(n-6)))



################################## 3. #############################
 
# Cases
pois_fit_1 <- glm(Difference~ Days_after_Start+people_vaccinated,
             data = vac_nld_320, family = poisson)

summary(pois_fit_1)
# Segmented Poisson

set.seed(777)
seg_fit_1 <- NULL
try(seg_fit_1 <- segmented(pois_fit_1, seg.Z = ~ Days_after_Start,
                           psi=c(340, 380), npsi = 2,
                           control = seg.control(it.max = 10000, n.boot = 50)))
summary(seg_fit_1)

(psi_1<-seg_fit_1$psi[3])
(psi_2<-seg_fit_1$psi[4])

# ���յ� �𵨷� fitting
nld_predict_seg1<-data.frame(Days_after_Start=vac_nld_fin$Days_after_Start,
                            predict=exp(predict(seg_fit_1,vac_nld_fin)))


### segmented model with days parameter

p_1 <- ggplot(vac_nld_fin, aes(x = Days_after_Start, y = Difference)) +
  geom_point() +
  geom_line(mapping = aes(x = Days_after_Start, y = predict),
            data = nld_predict_seg1, color = "tomato") +
  geom_vline(xintercept = psi_1, color = "grey", size = 1.5, linetype="dashed") +
  geom_vline(xintercept = psi_2, color = "grey", size = 1.5, linetype="dashed") +
  theme_bw()

p_1


# evaluate mse

nld_predict_seg1_321420<-nld_predict_seg1 %>% filter(Days_after_Start>387)
vac_nld_321420 <- vac_nld_fin %>% filter(Days_after_Start>387)

# ���⼭ mse�� n-5�ε�, getMSE �Լ� ��ü�� n-3�� �ϴ� ������ �� �����Ƿ�,
# �Ʒ��� ���� �� ������ ���־���.
n<-nrow(nld_predict_seg1_321420)
(MSE_seg1<-getMSE(vac_nld_321420$Difference,nld_predict_seg1_321420$predict)*((n-3)/(n-5)))
