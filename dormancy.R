# Dormancy exercise

#### Set up ####
library(ggplot2) 
library(RColorBrewer)
library(MuMIn)
dorm = read.csv("C:/Users/lu1280ca/Work Folders/Downloads/dormancy.csv", sep = ",")

#### Data exploration ####
head(dorm)
str(dorm)
unique(dorm$pop) # there are 4 populations: LM, PM, T, CC
max(dorm$timetosowing)
min(dorm$timetosowing)
unique(dorm$nseed)

#### Data partitioning ####
dorm_lm = dorm[dorm$pop=="LM",]
dorm_pm = dorm[dorm$pop=="PM",]
dorm_t = dorm[dorm$pop=="T",]
dorm_cc = dorm[dorm$pop=="CC",]

# number of seeds * proportion of success = number of successes
# number of seeds - number of successful seeds = number of failures

sum(dorm_lm$germ2 * dorm_lm$nseed) #Successes
sum(dorm_pm$germ2 * dorm_pm$nseed) #Successes
sum(dorm_t$germ2 * dorm_t$nseed) #Successes
sum(dorm_cc$germ2 * dorm_cc$nseed) #Successes
notgerm = dorm_lm$nseed - germ #Failures

sum(dorm_lm$nseed)
sum(dorm_pm$nseed)
sum(dorm_t$nseed)
sum(dorm_cc$nseed)
mean(dorm_lm$timetosowing)

#### Model fitting ####
mod2p1= glm(germ2 ~ timetosowing + MCseed, "binomial", weights=nseed, data=dorm_lm)
mod2p2 = glm(germ2 ~ timetosowing + MCseed, "binomial", weights=nseed, data=dorm_pm)
mod2p3= glm(germ2 ~ timetosowing + MCseed, "binomial", weights=nseed, data=dorm_t)
mod2p4= glm(germ2 ~ timetosowing + MCseed, "binomial", weights=nseed, data=dorm_cc)
summary(mod2p1)
summary(mod2p2)
summary(mod2p3)
summary(mod2p4)

r.squaredGLMM(mod2p1)
r.squaredGLMM(mod2p2)
r.squaredGLMM(mod2p3)
r.squaredGLMM(mod2p4)

#### T50 ####
get_T50=function(model, data){
  coefs=coef(model)
  xc=mean(data$MCseed)
  T50=-(coefs[1] + coefs[3] * xc) / coefs[2]
  return(T50)
}

T50_LM=get_T50(mod2p1, dorm_lm)
T50_PM=get_T50(mod2p2, dorm_pm)
T50_T=get_T50(mod2p3, dorm_t)
T50_CC=get_T50(mod2p4, dorm_cc)

T50_LM
T50_PM
T50_T
T50_CC


#### Time to sowing plot ####
pred_time=function(model, data, pop_code) {
  coefs=coef(model)
  x=data$timetosowing
  xc=mean(data$MCseed) # constant
  x_pred=seq(min(x), max(x), by = 0.1)
  y_hat=coefs[1] + coefs[2]*x_pred + coefs[3]*xc
  p_hat=plogis(y_hat)
  T50 = -(coefs[1] + coefs[3] *xc)/coefs[2]
  data.frame(
    timetosowing=x_pred,
    p_hat= p_hat,
    T50= T50,
    population= pop_code)}

df_LM=pred_time(mod2p1, dorm_lm,"LM")
df_PM=pred_time(mod2p2, dorm_pm,"PM")
df_T=pred_time(mod2p3, dorm_t,"T")
df_CC=pred_time(mod2p4, dorm_cc,"CC")

pred_time_all=rbind(df_LM, df_PM, df_T, df_CC)

ggplot() +
  geom_line(data=pred_time_all,
            aes(x=timetosowing, y=p_hat, color=population),
            linewidth= 1) +
  geom_point(data=unique(pred_time_all[c("T50","population")]),
             aes(x=T50, y= 0.5, color=population),
             size=3) +
  scale_color_manual(values=brewer.pal(4,"Set2"))+
  geom_hline(yintercept=0.5, linetype="dashed") +
  geom_vline(xintercept=pred_time_all$T50, linetype ="dashed")+
  labs(x ="Time to sowing (days)",
       y ="Germination probability",
       color = "Population") +
  theme_bw() 

#### Seed size plot ####
pred_size=function(model, data, T50, pop_label) {
  coefs=coef(model)
  x_pred=seq(min(data$MCseed), max(data$MCseed), by=0.1)
  y_hat=coefs[1] + coefs[2] * T50 + coefs[3] * x_pred
  S50=-(coefs[1] + coefs[2] *T50)/coefs[3]
  p_hat=plogis(y_hat)
  data.frame(
    seedsize = x_pred,
    p_hat= p_hat,
    population= pop_label,
    S50=S50,
    T50= T50)}

df_PM_size=pred_size(mod2p2, dorm_pm, T50_PM, "PM")
df_T_size=pred_size(mod2p3, dorm_t,  T50_T, "T")
df_CC_size=pred_size(mod2p4, dorm_cc, T50_CC, "CC")

pred_size_all <- rbind(df_PM_size, df_T_size, df_CC_size)

ggplot() +
  geom_line(data = pred_size_all,
            aes(x = seedsize, y = p_hat, color = population),
            size = 1) +
  geom_point(data=unique(pred_size_all[c("S50","population")]),
             aes(x=S50, y= 0.5, color=population),
             size=3) +
  scale_color_manual(values=c("#66C2A5", "#8DA0CB","#E78AC3"))+
  geom_hline(yintercept=0.5, linetype="dashed") +
  geom_vline(xintercept=pred_size_all$S50, linetype ="dashed")+
  labs(x = "Seed size as deviation from population's mean (mg)",
       y = "Germination probability",
       color = "Population")+
  theme_bw()
