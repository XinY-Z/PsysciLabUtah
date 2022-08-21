### load packages and stuff ####
library(data.table)     # data processing
library(lavaan)         # structural equation modeling
library(semPlot)        # plot SEM model
library(lmerTest)       # LMM/GLMM
#library(glmmTMB)        # Beta GLMM
library(robustlmm)      # robust LMM
library(ggplot2)        # plotting
library(e1071)          # SVR/SVM
library(lcmm)           # growth mixture model/latent class growth model
#library(mlogit)         # multinomial regression (mixed)
library(nnet)           # multinomial regression
library(caret)          # train predicting models
library(MANOVA.RM)      # bootstrapping manova
library(markovchain)    # transition matrix
library(randomForest)   # random forest classifier


setwd('C:/Users/XinZ/Box/DATA/Xin PDRP/')
#dt <- fread('./asr_meta_cleaned.csv')    ## ASR dataset
dt <- fread('../linked files/asr_depthsession_linked.csv')
dt_h <- fread('../February 2020/Coded_MISC+Topics/miscglobalsmerged_03.06.2020.txt', sep='|')
set.seed(123)


## therapist talk turns
turn <- dt[speaker=='T', .(th.turn=.N), session]
setkey(turn, 'session'); setkey(dt, 'session')
dt <- turn[dt]; rm(turn)

## reflection
count <- dt[codes %in% c('REC', 'RES'), .(countreflection = .N), by=session]
setkey(count, 'session'); setkey(dt, 'session')
dt <- count[dt]; rm(count)
dt[, ratioreflection := countreflection/th.turn]

## open questions
count <- dt[codes=='QUO', .(countopenquestion = .N), by=session]
setkey(count, 'session'); setkey(dt, 'session')
dt <- count[dt]; rm(count)
dt[, ratioopenquestion := countopenquestion/th.turn]

## Other data
dt_s <- fread('../February 2020/EMR_Data/depth_session_info_08.08.2020.txt', sep='|')
dt_c <- fread('../February 2020/EMR_Data/depth_client_demo_08.08.2020.txt')
dt_c[, V1 := NULL]
dt_t <- fread('../February 2020/Clinician_Data/clinician_demos_supe.csv')

## merge datasets
setkey(dt, 'uccclientid_asr'); setkey(dt_s, 'uccclientid')
dt <- 
  dt_s[, head(.SD, 1), uccclientid][, .(uccclientid, pt_pretest, pt_posttest)][dt]


### descriptives ####
dt[, client_epi := as.factor(paste(uccclientid_asr, epi, sep='_'))]
dt_sess <- dt[, head(.SD, 1), session]

## pre- and post-DI comparison
ggplot(data=dt_sess[, head(.SD, 1), c('uccclientid', 'epi')]) +
  geom_density(aes(pt_pretest, fill='Pre-treatment'), alpha=.3) +
  geom_density(aes(pt_posttest, fill='Post-treatment'), alpha=.3) +
  scale_fill_manual(name='', 
                    values=c('Pre-treatment'='#AD0000', 'Post-treatment'='#FFC000')) +
  labs(title='Distribution of Distress Index', x='Distress Index') +
  theme_bw()

## distribution of difference DI
ggplot(data=dt_sess[, head(.SD, 1), c('uccclientid', 'epi')]) +
  geom_density(aes(pt_posttest-pt_pretest), fill='#AD0000', alpha=.3) +
  geom_vline(aes(xintercept=mean(pt_posttest-pt_pretest)), linetype='longdash') +
  labs(title='Distribution of Distress Index Change', x='Change in Distress Index') +
  theme_bw()

## average DI over time
ggplot(dt_sess, aes(x=as.factor(sessionNumber), y=ccaps_di)) +
  geom_boxplot() +
  geom_smooth(aes(x=sessionNumber, y=ccaps_di)) +
  labs(title='Trajectory of Distress Index by Session', 
       x='Session #', 
       y='Distress Index') +
  theme_bw()

## individual trajectories of DI
dt_sess[, client_epi := as.factor(paste(uccclientid_asr, epi, sep='_'))]

ggplot(dt_sess, aes(x=sessionNumber, y=ccaps_di)) +
  geom_line(aes(color=client_epi), show.legend=F, alpha=0.2) +
  #geom_point(aes(color=client_epi), show.legend=F, alpha=0.2) +
  scale_x_continuous(breaks=min(dt_sess$sessionNumber):max(dt_sess$sessionNumber),
                     minor_breaks=NULL) +
  labs(title='Trajectory of Distress Index per Client + Episode', 
       x='Session #', 
       y='Distress Index') +
  theme_bw()

dt_sess[, hist(sessionNumber, breaks=0:55)]


## Alliance scores
ggplot(dt_sess, aes(x=as.factor(sessionNumber), y=satisfaction)) +
  geom_boxplot() +
  geom_smooth(aes(x=sessionNumber, y=satisfaction)) +
  labs(title='Trajectory of Overall Alliance by Session', 
       x='Session #', 
       y='Overall Alliance') +
  theme_bw()

fill_vector <- c('Empathy'='#AD0000', 
           'Collaboration'='#DA70D6',
           'Alliance'='#FFC000')
ggplot(dt_sess) +
  geom_density(aes(x=empathy, fill='Empathy'), alpha=0.3) +
  geom_density(aes(x=mispirit, fill='Collaboration'), adjust=2, alpha=0.3) +
  geom_density(aes(x=satisfaction, fill='Alliance'), adjust=2, alpha=0.3) +
  scale_fill_manual(name='', values=fill_vector) +
  labs(title='Distribution of 3 Common Factors per Session', x='Score') +
  theme_bw()

dt_sess$alliance_1 <- 
  as.numeric(
    sapply(dt_sess$alliance_1, function(x) stringr::str_extract(x, '\\d'))
  )
dt_sess$alliance_2 <- 
  as.numeric(
    sapply(dt_sess$alliance_2, function(x) stringr::str_extract(x, '\\d'))
  )
dt_sess$alliance_3 <- 
  as.numeric(
    sapply(dt_sess$alliance_3, function(x) stringr::str_extract(x, '\\d'))
  )
dt_sess$alliance_4 <- 
  as.numeric(
    sapply(dt_sess$alliance_4, function(x) stringr::str_extract(x, '\\d'))
  )
ggplot(melt(dt_sess[, .(sessionNumber, alliance_1, alliance_2, alliance_3, alliance_4)],
            id.vars=c('sessionNumber')), 
       aes(x=as.factor(sessionNumber))) +
  geom_boxplot(aes(y=value)) +
  geom_smooth(aes(x=sessionNumber, y=value)) +
  facet_grid(variable ~ ., scales = 'fixed') +
  labs(title='Trajectory of Alliance Items by Session', 
       x='Session #', 
       y='Score') +
  theme_bw()

ggplot(dt_sess, aes(x=sessionNumber, y=satisfaction)) +
  geom_line(aes(color=client_epi), show.legend=F, alpha=0.3) +
  # geom_point(aes(color=client_epi), show.legend=F) +
  scale_x_continuous(breaks=min(dt_sess$sessionNumber):max(dt_sess$sessionNumber),
                     minor_breaks=NULL) +
  labs(title='Trajectory of Overall Alliance per Client + Episode', 
       x='Session #', 
       y='Overall Alliance') +
  theme_bw()


### residualized gain model ####

### test whether residualized change model is suitable here according to 
### Castro-Schilo & Grimm, 2018. Correlations should be small.
dt[, head(.SD, 1), client_epi][!is.na(pt_pretest), cor(pt_pretest, empathy)]
dt[, head(.SD, 1), client_epi][!is.na(pt_pretest), cor(pt_pretest, mispirit)]
dt[, head(.SD, 1), client_epi][!is.na(pt_pretest), cor(pt_pretest, ave_QUO)]
dt[, head(.SD, 1), client_epi][!is.na(pt_pretest), cor(pt_pretest, (ave_REC+ave_RES))]
dt[, head(.SD, 1), client_epi][!is.na(pt_pretest), cor(pt_pretest, (ave_FA+ave_GI+ave_MIA+ave_MIN+ave_QUC+ave_ST))]
## all ok

### hierarchical multiple regression
model0_lm_intv <- lm('pt_posttest ~ 1', dt[, head(.SD, 1), client_epi])
model1_lm_intv <- lm('pt_posttest ~ pt_pretest', dt[, head(.SD, 1), client_epi])
model2_lm_intv <- lm('pt_posttest ~ pt_pretest+empathy', dt[, head(.SD, 1), client_epi])
model3_lm_intv <- 
  lm('pt_posttest ~ pt_pretest+empathy+mispirit', dt[, head(.SD, 1), client_epi])
model4_lm_intv <- 
  lm('pt_posttest ~ pt_pretest+empathy+mispirit+ave_QUO', 
     dt[, head(.SD, 1), client_epi])
model5_lm_intv <- 
  lm('pt_posttest ~ pt_pretest+empathy+mispirit+ave_QUO+ave_REC', 
     dt[, head(.SD, 1), client_epi])
model6_lm_intv <- 
  lm('pt_posttest ~ pt_pretest+empathy+mispirit+ave_QUO+ave_REC+ave_RES', 
     dt[, head(.SD, 1), client_epi])
model7_lm_intv <- 
  lm('pt_posttest ~ pt_pretest+empathy+mispirit+ave_QUO+ave_REC+ave_RES+ave_FA+ave_GI+ave_MIA+ave_MIN+ave_QUC+ave_ST', 
     dt[, head(.SD, 1), client_epi])

anova(
  model0_lm_intv,
  model1_lm_intv,
  model2_lm_intv,
  model3_lm_intv,
  model4_lm_intv,
  model5_lm_intv,
  model6_lm_intv,
  model7_lm_intv
)
#   Res.Df    RSS Df Sum of Sq        F    Pr(>F)    
# 1    903 521.06                                    
# 2    902 319.13  1   201.928 586.5061 < 2.2e-16 ***
# 3    901 319.01  1     0.121   0.3523 0.5529503    
# 4    900 317.50  1     1.510   4.3851 0.0365356 *  
# 5    899 317.49  1     0.006   0.0180 0.8932591    
# 6    898 313.73  1     3.759  10.9192 0.0009896 ***
# 7    897 313.11  1     0.622   1.8057 0.1793674    
# 8    892 307.11  5     6.007   3.4894 0.0039520 ** 

summary(model3_lm_intv) # mispirit: B=-0.033418 SD=0.016154 t=-2.069 p=0.0389*, dR2=.003
summary(model5_lm_intv) # REC: B=-2.039798 SD=0.621832 t=-3.280 p=0.00108, dR2=.007
summary(model7_lm_intv) # all the rest F=3.4894, dR2=.013

## check if effect of mispirit on pt_posttest is mediated by REC
dt[, ave_REC_100 := ave_REC*100]
formula <- '
  pt_posttest ~ pt_pretest + ave_REC_100 + mispirit
  ave_REC_100 ~ mispirit
'
  
model_med <- sem(
  model=formula,
  data=dt[, .SD[1], client_epi]
)

summary(model_med, fit.measures=T, rsquare=T)
#                 Estimate  Std.Err  z-value  P(>|z|)
# pt_posttest ~                                       
#   pt_pretest        0.651    0.027   23.950    0.000
#   ave_REC          -2.070    0.607   -3.413    0.001
#   mispirit          0.002    0.014    0.115    0.908
# ave_REC ~                                           
#   mispirit          0.012    0.001   18.242    0.000
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)
# .pt_posttest       0.347    0.016   21.260    0.000
# .ave_REC           0.001    0.000   21.260    0.000
# 
# R-Square:
#                 Estimate
# pt_posttest       0.396
# ave_REC           0.269


### latent change score model ####
formula <- '
  ## latent score
  l.chg =~ 1*pt_posttest

  ## regression models
  pt_posttest ~ 1*pt_pretest
  #l.chg ~ ratioopenquestion
  
  ## intercepts
  l.chg ~ 1
  pt_pretest ~ 1
  #ratioopenquestion ~ 1
  pt_posttest ~ 0
  
  ## residual covariances
  pt_pretest ~~ pt_pretest + l.chg #+ ratioopenquestion
  l.chg ~~ l.chg
  #ratioopenquestion ~~ ratioopenquestion
  pt_posttest ~~ 0*pt_posttest
'
model_2.0 <- sem(formula, dt[, head(.SD, 1), uccclientid], estimator='ML', 
                 missing='fiml')
semPaths(object=model_2.0, what='par', esize=17, fade=F, sizeInt=5,
         color=list(man='white', lat='grey75'), label.cex=2, edge.label.cex=1.3, 
         edge.color='black')


### outcome, on within-therapist variability ####
## extract the deviations by the means for each client
get_dev <- function(fitted.model) {
  rr1 <- ranef(fitted.model, condVar = TRUE)
  rr2 <- data.frame(id=rownames(rr1[[1]]),
                    dev=unname(rr1[[1]]))
                    #se=sqrt(c(attr(rr1[[1]], "postVar"))))
  setDT(rr2)
  return(rr2)
}

## calculate the variance for each therapist
split_id <- function(fitted.model) {
  rr2 <- get_dev(fitted.model)
  rr2[, c('uccclientid', 'ucctherapistid_asr') := tstrsplit(id, ':')]
  rr2[, var_within_th := var(dev), ucctherapistid_asr]
  rr2 <- rr2[, .(ucctherapistid_asr = as.integer(ucctherapistid_asr), 
                   var_within)]
  return(rr2)
}

## append therapist variance back to the original dataset
combine <- function(fitted.model, DT) {
  rr <- split_id(fitted.model, level == level)
  setkey(rr, 'ucctherapistid_asr')
  setkey(DT, 'ucctherapistid_asr')
  newDT <- DT[rr[, .SD[1], ucctherapistid_asr]]
  return(newDT)
}

## residualized gain model
model_em1 <- rlmer(empathy ~ 1 + (1 | ucctherapistid_asr/uccclientid),
                   dt[, head(.SD, 1), by=session])

model_sp0 <- lmer(mispirit ~ 1 + (1 | ucctherapistid_asr/uccclientid),
                  dt[, head(.SD, 1), by=session])

formula <- 'countopenquestions ~ 1 + (1|ucctherapistid_asr/uccclientid) + offset(log(th.turn))'
model_op1.5 <- glmer.nb(formula=formula, dt[, head(.SD, 1), session])

formula <- 'countreflection ~ 1 + (1|ucctherapistid_asr/uccclientid) + offset(log(th.turn))'
model_re1.5 <- glmer.nb(formula=formula, dt[, head(.SD, 1), session])

dt <- combine(model_em1, dt)  # var_within
dt <- combine(model_op1.5, dt)  # i.var_within
dt <- combine(model_re1.5, dt)  # i.var_within.1
dt <- combine(model_sp0, dt)  # i.var_within.2


## residualized gain model
dt[, .SD[1], ucctherapistid_asr][!is.na(pt_pretest), cor(pt_pretest, var_within)]
dt[, .SD[1], ucctherapistid_asr][!is.na(pt_pretest), cor(pt_pretest, i.var_within)]
dt[, .SD[1], ucctherapistid_asr][!is.na(pt_pretest), cor(pt_pretest, i.var_within.1)]
dt[, .SD[1], ucctherapistid_asr][!is.na(pt_pretest), cor(pt_pretest, i.var_within.2)]
summary(lm('pt_posttest ~ pt_pretest + var_within', dt[, .SD[1], ucctherapistid_asr]), 
        fit.measures=T)
summary(lm('pt_posttest ~ pt_pretest + i.var_within', dt[, .SD[1], ucctherapistid_asr]), 
        fit.measures=T)
summary(lm('pt_posttest ~ pt_pretest + i.var_within.1', dt[, .SD[1], ucctherapistid_asr]), 
        fit.measures=T)
summary(lm('pt_posttest ~ pt_pretest + i.var_within.2', dt[, .SD[1], ucctherapistid_asr]), 
        fit.measures=T)
## no relationship between between-client variance of these measures and outcomes


## within-client variability
dt_ct <- dt[, .SD[1], .SDcols=c('pt_pretest', 'pt_posttest'), by=uccclientid]
dt_ct[, var_em := dt[, var(empathy), uccclientid][, V1]]
dt_ct[, var_sp := dt[, var(mispirit), uccclientid][, V1]]
dt_ct[, var_op := dt[, var(ratioopenquestion), uccclientid][, V1]]
dt_ct[, var_re := dt[, var(ratioreflection), uccclientid][, V1]]
dt_ct[, alliance := dt[, mean(satisfaction, na.rm=T), uccclientid][, V1]]

summary(lm('pt_posttest ~ pt_pretest', dt_ct), fit.measures=T)
summary(lm('pt_posttest ~ pt_pretest + alliance', dt_ct), fit.measures=T)
summary(lm('pt_posttest ~ pt_pretest + alliance + var_em', dt_ct), fit.measures=T)
summary(lm('pt_posttest ~ pt_pretest + alliance + var_em + var_sp', dt_ct), fit.measures=T)
summary(lm('pt_posttest ~ pt_pretest + alliance + var_em + var_sp + var_op', dt_ct), fit.measures=T)
summary(lm('pt_posttest ~ pt_pretest + alliance + var_em + var_sp + var_op + var_re', dt_ct), fit.measures=T)
# alliance accounts for 11% of variability in residualized gain
# no relationship between between-session variance of these measures and outcome


## multilevel model ####
null.model0 <- lmer('pt_posttest ~ pt_pretest + (1|ucctherapistid_asr)', 
                    dt[, .SD[1], client_epi])
null.model1 <- lmer('pt_posttest ~ pt_pretest + (0+pt_pretest|ucctherapistid_asr)', 
                    dt[, .SD[1], client_epi])
# intercept only: ICC = 0.048
# slope only: ICC = 0.050

model_mlm_glb <- lmer(
  pt_posttest ~ pt_pretest + empathy + mispirit
)


## intervention composition and outcome ####
## calculate the intervention proportions in each session
intv <- data.table(matrix(ncol=10, nrow=0))
colnames(intv) <- 
  c('session', 'FA', 'GI', 'MIA', 'MIN', 'QUC', 'QUO', 'REC', 'RES', 'ST')
for(i in dt[, unique(session)]) {
  temp <- c('session'=i, 'FA'=0, 'GI'=0, 'MIA'=0, 'MIN'=0, 'QUC'=0, 'QUO'=0,
            'REC'=0, 'RES'=0, 'ST'=0)
  dt_session <- dt[session==i & speaker=='T', codes]
  intv_comp <- prop.table(table(dt_session))
  for(j in names(intv_comp)) {
    if(j %in% names(temp)) {
      temp[j] <- intv_comp[j]
    }
  }
  intv <- rbind(intv, t(as.matrix(temp)))
}

## calculate entropy and attach back to the original dataset
intv[, entropy := -diag(as.matrix(intv[, 2:10]) %*% t(log(as.matrix(intv[, 2:10]))))]
intv[is.nan(entropy), entropy := 0]
setkey(intv, 'session'); setkey(dt, 'session')
dt <- intv[dt]

## aggregate entropy by client
dt[, 'ave_entropy' := mean(entropy, na.rm=T), uccclientid_asr]

## see if entropy related to outcome/process ####
dt[, ccaps_di_1 := shift(ccaps_di), client_epi]
dt[, satisfaction_1 := shift(satisfaction), client_epi]

summary(lm('pt_posttest ~ pt_pretest + ave_entropy', dt[, .SD[1], uccclientid_asr]),
        fit.measures=T)
summary(lm('satisfaction ~ satisfaction_1 + entropy', dt[, .SD[1], session]), fit.measures=T)
summary(lm('ccaps_di ~ ccaps_di_1 + entropy', dt[, .SD[1], session]), fit.measures=T)
# no relationship between intervention diversity and outcome
# trend-level negative association between intervention diversity and alliance, R2=.001
# significant positive assocation between intervention diversity and distress, R2=.001

## see if intervention composition related to outcome/process ####
dt[, paste0('ave_', names(dt)[2:10]) := lapply(.SD, mean), .SDcols=2:10, by=uccclientid_asr]

formula <- 'pt_posttest ~ pt_pretest'
summary(lm(formula, dt[, .SD[1], client_epi]), fit.measures=T)

formula <- 
  'pt_posttest ~ pt_pretest + ave_FA + ave_GI + ave_MIA + ave_MIN + ave_QUC + ave_QUO + ave_REC + ave_RES + ave_ST'
summary(lm(formula, dt[, .SD[1], client_epi]), fit.measures=T)
# FA-, GI-, MIA., QUO-, REC-, RES-
# delta R2 = .410 - .388 = .022

## see if intervention composite relates to outcome while considering wihtin-caseload variability
#temp <- dt[, .SD[1], session]
#temp[, c('ccaps_di_1', 'satisfaction_1') := lapply(.SD, shift), 
#     .SDcols=c('ccaps_di', 'satisfaction'), 
#     client_epi]
#temp <- temp[, .(session, ccaps_di_1, satisfaction_1)]
#dt <- dt[temp]

#formula <- 
#  'pt_posttest ~ pt_pretest + FA + GI + MIA + MIN + QUC + QUO + REC + RES + ST + ave_FA + ave_GI + ave_MIA + ave_MIN + ave_QUC + ave_QUO + ave_REC + ave_RES + ave_ST + (0+FA|uccclientid_asr) + (0+GI|uccclientid_asr) + (0+MIA|uccclientid_asr) + (0+MIN|uccclientid_asr) + (0+QUC|uccclientid_asr) + (0+QUO|uccclientid_asr) + (0+REC|uccclientid_asr) + (0+RES|uccclientid_asr) + (0+ST|uccclientid_asr)'
#lmm <- lmer(formula, dt[, .SD[1], session])
#summary(lmm)

formula <- 
  'satisfaction ~ FA + GI + MIA + MIN + QUC + QUO + REC + RES + ST'
summary(lm(formula, dt[, .SD[1], session]), fit.measures=T)
## CONCLUSION:
## FA+, GI+, MIA+, MIN+, QUC., QUO+, REC+, RES+
## R2 = .03

formula <- 
  'alliance ~ ave_FA + ave_GI + ave_MIA + ave_MIN + ave_QUC + ave_QUO + ave_REC + ave_RES + ave_ST'
summary(lm(formula, dt[, .SD[1], uccclientid_asr]), fit.measures=T)
## CONCLUSION:
## FA+, GI+, MIA+, QUC+, QUO+, REC+, RES+
## R2 = .09


## see if SVR can capture multidimensional features for outcome ####
## get evaluation metrics
rmse <- function(predY, trueY) {
  val <- sqrt(mean((predY - trueY)^2))
  return(val)
}
R2 <- function(predY, trueY) {
  ssr <- sum((trueY - predY)^2)
  sst <- sum((trueY - mean(trueY))^2)
  r2 <- 1 - ssr/sst
  return(r2)
}

## split data into train the test
dat <- dt[, .SD[1],
          client_epi,
          .SDcols=c('pt_posttest', 
                    'pt_pretest', 
                    'empathy',
                    'mispirit',
                    'ave_FA', 
                    'ave_GI', 
                    'ave_MIN', 
                    'ave_MIA', 
                    'ave_QUC', 
                    'ave_QUO', 
                    'ave_REC',
                    'ave_RES', 
                    'ave_ST'
                    )
          ][, -1]

set.seed(123)
r2s <- c()
rmses <- c()
i <- 1
while(i <= 50) {
  split_ind <- createDataPartition(dat$pt_posttest, p=0.8, list=F)
  dat_train <- dat[split_ind]
  dat_test <- dat[!split_ind]
  
  ## tune the model, default by Gaussian kernel
  fitControl_svr_misc <- trainControl(
    method='repeatedcv',
    number=10,
    repeats=5,
    search='grid',
    selectionFunction='best',
    savePredictions=T
  )
  model_svr_misc <- train(
    pt_posttest ~ .,
    data=dat_train,
    method='svmRadial',
    metric='RMSE',
    trControl=fitControl_svr_misc,
    #verbose=F
  )
  
  # model_svr_misc <- lm(
  #   pt_posttest ~ .,
  #   data=dat_train
  # )

  preds <- predict(model_svr_misc, dat_test[, .SD, .SDcols=!'pt_posttest'])
  r2 <- R2(preds, dat_test$pt_posttest)
  rmse <- RMSE(preds, dat_test$pt_posttest)

  r2s <- c(r2s, r2)
  rmses <- c(rmses, rmse)
  i <- i + 1
}
mean(r2s, na.rm=T)
mean(rmses, na.rm=T)
# SVR (L2 regularization)
# mean R2 = 0.3364007
# mean RMSE = 0.6141709
#
# LASSO (L1 regularization)
# mean R2 = 0.4097515
# mean RMSE = 0.5825109
#
# As a reference: null linear model predictivity
# lm(pt_posttest ~ pt_pretest), 1000 iter
# mean R2 = 0.3840667
# mean RMSE = 0.590718
#
# As a reference: full linear model predictivity
# lm(pt_posttest ~ .), 1000 iter
# mean R2 = 0.4017732
# mean RMSE = 0.5906903

# ## using full dataset
# svr_intv1 <- best.svm(
#   pt_posttest ~ .,
#   data=dat_train,
#   epsilon=seq(0, 1, .1),
#   cost=2^seq(.5, 8, .5),
#   tunecontrol=tune.control(nrepeat=10,
#                            sampling='cross')
# )
# 
# preds <- predict(svr_intv1$best.model, dat_test[, .SD, .SDcols=!'pt_posttest'])
# 
# rmse(preds - dat_test$pt_posttest)
# R2(preds, dat_test$pt_posttest)
# # RMSE = 0.6407824
# # R2 = 0.3019087


## If using linear kernel
svr_intv1_l <- tune.svm(
  pt_posttest ~ .,
  data=dat,
  kernel='linear',
  epsilon=seq(0, 1, .1),
  cost=2^seq(.5, 8, .5)
)

rmse(svr_intv1_l$best.model$resid)
r2_1_linear <- R2(dat$pt_posttest, svr_intv1_l$best.model$fitted)
print(r2_1_l)
# RMSE = .58
# R2 = .395, no better than lm

coef(svr_intv1_l$best.model)
## very small coefficients for each variable except pt_pretest


## get baseline
dat <- dt[, .SD[1],
          uccclientid_asr,
          .SDcols=c('pt_posttest', 
                    'pt_pretest')][, -1]

svr_intv0 <- tune(
  svm, 
  pt_posttest ~ pt_pretest,
  data=dat,
  ranges=
    list(epsilon=seq(0, 1, .1), cost=2^seq(.5, 8, .5))
)

plot(svr_intv0)

rmse(svr_intv0$best.model$resid)
r2_0 <- R2(dat$pt_posttest, svr_intv0$best.model$fitted)
print(r2_0)

deltaR2 <- r2_1 - r2_0
print(deltaR2)
# delta R2 = .14


## (roughly) inspect relative weight on each variable
## coefs stores (alpha-alpha*), SV stores support vectors x_n. 
## Recall predicting function is 
## f(x_new) = sum_n((alpha_n-alpha_n*)K(x_n, x_new))+b
t(svr_intv1$best.model$SV) %*% svr_intv1$best.model$coefs
#                 [,1]
# pt_pretest  48.515487
# ave_FA       4.503464
# ave_GI      -5.664318
# ave_MIN     12.306098
# ave_MIA      9.251085
# ave_QUC      6.049981
# ave_QUO     -7.252444
# ave_REC    -11.936825
# ave_RES      2.479089
# ave_ST       5.237112


## see if SVR can capture multidimensional features for process ####
temp <- dt[, .(alliance = mean(satisfaction, na.rm=T)), client_epi]
dt <- temp[dt, on='client_epi']

dat <- dt[, .SD[1],
          session,
          .SDcols=c('satisfaction', 
                    'FA', 
                    'GI', 
                    'MIN', 
                    'MIA', 
                    'QUC', 
                    'QUO', 
                    'REC', 
                    'RES', 
                    'ST')][, -1]

svr_intv1_sat <- tune.svm(
  satisfaction ~ FA + GI + MIN + MIA + QUC + 
    QUO + REC + RES + ST,
  data=dat,
  epsilon=seq(0, 1, .1),
  cost=2^seq(.5, 8, .5)
)
# too slow

rmse(svr_intv1_sat$best.model$residuals)
R2(dat$satisfaction[!is.na(dat$satisfaction)], svr_intv1_sat$best.model$fitted)
# RMSE: 1.30
# R2: 0.11

t(svr_intv1_sat$best.model$SV) %*% svr_intv1_sat$best.model$coefs
#           [,1]
# FA  -2.1980149
# GI  -1.6314012
# MIN -1.3757271
# MIA  7.4960868
# QUC -3.2611018
# QUO -5.1467359
# REC  3.9238028
# RES  2.2859881
# ST   0.9360411


## Profile symptom trajectory by growth mixture modeling ####
temp <- data.table(client_epi=dt[, unique(client_epi)])
temp[, ID := .I]
dt_sess <- temp[dt_sess, on='client_epi']

gmm1 <- hlme(fixed=ccaps_di ~ sessionNumber + log(sessionNumber), 
             subject='ID', 
             random=~1 + sessionNumber + log(sessionNumber), 
             ng=1, 
             data=dt_sess)

## per Baldwin et al., 2009
dt_sess[, 'sessionNumber2' := sessionNumber^2]
dt_sess[, 'sessionNumber3' := sessionNumber^3]
gmm1.1 <- hlme(fixed=ccaps_di ~ sessionNumber + sessionNumber2 + sessionNumber3, 
             subject='ID', 
             random=~1 + sessionNumber + sessionNumber2, 
             ng=1, 
             data=dt_sess)
summarytable(gmm1, gmm1.1)
#        G    loglik npm      BIC %class1
# gmm1   1 -2151.514  10 4370.940     100
# gmm1.1 1 -2162.158  11 4399.019     100

gmm2 <- gridsearch(rep=10, 
                   maxiter=10, 
                   minit=gmm1, 
                   hlme(fixed=ccaps_di ~ sessionNumber + log(sessionNumber), 
                        mixture=~1 + sessionNumber + log(sessionNumber), 
                        subject='ID', 
                        random=~1 + sessionNumber + log(sessionNumber), 
                        ng=2, 
                        data=dt_sess, 
                        nwg=T)
                   )
gmm3 <- gridsearch(rep=10, 
                   maxiter=10, 
                   minit=gmm1, 
                   hlme(fixed=ccaps_di ~ sessionNumber + log(sessionNumber), 
                        mixture=~1 + sessionNumber + log(sessionNumber), 
                        subject='ID', 
                        random=~1 + sessionNumber + log(sessionNumber), 
                        ng=3, 
                        data=dt_sess, 
                        nwg=T)
                   )
gmm4 <- gridsearch(rep=25, 
                   maxiter=10, 
                   minit=gmm1, 
                   hlme(fixed=ccaps_di ~ sessionNumber + log(sessionNumber), 
                        mixture=~1 + sessionNumber + log(sessionNumber), 
                        subject='ID', 
                        random=~1 + sessionNumber + log(sessionNumber), 
                        ng=4, 
                        data=dt_sess, 
                        nwg=T)
)

summarytable(gmm1, gmm2, gmm3, gmm4)
#      G    loglik npm      BIC   %class1  %class2  %class3  %class4
# gmm1 1 -2151.514  10 4370.940 100.00000                  
# gmm2 2 -2121.159  15 4344.187  18.76404 81.23596         
# gmm3 3 -2109.125  20 4354.074  81.01124 17.86517 1.123596
# gmm4 4 -2098.413  25 4366.606  16.85393 65.95506 15.39326 1.797753
chisq.diff <- -2*(gmm1$loglik - gmm4$loglik)
df.diff <- 25-10
print(1-pchisq(q=chisq.diff, df=df.diff))
# p < .001


summary(gmm4)
# Fixed effects in the class-membership model:
#   (the class of reference is the last class) 
# 
#                      coef      Se   Wald p-value
# intercept class1  1.24927 0.38121  3.277 0.00105
# intercept class2  0.92796 0.39102  2.373 0.01764
# intercept class3  2.31329 0.36385  6.358 0.00000
# 
# Fixed effects in the longitudinal model:
#
#                               coef      Se   Wald p-value
# intercept class1           2.49359 0.12510 19.932 0.00000
# intercept class2           0.81426 0.10220  7.967 0.00000
# intercept class3           2.05908 0.06778 30.380 0.00000
# intercept class4           2.41241 0.27151  8.885 0.00000
# sessionNumber class1       0.06394 0.01802  3.549 0.00039
# sessionNumber class2      -0.00187 0.01728 -0.108 0.91400
# sessionNumber class3       0.02330 0.01019  2.285 0.02229
# sessionNumber class4      -0.37703 0.06450 -5.845 0.00000
# log(sessionNumber) class1 -0.44776 0.11102 -4.033 0.00006
# log(sessionNumber) class2 -0.19202 0.10108 -1.900 0.05748
# log(sessionNumber) class3 -0.54259 0.06262 -8.665 0.00000
# log(sessionNumber) class4  0.95926 0.35596  2.695 0.00704

# Variance-covariance matrix of the random-effects:
#                    intercept sessionNumber log(sessionNumber)
# intercept            0.42428                                 
# sessionNumber       -0.00368       0.00602                   
# log(sessionNumber)  -0.16639      -0.03836            0.33948

#                                     coef      Se
# Proportional coefficient class1  0.85348 0.21074
# Proportional coefficient class2  0.36558 0.10726
# Proportional coefficient class3  0.94457 0.20472
# Residual standard error:         0.29025 0.00506


temp <- as.data.table(gmm4$pprob)
temp <- temp[, .(ID, class)]
setkey(temp, 'ID'); setkey(dt_sess, 'ID')
dt_sess <- temp[dt_sess]

func1 <- function(x) gmm4$best[4] + gmm4$best[8]*x + gmm4$best[12]*log(x)
func2 <- function(x) gmm4$best[5] + gmm4$best[9]*x + gmm4$best[13]*log(x)
func3 <- function(x) gmm4$best[6] + gmm4$best[14]*log(x)
func4 <- function(x) gmm4$best[7] + gmm4$best[11]*x + gmm4$best[15]*log(x)

# blue, grey, yellow, red
# class1, class2, class3, class4
temp <- dt_sess[!is.na(class)][, mean(ccaps_di, na.rm=T), .(class, sessionNumber)]
temp2 <- dt_sess[!is.na(class), .N, .(class, sessionNumber)][N >= 5]
temp <- temp[temp2, on=c('class', 'sessionNumber')]
temp <- temp[order(class, sessionNumber)]
cbPalette <- c("#56B4E9", "#999999", '#E69F00', "red")

ggplot(dt_sess[!is.na(class)], aes(x=sessionNumber, y=ccaps_di)) +
  geom_line(aes(group=client_epi, color=as.factor(class)), show.legend=F, alpha=0.2) +
  #geom_point(aes(color=client_epi), show.legend=F, alpha=0.2) +
  #stat_function(fun=func1, color='#56B4E9', alpha=0.5, size=0.8) +
  #stat_function(fun=func2, color='#999999', alpha=0.5, size=0.8) +
  #stat_function(fun=func3, color='#E69F00', alpha=0.5, size=0.8) +
  #stat_function(fun=func4, color='red', alpha=0.5, size=0.8) +
  geom_line(data=temp, aes(x=sessionNumber, y=V1, group=class, 
                           color=as.factor(class)), size=0.8) +
  scale_x_continuous(breaks=min(dt_sess$sessionNumber):max(dt_sess$sessionNumber),
                     minor_breaks=NULL,
                     limits=c(1, 25)) +
  scale_y_continuous(limits=c(0, 4)) +
  scale_color_manual(name='Group', values=cbPalette) +
  labs(title='Trajectory of Distress Index per Client & Episode', 
       x='Session #', 
       y='Distress Index') +
  theme_bw()

## multinomial regression to see which interventions better predict class ####
model_mn_intv <- 
  multinom(class ~ ave_FA+ave_GI+ave_MIA+ave_QUC+ave_QUO+ave_REC+ave_RES+ave_ST,
           dt_sess[!is.na(class), .SD[1], client_epi])
z <- summary(model_mn_intv)$coefficients / summary(model_mn_intv)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
exp(coef(model_mn_intv)/100)   # odds ratio
exp(coef(model_mn_intv)/100) / (1 + exp(coef(model_mn_intv)/100))   # probability
print(p)    # p-value by z-test

logLik1 <- logLik(model_mn_intv)
logLik0 <- logLik(multinom(
  class ~ 1,
  dt_sess[!is.na(class), .SD[1], client_epi]
))
sprintf('McFadden R2: %.4f', 1 - (logLik1/logLik0))

# odds ratio
#   (Intercept)   ave_FA   ave_GI   ave_MIA   ave_QUC  ave_QUO  ave_REC  ave_RES    ave_ST
# 2   0.8503727 1.232078 1.221805 1.0781419 1.2072727 1.262526 1.269835 1.153546 1.1351567
# 3   0.8116172 1.310051 1.326861 1.0553305 1.1737937 1.358631 1.315729 1.202647 1.0105096
# 4   0.9533304 1.037518 1.072719 0.9443119 0.9879763 1.005993 1.092923 1.037414 0.8311269
#
# probability
#    (Intercept)    ave_FA    ave_GI   ave_MIA   ave_QUC   ave_QUO   ave_REC   ave_RES
# 3   0.4480070 0.5671092 0.5702364 0.5134602 0.5399747 0.5760253 0.5681706 0.5460008
# 2   0.4595683 0.5519870 0.5499156 0.5188009 0.5469522 0.5580162 0.5594394 0.5356497
# 4   0.4880538 0.5092069 0.5175420 0.4856792 0.4969759 0.5014937 0.5221993 0.5091817
#      ave_ST
# 3 0.5026137
# 2 0.5316503
# 4 0.4538882
#
# p-values
#   (Intercept)      ave_FA      ave_GI   ave_MIA    ave_QUC     ave_QUO    ave_REC
# 3  0.03540553 0.006085348 0.009949711 0.6670109 0.18238931 0.005009265 0.01171508
# 2  0.02493652 0.003604688 0.012929468 0.4210406 0.03364284 0.004215320 0.00261410
# 4  0.82176149 0.860393536 0.764632665 0.8309674 0.96330487 0.979993943 0.70366841
#      ave_RES    ave_ST
# 3 0.09636225 0.9379685
# 2 0.07880073 0.1939590
# 4 0.87712641 0.5372552
#
# McFadden R2: 0.0317


## manova to see if intervention composites differ between classes ####
psych::mardia(dt_sess[, .SD[1], client_epi][, 69:(77-1)])
# Highly multivariate non-normal

## map proportion p in (0, 1) to R through logit link
temp <- dt_sess[, .SD[1], by=client_epi, .SDcols=69:77]
temp[, paste0('logit_', names(temp[, 2:ncol(temp)])) := 
       lapply(.SD, function(x) log(x/(1-x))),
     .SDcols=2:ncol(temp)]
temp[is.infinite(logit_ave_ST), logit_ave_ST := 0]
temp[, grep('^ave_', names(temp)) := NULL]
dt_sess <- dt_sess[temp, on='client_epi']

## parametric method
psych::mardia(temp[, 2:(ncol(temp)-1)])
temp[, lapply(.SD, shapiro.test), .SDcols=2:ncol(temp)]   # still not normal, but...
dt_sess <- temp[dt_sess, on='client_epi']

summary(
  manova(
    cbind(logit_ave_FA,
          logit_ave_GI,
          logit_ave_MIA,
          logit_ave_MIN,
          logit_ave_QUC,
          logit_ave_QUO,
          logit_ave_REC,
          logit_ave_RES,
          logit_ave_ST)
    ~ class,
    dt_sess[, .SD[1], client_epi]
  )
)

for(j in names(dt_sess)[79:87]) {
   print(
     pairwise.t.test(
       dt_sess[, .SD[1], client_epi][[j]], 
       dt_sess[, .SD[1], client_epi][['class']]
     )
   )
}
# significant predictors: MIN(1v2, 1v3), QUO(1v2), ST(1v3, 2v3)


## nonparametric method
model_rmanova_intv <- 
  MANOVA.wide(
    cbind(ave_FA, 
          ave_GI, 
          ave_MIA, 
          ave_QUC, 
          ave_QUO, 
          ave_REC, 
          ave_RES,
          ave_ST) ~ as.factor(class),
    data=dt_sess[, .SD[1], client_epi],
    subject='client_epi',
    iter=1000
  )
summary(model_manova)
# Wald-Type Statistic (WTS):
#                  Test statistic df   p-value 
# as.factor(class) "62.81"        "24" "<0.001"
# 
# modified ANOVA-Type Statistic (MATS):
#                   Test statistic
# as.factor(class)         44.478
# 
# p-values resampling:
#                   paramBS (WTS) paramBS (MATS)
# as.factor(class) "0.031"       "0.051" 

simCI(model_manova, contrast='pairwise', type='Tukey')
#   contrast p.value
#      2 - 1  0.3031
#      3 - 1  0.1625
#      4 - 1  0.9959
#      3 - 2  0.7544
#      4 - 2  0.9698
#      4 - 3  0.8347


## manova to see if empathy and mispirit differ between classes ####
meansByClass <- 
  dt_sess[, .(class=class[1], 
              empathyMean=mean(empathy, na.rm=T), 
              mispiritMean=mean(mispirit, na.rm=T)), 
          client_epi]

shapiro.test(meansByClass[, empathyMean])
shapiro.test(meansByClass[, mispiritMean])
psych::mardia(meansByClass[, .(empathyMean, mispiritMean)])

car::leveneTest(meansByClass[, empathyMean], meansByClass[, class])
car::leveneTest(meansByClass[, mispiritMean], meansByClass[, class])
heplots::boxM(meansByClass[, .(empathyMean, mispiritMean)], meansByClass[, class])

## parametric test with transformed data
summary(
  manova(
    cbind(empathyMean, mispiritMean) ~ class,
    meansByClass
  )
)
#            Df    Pillai approx F num Df den Df  Pr(>F)  
# V1          1 0.0072298   3.2298      2    887 0.04003 *
# Residuals 888  


## nonparametric test using bootstrapping
model_bootmanova_emsp <- 
  MANOVA.wide(
    cbind(V2, V3) ~ as.factor(V1),
    data=dt_sess[, 
                 .(class[1], mean(empathy, na.rm=T), mean(mispirit, na.rm=T)), 
                 client_epi],
    iter=1000,
    resampling='WildBS',
    seed=123
  )
summary(model_bootmanova_emsp)
# Wald-Type Statistic (WTS):
#   Test statistic df  p-value
# as.factor(V1) "13.066"       "6" "0.042"
# 
# modified ANOVA-Type Statistic (MATS):
#   Test statistic
# as.factor(V1)         16.344
# 
# p-values resampling:
#   WildBS (WTS) WildBS (MATS)
# as.factor(V1) "0.053"      "0.022"

## nonparametric test using rank-sum
model_rmanova_emsp <- 
  rankMANOVA::rankMANOVA(
    cbind(V2, V3) ~ as.factor(V1),
    data=dt_sess[, 
                 .(class[1], mean(empathy, na.rm=T), mean(mispirit, na.rm=T)), 
                 client_epi],
    iter=1000,
    resampling='WildBS',
    seed=123
  )
summary(model_rmanova_emsp)
# Test: p-values based on the wild bootstrap
# Test statistic p-value
# as.factor(V1) "8.72"         "0.327"

## descriptive analysis
pairwise.t.test(meansByClass[, empathyMean], meansByClass[, class])
#   1     2     3    
# 2 0.555 -     -    
# 3 0.053 0.332 -    
# 4 1.000 1.000 1.000
dt_sess[, .(class[1], 
            mean(empathy, na.rm=T),
            mean(mispirit, na.rm=T)),
        client_epi
        ][, mean(V2), V1]
# class   mean
# 2   3.838497
# 4   3.847019
# 1   3.788610
# 3   3.898799

pairwise.t.test(meansByClass[, mispiritMean], meansByClass[, class])
#   1     2     3    
# 2 0.037 -     -    
# 3 0.036 1.000 -    
# 4 1.000 1.000 1.000
meansByClass[, mean(mispiritMean), class]
# class   mean
# 2   4.545637
# 4   4.371429
# 1   4.188556
# 3   4.661288

simCI(model_rmanova_emsp, contrast='pairwise', type='Tukey')
#   contrast p.value
#     2 - 1   0.309
#     3 - 1   0.211
#     4 - 1   0.986
#     3 - 2   0.873
#     4 - 2   0.998
#     4 - 3   0.952

## see if therapists also clutered by classes ####
fisher.test(
  as.matrix(
    table(dt_sess[, .SD[1], client_epi][, .(as.factor(class), as.factor(ucctherapistid_asr))])
  ),
  simulate.p.value=T,
  B=1e5
)
# classes 1-4: p-value = 0.4103
# classes 1-3: p-value = 0.1921


## see if alliance is clustered by classes using anova ####
shapiro.test(dt_sess[, alliance[1], client_epi][, V1])
car::leveneTest(
  dt_sess[, alliance[1], client_epi][, V1], 
  dt_sess[, class[1], client_epi][, as.factor(V1)]
)
# not normally distributed, variances are homogeneous

## rank-based anova
kruskal.test(
  dt_sess[, alliance[1], client_epi][, V1], 
  dt_sess[, class[1], client_epi][, V1]
)
# chi^2=7.45, df=3, p=.059, marginal significance

## anova by bootstrap
lmboot::ANOVA.boot(
  V2~as.factor(V1), 
  data=dt_sess[, .(class[1], alliance[1]), client_epi][!is.nan(V2) & !is.na(V1)],
  seed=222
)$`p-value`
# p~=.063, marginal significance

## descriptive
dt_sess[, mean(alliance[1], na.rm=T), class]
# class     mean
#     2 7.000000
#     1 6.639578
#     3 9.000000
#     4 8.527815

## post-hoc test
pairwise.wilcox.test(
  dt_sess[, alliance[1], client_epi][, V1], 
  dt_sess[, class[1], client_epi][, V1]
)
# 1    2    3   
# 2 0.84 -    -   
# 3 0.40 0.28 -   
# 4 0.28 0.28 0.44

## see if clients' past SI relate to classes ####
length(intersect(dt_sess[, unique(uccclientid_asr)], dt_c[, unique(uccclientid)]))
dt_sess[, uniqueN(uccclientid_asr)]
dt_c[, uniqueN(uccclientid)]
# dt_sess is a subset of dt_c

dt_c <- dt_c[uccclientid %in% dt_sess[, unique(uccclientid_asr)]]
setkey(dt_sess, 'uccclientid_asr'); setkey(dt_c, 'uccclientid')
dt_sess <- dt_c[dt_sess]; rm(dt_c)

asmpCheck.chisq <- function(xsq) {
  prop <- sum(xsq$expected < 5) / (dim(xsq$expected)[1] * dim(xsq$expected)[2])
  print(prop)
  if(prop > .20) print('small-size expected values more than 20% of all expected values')
}


clientid_si <- dt_sess[, .SD[1], uccclientid][con_suic==1, uccclientid]
class_si <- dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_si, class]
class_nsi <- dt_sess[, .SD[1], uccclientid][!uccclientid %in% clientid_si, class]
temp <- chisq.test(cbind(table(class_si), table(class_nsi)))
asmpCheck.chisq(temp)
# X-squared = 46.01, df = 3, p-value = 5.643e-10

rstatix::cramer_v(cbind(table(class_si), table(class_nsi)))
# Cramer's V = .236, medium to large

cbind(prop.table(table(class_si)), prop.table(table(class_nsi)))
#   class_si   class_nosi
# 1 0.26829268 0.1205937
# 2 0.64459930 0.6734694
# 3 0.06271777 0.1910946
# 4 0.02439024 0.0148423


## see if SI clients who had SI <= past 1 month relate to class ####
clientid_si <- dt_sess[, .SD[1], uccclientid][
  con_suic == 1 &
    con_sui_lt %in% c("Within the last month", "Within the last 2 weeks"), 
  uccclientid
]
clientid_nsi <- dt_sess[, .SD[1], uccclientid][
  con_suic == 1 &
    con_sui_lt %in% c("Within the last 1-5 years", 
                      "Within the last year",
                      "More than 5 years ago"), 
  uccclientid
]
class_si <- dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_si, class]
class_nsi <- dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_nsi, class]
temp <- chisq.test(cbind(table(class_si), table(class_nsi)))
asmpCheck.chisq(temp)
fisher.test(cbind(table(class_si), table(class_nsi)))
# p-value = 0.1624

cbind(prop.table(table(class_si)), prop.table(table(class_nsi)))
#   class_si   class_nosi
# 1 0.30357143 0.25974026
# 2 0.62500000 0.64935065
# 3 0.01785714 0.07359307
# 4 0.05357143 0.01731602


## see if SA relates to class ####
clientid_sa <- dt_sess[, .SD[1], uccclientid][att_suic==1, uccclientid]
class_sa <- dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_sa, class]
class_nsa <- dt_sess[, .SD[1], uccclientid][!uccclientid %in% clientid_sa, class]
temp <- chisq.test(cbind(table(class_sa), table(class_nsa)))
asmpCheck.chisq(temp)
# X-squared = 13.775, df = 3, p-value = 0.003229

rstatix::cramer_v(cbind(table(class_sa), table(class_nsa)))
# Cramer's V = .129, medium to large

cbind(prop.table(table(class_sa)), prop.table(table(class_nsa)))
#   class_si   class_nosi
# 1 0.28846154 0.16408269
# 2 0.61538462 0.66666667
# 3 0.03846154 0.15374677
# 4 0.05769231 0.01550388

## see if SA clients who had SA <= past 1 month relate to class ####
clientid_sa <- dt_sess[, .SD[1], uccclientid][
  att_suic == 1 &
    att_sui_lt %in% c("Within the last month", "Within the last 2 weeks"), 
  uccclientid
]
clientid_nsa <- dt_sess[, .SD[1], uccclientid][
  att_suic == 1 &
    att_sui_lt %in% c("Within the last 1-5 years", 
                      "Within the last year",
                      "More than 5 years ago"), 
  uccclientid
]
table_sa <- table(dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_sa, class])
table_nsa <- table(dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_nsa, class])
n <- max(length(table_sa), length(table_nsa))
length(table_sa) <- n
length(table_nsa) <- n
table_sa[is.na(table_sa)] <- 0
table_nsa[is.na(table_nsa)] <- 0
temp <- chisq.test(cbind(table_sa, table_nsa))
asmpCheck.chisq(temp)
fisher.test(cbind(table_sa, table_nsa))
# p-value = 0.2195

cbind(prop.table(table_sa), prop.table(table_nsa))
#   class_si   class_nosi
# 1 0.30357143 0.25974026
# 2 0.62500000 0.64935065
# 3 0.01785714 0.07359307
# 4 0.05357143 0.01731602


## see if past trauma relates to class ####
clientid_tr <- dt_sess[, .SD[1], uccclientid][trauma==1, uccclientid]
clientid_ntr <- dt_sess[, .SD[1], uccclientid][trauma==0, uccclientid]
class_tr <- dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_tr, class]
class_ntr <- dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_ntr, class]
temp <- chisq.test(cbind(table(class_tr), table(class_ntr)))
asmpCheck.chisq(temp)
# X-squared = 9.8682, df = 3, p-value = 0.01972

rstatix::cramer_v(cbind(table(class_tr), table(class_ntr)))
# Cramer's V = .110, medium

cbind(prop.table(table(class_tr)), prop.table(table(class_ntr)))
#   class_tr   class_ntr
# 1 0.19034091 0.15184382
# 2 0.68750000 0.64859002
# 3 0.10511364 0.18004338
# 4 0.01704545 0.01952278


## see if trauma clients who had trauma <= past 1 month relate to class ####
clientid_tr <- dt_sess[, .SD[1], uccclientid][
  trauma==1 & 
    trauma_lt %in% c("Within the last month", "Within the last 2 weeks"), 
  uccclientid
]
clientid_ntr <- dt_sess[, .SD[1], uccclientid][
  trauma==1 & 
    trauma_lt %in% c("Within the last 1-5 years", 
                     "Within the last year",
                     "More than 5 years ago"), 
  uccclientid
]
table_tr <- table(dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_tr, class])
table_ntr <- table(dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_ntr, class])
n <- max(length(table_tr), length(table_ntr))
length(table_tr) <- n
length(table_ntr) <- n
table_tr[is.na(table_tr)] <- 0
table_ntr[is.na(table_ntr)] <- 0
temp <- chisq.test(cbind(table_tr, table_ntr))
asmpCheck.chisq(temp)
fisher.test(cbind(table_tr, table_ntr))
# p-value = 0.558


## see if SI, SA, and trauma prdict class ####
dt_sess_sel <- dt_sess[, .SD[1], uccclientid][!is.na(class), .(
  con_suic,
  att_suic,
  trauma,
  class=as.factor(make.names(class))
)]
dt_sess_sel <- na.omit(dt_sess_sel)
split_ind <- createDataPartition(dt_sess_sel[, class], times=1, p=.70, list=F)
train_sel <- dt_sess_sel[split_ind]
test_sel <- dt_sess_sel[-split_ind]

train_sel_up <- 
  setDT(upSample(x=train_sel[, .SD, .SDcols=!'class'], y=train_sel[, class]))
train_sel_down <- 
  setDT(downSample(x=train_sel[, .SD, .SDcols=!'class'], y=train_sel[, class]))
train_sel_SMOTE <- 
  setDT(DMwR::SMOTE(class~., train_sel))

fitControl_mn_hist <- 
  trainControl(
    method='repeatedcv',
    number=10,
    repeats=10,
    classProbs=T,
    summaryFunction=multiClassSummary,
    savePredictions=T
  )
model_mn_hist <- 
  train(
    as.factor(Class) ~ .,
    data=train_sel_up,
    method='multinom',
    trControl=fitControl_mn_hist,
    metric='logLoss',
    verbose=F,
  )

logLik1 <- logLik(model_mn_hist$finalModel)
logLik0 <- logLik(nnet::multinom(Class ~ 1, train_sel_up))
sprintf('McFadden R2: %.4f', 1 - (logLik1/logLik0))
# R2 = 0.08, poor fit

preds_class <- predict(model_mn_hist, test_sel[, .SD, .SDcols=!'class'], type='raw')
preds_prob <- predict(model_mn_hist, test_sel[, .SD, .SDcols=!'class'], type='prob')

F_meas(table(preds_class, test_sel[, class]))
# F1 score: 0.2954545

baseline_meas <- function(obs, meas=NULL, type='bootstrap', iter=1000, seed=123) {
  if(!is.factor(obs)) {
    stop('Check if obs is a factor vector. If you are using numeric vector, convert the vector to factor by as.factor')
  }
  if(is.null(meas)) {
    stop('Please specify measure. You may type "recall", "precision", or "f1"')
  }
  else if(!meas %in% c('recall', 'precision', 'f1')) {
    stop('Unknown measure. Please type "recall", "precision", or "f1"')
  }
  
  measure <- function(cm, meas, majority=F) {
    precision <- diag(cm) / rowSums(cm)
    recall <- diag(cm) / colSums(cm)
    f1 <- ifelse(precision+recall == 0, 0, 2*precision*recall/(precision+recall))
    
    precision[is.na(precision)] <- 0
    recall[is.na(recall)] <- 0
    f1[is.na(f1)] <- 0
    
    if(isFALSE(majority)) FUN <- mean
    else FUN <- max
    
    if(meas=='recall') FUN(recall)
    else if(meas=='precision') FUN(precision)
    else if(meas=='f1') FUN(f1)
  }
  if(type=='bootstrap') {
    random_sample <- function(obs) {
      c <- levels(obs)
      n <- length(obs)
      p <- prop.table(table(obs))
      x <- sample(c, size=n, p=p, replace=T)
      return(x)
    }
    set.seed(seed)
    scores <- c()
    i <- 1
    while(i <= iter) {
      preds <- factor(random_sample(obs), levels=levels(obs))
      score <- measure(table(preds, obs), meas=meas)
      scores <- c(scores, score)
      i <- i + 1
    }
  }
  else if(type=='surrogate') {
    set.seed(seed)
    scores <- c()
    i <- 1
    while(i <= iter) {
      preds <<- sample(obs, length(obs))
      score <- measure(table(preds, obs), meas=meas)
      scores <- c(scores, score)
      i <- i + 1
    }
  }
  else if(type=='majority') {
    level <- levels(obs)
    scores <- c()
    for(l in level) {
      preds <- factor(rep(l, length(obs)), levels=levels(obs))
      score <- measure(table(preds, obs), meas=meas, majority=T)
      scores <- c(scores, score)
    }
  }
  else {
    stop('Unknown method. Currently only supports "bootstrap", "surrogate", "majority".')
  }
  return(mean(scores, na.rm=T))
}
baseline_meas(test_sel[, class], 'f1', type='bootstrap')
# baseline F1: 0.2489889
# baseline prAUC: 0.25

confusionMatrix(preds_class, test_sel[, class], mode='prec_recall')
# pred vs. ref
#     X1  X2  X3  X4
# X1  13  31   2   1
# X2   0   0   0   0
# X3  18 108  28   3
# X4  10  23   5   0

multiClassSummary(
  data.table(obs=test_sel[, class], 
             pred=preds_class,
             preds_prob), 
  c('X1', 'X2', 'X3', 'X4')
)
# recall: 0.27926829
# precision: 0.3136324
# rocAUC: 0.50114054
# prAUC: 0.21375261


## see which primary concern predicts classes ####
model_mn_pc <- 
  multinom(
    as.factor(class) ~ 
      pc_academic +
      pc_adhd +
      pc_adjust_uofu +
      pc_adjust_slc +
      pc_adjust_usa +
      pc_anger +
      pc_anxiety +
      pc_bipolar +
      pc_body_image +
      pc_career_issues +
      pc_depression +
      pc_discrimination +
      pc_divorce_separate +
      pc_eating_disorder +
      pc_existential_identity +
      pc_being_parent +
      pc_fam_origin +
      pc_financial +
      pc_grief +
      pc_phys_health +
      pc_homeless +
      pc_learning_disorder +
      pc_legal +
      pc_loneliness +
      pc_ocd +
      pc_tsd +
      pc_porn +
      pc_racism +
      pc_rel_friends +
      pc_rel_part +
      pc_religion +
      pc_self_esteem +
      pc_self_harm +
      pc_sexual_assault +
      pc_sexual_concerns +
      pc_sexual_orientation +
      pc_shy +
      pc_social_anx +
      pc_subs_use +
      pc_suicide_attempt +
      pc_test_perf_anx +
      pc_td_halluc +
      pc_thought_suicide +
      pc_trauma +
      pc_work +
      pc_other,
    dt_sess[!is.na(class), .SD[1], uccclientid]
  )
z <- summary(model_mn_pc)$coefficients / summary(model_mn_pc)$standard.errors
p <- (1 - pnorm(abs(z), mean=0, sd=1)) * 2
exp(coef(model_mn_pc))   # odds ratio
print(p)    # p-value by z-test

logLik1 <- logLik(model_mn_pc)
logLik0 <- logLik(multinom(
  class ~ 1,
  dt_sess[!is.na(class), .SD[1], uccclientid]
))
sprintf('McFadden R2: %.4f', 1 - (logLik1/logLik0))

# Significant predictors: 
# academic(1v4), anxiety(1v234), body image(1v2*, 1v4), depression(1v234), eating disorder(1v3), 
# family origin(1v2*), physical health(1v4), homeless(1v3*), legal(1v3*), 
# loneliness(1v34), porn(1v34), racism(1v4), rel friends(1v4), rel partner(1v4),
# religion(1v4), self-esteem(1v4*), sex assault(1v4*), sex orien(1v4), subs use(1v3*)
# thought suicide(1v3), work(1v3*)
#
# Odds ratios:
# academic(1.028*), anxiety(.998***, .997***, 1.025***), body image(1.005, 1.021*), 
# depression(.994***, .984***, 1.006***), eating disorder(.88***), family origin(.996),
# physical health(.83***), homeless(.94), legal(.96), loneliness(.986***, 1.02*), 
# porn(1.02*, 1.03*), racism(.86***), rel friends(.95*), rel partner(1.02*), religion(.767***), 
# self-esteem(.98), sex assualt(1.03), sex orient(1.03*), subs use(1.01), 
# thought suicide(.984*), work(.99)
#
# McFadden R2: .219

## change reference group to 2
dt_sess[, class := as.factor(class)]
dt_sess$class <- relevel(dt_sess$class, ref=2)
# Significant predictors: 
# academic(2v4), adhd(2v3*), adjust_usa(2v3), anger(2v4*), 
# anxiety(2v3), bipolor(2v3), depression(2v34), eating disorder(2v3), 
# family origin(2v3*), physical health(2v4), homeless(2v3),
# loneliness(2v34), racism(2v4), rel friends(2v4), rel partner(2v4),
# religion(2v4), self-esteem(2v4*), self-harm(2v3), 
# sex assault(2v4), sex concern(2v3*4), sex orien(2v4), subs use(2v3)
# suicide attempt(2v4)
# 
# odds ratios:
# academic(1.028*), adhd(.990), adjust_usa(.869***), anger(.999), anxiety(.991***), 
# bipolar(.976*), depression(.990***, 1.012***), eating disorder(.879***), 
# family origin(1.006), physical health(.808***), homeless(.841***), 
# loneliness(.987***, 1.022*), racism(.843***), rel friends(.957*), rel partner(1.019*),
# religion(.771***), self-esteem(.981), self-harm(.867***), sex assault(1.032*), 
# sex concern(1.008, .875***), sex orien(1.043*), subs use(1.015*), suicide attempt(.891***)

## change reference group to 3
dt_sess$class <- relevel(dt_sess$class, ref=3)
# significant odds ratios:
# academic(1.029*), anger(1.020), anxiety(1.016***), bipolar(1.038), depression(1.023***), 
# physical health(.847***), loneliness(1.035**), racism(.881***), rel friends(.954*), 
# rel partner(1.016), religion(.788***), self-esteem(.981), sex assault(1.037*), 
# sex concern(.892***), sex orien(1.046*), test perf(1.020), thought suicide(1.022)


## use multinomial regression to build predicting model and see how it works
dt_sess_sel <- dt_sess[, .SD[1], uccclientid][!is.na(class), .(
  pc_academic,
  pc_adhd,
  pc_adjust_uofu,
  pc_adjust_slc,
  pc_adjust_usa,
  pc_anger,
  pc_anxiety,
  pc_bipolar,
  pc_body_image,
  pc_career_issues,
  pc_depression,
  pc_discrimination,
  pc_divorce_separate,
  pc_eating_disorder,
  pc_existential_identity,
  pc_being_parent,
  pc_fam_origin,
  pc_financial,
  pc_grief,
  pc_phys_health,
  pc_homeless,
  pc_learning_disorder,
  pc_legal,
  pc_loneliness,
  pc_ocd,
  pc_tsd,
  pc_porn,
  pc_racism,
  pc_rel_friends,
  pc_rel_part,
  pc_religion,
  pc_self_esteem,
  pc_self_harm,
  pc_sexual_assault,
  pc_sexual_concerns,
  pc_sexual_orientation,
  pc_shy,
  pc_social_anx,
  pc_subs_use,
  pc_suicide_attempt,
  pc_test_perf_anx,
  pc_td_halluc,
  pc_thought_suicide,
  pc_trauma,
  pc_work,
  pc_other,
  class=as.factor(make.names(class))
)]
dt_sess_sel <- na.omit(dt_sess_sel)
split_ind <- createDataPartition(dt_sess_sel[, class], times=1, p=.70, list=F)
train_sel <- dt_sess_sel[split_ind]
test_sel <- dt_sess_sel[-split_ind]

train_sel_up <- 
  setDT(upSample(x=train_sel[, .SD, .SDcols=!'class'], y=train_sel[, class]))
train_sel_down <- 
  setDT(downSample(x=train_sel[, .SD, .SDcols=!'class'], y=train_sel[, class]))
train_sel_SMOTE <- 
  setDT(DMwR::SMOTE(class~., train_sel))

fitControl_mn_pc <- 
  trainControl(
    method='repeatedcv',
    number=10,
    repeats=5,
    classProbs=T,
    summaryFunction=multiClassSummary,
    savePredictions=T
  )
model_mn_pc <- 
  train(
    as.factor(Class) ~ .,
    data=train_sel_up,
    method='multinom',
    trControl=fitControl_mn_pc,
    metric='logLoss',
    verbose=F,
  )

logLik1 <- logLik(model_mn_pc$finalModel)
logLik0 <- logLik(multinom(Class ~ 1, train_sel_up))
sprintf('McFadden R2: %.4f', 1 - (logLik1/logLik0))
# this may not be useful since train set is augmented
# McFadden R2 (logLoss): 0.4978 
# McFadden R2 (Mean_F1): 0.5491

preds_class <- predict(model_mn_pc, test_sel[, .SD, .SDcols=!'class'], type='raw')
preds_prob <- predict(model_mn_pc, test_sel[, .SD, .SDcols=!'class'], type='prob')

F_meas(table(preds_class, test_sel[, class]))
# F1 score (logLoss): 0.3214286
# F1 score (Mean_F1): 0.3166667
# F1 score (rocAUC): 0.3166667
# F1 score (prAUC): 0.3214286

baseline_meas(test_sel[, class], meas='f1', type='bootstrap')
# F1 baseline: 0.2488061 (bootstrap)
# F1 baseline: 0.2502215 (surrogate)
# F1 baseline: 0.3447465 (majority guess)
# prAUC baseline: 0.25 (majority guess)

confusionMatrix(preds_class, test_sel[, class], mode='prec_recall')
# Accuracy : 0.4106        
# 95% CI : (0.3485, 0.4748)
# No Information Rate : 0.6667          
# P-Value [Acc > NIR] : 1               
# Kappa : 0.1394         
# Mcnemar's Test P-Value : 1.343e-10 
#
#                      Class: X1 Class: X2 Class: X3 Class: X4
# Precision              0.25714    0.6914    0.3506   0.00000
# Recall                 0.42857    0.3415    0.7500   0.00000
# F1                     0.32143    0.4571    0.4779       NaN
# Prevalence             0.17073    0.6667    0.1463   0.01626
# Detection Rate         0.07317    0.2276    0.1098   0.00000
# Detection Prevalence   0.28455    0.3293    0.3130   0.07317
# Balanced Accuracy      0.58683    0.5183    0.7560   0.46281

multiClassSummary(
  data.table(obs=test_sel[, class], 
             pred=preds_class,
             preds_prob), 
  c('X1', 'X2', 'X3', 'X4')
)
# logLoss                      AUC                  prAUC               Accuracy 
# 1.2367360              0.6814433              0.3597695              0.4105691 
# Kappa                Mean_F1       Mean_Sensitivity       Mean_Specificity 
# 0.1394036                NaN              0.3800087              0.7819361 
# Mean_Pos_Pred_Value    Mean_Neg_Pred_Value    Mean_Precision        Mean_Recall 
# 0.3247876              0.7845732              0.3247876              0.3800087 
# Mean_Detection_Rate Mean_Balanced_Accuracy 
# 0.1026423              0.5809724 


## see if {} appear to be indicators of group 4 ####
# {} = academic, depression, loneliness, rel partner, sex assualt, sex orien
clientid_x <- 
  dt_sess[, .SD[1], uccclientid][
    pc_academic==1 | 
      #pc_depression==1 |
      pc_loneliness==1 |
      pc_rel_part==1 |
      pc_sexual_assault==1 |
      pc_sexual_orientation==1
    , uccclientid
  ]
clientid_nx <- dt_sess[!uccclientid %in% clientid_x, unique(uccclientid)]
# too many samples in clientid_x, will use stepwise method to incrementally remove variables
# order determined by the # samples when a variable==1 from small to large, i.e.
# dt_sess[, .SD[1], uccclientid][pc_academic==1 & class==4, .N]

table_x <- table(dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_x, class])
table_nx <- table(dt_sess[, .SD[1], uccclientid][uccclientid %in% clientid_nx, class])
n <- max(length(table_x), length(table_nx))
length(table_x) <- n
length(table_nx) <- n
table_x[is.na(table_x)] <- 0
table_nx[is.na(table_nx)] <- 0
temp <- chisq.test(cbind(table_x, table_nx))
asmpCheck.chisq(temp)
# baseline: X-squared = 47.21, df = 3, p-value = 3.135e-10, N=734:106

rstatix::cramer_v(cbind(table_x, table_nx))
# baseline: Cramer's V = .238

prop.table(cbind(table_x, table_nx), margin=2)

# baseline (all variables)
#   class_x   class_nx
# 1 0.19001387 0.04761905
# 2 0.67267684 0.60000000
# 3 0.11650485 0.35238095
# 4 0.02080444 0.00000000
# 
# -sex assault
# 1 0.19241573 0.04385965
# 2 0.67134831 0.61403509
# 3 0.11516854 0.34210526
# 4 0.02106742 0.00000000
# 
# -sex assault + sex orien
# 1 0.19405099 0.04166667
# 2 0.66997167 0.62500000
# 3 0.11473088 0.33333333
# 4 0.02124646 0.00000000
# 
# -sex assault + sex orien + rel partner
# 1 0.19878604 0.06586826
# 2 0.68285281 0.58682635
# 3 0.09559939 0.34730539
# 4 0.02276176 0.00000000
# 
# -sex assault + sex orien + rel partner + loneliness
# 1 0.20127796 0.06586826
# 2 0.68051118 0.58682635
# 3 0.09424920 0.34730539
# 4 0.02396166 0.00000000
# 
# -sex assault + sex orien + rel partner + loneliness + academic
# 1 0.22352941 0.088607595
# 2 0.68431373 0.629746835
# 3 0.06666667 0.275316456
# 4 0.02549020 0.006329114
#
#
# -depression
# 1 0.19211823 0.1152074
# 2 0.66009852 0.6728111
# 3 0.12315271 0.2119816
# 4 0.02463054 0.0000000
#
# -depression + academic
# 1 0.6267606 0.3732394
# 2 0.5602190 0.4397810
# 3 0.4545455 0.5454545
# 4 0.8000000 0.2000000
#
# -depression + academic + loneliness
# 1 0.3239437 0.6760563
# 2 0.3229927 0.6770073
# 3 0.4049587 0.5950413
# 4 0.6000000 0.4000000
#
# -depression + academic + loneliness + rel_part
# 1 0.09859155 0.9014085
# 2 0.08211679 0.9178832
# 3 0.06611570 0.9338843
# 4 0.20000000 0.8000000
#
# -depression + academic + loneliness + rel_part + sex_orien
# 1 0.03521127 0.9647887
# 2 0.04014599 0.9598540
# 3 0.03305785 0.9669421
# 4 0.13333333 0.8666667


## try random forest classifier to see which predictors have more weights ####
dt_sess_sel <- dt_sess[, .SD[1], uccclientid][!is.na(class), .(
   pc_academic,
   pc_adhd,
   pc_adjust_uofu,
   pc_adjust_slc,
   pc_adjust_usa,
   pc_anger,
   pc_anxiety,
   pc_bipolar,
   pc_body_image,
   pc_career_issues,
   pc_depression,
   pc_discrimination,
   pc_divorce_separate,
   pc_eating_disorder,
   pc_existential_identity,
   pc_being_parent,
   pc_fam_origin,
   pc_financial,
   pc_grief,
   pc_phys_health,
   pc_homeless,
   pc_learning_disorder,
   pc_legal,
   pc_loneliness,
   pc_ocd,
   pc_tsd,
   pc_porn,
   pc_racism,
   pc_rel_friends,
   pc_rel_part,
   pc_religion,
   pc_self_esteem,
   pc_self_harm,
   pc_sexual_assault,
   pc_sexual_concerns,
   pc_sexual_orientation,
   pc_shy,
   pc_social_anx,
   pc_subs_use,
   pc_suicide_attempt,
   pc_test_perf_anx,
   pc_td_halluc,
   pc_thought_suicide,
   pc_trauma,
   pc_work,
   pc_other,
   class
 )
]

dt_sess_sel[, names(dt_sess_sel) := lapply(.SD, as.factor)]
dt_sess_sel <- na.omit(dt_sess_sel)
split_ind <- caret::createDataPartition(dt_sess_sel[, class], times=1, p=.70, list=F)
train_sel <- dt_sess_sel[split_ind]
test_sel <- dt_sess_sel[-split_ind]

pred_sel <- rfcv(
  trainx=train_sel[, .SD, .SDcols=!'class'],
  trainy=train_sel[, class],
  cv.fold=10
)
# num of vars used at each step and their corresponding error rates
#        46        23        12         6         3         1 
# 0.3541667 0.3524306 0.3385417 0.3454861 0.3350694 0.3420139

tune_randfor_pc <- 
  tuneRF(
    x=train_sel_up[, .SD, .SDcols=!'Class'],
    y=train_sel_up[, Class], 
    ntreeTry=100,
    doBest=T,
    xtest=test_sel[, .SD, .SDcols=!'class'],
    ytest=test_sel[, class],
    importance=T, 
    proximity=T
  )
# OOB estimate of  error rate: 31.92%
# Confusion matrix:
#   1   2 3 class.error
# 1 0 100 0 1.000000000
# 2 0 382 1 0.002610966
# 3 0  80 4 0.952380952
# Test set error rate: 32.23%
# Confusion matrix:
#   1   2 3 class.error
# 1 0  42 0 1.000000000
# 2 0 163 1 0.006097561
# 3 0  35 1 0.972222222
# 
# biased towards the majority class

## use oversampling/undersampling method to overcome imbalanced training data
train_sel_up <- 
  setDT(caret::upSample(x=train_sel[, .SD, .SDcols=!'class'], y=train_sel[, class]))
# OOB estimate of  error rate: 13.05%
# Confusion matrix:
#   1   2   3 class.error
# 1 355  12  16  0.07310705
# 2  57 266  60  0.30548303
# 3   0   5 378  0.01305483
# Test set error rate: 46.28%
# Confusion matrix:
#   1   2  3 class.error
# 1 11  26  5   0.7380952
# 2 23 107 34   0.3475610
# 3  3  21 12   0.6666667

train_sel_down <- 
  setDT(caret::downSample(x=train_sel[, .SD, .SDcols=!'class'], y=train_sel[, class]))
# OOB estimate of  error rate: 48.41%
# Confusion matrix:
#   1  2  3 class.error
# 1 45 27 12   0.4642857
# 2 30 31 23   0.6309524
# 3 11 19 54   0.3571429
# Test set error rate: 57.02%
# Confusion matrix:
#   1  2  3 class.error
# 1 26 13  3   0.3809524
# 2 60 57 47   0.6524390
# 3  4 11 21   0.4166667

train_sel_SMOTE <- 
  setDT(DMwR::SMOTE(class~., train_sel))
# OOB estimate of  error rate: 25.17%
# Confusion matrix:
#   1   2   3 class.error
# 1 44  31   7   0.4634146
# 2 13 194  47   0.2362205
# 3  3  47 202   0.1984127
# Test set error rate: 47.93%
# Confusion matrix:
#   1  2  3 class.error
# 1  7 26  9   0.8333333
# 2 12 99 53   0.3963415
# 3  1 15 20   0.4444444

importance(tune_randfor_pc)
varImpPlot(tune_randfor_pc)

# the failure of random forest to make accurate prediction may due to the fact that latent
# clases are by nature not clearly boundaried, e.g., there are significant overlaps
# between groups 2 and 3.

## 
## calculate transition matrix of codes ####
extractTransitionMatrix <- function(x) {
  mat <- markovchainFit(x)
  return(mat$estimate@transitionMatrix)
}

dt_sess_tr <- data.table(
  ucctherapistid=numeric(),
  uccclientid=numeric(),
  session=numeric(),
  trMat=numeric()
)
entry <- c()
for(i in unique(dt$session)) {
  th_id <- dt[session==i, unique(ucctherapistid_asr)]
  ct_id <- dt[session==i, unique(uccclientid_asr)]
  # trMat <- extractTransitionMatrix(dt[session==i & speaker=='T', codes])
  codes <- dt[session==i & speaker=='T', codes]
  temp <- 1 - mean(codes == shift(codes), na.rm=T)
  
  if(prod(dim(trMat)) != 81) next
  
  # temp2 <- prop.table(table(dt[session==i & speaker=='T', codes]))
  # entry <- c(th_id, ct_id, i, 1 - sum(diag(trMat) * temp2))
  entry <- c(entry, temp)
  # dt_sess_tr <- rbind(dt_sess_tr, t(entry), use.names=F)
  
  # dim(trMat) <- c(1, prod(dim(trMat)))
  # entry <- list(th_id, ct_id, i)
  # entry <- append(entry, as.list(trMat))
  # dt_sess_tr <- rbind(dt_sess_tr, entry, fill=T)
}
