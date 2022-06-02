rm(list=ls())

library(tidyverse)
library(fixest)
library(modelsummary)

source('1-codes-2022/bwc/2-analysis/aux_diff_coef.R')
source('1-codes-2022/bwc/2-analysis/rand_inference_funs.R')


dat = readr::read_rds('0-data/3-analysis-data/event_level_analysis_full_data.rds')
strat = readr::read_rds('0-data/2-processed-data/2-randomization/stratification-bins-full-data.rds') %>%
        dplyr::mutate(no_experimental_officer = dplyr::if_else(strat_sum == 0,1,0)) %>%
        dplyr::select(-strat_sum)

dat = dat %>% dplyr::left_join(strat,by='id_event')

rm(strat)

dat$precinct_day = as.factor(as.numeric(dat$day)*1e5+as.numeric(dat$id_precinct)) 
dat$precinct_week = as.factor(dat$experimental_week*1e5+as.numeric(dat$id_precinct))
dat$weeks_from_start = as.factor(as.character(dat$experimental_week))

dat$precinct_id = as.factor(as.character(dat$id_precinct))

##############################################################################
## Between Event - Experimental Precinct - Post Data - Cluster Precinct Day ##
##############################################################################

dat_main = dat %>%
        dplyr::filter(early_post == 1 & dummy_blackout == 0 & experimental_precinct == 1)

##################
## Main Effects ##
##################

res = feols(c(dummy_recording,dummy_police_report,dummy_victims,negative_interaction_index,
              citizen_bad_behavior,dep_var_force_exc_hand_arrest,dep_var_hand_arrest) ~ 
                    treat +  ..("strat")  | precinct_week + weekday + n_officers ,
            data = dat_main,cluster=~precinct_week)

res_list = summary(.l(res),cluster=~precinct_week)

coef_res = res_list %>%
        purrr::map( ~ .x['coefficients']) %>%
        purrr::map( ~ .x[[1]][stringr::str_detect(names(.x[[1]]),pattern='treat')])

names = names(coef_res[[1]])

coef_res = coef_res %>% dplyr::bind_cols()

names(coef_res) = names(res)

coef_res = cbind(names,coef_res)


# add randomization inference p-value
ri = readr::read_rds('0-data/4-randomisation-inference/results/main_ate.rds')

ri = ri %>%
        dplyr::mutate(estimates = row.names(.),
                      names = dplyr::case_when(
                              stringr::str_detect(estimates,'treat') ~ 'treat'
                      )
                      
        ) %>%
        dplyr::arrange(names,estimates)


p_value = ri_result(estimate='treat',estimated_model = res,
                    estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

names(p_value) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
                   'Use of Force (exc. H/A)','Handcuff and/or Arrest')

p_value$term = ''

p_value = p_value %>%
        dplyr::relocate(term)

attr(p_value,'position') = c(3)

## Formatting Table

cm <- c('treat' = 'Event treated')

names(res) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
               'Use of Force (exc. H/A)','Handcuff and/or Arrest')


table = modelsummary(res,
                     fmt = "%.3f",
                     estimate="{estimate}{stars}",
                     statistic = "({std.error})",
                     coef_map = cm,
                     add_rows = p_value,
                     stars =  c('*' = .1, '**' = .05,'***' = 0.01),
                     gof_omit = 'dat$|Std. Errors|N|FE|Deviance|R2|AIC|BIC|Log.Lik',
                     output='data.frame') %>%
        dplyr::filter(part != 'gof') %>%
        dplyr::select(-part,-statistic) %>%
        dplyr::mutate(
                term = dplyr::case_when(
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\(')+0) == 1 ~ '',
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\[')+0) == 1 ~ '',
                        TRUE ~ term)
                
        ) 

table %>%
        utils::write.table(sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
                           file='../paper/v16/new-tables/fragment-main-result-event-level.tex')


###########################
## Heterogeneity by Risk ##
###########################

res = feols(c(dummy_recording,dummy_police_report,dummy_victims,negative_interaction_index,
              citizen_bad_behavior,dep_var_force_exc_hand_arrest,dep_var_hand_arrest) ~ 
                    as.factor(risk_high) + as.factor(risk_high):treat +  ..("strat") | precinct_week + weekday + n_officers ,
            data = dat_main,cluster=~precinct_week)

res_list = summary(.l(res),cluster=~precinct_week)

coef_res = res_list %>%
        purrr::map( ~ .x['coefficients']) %>%
        purrr::map( ~ .x[[1]][stringr::str_detect(names(.x[[1]]),pattern='treat')])

names = names(coef_res[[1]])

coef_res = coef_res %>% dplyr::bind_cols()

names(coef_res) = names(res)

coef_res = cbind(names,coef_res)

ri = readr::read_rds('0-data/4-randomisation-inference/results/main_het_risk.rds')

ri = ri %>%
        dplyr::mutate(estimates = row.names(.),
                      names = dplyr::case_when(
                              stringr::str_detect(estimates,'as.factor\\(risk_high\\)0') ~ 'as.factor(risk_high)0:treat',
                              stringr::str_detect(estimates,'as.factor\\(risk_high\\)1') ~ 'as.factor(risk_high)1:treat')
                      
        ) %>%
        dplyr::arrange(names,estimates)

p_value_low = ri_result(estimate='as.factor(risk_high)0:treat',estimated_model = res,
                        estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

p_value_high = ri_result(estimate='as.factor(risk_high)1:treat',estimated_model = res,
                         estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

p_value = p_value_low %>%
        dplyr::bind_rows(p_value_high)

rm(p_value_low,p_value_high)

names(p_value) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
                   'Use of Force (exc. H/A)','Handcuff and/or Arrest')

p_value$term = ''

p_value = p_value %>%
        dplyr::relocate(term)

attr(p_value,'position') = c(3,3+3)


names(res) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
               'Use of Force (exc. H/A)','Handcuff and/or Arrest')

cm <- c('as.factor(risk_high)0:treat' = 'Event treated x Low Risk',
        'as.factor(risk_high)1:treat' = 'Event treated x High Risk')

table = modelsummary(res,
                     fmt = "%.3f",
                     estimate="{estimate}{stars}",
                     statistic = "({std.error})",
                     coef_map = cm,
                     add_rows = p_value,
                     stars =  c('*' = .1, '**' = .05,'***' = 0.01),
                     gof_omit = 'dat$|Std. Errors|N|FE|Deviance|R2|AIC|BIC|Log.Lik',
                     output='data.frame') %>%
        dplyr::filter(part != 'gof') %>%
        dplyr::select(-part,-statistic) %>%
        dplyr::mutate(
                term = dplyr::case_when(
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\(')+0) == 1 ~ '',
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\[')+0) == 1 ~ '',
                        TRUE ~ term)
                
        ) 

table %>%
        utils::write.table(sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
                           file='../paper/v16/new-tables/fragment-het-risk-event-level.tex')


# ## Coef Test
# 
# coef_test = purrr::map(res_list,function(x){
#         difftest_lm(x1='as.factor(risk_high)0:treat',x2='as.factor(risk_high)1:treat',model=x,type = 'x1 = x2') ## Always x1 against x2
# }) %>%
#         dplyr::bind_rows() %>%
#         dplyr::mutate(se = paste('(',se,')',sep='')) %>%
#         dplyr::mutate(est = dplyr::case_when(
#                 p_one_sided <= 0.01 ~ paste(as.character(est),'***',sep=''),
#                 p_one_sided <= 0.05 ~ paste(as.character(est),'**',sep=''),
#                 p_one_sided <= 0.1 ~ paste(as.character(est),'*',sep=''),
#                 p_one_sided > 0.1 ~ as.character(est)
#         )
#         ) %>%
#         dplyr::mutate(p = paste('[',p_one_sided,']',sep='')) %>%
#         dplyr::select(est,se,p) %>%
#         t() %>%
#         as.matrix()
# 
# name = as.matrix(c('Coef. Difference','',''),nrow=3,ncol=1)
# 
# coef_test = cbind(name,coef_test)
# 
# row.names(coef_test) = NULL
# 
# utils::write.table(coef_test,sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
#                    file='../paper/v15/new-tables/fragment-het-risk-event-level-coeftest-onesided.tex')

#########################
## Treatment Intensity ##
#########################

dat_main = dat_main %>%
        dplyr::mutate(treat_intensity = dplyr::case_when(
                (nT == 0) ~ 0,
                (nT == 1) ~ 1,
                (nT >= 2) ~ 2)
        )

res = feols(c(dummy_recording,dummy_police_report,dummy_victims,negative_interaction_index,
              citizen_bad_behavior,dep_var_force_exc_hand_arrest,dep_var_hand_arrest) ~ 
                    i(treat_intensity,ref=0) +  ..("strat") | precinct_week + weekday + n_officers,
            data = dat_main,cluster=~precinct_week)

res_list = summary(.l(res),cluster=~precinct_week)

coef_res = res_list %>%
        purrr::map( ~ .x['coefficients']) %>%
        purrr::map( ~ .x[[1]][stringr::str_detect(names(.x[[1]]),pattern='treat')])

names = names(coef_res[[1]])

coef_res = coef_res %>% dplyr::bind_cols()

names(coef_res) = names(res)

coef_res = cbind(names,coef_res)

#load randomization inference results
ri = readr::read_rds('0-data/4-randomisation-inference/results/main_het_treat_intensity.rds')

ri = ri %>%
        dplyr::mutate(estimates = row.names(.),
                      names = dplyr::case_when(
                              stringr::str_detect(estimates,'treat_intensity::1') ~ 'treat_intensity::1',
                              stringr::str_detect(estimates,'treat_intensity::2') ~ 'treat_intensity::2')
                      
        ) %>%
        dplyr::arrange(names,estimates)

p_value_low = ri_result(estimate='treat_intensity::1',estimated_model = res,
                        estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

p_value_high = ri_result(estimate='treat_intensity::2',estimated_model = res,
                         estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

p_value = p_value_low %>%
        dplyr::bind_rows(p_value_high)

rm(p_value_low,p_value_high)

names(p_value) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
                   'Use of Force (exc. H/A)','Handcuff and/or Arrest')

p_value$term = ''

p_value = p_value %>%
        dplyr::relocate(term)

attr(p_value,'position') = c(3,3+3)

## Formatting Table

cm <- c('treat_intensity::1' = 'Event treated by 1 Camera',
        'treat_intensity::2' = 'Event treated by 2 or More Cameras')


names(res) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
               'Use of Force (exc. H/A)','Handcuff and/or Arrest')


table = modelsummary(res,
                     fmt = "%.3f",
                     estimate="{estimate}{stars}",
                     statistic = "({std.error})",
                     coef_map = cm,
                     add_rows = p_value,
                     stars =  c('*' = .1, '**' = .05,'***' = 0.01),
                     gof_omit = 'dat$|Std. Errors|N|FE|Deviance|R2|AIC|BIC|Log.Lik',
                     output='data.frame') %>%
        dplyr::filter(term != 'Std.Errors') %>%
        dplyr::select(-part,-statistic) %>%
        dplyr::mutate(
                term = dplyr::case_when(
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\(')+0) == 1 ~ '',
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\[')+0) == 1 ~ '',
                        TRUE ~ term)
                
        ) 

table %>%
        utils::write.table(sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
                           file='../paper/v16/new-tables/fragment-het-cam-intensity-event-level.tex')



## Coef. Test
# 
# coef_test = purrr::map(res_list,function(x){
#         difftest_lm(x1='treat_intensity::1',x2='treat_intensity::2',model=x,type = 'x1 = x2') ## Always x1 against x2
# }) %>%
#         dplyr::bind_rows() %>%
#         dplyr::mutate(se = paste('(',se,')',sep='')) %>%
#         dplyr::mutate(est = dplyr::case_when(
#                 p_one_sided <= 0.01 ~ paste(as.character(est),'***',sep=''),
#                 p_one_sided <= 0.05 ~ paste(as.character(est),'**',sep=''),
#                 p_one_sided <= 0.1 ~ paste(as.character(est),'*',sep=''),
#                 p_one_sided > 0.1 ~ as.character(est)
#         )
#         ) %>%
#         dplyr::mutate(p = paste('[',p_one_sided,']',sep='')) %>%
#         dplyr::select(est,se,p) %>%
#         t() %>%
#         as.matrix()
# 
# name = as.matrix(c('Coef. Difference','',''),nrow=3,ncol=1)
# 
# coef_test = cbind(name,coef_test)
# 
# row.names(coef_test) = NULL
# 
# utils::write.table(coef_test,sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
#                    file='../paper/v15/new-tables/fragment-het-cam-intensity-event-level-coeftest-onesided.tex')

###########################
## Heterogeneity by Rank ##
###########################

dat_main = dat_main %>%
        dplyr::mutate(
                treat_rank = dplyr::case_when(
                        (nT_high_rank >= 1 & nT_low_rank == 0) ~ 'only_above',
                        (nT_high_rank == 0 & nT_low_rank >= 1) ~ 'only_below',
                        (nT_high_rank >= 1 & nT_low_rank >= 1) ~ 'both',
                        (nT_high_rank == 0 & nT_low_rank == 0) ~ 'none'),
                any_high_rank = (num_high_rank >= 1)+0
        )



res = feols(c(dummy_recording,dummy_police_report,dummy_victims,negative_interaction_index,
              citizen_bad_behavior,dep_var_force_exc_hand_arrest,dep_var_hand_arrest) ~
                    any_high_rank + i(treat_rank,ref='none') +  ..("strat")  | precinct_week + weekday + n_officers,
            data = dat_main,cluster=~precinct_week)


res_list = summary(.l(res),cluster=~precinct_week)

coef_res = res_list %>%
        purrr::map( ~ .x['coefficients']) %>%
        purrr::map( ~ .x[[1]][stringr::str_detect(names(.x[[1]]),pattern='treat')])

names = names(coef_res[[1]])

coef_res = coef_res %>% dplyr::bind_cols()

names(coef_res) = names(res)

coef_res = cbind(names,coef_res)

ri = readr::read_rds('0-data/4-randomisation-inference/results/main_het-rank.rds')

ri = ri %>%
        dplyr::mutate(estimates = row.names(.),
                      names = dplyr::case_when(
                              stringr::str_detect(estimates,'treat_rank::only_below') ~ 'treat_rank::only_below',
                              stringr::str_detect(estimates,'treat_rank::only_above') ~ 'treat_rank::only_above',
                              stringr::str_detect(estimates,'treat_rank::both') ~ 'treat_rank::both')
                      
        ) %>%
        dplyr::arrange(names,estimates)

p_value_below = ri_result(estimate='treat_rank::only_below',estimated_model = res,
                          estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

p_value_above = ri_result(estimate='treat_rank::only_above',estimated_model = res,
                          estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

p_value_both = ri_result(estimate='treat_rank::both',estimated_model = res,
                         estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

p_value = p_value_below %>%
        dplyr::bind_rows(p_value_above,p_value_both)

rm(p_value_below,p_value_above,p_value_both)

names(p_value) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
                   'Use of Force (exc. H/A)','Handcuff and/or Arrest')

p_value$term = ''

p_value = p_value %>%
        dplyr::relocate(term)

attr(p_value,'position') = c(3,3+3,3+3*2)



names(res) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
               'Use of Force (exc. H/A)','Handcuff and/or Arrest')

cm <- c('treat_rank::only_below' = 'Event Treated by Officer(s) with Soldier rank',
        'treat_rank::only_above' = 'Event Treated by Officer(s) with higher than Soldier rank',
        'treat_rank::both' = 'Event Treated by Officers of both types')

table = modelsummary(res,
                     fmt = "%.3f",
                     estimate="{estimate}{stars}",
                     statistic = "({std.error})",
                     coef_map = cm,
                     add_rows = p_value,
                     stars =  c('*' = .1, '**' = .05,'***' = 0.01),
                     gof_omit = 'dat$|Std. Errors|N|FE|Deviance|R2|AIC|BIC|Log.Lik',
                     output='data.frame') %>%
        dplyr::filter(part != 'gof') %>%
        dplyr::select(-part,-statistic) %>%
        dplyr::mutate(
                term = dplyr::case_when(
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\(')+0) == 1 ~ '',
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\[')+0) == 1 ~ '',
                        TRUE ~ term)
                
        ) 

table %>%
        utils::write.table(sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
                           file='../paper/v16/new-tables/fragment-camhold-rank-spec.tex')

#####################
## Additional Info ##
#####################

adjust_numeric = function(x){x = format(round(as.numeric(x),3),nsmall=3)}
adjust_percent = function(x){x = format(round(as.numeric(x),1),nsmall=1)
        x = paste(as.character(x),'\\%',sep='')
        return(x)}

mean_dep_var = dat_main %>%
        filter(treat == 0) %>%
        dplyr::select(dummy_recording,dummy_police_report,dummy_victims,negative_interaction_index, 
                      citizen_bad_behavior,dep_var_force_exc_hand_arrest,dep_var_hand_arrest) %>%
        dplyr::summarise_all(mean,na.rm=TRUE) %>% 
        dplyr::mutate_all(adjust_numeric) %>%
        dplyr::mutate_all(as.character) %>%
        as.data.frame()

# mean_dep_var[which(mean_dep_var == '0')] = '0.000'

nobs = as.character(round(nrow(dat_main),digits=0))

rows = tribble(~term, ~'Dispatch Recorded',~'Police Report',~'Victims',~'Negative Interaction Index',
               ~'Citizen Bad Behavior',~'Use of Force (exc. H/A)',~'Handcuff and/or Arrest',
               'Mean DV Control',mean_dep_var[,1],mean_dep_var[,2],mean_dep_var[,3],
               mean_dep_var[,4],mean_dep_var[,5],mean_dep_var[,6],mean_dep_var[,7],
               'N',nobs,nobs,nobs,nobs,nobs,nobs,nobs)

n = mean_dep_var
n[,] = nobs

add = mean_dep_var %>%
        dplyr::bind_rows(n) 

add = add %>%
        mutate(term = NA) %>%
        relocate(term) 

add$term[1] = 'Mean Dep. Var. Control Events'
add$term[2] = '\\textit{N}'


add %>% utils::write.table(sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
                           file='../paper/v16/new-tables/fragment-main-event-level-add-info.tex')

##############################################
## Create Violence Measures by Census Tract ##
##############################################

dat = dat %>%
        dplyr::mutate(
                type_crime = ((aarmedrob + aburglary + aassault + asexual + 
                                       adomestic + aland + aland_violent + amob + 
                                       ariot + amurder + anoise + averbal + athreat + 
                                       adrug + aother_crime_nonviolent + aother_crime_violent) > 0)+0,
                type_stopsearch = ((astopsearch_ind + astopsearch_veh + astopsearch_rescom)> 0)+0,
                type_traffic = ((atrafficaccident + atrafficothers) > 0)+0,
                type_other = ((type_crime + type_stopsearch + type_traffic)==0)+0,
                event_type = dplyr::case_when(
                        (type_crime == 1) ~ 1,
                        (type_stopsearch == 1 & type_crime == 0 & type_traffic == 0) ~ 2,
                        (type_traffic == 1 & type_crime == 0 & type_stopsearch == 0) ~ 3,
                        (type_other == 1) ~ 4)
        )

dat$type_crime[is.na(dat$type_crime)] = 0
dat$type_stopsearch[is.na(dat$type_stopsearch)] = 0
dat$type_traffic[is.na(dat$type_traffic)] = 0
dat$type_other[is.na(dat$type_other)] = 0

## Select Violence Variables

dat_pre = dat %>%
        dplyr::filter(experimental_week %in% -26:-14) %>%
        dplyr::select(id_event,id_precinct,code_tract,dep_var_force_exc_hand_arrest,type_crime) %>%
        dplyr::mutate(dep_var_force_exc_hand_arrest = dep_var_force_exc_hand_arrest/100)

## Keep Unique Precinct for Each Census Tract

precinct_tract = dat_pre %>%
        dplyr::group_by(id_precinct,code_tract) %>%
        count() %>%
        dplyr::ungroup() %>%
        dplyr::group_by(code_tract) %>%
        slice_max(n) %>%
        slice_sample(n=1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) 

crime = dat %>%
        dplyr::select(-id_precinct) %>%
        dplyr::filter(experimental_week %in% -13:-1) %>%
        dplyr::left_join(precinct_tract,by='code_tract') %>%
        dplyr::select(code_tract,id_precinct,type_crime,dummy_victims,dep_var_force_exc_hand_arrest,
                      dep_var_hand_arrest,citizen_bad_behavior) %>%
        # filter(experimental_week < 0 & !is.na(code_tract)) %>%
        group_by(code_tract,id_precinct) %>%
        summarise(sum_crime = sum(type_crime,na.rm=TRUE),
                  sum_victims = sum(dummy_victims)/100,
                  sum_force = sum(dep_var_force_exc_hand_arrest)/100,
                  sum_hand_arrests = sum(dep_var_hand_arrest)/100,
                  sum_dis_contempt = sum(citizen_bad_behavior)/100,
                  sum_all = n())

crime = crime %>%
        group_by(id_precinct) %>%
        mutate(above_events = case_when(sum_all <= median(sum_all,na.rm=T) ~ 0,
                                        sum_all > median(sum_all,na.rm=T) ~ 1),
               above_crime = case_when(sum_crime <= median(sum_crime,na.rm=T) ~ 0,
                                       sum_crime > median(sum_crime,na.rm=T) ~ 1),
               above_victims = case_when(sum_victims <= median(sum_victims,na.rm=T) ~ 0,
                                         sum_victims > median(sum_victims,na.rm=T) ~ 1),
               above_force = case_when(sum_force <= median(sum_force,na.rm=T) ~ 0,
                                       sum_force > median(sum_force,na.rm=T) ~ 1),
               above_hand_arrest = case_when(sum_hand_arrests <= median(sum_hand_arrests,na.rm=T) ~ 0,
                                             sum_hand_arrests > median(sum_hand_arrests,na.rm=T) ~ 1),
               above_dis_contempt = case_when(sum_dis_contempt <= median(sum_dis_contempt,na.rm=T) ~ 0,
                                              sum_dis_contempt > median(sum_dis_contempt,na.rm=T) ~ 1),
               events_pctile = ntile(sum_all,100),
               crime_pctile = ntile(sum_crime,100),
               victims_pctile = ntile(sum_victims,100),
               force_pctile = ntile(sum_force,100),
               hand_arrest_pctile = ntile(sum_hand_arrests,100),
               dis_contempt_pctile = ntile(sum_dis_contempt,100)
        ) %>%
        ungroup()

names(crime)[2:ncol(crime)] = paste0(names(crime)[2:ncol(crime)],'_bl_ctract')

##########################
## Joining with Data ##
##########################

dat = dat %>%
        dplyr::left_join(crime,by='code_tract')

dat = dat %>%
        dplyr::mutate(
                crime_quantile_bl_ctract = dplyr::case_when(
                        crime_pctile_bl_ctract <= 25 ~ 1,
                        crime_pctile_bl_ctract > 25 & crime_pctile_bl_ctract <= 50  ~ 2,
                        crime_pctile_bl_ctract > 50 & crime_pctile_bl_ctract <= 75  ~ 3,
                        crime_pctile_bl_ctract > 75 ~ 4),
                events_quantile_bl_ctract = dplyr::case_when(
                        events_pctile_bl_ctract <= 25 ~ 1,
                        events_pctile_bl_ctract > 25 & events_pctile_bl_ctract <= 50  ~ 2,
                        events_pctile_bl_ctract > 50 & events_pctile_bl_ctract <= 75  ~ 3,
                        events_pctile_bl_ctract > 75 ~ 4)
        )

dat_main = dat %>%
        dplyr::filter(early_post == 1 & dummy_blackout == 0 & experimental_precinct == 1)


dat_main$above_crime_bl_ctract[is.na(dat_main$above_crime_bl_ctract)] = 0
dat_main$above_force_bl_ctract[is.na(dat_main$above_force_bl_ctract)] = 0

res = feols(c(dummy_recording,dummy_police_report,dummy_victims,negative_interaction_index,
              citizen_bad_behavior,dep_var_force_exc_hand_arrest,dep_var_hand_arrest) ~ 
                    as.factor(above_force_bl_ctract) + as.factor(above_force_bl_ctract):treat +  
                    ..("strat") | precinct_week + weekday + n_officers ,
            data = dat_main,cluster=~precinct_week)

res_list = summary(.l(res),cluster=~precinct_week)

coef_res = res_list %>%
        purrr::map( ~ .x['coefficients']) %>%
        purrr::map( ~ .x[[1]][stringr::str_detect(names(.x[[1]]),pattern='treat')])

names = names(coef_res[[1]])

coef_res = coef_res %>% dplyr::bind_cols()

names(coef_res) = names(res)

coef_res = cbind(names,coef_res)

ri = readr::read_rds('0-data/4-randomisation-inference/results/main_het-force-ctract.rds')

ri = ri %>%
        dplyr::mutate(estimates = row.names(.),
                      names = dplyr::case_when(
                              stringr::str_detect(estimates,'as.factor\\(above_force_bl_ctract\\)0') ~ 'as.factor(above_force_bl_ctract)0:treat',
                              stringr::str_detect(estimates,'as.factor\\(above_force_bl_ctract\\)1') ~ 'as.factor(above_force_bl_ctract)1:treat')
                      
        ) %>%
        dplyr::arrange(names,estimates)

p_value_low = ri_result(estimate='as.factor(above_force_bl_ctract)0:treat',estimated_model = res,
                        estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

p_value_high = ri_result(estimate='as.factor(above_force_bl_ctract)1:treat',estimated_model = res,
                         estimated_coef = coef_res,ri_data = ri,type_test = 'two-tailed') %>%
        purrr::map( ~ .x['p_value']) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate_all(adjust_p)

p_value = p_value_low %>%
        dplyr::bind_rows(p_value_high)

rm(p_value_low,p_value_high)

names(p_value) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
                   'Use of Force (exc. H/A)','Handcuff and/or Arrest')

p_value$term = ''

p_value = p_value %>%
        dplyr::relocate(term)

attr(p_value,'position') = c(3,3+3)


names(res) = c('Dispatch Recorded','Police Report','Victims','Negative Interaction Index','Citizen Bad Behavior',
               'Use of Force (exc. H/A)','Handcuff and/or Arrest')

cm <- c('as.factor(above_force_bl_ctract)0:treat' = 'Event treated x Below Median Use-of-Force',
        'as.factor(above_force_bl_ctract)1:treat' = 'Event treated x Above Median Use-of-Force')


table = modelsummary(res,
                     fmt = "%.3f",
                     estimate="{estimate}{stars}",
                     statistic = "({std.error})",
                     coef_map = cm,
                     add_rows = p_value,
                     stars =  c('*' = .1, '**' = .05,'***' = 0.01),
                     gof_omit = 'dat$|Std. Errors|N|FE|Deviance|R2|AIC|BIC|Log.Lik',
                     output='data.frame') %>%
        dplyr::filter(part != 'gof') %>%
        dplyr::select(-part,-statistic) %>%
        dplyr::mutate(
                term = dplyr::case_when(
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\(')+0) == 1 ~ '',
                        (stringr::str_detect(`Dispatch Recorded`,pattern='\\[')+0) == 1 ~ '',
                        TRUE ~ term)
                
        ) 

table %>%
        utils::write.table(sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
                           file='../paper/v16/new-tables/fragment-het-force-ctract-event-level.tex')
