library(fixest)

data(trade)

## Inner Part of the Table

res = fepois(Euros ~ log(dist_km) | csw0(Year, Destination, Origin), trade,
                   cluster = ~Origin+Destination)

res_list = summary(.l(res))

names(res) = c('Model 1','Model 2','Model 3','Model 4')

cm <- c('log(dist_km)' = 'Log\\(Dist\\)')

table = modelsummary(res,
                     fmt = "%.3f",
                     estimate="{estimate}{stars}",
                     statistic = "({std.error})",
                     coef_map = cm,
                     #add_rows = rows,
                     stars =  c('*' = .1, '**' = .05,'***' = 0.01),
                     gof_omit = 'dat$|Std. Errors|N|FE|Deviance|R2|AIC|BIC|Log.Lik',
                     output='data.frame') %>%
  dplyr::filter(part != 'gof') %>%
  dplyr::select(-part,-statistic) %>%
  dplyr::mutate(
    term = dplyr::case_when(
      (stringr::str_detect(`Model 1`,pattern='\\(')+0) == 1 ~ '',
      (stringr::str_detect(`Model 2`,pattern='\\[')+0) == 1 ~ '',
      TRUE ~ term)
    
  ) 

table %>%
  utils::write.table(sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
                     file='../paper/v15/new-tables/fragment-threat-identification-pap-blackout.tex')

## Add Info

adjust_numeric = function(x){x = format(round(as.numeric(x),3),nsmall=3)}
adjust_percent = function(x){x = format(round(as.numeric(x),1),nsmall=1)
x = paste(as.character(x),'\\%',sep='')
return(x)}

vars = c('share_treated','more_than_2_force_baseline')

# 'lat','long','time_to_dispatch_min',
# 'time_to_dispatch_greater_5_min','dummy_activepolicing','dummy_telephone','risk_high')

mean_dep_var = dat_main %>%
  filter(normal_day == 0) %>%
  summarise_at(vars,mean,na.rm=TRUE) %>%
  ungroup() %>%
  dplyr::mutate_if(stringr::str_detect(names(.),'share_treated|more_than_2_force_baseline'),
                   function(var){var*100}) %>%
  dplyr::mutate_all(adjust_numeric) %>%
  dplyr::mutate_all(as.character) %>%
  as.data.frame()

nobs = res$`Share of Officers Treated`$nobs

coefs = t(as.data.frame(coefficients(res)[,'normal_day']))
colnames(coefs) = names(mean_dep_var)

scaled_coef = (coefs/as.numeric(mean_dep_var))*100
scaled_coef = as_tibble(scaled_coef)

mean_dep_var = mutate_all(mean_dep_var,adjust_numeric)
scaled_coef = mutate_all(scaled_coef,adjust_percent)

n = mean_dep_var
n[,] = as.character(nobs)

add = mean_dep_var %>%
  dplyr::bind_rows(scaled_coef) %>%
  dplyr::bind_rows(n) 

add = add %>%
  mutate(term = NA) %>%
  relocate(term) 

add$term[1] = 'Mean Dep. Var. Control Shifts'
add$term[2] = 'Scaled Effect Size'
add$term[3] = '\\textit{N}'


add %>% utils::write.table(sep=' & ',quote=FALSE,col.names = FALSE,row.names = FALSE,eol='\\\\',
                           file='../paper/v15/new-tables/fragment-threat-identification-pap-blackout-add-info.tex')



  create_table_format = function(){
  
  
  
  
  
}