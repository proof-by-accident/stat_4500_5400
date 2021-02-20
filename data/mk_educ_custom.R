library(tidyverse)
library(magrittr)
library(broom)
library(car)

educ = read.csv('education_exp.csv')

# source: https://www.census.gov/data/tables/2020/econ/qtax/historical.html
gdp = read.csv('gdp.csv')

# source: https://www.census.gov/data/tables/2020/econ/qtax/historical.html
tax = read.csv('income_tax_revenue.csv')

#source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295
pop = read.csv('state_pops.csv')

states = read.csv('state_codes.csv')
names(states) %<>% tolower

gdp['tax'] = tax$tax*1000/pop$pop
gdp['gdp'] = gdp$gdp*1e6/pop$pop
gdp %<>% left_join(states, by = 'state')
gdp %<>% left_join(pop, by='state')

educ %<>% left_join(gdp[,c('code','gdp','tax')],by=c('state'='code'))
names(educ)[2] = 'educ'

tax.mod = lm(tax~gdp, data=educ)
newgdp = data.frame('gdp'=educ$gdp[is.na(educ$tax)])
educ$tax[is.na(educ$tax)] = predict(tax.mod, newdata=newgdp)

#tax = educ$tax
#inc = educ$inc
#a = norm(tax,type='2')*(tax%*%inc)/norm(inc,type='2')
#r = 1e-2*a
#r=0
#educ$tax = (1-r)tax + r*inc
#educ$tax = educ$tax + rnorm(50,0,.2*sd(educ$tax))
educ$tax %<>% as.integer
educ$gdp %<>% as.integer

full.mod = lm(educ~., data=educ%>%select(-state))
educ['resid'] = full.mod %>% augment %>% select('.resid')
educ %<>% group_by(region) %>% mutate(wls.weights = 1/var(resid)) %>% select(-'resid')
write.csv(educ, 'education.csv', row.names=FALSE)

