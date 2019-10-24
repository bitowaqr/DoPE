

# analyiss 

options(scipen = 99)
# lsoa_df = read.csv("output/lsoa_df.csv",stringsAsFactors = F)

# explore correlations
library(corrplot)
library(ggplot2)
cor_mat = cor(lsoa_df[,-1])
rownames(cor_mat) = colnames(cor_mat) = substr(colnames(cor_mat),1,20)
corrplot(cor_mat,addCoef.col = "black")


melted <- reshape2::melt(lsoa_df, id.vars=c("code", "run_count"))

p <- ggplot(melted, aes(value, run_count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable,ncol = 3,scales = "free")
p




model2 = glm(run_count ~ imd + pop_density+mn_dstn + perc_working+perc_bme,
             data = lsoa_df,
             family = poisson(link="log"),
             offset = log(total_pop))
summary(model2)
x = summary(model2)
1-((x$deviance-length(coef(x)[,1]))/x$null.deviance)

scaled_df = data.frame(code = lsoa_df$code,
                       run_count = lsoa_df$run_count,
                       scale(lsoa_df[,-c(1,2,12)]),total_pop = lsoa_df$total_pop,stringsAsFactors = F)
model3 = glm(run_count ~ d_income + d_employment + d_education + d_health + d_crime + d_housing + d_enviroment + mn_dstn + perc_non_working_age + log(pop_density)   + perc_bme,
             data = scaled_df,
             family = poisson(link="log"),
             offset = log(total_pop))
summary(model3)
x = summary(model3)
1-((x$deviance-length(coef(x)[,1]))/x$null.deviance)


model3.2 = glm(run_count ~ imd *perc_bme+ mn_dstn,
             data = scaled_df,
             family = poisson(link="log"),
             offset = log(total_pop))
x = summary(model3.2)
r2.2 = 1-((x$deviance-length(coef(x)[,1]))/x$null.deviance)
  
  
ggplot(lsoa_df, aes( x=imd,y=run_count)) +
  geom_point() +
  geom_smooth()
  
    




model4 = glm(run_count ~ imd + mn_dstn + perc_bme,
             data = scaled_df,
             family = poisson(link="log"),
             offset = log(total_pop))
summary(model4)


ggplot(lsoa_df,aes(x=perc_bme,y=run_count)) +
  geom_point() +
  geom_smooth()

ggplot(lsoa_df,aes(x=(perc_bme),y=log(run_count))) +
  geom_point() +
  geom_smooth()




lsoa_cntrds$name[lsoa_cntrds$code == "E06000031"]

