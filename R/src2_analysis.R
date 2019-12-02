

# analyiss 
options(scipen = 99)
# lsoa_df = read.csv("output/lsoa_df.csv",stringsAsFactors = F)

# explore correlations
library(corrplot)
library(ggplot2)

# custom fun
get_mcfadden = function(model){
  x = summary(model)
  1-((x$deviance-length(coef(x)[,1]))/x$null.deviance)
}


#### Corrplot
cor_mat = cor(lsoa_df[,-1])
rownames(cor_mat) = colnames(cor_mat) = substr(colnames(cor_mat),1,20)
corrplot(cor_mat,addCoef.col = "black",diag = T ,type="upper")

#### bivariate associations
melted <- reshape2::melt(lsoa_df, id.vars=c("code", "run_count"))

p <- ggplot(melted, aes(value, run_count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable,ncol = 3,scales = "free")
p



#### GLM models
model2 = glm(run_count ~ imd + pop_density+mn_dstn + perc_non_working_age+perc_bme,
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




# lsoa_cntrds$name[lsoa_cntrds$code == "E06000031"]

##################################################
##################################################
#               CHANGES 01.12.2019
##################################################
##################################################

#############
# Full model + strata
######

# unadjusted relationship
ggplot(lsoa_df) +
  geom_point(aes(x=perc_bme,y =run_count/total_pop)) +
  geom_smooth(aes(x=perc_bme,y =run_count/total_pop)) 

# unadjusted relationship on log scale
ggplot(lsoa_df) +
  geom_point(aes(x=log(perc_bme),y =run_count/total_pop)) +
  geom_smooth(aes(x=log(perc_bme),y =run_count/total_pop)) 

### some more meaningful perc bme strata (non linear)
perc_bme_strata = cut(lsoa_df$perc_bme,breaks = c(0,.01,.02,.03,.04,.05,.1,.2,Inf),include.lowest = T)
ggplot(lsoa_df) +
  geom_boxplot(aes(y=run_count/total_pop,fill=perc_bme_strata,x=perc_bme_strata),alpha=0.6)  +
  coord_cartesian(ylim=c(0,0.3)) +
  theme_minimal()

# glms
p.glm0 = glm(run_count ~ imd + perc_bme+ poly(mn_dstn,3) + (perc_non_working_age) + pop_density   ,
             data = lsoa_df,
             family = poisson(link="log"),
             offset = log(total_pop))
# summary(p.glm0)
get_mcfadden(p.glm0)

p.glm1 = glm(run_count ~ imd + perc_bme_strata+poly(mn_dstn,3) + (perc_non_working_age) + pop_density   ,
             data = lsoa_df,
             family = poisson(link="log"),
             offset = log(total_pop))
# summary(p.glm1)
get_mcfadden(p.glm1)

p.glm2 = glm(run_count ~ imd+poly(perc_bme,2) + poly(mn_dstn,3) + (perc_non_working_age) + pop_density   ,
             data = lsoa_df,
             family = poisson(link="log"),
             offset = log(total_pop))
summary(p.glm2)  # my favorite
get_mcfadden(p.glm2)
round(exp(coefficients(p.glm2)),4)
# mh... not really any justification for including non linear terms in the model....?!

# p.glm3 = glm(run_count ~ imd + poly(mn_dstn,3) + (perc_non_working_age) + pop_density   + perc_bme_strata*perc_bme,
#              data = lsoa_df,
#              family = poisson(link="log"),
#             offset = log(total_pop))
# summary(p.glm3) # weird
# get_mcfadden(p.glm3)


## partial R2 values for venn diagramm?? ----
# install.packages("rsq")

library(rsq)
attach(lsoa_df) # unfortuntely, the following analysis needs the data to be loade din the GlobalEnv... 

abr_pr2= function(x,y){ # abbreviaed function
  rsq.partial(x,y,type = "kl")$partial.rsq
} 

p.full = glm(run_count ~ imd + poly(mn_dstn,3) + (perc_non_working_age) + pop_density   + perc_bme,
             family = poisson(link="log"),
             offset = log(total_pop))

p.n_distance = update(p.full,.~.-poly(mn_dstn,3))
p.n_imd = update(p.full,.~.-imd)
p.n_bme = update(p.full,.~.-perc_bme)

#- KL-Divergence-Based R-Squared -----
rsq.kl(p.full)
rsq.kl(p.n_distance)
rsq.kl(p.n_imd)
rsq.kl(p.n_bme)

## KL-Divergence-Based partial R-Squared ----
# partial ????2- The proportion of residual variation explained by adding the predictor 
# to the constrained model (the full model without the predictor). 
abr_pr2(p.full,p.n_imd)
abr_pr2(p.full,p.n_bme)
abr_pr2(p.full,p.n_distance)

## illustration VENN DIAGRAMM ------
p.base = glm(run_count ~ 1+ (perc_non_working_age) + pop_density   ,
             family = poisson(link="log"),
             offset = log(total_pop))

# imd + poly(mn_dstn,3) +poly(perc_bme,2)
r2.base = rsq.kl(p.base,adj = T)
r2.full = rsq.kl(p.full,adj=T)

# singles
mar.r2.imd = r2.full - rsq.kl(update(p.full,.~.-imd),adj=T)
mar.r2.bme = r2.full - rsq.kl(update(p.full,.~.-perc_bme),adj=T)
mar.r2.dist = r2.full - rsq.kl(update(p.full,.~.-poly(mn_dstn,3)),adj=T)

# doubles
mar.r2.imd_bme = r2.full - rsq.kl(update(p.full,.~.-imd-perc_bme),adj=T)
mar.r2.imd_dist = r2.full - rsq.kl(update(p.full,.~.-imd-poly(mn_dstn,3)),adj=T)
mar.r2.bme_dist = r2.full - rsq.kl(update(p.full,.~.-poly(mn_dstn,3)-perc_bme),adj=T)

# triple 
mar.r2.bme_dist_imd = r2.full - rsq.kl(update(p.full,.~.-imd-poly(mn_dstn,3)-perc_bme),adj=T)
detach(lsoa_df)

# install.packages("VennDiagram")
library(VennDiagram)

# imd distance bme value
vennMat = matrix(ncol=4, byrow = T,
                 data= c(0,0,0,r2.base,
                         1,1,1,mar.r2.bme_dist_imd,
                         1,0,0,mar.r2.imd,
                         0,1,0,mar.r2.dist,
                         0,0,1,mar.r2.bme,
                         1,1,0,mar.r2.imd_dist,
                         1,0,1,mar.r2.imd_bme,
                         0,1,1,mar.r2.bme_dist)
                 )
vennMat = data.frame(vennMat)
names(vennMat) = c("imd","dist","bme")

sets = vector(mode = 'list', length = ncol(vennMat)-1)
names(sets) = names(vennMat)[1:(ncol(vennMat)-1)]
lastElement = 0
for (i in 1:nrow(vennMat)){
  elements = seq(lastElement,(lastElement+vennMat[i,ncol(vennMat)]),by=0.0001)
  lastElement = elements[length(elements)]+0.0001
  for (j in 1:(ncol(vennMat)-1)){
    if (vennMat[i,j]==1){
      sets[[j]]=c(sets[[j]],elements)
    }
  }
}

laVenn = venn.diagram(sets,filename=NULL, print.mode=c("raw"), cex=1.5,cat.cex=2)
for(l in 1:length(laVenn)){
  if(!is.null(laVenn[[l]]$label)){
    if(!is.na(as.numeric(laVenn[[l]]$label))){
    laVenn[[l]]$label = round(as.numeric(laVenn[[l]]$label)/10000,2)
    }
    }
  }

vp = viewport(height=unit(8.17, "inches"), width=unit(11.59, "inches"))
# grid.rect(vp=vp,gp=gpar(col="whit"),height = unit(1/1.2, "npc"),y = unit(1-1/1.2, "npc"))
grid.draw(laVenn)
grid.text("R2 values Venn Diagram", x = unit(0.5, "npc"), y = unit(0.99, "npc"),
          just = "centre", hjust = NULL, vjust = 1)

# plot shows R2 of different (partially) overlapping models
# one can infer from this the margingal R2 change of adding a variable to a given model

