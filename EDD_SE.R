
library(dplyr)

getwd()
setwd("C:/Users/mmquazi/Desktop/NIS_Project/pancreaticcancermeta_analysis")
df=read.csv('data/raw_data.csv', header = TRUE, stringsAsFactors = FALSE)
head(df)

# calculate prop of women in trials and prop of women in gbd population 
df <- df %>%
  mutate(
    ppw=Total_Females_in_Study/Total_Number_of_participants,
    se_ppw=sqrt(ppw*(1-ppw)/Total_Number_of_participants),
    ppopw=gbd_val_Female/gbd_val_Both,
    edd=ppw-ppopw
  )

# Calculate the standard errors and the gamma dist par for female and male
df <- df %>%
  mutate(
    SE_female=(gbd_upper_Female-gbd_lower_Female)/(2*1.96),
    SE_male=(gbd_upper_Male-gbd_lower_Male)/(2*1.96),
    # Calculate gamma dist par for female
    shape_female=(gbd_val_Female/SE_female)^2,
    scale_female=(SE_female^2)/gbd_val_Female,
    # Calculate gamma dist par for male
    shape_male=(gbd_val_Male/SE_male)^2,
    scale_male=(SE_male^2)/gbd_val_Male
)

# initialize values for gamma simulation
df$se_ppopw=NA
df$se_edd=NA
df$log_ppr=NA
df$se_log_ppr=NA

attach(df)
# estimation of se using gamma dist
runs=1000000
for(i in 1:nrow(df)){
  female_est=rgamma(n=runs,shape=shape_female[i],scale=scale_female[i])
  male_est=rgamma(n=runs,shape=shape_male[i],scale=scale_male[i])
  ppopw_est=female_est/(male_est+female_est)
  df$se_ppopw[i]=sd(ppopw_est)
  
  ppw_est=rbeta(shape1=df$Total_Females_in_Study[i],shape2=df$Total_Males_in_Study[i],n=runs)
  df$log_ppr[i]=log(df$ppw[i]/df$ppopw[i])
  df$se_log_ppr[i]=sd(log(ppw_est/ppopw_est))
}
df$se_edd=sqrt(df$se_ppw^2 + df$se_ppopw^2)
write.csv(df,"data/processed_data.csv")
