###############
# Herring Age data
###############

dfB_biol<-bio_all |> 
  mutate(year=SurveyYear) |> 
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  filter(year>=min_year & year<=max_year)

# Add rectangles and define 8 length groups for herring
df_length_at_age<-dfB_biol|> 
  left_join(df_rec) |> # Add ruhnu rectangles based on year and haul number
  filter(CatchSpeciesCode==126417) |> 
  mutate(length=as.numeric(BiologyLengthClass), # shorten names
         age=as.numeric(BiologyIndividualAge))|> 
  mutate(length_group=ifelse(length<60, 1, NA)) |> 
  mutate(length_group=ifelse(length>=60  & length<70, 2, length_group)) |> 
  mutate(length_group=ifelse(length>=70 & length<80, 3, length_group)) |> 
  mutate(length_group=ifelse(length>=80 & length<90, 4, length_group)) |> 
  mutate(length_group=ifelse(length>=90 & length<100, 5, length_group)) |> 
  mutate(length_group=ifelse(length>=100 & length<110, 6, length_group)) |> 
  mutate(length_group=ifelse(length>=110 & length<120, 7, length_group)) |> 
  mutate(length_group=ifelse(length>=120 & length<130, 8, length_group)) |> 
  mutate(length_group=ifelse(length>=130 & length<140, 9, length_group)) |> 
  mutate(length_group=ifelse(length>=140 & length<150, 10, length_group)) |> 
  mutate(length_group=ifelse(length>=150 & length<160, 11, length_group)) |> 
  mutate(length_group=ifelse(length>=160 & length<170, 12, length_group)) |> 
  mutate(length_group=ifelse(length>=170 & length<180, 13, length_group)) |> 
  mutate(length_group=ifelse(length>=180, 14, length_group)) 
df_length_at_age
#View(df_length_at_age |> select(length, length_group))

# Check for missing rectangle info, should be empty
df_length_at_age |> filter(is.na(rec_ruhnu)==T)

# Age without grouping by length, nice to know
df_pivot<-df_length_at_age |> 
  select(length, age) |> group_by(length, age) |> 
  summarise(n=n())|> # count works as each row is an individual 
  pivot_wider(names_from = age, values_from = n) |> 
  select(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`, `8`,`9`,`10`,`11`,`12`,`13`,`14`,`NA`)
print(n=35, x=df_pivot)
sum(df_pivot[,2:17], na.rm=T)# 9347
#View(df_pivot)


# Pool older ages with 8 year olds, remove missing ages
# Length as the first grouping argument keeps the length groups in correct order in the pivot table
age_plus<-8
Nages<-age_plus+1 # +1 are the 0yr olds

df<-df_length_at_age |>
  mutate(age=ifelse(age>age_plus, age_plus, age)) |>   # pool ages >=8 together (9th age group)
  filter(is.na(age)==F) |> # Remove individuals with missing age 
  group_by(length_group, year, rec_ruhnu, age) |> 
  summarise(n=n())
df
#View(df)
# This should be empty
df |> filter(is.na(age)==T)

# Pivot, not currently used
df_pivot<-  df|> 
  pivot_wider(names_from = length_group, values_from = n) 
#View(df_pivot)

print(n=350, x=df_pivot) # Looks correct
sum(df_pivot[,4:11], na.rm=T) #9336


# nGobs[l,r,y]: Age sample size per length, rectangle and year
# Gobs[1:Nages,l,r,y]: Sample size on age samples from length group l
# ===================================================================
df
# Let's take ages 0-9 (10 age groups)
G_obs<-array(NA, dim=c(10,N_lh,4,Nyears))
for(i in 1:dim(df)[1]){
  y<-df$year[i]-(min_year-1)
  r<-df$rec_ruhnu[i]
  r<-df$rec_ruhnu[i]
  l<-df$length_group[i]
  a<-df$age[i]+1 # first age groups is 0+ 
  G_obs[a,l,r,y]<-df$n[i]
}
G_obs
sum(G_obs, na.rm=T) # 9331

# Use complete function to fill in rows that are missing (no individuals of 
#particular length in any of the rectangles on given year)
df_nG_obs_complete<-df |> 
  summarise(ntot=sum(n))|> 
  pivot_wider(names_from = rec_ruhnu, values_from = ntot) |>
  select(length_group, year, `1`,`2`,`3`,`4`)|> # Order as pivot_wider may otherwise mess these up
  ungroup() |> complete(length_group,year)
#View(df_nG_obs_complete) # Filled group 1 for 2016 and groups 1 and 3 for 2019

nG_obs<-array(NA, dim=c(N_lh,4,Nyears))
for(y in 1:Nyears){
  for(r in 1:4){
    nG_obs[,r,y]<-as.data.frame(  df_nG_obs_complete |> 
                                    filter(year==(y+min_year-1))  |> 
                                    select(-length_group, -year))[,r] 
  }
}
nG_obs

#nG_obs<-as.matrix(nG_obs)
sum(nG_obs, na.rm=T) # 2775 in 2022-2024

# NOTE! REPLACE NA's IN AGE DATA WHERE NA NOT SUITABLE OR ACTUALLY 0
#####################################################################
# For computational reasons, sample size can't be missing so imput sample of 500 
# for all that are currently NA. Number per age will be predicted by the model
# AND
# In cases where sample was not missing, the NA's in G_obs should be replaced with 0s
for(i in 1:N_lh){
  for(r in 1:4){
    for(y in 1:Nyears){
      if(is.na(nG_obs[i,r,y])==T){
        nG_obs[i,r,y]<-500}else{ # Input imaginary 500 sample where no sample was taken
          for(a in 1:10){
            if(is.na(G_obs[a,i,r,y])==T){
              G_obs[a,i,r,y]<-0 # Input zero when sample size is not NA but none was observed (==real 0s)
            }
          }
        }
    }
  }
}
G_obs
nG_obs

#G_obs[,,3,6]




