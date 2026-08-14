
df<-df_n_at_age |> 
  mutate(ICES_SD=ifelse(rec=="43H2"|
                          rec=="43H3"|
                          rec=="43H4"|
                          rec=="44H2"|
                          rec=="44H3"|
                          rec=="44H4"|
                          rec=="45H2"|
                          rec=="45H3"|
                          rec=="45H4", 28.1, ICES_SD)) |> 
  filter(ICES_SD==28.1)

df2<-df |> filter(ICES_SD==28.1, species==126417) |> 
  group_by(age) |> summarise(n_at_age= round(sum(n_age_at_length, na.rm=T),2)) |> 
  filter(is.na(n_at_age)==F) |> mutate(age=ifelse(is.na(age)==T, 100, age))
df2










pivot_n_at_age<-df_n_at_age |> group_by(species, rec, age) |> 
  summarise(n_at_age= round(sum(n_age_at_length),2)) |> 
  left_join(df_rec_ICES_SD, relationship="many-to-many") |> 
  arrange(age, species, ICES_SD,rec) |> 
  pivot_wider(names_from = age, values_from = n_at_age) |> 
  mutate(NTOT=rowSums(across(c(`0`:`19`)), na.rm = T)) |> 
  #rename(N0=`0`,N1=`1`,N2=`2`,N3=`3`,N4=`4`,N5=`5`,N6=`6`,N7=`7`,N8=`8`,
  #       N9=`9`,N10=`10`,N11=`11`#,N12=`12`
  #       )|> 
  select(species,ICES_SD,rec,NTOT,everything()) |> 
  ungroup()





df<-pivot_n_at_age|> filter(ICES_SD==28.1) |> 
  select(-ICES_SD, -rec)|> filter(species==126417) 
View(df)

df<-|> 
  summarise(tot=sum(NTOT))



View(df_n_at_age |> filter(species==126417) |> filter(is.na(age==T))
     )


tmp<-n_per_age_length|> filter(species==126417) |> filter(is.na(age==T))     
View(tmp)                          
