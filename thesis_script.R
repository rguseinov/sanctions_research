library(dplyr)
library(tidyr)
library(ggplot2)
library(glmmTMB)
library(countrycode)
library(sandwich)
library(ggeffects)
library(marginaleffects)
library(clubSandwich)
library(parameters)
library(stargazer)
library(xtable)
library(texreg)
library(sjPlot)
library(car)

df_reg_2 = read.csv('df_reg_2.csv')

#Visual diagnostics
df_reg_2 %>% 
  filter(cnts_destabilisation < 40) %>% 
  ggplot(., aes(x=cnts_destabilisation))+ 
  geom_histogram(binwidth=1,fill="#69b3a2", color="#e9ecef")+
  labs(x = 'Number of CNTS destabilisation events', y = 'Count', title = 'Distribution of CNTS destabilisation events', subtitle = '1960-2005')+
  theme_classic()

df_reg_2 %>% 
  filter(UCD_campaign < 40) %>% 
  ggplot(., aes(x=UCD_campaign))+ 
  geom_vline(aes(xintercept = mean(UCD_campaign)), color = "red", linewidth = 1)+
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef")+
  labs(x = 'Number of UCD events', y = 'Count', title = 'Distribution of UCD events', subtitle = '1960-2005')+
  theme_classic()

#Regional distribution
df_reg_vis = df_reg_2 %>% 
  mutate(cnts_binary = ifelse(cnts_destabilisation >= 1, 1, cnts_destabilisation),
         ucd_binary = ifelse(UCD_campaign >= 1, 1, UCD_campaign))
dfpivot = df_reg_vis %>% 
  group_by(region) %>% 
  count(cnts_binary) %>% 
  filter(cnts_binary == 1) %>%
  ungroup() %>% 
  mutate(cnts = n / sum(n) * 100) %>% 
  dplyr::select(-c(n, cnts_binary))

dfpivot2 = df_reg_vis %>% 
  group_by(region) %>% 
  count(ucd_binary) %>% 
  filter(ucd_binary == 1) %>%
  ungroup() %>% 
  mutate(ucd = n / sum(n) * 100) %>% 
  dplyr::select(-c(n, ucd_binary))
dfpivot = merge(dfpivot, dfpivot2, by.x = 'region', by.y = 'region')
dfpivot = dfpivot %>% 
  dplyr::select(-c(ucd.y))
#then create a pivot_longer table, where the columns become the rows
long_df = dfpivot %>% 
  group_by(region) %>% 
  pivot_longer(cols = c('cnts', "ucd"), names_to = 'destabilisation', values_to = 'value')
#save pivot table
#visualise big dodge
ggplot(long_df, aes(x = region, y = value, fill = destabilisation)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = paste(as.character(round(value, 2)),'%', sep=""), y = value + 0.05),
            position = position_dodge(0.9),
            vjust = 0)+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(title = 'Regional coverage of political destabilisation events',
       subtitle = 'Urban Social Disorder & Cross-National Time-Series data. 1960-2005',
       x='Regions ( World Bank Development Indicators)',
       y = 'Coverage (%)',
       fill = 'Datasets')+
  theme_classic()


#Compute first step of instrumental variable regression
step1 <- glm(imposition_binary ~ Mean_Frequency_UN + flow2 + us_aid_sum + as.factor(country), data=df_reg_2, family = binomial(link = 'probit'))
df_reg_2$firststage_fitted <- step1$fitted.values #extract fitter values

#Instrumental variable. UCD. Poisson regression
model1_ucd <- glmmTMB(UCD_campaign ~ firststage_fitted + log(gdp_pcap+0.01) + v2x_polyarchy,
                      data=df_reg_2,
                      ziformula=~1,
                      family=poisson)
model2_ucd <- glmmTMB(UCD_campaign ~ firststage_fitted + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index,
                      data=df_reg_2,
                      ziformula=~1,
                      family=poisson)
model3_ucd <- glmmTMB(UCD_campaign ~ firststage_fitted + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure,
                      data=df_reg_2,
                      ziformula=~1,
                      family=poisson)
model4_ucd <- glmmTMB(UCD_campaign ~ firststage_fitted + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure + log(constant_dollars_usaid+0.01),
                      data=df_reg_2,
                      ziformula=~1,
                      family=poisson)
model_parameters(
  model1_ucd,
  ci = 0.95,
  vcov = 'HC3')
model_parameters(
  model2_ucd,
  ci = 0.95,
  vcov = 'HC3')
model_parameters(
  model3_ucd,
  ci = 0.95,
  vcov = 'HC3')
model_parameters(
  model4_ucd,
  ci = 0.95,
  vcov = 'HC3') #robust standard errors
texreg(list(model1_ucd,model2_ucd,model3_ucd,model4_ucd),caption="Zero-inflated Poisson model. UCD data", label="tab:ucd_poisson") #Latex output

#Instrumental variable. Zero-inflated negative binomial. Hypothesis 1
model1_ucd_zb <- glmmTMB(UCD_campaign ~ firststage_fitted + log(gdp_pcap+0.01) + v2x_polyarchy,
                      data=df_reg_2,
                      ziformula=~1,
                      family=nbinom2)
model2_ucd_zb <- glmmTMB(UCD_campaign ~ firststage_fitted + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index,
                      data=df_reg_2,
                      ziformula=~1,
                      family=nbinom2)
model3_ucd_zb <- glmmTMB(UCD_campaign ~ firststage_fitted + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure,
                      data=df_reg_2,
                      ziformula=~1,
                      family=nbinom2)
model4_ucd_zb <- glmmTMB(UCD_campaign ~ firststage_fitted + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure + log(constant_dollars_usaid+0.01),
                      data=df_reg_2,
                      ziformula=~1,
                      family=nbinom2)
model_parameters(
  model1_ucd_zb,
  ci = 0.95,
  vcov = 'HC3')
model_parameters(
  model2_ucd_zb,
  ci = 0.95,
  vcov = 'HC3')
model_parameters(
  model3_ucd_zb,
  ci = 0.95,
  vcov = 'HC3')
model_parameters(
  model4_ucd_zb,
  ci = 0.95,
  vcov = 'HC3')
texreg(list(model1_ucd_zb,model2_ucd_zb,model3_ucd_zb,model4_ucd_zb),caption="Zero-inflated negative binomial model. UCD data", label="tab:ucd_zinb")

#Hypothesis 2. Imposing economic sanctions on advanced democracies reduces the number of political destabilisation events
df_reg_2$advanced_democ = ifelse(df_reg_2$v2x_polyarchy >= 0.8, 1, 0)
model1_ucd_zb_hyp2 <- glmmTMB(UCD_campaign ~ firststage_fitted*advanced_democ + log(gdp_pcap+0.01) + v2x_polyarchy + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model2_ucd_zb_hyp2 <- glmmTMB(UCD_campaign ~ firststage_fitted*advanced_democ + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model3_ucd_zb_hyp2 <- glmmTMB(UCD_campaign ~ firststage_fitted*advanced_democ + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure  + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model4_ucd_zb_hyp2 <- glmmTMB(UCD_campaign ~ firststage_fitted*advanced_democ + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure + log(constant_dollars_usaid+0.01) + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
#get robust standard errors with model_parameters

#Visualise interaction term
plot_model(model4_ucd_zb_hyp2, type = 'pred', terms = c('firststage_fitted', 'advanced_democ'),
           ci.lvl = NA)+
  labs(title='Predicted counts of
political destabilisation events',
       x = 'Imposition of sanctions',
       y = 'Predicted number of UCD events',
       col = 'Advanced democracy')+
  theme_classic()

texreg(list(model1_ucd_zb_hyp2,model2_ucd_zb_hyp2,model3_ucd_zb_hyp2,model4_ucd_zb_hyp2),caption="Hypothesis 2. Zero-inflated negative binomial model. UCD data", label="tab:ucd_zinb_hyp2") #latex output

#Hypothesis3
model1_ucd_zb_hyp3 <- glmmTMB(UCD_campaign ~ firststage_fitted*institution + log(gdp_pcap+0.01) + v2x_polyarchy + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model2_ucd_zb_hyp3 <- glmmTMB(UCD_campaign ~ firststage_fitted*institution + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model3_ucd_zb_hyp3 <- glmmTMB(UCD_campaign ~ firststage_fitted*institution + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure  + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model4_ucd_zb_hyp3 <- glmmTMB(UCD_campaign ~ firststage_fitted*institution + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure + log(constant_dollars_usaid+0.01) + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)#Sanctions, imposed by an international institution, decrease the number of political destabilisation effects
#visualise
plot_model(model4_ucd_zb_hyp3, type = 'pred', terms = c('firststage_fitted', 'institution'),
           ci.lvl = NA)+
  labs(title='Predicted counts of
political destabilisation events',
       x = 'Imposition of sanctions',
       y = 'Predicted number of UCD events',
       col = 'Int. Institution')+
  theme_classic()
texreg(list(model1_ucd_zb_hyp3,model2_ucd_zb_hyp3,model3_ucd_zb_hyp3,model4_ucd_zb_hyp3),caption="Hypothesis 3. Zero-inflated negative binomial model. UCD data", label="tab:ucd_zinb_hyp3")

#Hypothesis 4
model1_ucd_zb_hyp4 <- glmmTMB(UCD_campaign ~ firststage_fitted + targeted_narrow_binary + log(gdp_pcap+0.01) + v2x_polyarchy + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model2_ucd_zb_hyp4 <- glmmTMB(UCD_campaign ~ firststage_fitted + targeted_narrow_binary + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model3_ucd_zb_hyp4 <- glmmTMB(UCD_campaign ~ firststage_fitted + targeted_narrow_binary + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure  + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model4_ucd_zb_hyp4 <- glmmTMB(UCD_campaign ~ firststage_fitted + targeted_narrow_binary + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure + log(constant_dollars_usaid+0.01) + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
texreg(list(model1_ucd_zb_hyp4,model2_ucd_zb_hyp4,model3_ucd_zb_hyp4,model4_ucd_zb_hyp4),caption="Hypothesis 4. Zero-inflated negative binomial model. UCD data", label="tab:ucd_zinb_hyp4")

#Robustness check
model4_cnts_zb_hyp1 <- glmmTMB(cnts_destabilisation ~ firststage_fitted + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure + log(constant_dollars_usaid+0.01),
                         data=df_reg_2,
                         ziformula=~1,
                         family=nbinom2)
model4_cnts_zb_hyp2 <- glmmTMB(cnts_destabilisation ~ firststage_fitted*advanced_democ + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure + log(constant_dollars_usaid+0.01) + (1|cow),
                              data=df_reg_2,
                              ziformula=~1,
                              family=nbinom2)
model4_cnts_zb_hyp3 <- glmmTMB(cnts_destabilisation ~ firststage_fitted*institution + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure + log(constant_dollars_usaid+0.01) + (1|cow),
                              data=df_reg_2[df_reg_2$cnts_destabilisation <= 10,],
                              ziformula=~1,
                              family=nbinom2)
model4_cnts_zb_hyp4 <- glmmTMB(cnts_destabilisation ~ firststage_fitted + targeted_narrow_binary + log(gdp_pcap+0.01) + v2x_polyarchy + demographic_index + tenure + log(constant_dollars_usaid+0.01) + (1|cow),
                              data=df_reg_2[df_reg_2$cnts_destabilisation <= 10,],
                              ziformula=~1,
                              family=nbinom2)
texreg(list(model4_cnts_zb_hyp1,model4_cnts_zb_hyp2,model4_cnts_zb_hyp3,model4_cnts_zb_hyp4),caption="Hypothesies 1-4, model 4 (all covariates). Zero-inflated negative binomial model. CNTS data", label="tab:cnts_zinb_hyp1-4")
