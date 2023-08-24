#Set working directory and libraries
setwd('D:\\Users\\sergi\\OneDrive\\Desktop\\0. Manchester\\0. Courses\\I. 2023-1\\Dissertation2.0')
library(haven)
library(rvest)
library(tidyverse)
library(tm)
library(stringr)
library(purrr)
library(rio)

#################### I. Detection of recurrent questions (Wave 1 to 12) #######################

datasets <- list.files(path = 'Data/',pattern = ".indresp.dta") #Create a list with the names of all individual datasets (from wave 1 to 12)

import_datasets <- function(df){
  df <- read_dta(df,n_max=5)
  df <- as.data.frame(df)
} #Function to read datasets - Only 5 rows because we are interested in the name of columns, not the cases themselves.

prefixes <- list('a_','b_','c_','d_','e_','f_','g_','h_','i_','j_','k_','l_')
waves1_12 <- list()

for (i in 1:length(datasets)){
  setwd('D:\\Users\\sergi\\OneDrive\\Desktop\\0. Manchester\\0. Courses\\I. 2023-1\\Dissertation2.0\\Data/')
  wave_number <- paste0('wave_',i)
  waves1_12[[i]] <- import_datasets(datasets[i]) 
  assign(wave_number,gsub(prefixes[[i]], '', colnames(waves1_12[[i]]))) 
} # Loop to read the column names of the 12 waves and eliminate the sub fix that uniquely identify each wave (i.e 'a_' for first wave). The reason is that we wan to identify the questions that are present in all 12 waves.

#Concatenate all elements in a single list and create a table that counts how many times a code appears
variables_w1_w12 <- c(wave_1,wave_2,wave_3,wave_4,wave_5,wave_6,
                      wave_7,wave_8,wave_9,wave_10,wave_11,wave_12)

element_counts <- table(variables_w1_w12)

#Convert the table to a dataframe, add names to columns and arrange
variables_w1_w12  <- as.data.frame(element_counts)

colnames(variables_w1_w12 ) <- c("code", "occurrences")

variables_w1_w12  <- variables_w1_w12  %>% arrange(desc(occurrences))

#################### II. Web scrapping #######################

##### A.Module name
modules_split <- c('caring_w12', '\\sselfemployment_w12', '\\scurrentemployment_w12','\\sdemographics_w12','\\schildcare_w12','\\sjobsatisfaction_w12','\\snonemployment_w12','\\ssecondjobs_w12','\\shouseholdfinances_w12','\\sdisability_w12','\\sscaghq_w12','\\semployees_w12','\\sscasf12_w12',
                   '\\sbenefits_w12','\\sscasatisfaction_w12','\\sconsents_w1','\\scontactdetails_w1','\\sdiscrimination_w1',
                   '\\semploymentstatushistory_w1','\\senvironmentalbehaviour_w1','\\sethnicityandnationalidentity_w1',
                   '\\sfamilybackground_w1','\\sfamilynetworks_w1','\\sharassment_w1','\\sfertilityhistory_w1',
                   '\\sinitialconditions_w1','\\slanguage_w1','\\smigrationhistory_w1','\\sparentsandchildren_w1',
                   '\\spartnershiphistory_w1','\\spolitics_w1','\\sreligion_w1','\\sremittances_w1','\\sstablecontact_w1',
                   '\\shealth_w1','\\shhgrid_w1') 
pattern <- paste0("(?=(\\s", paste(modules_split, collapse = "|"), "))")

##### B. Selector gadget - grab information from module's web

#a. Links to web
selfemployment_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/selfemployment_w12'
caring_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/caring_w12'
currentemployment_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/currentemployment_w12'
demographics_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/demographics_w12'
childcare_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/childcare_w12'
jobsatisfaction_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/jobsatisfaction_w12'
nonemployment_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/nonemployment_w12'
secondjobs_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/secondjobs_w12'
householdfin_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/householdfinances_w12'
disability_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/disability_w12'
ghq_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/scaghq_w12'
employees_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/employees_w12'
sf12_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/scasf12_w12'
benefits_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/benefits_w12'
scasatisfaction_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/12/questionnaire-module/scasatisfaction_w12'
consents_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/consents_w1'
contactdetails_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/contactdetails_w1'
discrimination_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/discrimination_w1'
employmentstatus_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/employmentstatushistory_w1'
environmentalbehavior_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/environmentalbehaviour_w1'
ethnicity_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/ethnicityandnationalidentity_w1'
family_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/familybackground_w1'
familynetworks_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/familynetworks_w1'
harrasment_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/harassment_w1'
fertility_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/fertilityhistory_w1'
initialconditions_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/initialconditions_w1'
language_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/language_w1'
migration_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/migrationhistory_w1'
parentschildren_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/parentsandchildren_w1'
partnership_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/partnershiphistory_w1'
politics_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/politics_w1'
religion_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/religion_w1'
remitances_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/remittances_w1'
stablecontact_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/stablecontact_w1'
health_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/health_w1'
householdgrid_link <- 'https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/1/questionnaire-module/hhgrid_w1'
links <- c(selfemployment_link,caring_link,currentemployment_link,demographics_link,
           childcare_link,jobsatisfaction_link,nonemployment_link,secondjobs_link,
           householdfin_link,disability_link,ghq_link,employees_link,sf12_link,
           benefits_link,scasatisfaction_link,consents_link,contactdetails_link,
           discrimination_link,employmentstatus_link,environmentalbehavior_link,
           ethnicity_link,family_link,familynetworks_link,harrasment_link,
           fertility_link,initialconditions_link,language_link,migration_link,
           parentschildren_link,partnership_link,politics_link,religion_link,remitances_link,
           stablecontact_link,health_link,householdgrid_link) 

#b. Nodes
selfemployment_node <- '#selfemployment_w12\\.jsmainwktrv h3 , #selfemployment_w12\\.jsttwtb_cawi h3 , #selfemployment_w12\\.jsrsntrvchngo dd~ dd , #selfemployment_w12\\.jsrsntrvchngo h3 , #selfemployment_w12\\.jsrsntrvchng dd~ dd , #selfemployment_w12\\.jsrsntrvchng h3 , #selfemployment_w12\\.jsmaintrvchng dd~ dd , #selfemployment_w12\\.jsmaintrvchng h3 , #selfemployment_w12\\.jspyni dd~ dd , #selfemployment_w12\\.jspyni h3 , #selfemployment_w12\\.jspytx dd~ dd , #selfemployment_w12\\.jspytx h3 , #selfemployment_w12\\.jspayw dd~ dd , #selfemployment_w12\\.jspayw h3 , #selfemployment_w12\\.jsownoth dd~ dd , #selfemployment_w12\\.jsownoth h3 , #selfemployment_w12\\.jsworkac dd~ dd , #selfemployment_w12\\.jsworkac h3 , #selfemployment_w12\\.jsprni dd~ dd , #selfemployment_w12\\.jsprni h3 , #selfemployment_w12\\.jsprtx dd~ dd , #selfemployment_w12\\.jsprtx h3 , #selfemployment_w12\\.jsprls dd~ dd , #selfemployment_w12\\.jsprls h3 , #selfemployment_w12\\.jspart dd~ dd , #selfemployment_w12\\.jspart h3 , #selfemployment_w12\\.jsaccs dd~ dd , #selfemployment_w12\\.jsaccs h3 , #selfemployment_w12\\.jsseissout dd~ dd , #selfemployment_w12\\.jsseissout h3 , #selfemployment_w12\\.jsseissap dd~ dd , #selfemployment_w12\\.jsseissap h3 , #selfemployment_w12\\.jshrcpr dd~ dd , #selfemployment_w12\\.jshrcpr h3 , #selfemployment_w12\\.jssize h3 , #selfemployment_w12\\.jssize dd~ dd , #selfemployment_w12\\.jsprf_cawi dd~ dd , #selfemployment_w12\\.jsprf_cawi h3 , #selfemployment_w12\\.jsseissnum dd~ dd , #selfemployment_w12\\.jsseissnum h3 , #selfemployment_w12\\.jsboss dd~ dd , #selfemployment_w12\\.jsboss h3 , #selfemployment_w12\\.jsttwt h3 , #selfemployment_w12\\.jsttwt dd~ dd , #selfemployment_w12\\.jswktrv h3 , #selfemployment_w12\\.jsownsum h3 , #selfemployment_w12\\.jsseissbm h3 , #selfemployment_w12\\.jsnitax h3 , #selfemployment_w12\\.jswktrvfar h3 , #selfemployment_w12\\.jsowotam h3 , #selfemployment_w12\\.jspayu h3 , #selfemployment_w12\\.jsseissam h3 , #selfemployment_w12\\.jsprhr h3 , #selfemployment_w12\\.jshrs h3 , #selfemployment_w12\\.jspl h3 , #selfemployment_w12\\.jsprbm h3 , #selfemployment_w12\\.jstypeb h3 , #selfemployment_w12\\.jsownamt h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #selfemployment_w12\\.jsprf h3 , #selfemployment_w12\\.jsprem h3 , #selfemployment_w12\\.jsseissem h3 , #selfemployment_w12\\.jsprey4 h3 , #selfemployment_w12\\.jsprby4 h3 , #selfemployment_w12\\.jsttwtb h3 , #selfemployment_w12\\.jsseissey4 h3 , #selfemployment_w12\\.jsseissby4 h3'
caring_node <- '#caring_w12\\.naidxhh dd~ dd , #caring_w12\\.naidxhh h3 , #caring_w12\\.aidhh dd~ dd , #caring_w12\\.aidhh h3 , #caring_w12\\.casch h3 , #caring_w12\\.aideft h3 , #caring_w12\\.aidhu2 h3 , #caring_w12\\.aidhu1 h3 , #caring_w12\\.aidhua h3 , h3+ .dl-horizontal dd:nth-child(2) , dd:nth-child(6) , #caring_w12\\.aidxhh dd:nth-child(2) , #caring_w12\\.aidhrs h3 , #caring_w12\\.aidxhh h3'
currentemployment_node <- '#currentemployment_w12\\.jbsempchk dd~ dd , #currentemployment_w12\\.jbsempchk h3 , #currentemployment_w12\\.jbsoc00chk dd~ dd , #currentemployment_w12\\.jbsoc00chk h3 , #currentemployment_w12\\.jbsic07chk dd~ dd , #currentemployment_w12\\.jbsic07chk h3 , #currentemployment_w12\\.jbsemp dd~ dd , #currentemployment_w12\\.jbsemp h3 , #currentemployment_w12\\.jboffy dd~ dd , #currentemployment_w12\\.jboffy h3 , #currentemployment_w12\\.jboff dd~ dd , #currentemployment_w12\\.jboff h3 , #currentemployment_w12\\.jbhas dd , #currentemployment_w12\\.jbhas h3 , #currentemployment_w12\\.jbbgdat h3 , #currentemployment_w12\\.jbterm2 h3 , #currentemployment_w12\\.jbsoc00 h3 , #currentemployment_w12\\.jbsic07 h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #currentemployment_w12\\.jbterm1 h3 , #currentemployment_w12\\.jbbgm h3 , #currentemployment_w12\\.jbbgy h3 , #currentemployment_w12\\.jbbgd h3'
demographics_node <- '#demographics_w12\\.psex h3 , #demographics_w12\\.pbirthm h3 , #demographics_w12\\.pdvage h3 , #demographics_w12\\.pbirthy h3 , #demographics_w12\\.pbirthd h3 , #demographics_w12\\.dcdobm h3 , #demographics_w12\\.xpmvyr h3 , #demographics_w12\\.dcdoby h3 , #demographics_w12\\.mobuse dd~ dd , #demographics_w12\\.mobuse h3 , #demographics_w12\\.drive dd~ dd , #demographics_w12\\.drive h3 , #demographics_w12\\.caruse dd~ dd , #demographics_w12\\.caruse h3 , #demographics_w12\\.mlstatchk dd~ dd , #demographics_w12\\.mlstatchk h3 , #demographics_w12\\.xpcnto dd~ dd , #demographics_w12\\.xpcnto h3 , #demographics_w12\\.xpmvwhr dd~ dd , #demographics_w12\\.xpmvwhr h3 , #demographics_w12\\.xpmvwhn dd~ dd , #demographics_w12\\.xpmvwhn h3 , #demographics_w12\\.xpabr dd~ dd , #demographics_w12\\.xpabr h3 , #demographics_w12\\.xpmove dd~ dd , #demographics_w12\\.xpmove h3 , #demographics_w12\\.lkmove dd~ dd , #demographics_w12\\.lkmove h3 , #demographics_w12\\.mvever dd~ dd , #demographics_w12\\.mvever h3 , #demographics_w12\\.dcsex dd~ dd , #demographics_w12\\.dcsex h3 , #demographics_w12\\.netpusenew dd~ dd , #demographics_w12\\.netpusenew h3 , #demographics_w12\\.smartmob dd~ dd , #demographics_w12\\.smartmob h3 , #demographics_w12\\.mlstat dd~ dd , #demographics_w12\\.mlstat h3 , #demographics_w12\\.jbstat dd~ dd , #demographics_w12\\.jbstat h3 , #demographics_w12\\.rindiv dd~ dd , #demographics_w12\\.rindiv h3 , #demographics_w12\\.mobcomp h3 , #demographics_w12\\.xpmvarea h3 , #demographics_w12\\.xpmvpcode h3 , #demographics_w12\\.xpmvtown h3 , #demographics_w12\\.xpmvadd1 h3 , #demographics_w12\\.mvmnth h3 , #demographics_w12\\.dcdobu16 h3 , #demographics_w12\\.dcname h3 , #demographics_w12\\.chkresp h3 , #demographics_w12\\.xpmvmnth h3 , #demographics_w12\\.dcsname h3 , #demographics_w12\\.xpcnt h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #demographics_w12\\.dcdobd h3 , #demographics_w12\\.mvyr h3'
childcare_node <- '#childcare_w12\\.wrkch8code h3 , #childcare_w12\\.wrkch3code h3 , #childcare_w12\\.ccwork dd~ dd , #childcare_w12\\.ccwork h3 , #childcare_w12\\.ccare dd~ dd , #childcare_w12\\.ccare h3 , #childcare_w12\\.mostuse h3 , #childcare_w12\\.wrkch7 h3 , #childcare_w12\\.wrkch2 h3 , #childcare_w12\\.wrkch2a h3 , #childcare_w12\\.wrkch1c h3 , #childcare_w12\\.wrkch1a h3 , #childcare_w12\\.wrkch8 h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #childcare_w12\\.wrkch3 h3'
jobsatisfaction_node <- '#jobsatisfaction_w12\\.jbsat h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2)'
nonemployment_node <- '#nonemployment_w12\\.jlsize dd~ dd , #nonemployment_w12\\.jlsize h3 , #nonemployment_w12\\.jlmngr dd~ dd , #nonemployment_w12\\.jlmngr h3 , #nonemployment_w12\\.jubgn dd~ dd , #nonemployment_w12\\.jubgn h3 , #nonemployment_w12\\.jlsemp dd~ dd , #nonemployment_w12\\.jlsemp h3 , #nonemployment_w12\\.julkjb dd~ dd , #nonemployment_w12\\.julkjb h3 , #nonemployment_w12\\.julk4wk dd~ dd , #nonemployment_w12\\.julk4wk h3 , #nonemployment_w12\\.eprosh h3 , #nonemployment_w12\\.jlsoc00 h3 , #nonemployment_w12\\.jlsic07 h3 , #nonemployment_w12\\.jlendm h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #nonemployment_w12\\.julk4x h3 , #nonemployment_w12\\.jlboss h3 , #nonemployment_w12\\.jbhad h3 , #nonemployment_w12\\.jlendy h3'
secondjobs_node <- '#secondjobs_w12\\.j2semp dd~ dd , #secondjobs_w12\\.j2semp h3 , #secondjobs_w12\\.j2soc00 h3 , #secondjobs_w12\\.j2pay h3 , h3+ .dl-horizontal dd:nth-child(2) , #secondjobs_w12\\.j2hrs h3 , dd:nth-child(6) , #secondjobs_w12\\.j2has dd:nth-child(2) , #secondjobs_w12\\.j2has h3'
householdfin_node <- '#householdfinances_w12\\.fiyrdb2 h3+ .dl-horizontal dd , #householdfinances_w12\\.fiyrdb2 h3 , #householdfinances_w12\\.fiyrdb1 h3+ .dl-horizontal dd , #householdfinances_w12\\.fiyrdb1 h3 , #householdfinances_w12\\.fiyrdb3 h3+ .dl-horizontal dd , #householdfinances_w12\\.fiyrdb3 h3 , #householdfinances_w12\\.fiyrdb6 h3+ .dl-horizontal dd , #householdfinances_w12\\.fiyrdb6 h3 , #householdfinances_w12\\.fiyrdb4 h3+ .dl-horizontal dd , #householdfinances_w12\\.fiyrdb4 h3 , #householdfinances_w12\\.fiyrdb5 h3+ .dl-horizontal dd , #householdfinances_w12\\.fiyrdb5 h3 , #householdfinances_w12\\.fiyrdia_cawi h3+ .dl-horizontal dd , #householdfinances_w12\\.finfut dd:nth-child(2) , #householdfinances_w12\\.finnow dd:nth-child(2) , #householdfinances_w12\\.fiyrdia_cawi dd~ dd , #householdfinances_w12\\.fiyrdia_cawi h3 , #householdfinances_w12\\.finfut h3 , #householdfinances_w12\\.finnow h3 , dd:nth-child(6) , #householdfinances_w12\\.fiyrdia dd:nth-child(2) , #householdfinances_w12\\.fiyrdia h3'
disability_node <- '#disability_w12\\.inthealth dd , #disability_w12\\.inthealth h3 , #disability_w12\\.health dd , #disability_w12\\.health h3 , #disability_w12\\.dissev h3 , dd:nth-child(6) , dd:nth-child(2) , #disability_w12\\.disdif h3'
ghq_node <- '#scaghq_w12\\.scghql h3 , #scaghq_w12\\.scghqk h3 , #scaghq_w12\\.scghqj h3 , #scaghq_w12\\.scghqi h3 , #scaghq_w12\\.scghqh h3 , #scaghq_w12\\.scghqg h3 , #scaghq_w12\\.scghqf h3 , #scaghq_w12\\.scghqe h3 , #scaghq_w12\\.scghqd h3 , #scaghq_w12\\.scghqc h3 , #scaghq_w12\\.scghqb h3 , h3+ .dl-horizontal dd , #scaghq_w12\\.scghqa h3'
employees_node <- '#employees_w12\\.mainwktrv h3 , #employees_w12\\.rsntrvchng dd~ dd , #employees_w12\\.rsntrvchng h3 , #employees_w12\\.maintrvchng dd~ dd , #employees_w12\\.maintrvchng h3 , #employees_w12\\.ovtnset dd~ dd , #employees_w12\\.ovtnset h3 , #employees_w12\\.basnset dd~ dd , #employees_w12\\.basnset h3 , #employees_w12\\.pvtpyset dd~ dd , #employees_w12\\.pvtpyset h3 , #employees_w12\\.pvtpay dd~ dd , #employees_w12\\.pvtpay h3 , #employees_w12\\.paytyp dd~ dd , #employees_w12\\.paytyp h3 , #employees_w12\\.payug dd~ dd , #employees_w12\\.payug h3 , #employees_w12\\.paynwc_cawi dd~ dd , #employees_w12\\.paynwc_cawi h3 , #employees_w12\\.paygwc_cawi dd~ dd , #employees_w12\\.paygwc_cawi h3 , #employees_w12\\.jbhrcpr dd~ dd , #employees_w12\\.jbhrcpr h3 , #employees_w12\\.jbsectpub dd~ dd , #employees_w12\\.jbsectpub h3 , #employees_w12\\.jbsect dd~ dd , #employees_w12\\.jbsect h3 , #employees_w12\\.jbsizechk dd~ dd , #employees_w12\\.jbsizechk h3 , #employees_w12\\.jbmngrchk dd~ dd , #employees_w12\\.jbmngrchk h3 , #employees_w12\\.rsntrvchngo dd~ dd , #employees_w12\\.rsntrvchngo h3 , #employees_w12\\.basrate_cawi dd~ dd , #employees_w12\\.basrate_cawi h3 , #employees_w12\\.paygl_cawi dd~ dd , #employees_w12\\.paygl_cawi h3 , #employees_w12\\.payuwc dd~ dd , #employees_w12\\.payuwc h3 , #employees_w12\\.payusl dd~ dd , #employees_w12\\.payusl h3 , #employees_w12\\.paynwc dd~ dd , #employees_w12\\.paynwc h3 , #employees_w12\\.jbmngr dd~ dd , #employees_w12\\.jbmngr h3 , #employees_w12\\.paygwc dd~ dd , #employees_w12\\.paygwc h3 , #employees_w12\\.paynl_cawi dd~ dd , #employees_w12\\.paynl_cawi h3 , #employees_w12\\.wktrvfar h3 , #employees_w12\\.wktrv h3 , #employees_w12\\.jbnitax h3 , #employees_w12\\.jbpaidwho h3 , #employees_w12\\.jbprhr h3 , #employees_w12\\.ovtrate h3 , #employees_w12\\.basrate h3 , #employees_w12\\.extrate h3 , #employees_w12\\.payu h3 , #employees_w12\\.jbsize h3 , #employees_w12\\.jbpl h3 , #employees_w12\\.jbttwt h3 , #employees_w12\\.paynl h3 , #employees_w12\\.paygl h3 , #employees_w12\\.jbotpd h3 , #employees_w12\\.jbot h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #employees_w12\\.jbhrs h3 , #employees_w12\\.ovtrest h3 , #employees_w12\\.basrest h3 , #employees_w12\\.extrest h3'
sf12_node <- '#scasf12_w12\\.scsf7 h3 , #scasf12_w12\\.scsf6c h3 , #scasf12_w12\\.scsf6b h3 , #scasf12_w12\\.scsf6a h3 , #scasf12_w12\\.scsf5 h3 , #scasf12_w12\\.scsf4b h3 , #scasf12_w12\\.scsf4a h3 , #scasf12_w12\\.scsf2b h3 , #scasf12_w12\\.scsf2a h3 , #scasf12_w12\\.scsf1 h3 , #scasf12_w12\\.scsf3a h3 , h3+ .dl-horizontal dd , #scasf12_w12\\.scsf3b h3'
benefits_node <- '#benefits_w12\\.ficode h3 , #benefits_w12\\.fiseq h3 , #benefits_w12\\.missource h3 , #benefits_w12\\.fraddl dd~ dd , #benefits_w12\\.fraddl h3 , #benefits_w12\\.nfh dd , #benefits_w12\\.nfh h3 , #benefits_w12\\.frjt dd , #benefits_w12\\.frjt h3 , #benefits_w12\\.frwc dd~ dd , #benefits_w12\\.frwc h3 , #benefits_w12\\.intbenefit dd , #benefits_w12\\.intbenefit h3 , #benefits_w12\\.fitax dd~ dd , #benefits_w12\\.fitax h3 , #benefits_w12\\.benctc h3 , #benefits_w12\\.benctc dd~ dd , h3+ .dl-horizontal dd:nth-child(2) , dd dd , #benefits_w12\\.benesa h3 , #benefits_w12\\.frjtpn h3 , #benefits_w12\\.frval h3 , #benefits_w12\\.bensta h3 , #benefits_w12\\.othben h3 , #benefits_w12\\.bendis h3 , #benefits_w12\\.benpen h3 , dd:nth-child(6) , #benefits_w12\\.benbase dd:nth-child(2) , #benefits_w12\\.benbase h3'
scasatisfaction_node <- '#scasatisfaction_w12\\.sclfsato h3 , #scasatisfaction_w12\\.sclfsat7 h3 , #scasatisfaction_w12\\.sclfsat2 h3 , #scasatisfaction_w12\\.sclfsat1 h3 , h3+ .dl-horizontal dd , #scasatisfaction_w12\\.sclfsat3 h3'
consents_node <- '#consents_w1\\.edlinkb h3 , #consents_w1\\.chedlink h3 , #consents_w1\\.chfllink h3 , #consents_w1\\.chhlthlink h3 , h3+ .dl-horizontal dd , #consents_w1\\.childpno h3 , #consents_w1\\.flaglink h3 , #consents_w1\\.healthlink h3 , #consents_w1\\.consent h3'
contactdetails_node <- '#contactdetails_w1\\.rcotherd h3 , #contactdetails_w1\\.othcont dd , #contactdetails_w1\\.othcont h3 , #contactdetails_w1\\.remail h3 , #contactdetails_w1\\.hasemail dd , #contactdetails_w1\\.hasemail h3 , #contactdetails_w1\\.rphwrk h3 , h3+ .dl-horizontal dd:nth-child(2) , #contactdetails_w1\\.rphmob h3 , dd:nth-child(6) , #contactdetails_w1\\.rhland dd:nth-child(2) , #contactdetails_w1\\.rhland h3'
discrimination_node <- '#discrimination_w1\\.restraindeny h3 , #discrimination_w1\\.traindeny h3 , #discrimination_w1\\.respromodeny h3 , #discrimination_w1\\.promodeny h3 , #discrimination_w1\\.eed12 h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #discrimination_w1\\.resjobdeny h3 , #discrimination_w1\\.jobdeny h3 , #discrimination_w1\\.joblook h3 , #discrimination_w1\\.discrimination h3'
employmentstatus_node <- '#employmentstatushistory_w1\\.pj1mngr dd~ dd , #employmentstatushistory_w1\\.pj1mngr h3 , #employmentstatushistory_w1\\.pj1boss dd~ dd , #employmentstatushistory_w1\\.pj1boss h3 , #employmentstatushistory_w1\\.pj1semp h3 , #employmentstatushistory_w1\\.pj1soc00 h3 , #employmentstatushistory_w1\\.leshsy4 h3 , #employmentstatushistory_w1\\.leshem h3 , #employmentstatushistory_w1\\.lgaped dd~ dd , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #employmentstatushistory_w1\\.leshst h3 , #employmentstatushistory_w1\\.lgaped h3 , #employmentstatushistory_w1\\.empstat h3'
environmentalbehavior_node <- '#environmentalbehaviour_w1\\.impevents h3 , #environmentalbehaviour_w1\\.howlng h3 , #environmentalbehaviour_w1\\.netuse dd , #environmentalbehaviour_w1\\.netuse h3 , #environmentalbehaviour_w1\\.mobuse dd , #environmentalbehaviour_w1\\.mobuse h3 , #environmentalbehaviour_w1\\.drive dd , #environmentalbehaviour_w1\\.envhabit11 dd , #environmentalbehaviour_w1\\.envhabit10 dd , #environmentalbehaviour_w1\\.envhabit9 dd , #environmentalbehaviour_w1\\.envhabit8 dd , #environmentalbehaviour_w1\\.envhabit7 dd , #environmentalbehaviour_w1\\.envhabit6 dd , #environmentalbehaviour_w1\\.envhabit5 dd , #environmentalbehaviour_w1\\.envhabit4 dd , #environmentalbehaviour_w1\\.envhabit3 dd , #environmentalbehaviour_w1\\.envhabit2 dd , #environmentalbehaviour_w1\\.envhabit1 dd , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #environmentalbehaviour_w1\\.carmiles h3 , #environmentalbehaviour_w1\\.drive h3 , #environmentalbehaviour_w1\\.envhabit11 h3 , #environmentalbehaviour_w1\\.envhabit10 h3 , #environmentalbehaviour_w1\\.envhabit9 h3 , #environmentalbehaviour_w1\\.envhabit8 h3 , #environmentalbehaviour_w1\\.envhabit7 h3 , #environmentalbehaviour_w1\\.envhabit6 h3 , #environmentalbehaviour_w1\\.envhabit5 h3 , #environmentalbehaviour_w1\\.envhabit4 h3 , #environmentalbehaviour_w1\\.envhabit3 h3 , #environmentalbehaviour_w1\\.envhabit2 h3 , #environmentalbehaviour_w1\\.envhabit1 h3 , #environmentalbehaviour_w1\\.environ h3'
ethnicity_node <- '#ethnicityandnationalidentity_w1\\.britid h3 , #ethnicityandnationalidentity_w1\\.smaid h3 , #ethnicityandnationalidentity_w1\\.maid dd , #ethnicityandnationalidentity_w1\\.maid h3 , #ethnicityandnationalidentity_w1\\.spaid h3 , #ethnicityandnationalidentity_w1\\.paid dd , #ethnicityandnationalidentity_w1\\.paid h3 , #ethnicityandnationalidentity_w1\\.racelo h3 , #ethnicityandnationalidentity_w1\\.racel h3 , h3+ .dl-horizontal dd:nth-child(2) , #ethnicityandnationalidentity_w1\\.natido h3 , dd:nth-child(6) , #ethnicityandnationalidentity_w1\\.natid dd:nth-child(2) , #ethnicityandnationalidentity_w1\\.natid h3 , #ethnicityandnationalidentity_w1\\.ethnicts h3'
family_node <- '#familybackground_w1\\.ynlp14 dd~ dd , #familybackground_w1\\.ynlp14 h3 , #familybackground_w1\\.lvag14 h3 , #familybackground_w1\\.agelh h3 , #familybackground_w1\\.lvag16 dd , #familybackground_w1\\.lvag16 h3 , #familybackground_w1\\.maedqf dd~ dd , #familybackground_w1\\.maedqf h3 , #familybackground_w1\\.paedqf dd~ dd , #familybackground_w1\\.paedqf h3 , #familybackground_w1\\.mgmrobo h3 , #familybackground_w1\\.mgmrob dd , #familybackground_w1\\.mgmrob h3 , #familybackground_w1\\.mgprobo h3 , #familybackground_w1\\.mgprob dd , #familybackground_w1\\.mgprob h3 , #familybackground_w1\\.pgmrobo h3 , #familybackground_w1\\.pgmrob dd , #familybackground_w1\\.pgmrob h3 , #familybackground_w1\\.pgprobo h3 , #familybackground_w1\\.pgprob dd , #familybackground_w1\\.pgprob h3 , #familybackground_w1\\.mayruk1 h3 , #familybackground_w1\\.mayruk dd~ dd , #familybackground_w1\\.mayruk h3 , #familybackground_w1\\.macobo h3 , #familybackground_w1\\.macob dd , #familybackground_w1\\.macob h3 , #familybackground_w1\\.payruk1 h3 , #familybackground_w1\\.payruk dd~ dd , #familybackground_w1\\.payruk h3 , #familybackground_w1\\.pacobo h3 , #familybackground_w1\\.pacob dd , #familybackground_w1\\.pacob h3 , #familybackground_w1\\.masoc00 h3 , #familybackground_w1\\.maju h3 , h3+ .dl-horizontal dd:nth-child(2) , #familybackground_w1\\.pasoc00 h3 , dd:nth-child(6) , #familybackground_w1\\.paju dd:nth-child(2) , #familybackground_w1\\.paju h3'
familynetworks_node <- '#familynetworks_w1\\.ftexw dd~ dd , #familynetworks_w1\\.ftexw h3 , #familynetworks_w1\\.ftexv h3 , #familynetworks_w1\\.kidspt h3 , #familynetworks_w1\\.chfar dd~ dd , #familynetworks_w1\\.chfar h3 , #familynetworks_w1\\.chcon dd~ dd , #familynetworks_w1\\.chcon h3 , #familynetworks_w1\\.chsee dd~ dd , #familynetworks_w1\\.chsee h3 , #familynetworks_w1\\.pafar dd~ dd , #familynetworks_w1\\.pafar h3 , #familynetworks_w1\\.pacon dd~ dd , #familynetworks_w1\\.pacon h3 , #familynetworks_w1\\.pasee dd~ dd , #familynetworks_w1\\.pasee h3 , #familynetworks_w1\\.mafar dd~ dd , #familynetworks_w1\\.mafar h3 , #familynetworks_w1\\.macon dd~ dd , #familynetworks_w1\\.macon h3 , #familynetworks_w1\\.masee dd~ dd , #familynetworks_w1\\.masee h3 , #familynetworks_w1\\.relkid h3 , #familynetworks_w1\\.farkid dd~ dd , #familynetworks_w1\\.farkid h3 , #familynetworks_w1\\.wekid dd~ dd , #familynetworks_w1\\.wekid h3 , #familynetworks_w1\\.seekid dd~ dd , #familynetworks_w1\\.seekid h3 , #familynetworks_w1\\.ohch16 dd~ dd , #familynetworks_w1\\.ohch16 h3 , #familynetworks_w1\\.parmar dd~ dd , #familynetworks_w1\\.parmar h3 , #familynetworks_w1\\.nrels6 dd~ dd , #familynetworks_w1\\.nrels6 h3 , #familynetworks_w1\\.nrels5 dd~ dd , #familynetworks_w1\\.nrels5 h3 , #familynetworks_w1\\.nrels4 dd~ dd , #familynetworks_w1\\.nrels4 h3 , #familynetworks_w1\\.nrels3 dd~ dd , #familynetworks_w1\\.nrels3 h3 , #familynetworks_w1\\.nrels2 dd~ dd , #familynetworks_w1\\.nrels2 h3 , #familynetworks_w1\\.nrels1 dd~ dd , #familynetworks_w1\\.nrels1 h3 , #familynetworks_w1\\.paage h3 , h3+ .dl-horizontal dd:nth-child(2) , #familynetworks_w1\\.maage h3 , dd:nth-child(6) , #familynetworks_w1\\.lvrel dd:nth-child(2) , #familynetworks_w1\\.lvrel h3'
harrasment_node <- '#harassment_w1\\.resattacked h3 , #harassment_w1\\.attackno h3 , #harassment_w1\\.attacked h3 , #harassment_w1\\.resavoid h3 , #harassment_w1\\.avoidno h3 , #harassment_w1\\.avoidance h3 , #harassment_w1\\.resunsafe h3 , #harassment_w1\\.unsafeno h3 , #harassment_w1\\.unsafe h3 , #harassment_w1\\.harassment h3 , #harassment_w1\\.resinsulted h3 , #harassment_w1\\.insultno h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #harassment_w1\\.insulted h3'
fertility_node <- '#fertilityhistory_w1\\.lchmorn dd~ dd , #fertilityhistory_w1\\.lchmorn h3 , #fertilityhistory_w1\\.lchmor dd~ dd , #fertilityhistory_w1\\.lchmor h3 , #fertilityhistory_w1\\.brfedend2 h3 , #fertilityhistory_w1\\.brfedend h3 , #fertilityhistory_w1\\.brfed dd~ dd , #fertilityhistory_w1\\.brfed h3 , #fertilityhistory_w1\\.bwtg5 dd~ dd , #fertilityhistory_w1\\.bwtg5 h3 , #fertilityhistory_w1\\.bwtk h3 , #fertilityhistory_w1\\.bwtoz h3 , #fertilityhistory_w1\\.bwtlb h3 , #fertilityhistory_w1\\.bwt h3 , #fertilityhistory_w1\\.bwtwk h3 , #fertilityhistory_w1\\.bwtel dd~ dd , #fertilityhistory_w1\\.bwtel h3 , #fertilityhistory_w1\\.bwtxp dd~ dd , #fertilityhistory_w1\\.bwtxp h3 , #fertilityhistory_w1\\.lchchk h3 , #fertilityhistory_w1\\.lchno h3 , #fertilityhistory_w1\\.lchal h3 , #fertilityhistory_w1\\.lchdoby h3 , #fertilityhistory_w1\\.lchdobm h3 , #fertilityhistory_w1\\.lchdobd h3 , #fertilityhistory_w1\\.lchsx dd~ dd , #fertilityhistory_w1\\.lchsx h3 , #fertilityhistory_w1\\.lchyd4 dd~ dd , #fertilityhistory_w1\\.lchyd4 h3 , #fertilityhistory_w1\\.lchlv dd~ dd , #fertilityhistory_w1\\.lchlv h3 , #fertilityhistory_w1\\.lnprnt h3 , #fertilityhistory_w1\\.lprnt h3 , #fertilityhistory_w1\\.lacyd4 dd~ dd , #fertilityhistory_w1\\.lacyd4 h3 , #fertilityhistory_w1\\.lacal dd~ dd , #fertilityhistory_w1\\.lacal h3 , #fertilityhistory_w1\\.lacno h3 , #fertilityhistory_w1\\.laclv dd~ dd , #fertilityhistory_w1\\.laclv h3 , #fertilityhistory_w1\\.lacyb4 h3 , #fertilityhistory_w1\\.lascst dd~ dd , #fertilityhistory_w1\\.lascst h3 , #fertilityhistory_w1\\.lacsx dd~ dd , #fertilityhistory_w1\\.lacsx h3 , #fertilityhistory_w1\\.lacby4 h3 , #fertilityhistory_w1\\.lacbm h3 , #fertilityhistory_w1\\.lacbd h3 , #fertilityhistory_w1\\.adoptno h3 , #fertilityhistory_w1\\.lnadopt dd~ dd , #fertilityhistory_w1\\.lnadopt h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #fertilityhistory_w1\\.ladopt h3 , #fertilityhistory_w1\\.fertility h3'
initialconditions_node <- '#initialconditions_w1\\.wlshue dd~ dd , #initialconditions_w1\\.wlshue h3 , #initialconditions_w1\\.wlshud dd~ dd , #initialconditions_w1\\.wlshud h3 , #initialconditions_w1\\.wlsh h3 , #initialconditions_w1\\.fednt dd~ dd , #initialconditions_w1\\.fednt h3 , #initialconditions_w1\\.fedlik h3 , #initialconditions_w1\\.edasp dd~ dd , #initialconditions_w1\\.edasp h3 , #initialconditions_w1\\.edtype h3 , #initialconditions_w1\\.feend h3 , #initialconditions_w1\\.fenow h3 , #initialconditions_w1\\.schname h3 , #initialconditions_w1\\.schok dd~ dd , #initialconditions_w1\\.schok h3 , #initialconditions_w1\\.schcode h3 , #initialconditions_w1\\.schlloc h3 , #initialconditions_w1\\.scend h3 , #initialconditions_w1\\.school h3 , #initialconditions_w1\\.qfvoc h3 , #initialconditions_w1\\.qualoc h3 , #initialconditions_w1\\.qfhigh h3 , #initialconditions_w1\\.citzno h3 , #initialconditions_w1\\.citzn h3 , #initialconditions_w1\\.plbornuk dd~ dd , #initialconditions_w1\\.plbornc dd~ dd , #initialconditions_w1\\.ukborn dd , #initialconditions_w1\\.wlshuc dd~ dd , #initialconditions_w1\\.wlshub dd~ dd , #initialconditions_w1\\.wlshua dd~ dd , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #initialconditions_w1\\.yr2uk4 h3 , #initialconditions_w1\\.plbornuko h3 , #initialconditions_w1\\.plbornuk h3 , #initialconditions_w1\\.plboth h3 , #initialconditions_w1\\.plbornc h3 , #initialconditions_w1\\.ukborn h3 , #initialconditions_w1\\.initial h3 , #initialconditions_w1\\.wlshuc h3 , #initialconditions_w1\\.wlshub h3 , h3+ dd , #initialconditions_w1\\.wlshua h3'
language_node <- '#language_w1\\.formdif h3 , #language_w1\\.engform h3 , #language_w1\\.readdif h3 , #language_w1\\.engread h3 , #language_w1\\.teldif h3 , #language_w1\\.engtel h3 , #language_w1\\.spkdif h3 , h3+ .dl-horizontal dd , #language_w1\\.engspk h3 , #language_w1\\.englang h3'
migration_node <- '#migrationhistory_w1\\.mlivedistf h3 , #migrationhistory_w1\\.mlivedist5 dd~ dd , #migrationhistory_w1\\.mlivedist5 h3 , #migrationhistory_w1\\.mlivedist h3 , #migrationhistory_w1\\.mnmoves h3 , #migrationhistory_w1\\.moveage h3 , #migrationhistory_w1\\.mlivedco h3 , #migrationhistory_w1\\.mlivedc h3 , #migrationhistory_w1\\.countryno2 h3 , #migrationhistory_w1\\.mnlived h3 , #migrationhistory_w1\\.mlived dd~ dd , #migrationhistory_w1\\.mlived h3 , #migrationhistory_w1\\.mreturned dd~ dd , #migrationhistory_w1\\.mreturned h3 , #migrationhistory_w1\\.mindirectco h3 , #migrationhistory_w1\\.mindirectc h3 , #migrationhistory_w1\\.countryno1 h3 , #migrationhistory_w1\\.mnotherc h3 , #migrationhistory_w1\\.mindirect h3 , #migrationhistory_w1\\.mabroadco h3 , #migrationhistory_w1\\.mabroadc h3 , #migrationhistory_w1\\.countryno h3 , #migrationhistory_w1\\.mabroad dd~ dd , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #migrationhistory_w1\\.mnabroad h3 , #migrationhistory_w1\\.mabroad h3 , #migrationhistory_w1\\.migration h3'
parentschildren_node <- '#parentsandchildren_w1\\.yellkid dd~ dd , #parentsandchildren_w1\\.yellkid h3 , #parentsandchildren_w1\\.cuddlekid dd~ dd , #parentsandchildren_w1\\.cuddlekid h3 , #parentsandchildren_w1\\.slapkid dd~ dd , #parentsandchildren_w1\\.slapkid h3 , #parentsandchildren_w1\\.praisekid dd~ dd , #parentsandchildren_w1\\.praisekid h3 , #parentsandchildren_w1\\.ruleskid dd~ dd , #parentsandchildren_w1\\.ruleskid h3 , #parentsandchildren_w1\\.talkmatter h3 , #parentsandchildren_w1\\.quarrel h3 , #parentsandchildren_w1\\.dinner dd~ dd , #parentsandchildren_w1\\.dinner h3 , #parentsandchildren_w1\\.socialkid h3 , #parentsandchildren_w1\\.kid2uni dd~ dd , #parentsandchildren_w1\\.kid2uni h3 , #parentsandchildren_w1\\.schtowncpt h3 , #parentsandchildren_w1\\.schnamecpt h3 , #parentsandchildren_w1\\.schnamecst h3 , #parentsandchildren_w1\\.schokst dd~ dd , #parentsandchildren_w1\\.schokst h3 , #parentsandchildren_w1\\.schcodest h3 , #parentsandchildren_w1\\.schsta dd~ dd , #parentsandchildren_w1\\.schsta h3 , #parentsandchildren_w1\\.childno h3 , #parentsandchildren_w1\\.samsch dd~ dd , #parentsandchildren_w1\\.samsch h3 , #parentsandchildren_w1\\.levelimp h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #parentsandchildren_w1\\.hlphmwk h3 , #parentsandchildren_w1\\.parchild h3'
partnership_node <- '#partnershiphistory_w1\\.lcsey4 h3 , #partnershiphistory_w1\\.lcsem h3 , #partnershiphistory_w1\\.lcsby4 h3 , #partnershiphistory_w1\\.lcsbm h3 , #partnershiphistory_w1\\.cohabno h3 , #partnershiphistory_w1\\.lncoh h3 , #partnershiphistory_w1\\.lcoh dd , #partnershiphistory_w1\\.lcoh h3 , #partnershiphistory_w1\\.lmspy4 h3 , #partnershiphistory_w1\\.lmspm h3 , #partnershiphistory_w1\\.lmdvy4 h3 , #partnershiphistory_w1\\.lmdvm h3 , #partnershiphistory_w1\\.lspwwd dd~ dd , #partnershiphistory_w1\\.lspwwd h3 , #partnershiphistory_w1\\.lmwwy4 h3 , #partnershiphistory_w1\\.lmwwm h3 , #partnershiphistory_w1\\.lmend dd~ dd , #partnershiphistory_w1\\.lmend h3 , #partnershiphistory_w1\\.lmcby4 h3 , #partnershiphistory_w1\\.lmcbm h3 , #partnershiphistory_w1\\.lmcoh dd~ dd , #partnershiphistory_w1\\.lmcoh h3 , #partnershiphistory_w1\\.lmary4 h3 , #partnershiphistory_w1\\.lmarm dd~ dd , #partnershiphistory_w1\\.lmarm h3 , #partnershiphistory_w1\\.marno h3 , #partnershiphistory_w1\\.pmarint dd~ dd , #partnershiphistory_w1\\.pmarint h3 , #partnershiphistory_w1\\.nmar h3 , #partnershiphistory_w1\\.lcmspy4 h3 , #partnershiphistory_w1\\.lcmspm h3 , #partnershiphistory_w1\\.lcmcby4 h3 , #partnershiphistory_w1\\.lcmcbm h3 , #partnershiphistory_w1\\.lcmcoh dd~ dd , #partnershiphistory_w1\\.lcmcoh h3 , #partnershiphistory_w1\\.mpno h3 , #partnershiphistory_w1\\.lcmary4 h3 , #partnershiphistory_w1\\.partint dd , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #partnershiphistory_w1\\.lcmarm h3 , #partnershiphistory_w1\\.partint h3 , #partnershiphistory_w1\\.partner h3'
politics_node <- '#politics_w1\\.vote3 h3+ .dl-horizontal dd , #politics_w1\\.vote3o h3+ .dl-horizontal dd , #politics_w1\\.vote4 h3+ .dl-horizontal dd , #politics_w1\\.vote4o h3+ .dl-horizontal dd , #politics_w1\\.vote5 h3+ .dl-horizontal dd , dd:nth-child(6) , #politics_w1\\.vote6 dd:nth-child(2) , #politics_w1\\.vote6 h3 , #politics_w1\\.vote5 h3 , #politics_w1\\.vote4o h3 , #politics_w1\\.vote4 h3 , #politics_w1\\.vote3o h3 , #politics_w1\\.vote3 h3 , #politics_w1\\.vote2 h3+ .dl-horizontal dd , #politics_w1\\.vote2 h3 , #politics_w1\\.vote1 dd , #politics_w1\\.vote1 h3'
religion_node <- '#religion_w1\\.oprlg3 h3 , #religion_w1\\.oprlg2 h3 , #religion_w1\\.oprlg1 dd~ dd , #religion_w1\\.oprlg1 h3 , #religion_w1\\.oprlg0 dd~ dd , #religion_w1\\.oprlg0 h3 , #religion_w1\\.niact dd~ dd , #religion_w1\\.niact h3 , #religion_w1\\.nireloth h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #religion_w1\\.nirel h3 , #religion_w1\\.oprlg0ni h3+ .dl-horizontal dd , #religion_w1\\.oprlg dd , #religion_w1\\.oprlg0ni h3 , #religion_w1\\.oprlg h3 , #religion_w1\\.religion h3'
remitances_node <- '#remittances_w1\\.remusamt h3 , #remittances_w1\\.remitusual dd~ dd , #remittances_w1\\.remitusual h3 , #remittances_w1\\.remcntryo h3 , #remittances_w1\\.remcntry h3 , #remittances_w1\\.remitamt h3 , #remittances_w1\\.remitreg h3 , #remittances_w1\\.remitfreq dd~ dd , #remittances_w1\\.remitfreq h3 , #remittances_w1\\.remittype h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #remittances_w1\\.remit h3 , #remittances_w1\\.remittance h3'
stablecontact_node <- '#stablecontact_w1\\.ctrel dd~ dd , #stablecontact_w1\\.ctrel h3 , #stablecontact_w1\\.ctemail h3 , #stablecontact_w1\\.cttel2 h3 , #stablecontact_w1\\.cttel1 h3 , #stablecontact_w1\\.ctpcode h3 , #stablecontact_w1\\.ctcnty h3 , #stablecontact_w1\\.cttown h3 , #stablecontact_w1\\.ctadd2 h3 , #stablecontact_w1\\.ctadd1 h3 , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #stablecontact_w1\\.ctcons dd , #stablecontact_w1\\.ctname h3 , #stablecontact_w1\\.ctcons h3 , #stablecontact_w1\\.stable h3'
health_node <- '#health_w1\\.hconda17 h3 , #health_w1\\.hconds17 dd~ dd , #health_w1\\.hconds17 h3 , #health_w1\\.hconda16 h3 , #health_w1\\.hconds16 dd~ dd , #health_w1\\.hconds16 h3 , #health_w1\\.hconda15 h3 , #health_w1\\.hconds15 dd~ dd , #health_w1\\.hconds15 h3 , #health_w1\\.hconda14 h3 , #health_w1\\.hconds14 dd~ dd , #health_w1\\.hconds14 h3 , #health_w1\\.hconda13 h3 , #health_w1\\.hconds13 dd~ dd , #health_w1\\.hconds13 h3 , #health_w1\\.hconda12 h3 , #health_w1\\.hconds12 dd~ dd , #health_w1\\.hconds12 h3 , #health_w1\\.hconda11 h3 , #health_w1\\.hconds11 dd~ dd , #health_w1\\.hconds11 h3 , #health_w1\\.hconda10 h3 , #health_w1\\.hconds10 dd~ dd , #health_w1\\.hconds10 h3 , #health_w1\\.hconda09 h3 , #health_w1\\.hconds09 dd~ dd , #health_w1\\.hconds09 h3 , #health_w1\\.hconda08 h3 , #health_w1\\.hconda07 h3 , #health_w1\\.hconds07 dd~ dd , #health_w1\\.hconds07 h3 , #health_w1\\.hconda06 h3 , #health_w1\\.hconds06 dd~ dd , #health_w1\\.hconds06 h3 , #health_w1\\.hconda05 h3 , #health_w1\\.hconds05 dd~ dd , #health_w1\\.hconds05 h3 , #health_w1\\.hconda04 h3 , #health_w1\\.hconds04 dd~ dd , #health_w1\\.hconds04 h3 , #health_w1\\.hconda03 h3 , #health_w1\\.hconds03 dd~ dd , #health_w1\\.hconds03 h3 , #health_w1\\.hconda02 h3 , #health_w1\\.hconds02 dd~ dd , #health_w1\\.hconds02 h3 , h3+ .dl-horizontal dd:nth-child(2) , #health_w1\\.hconda01 h3 , #health_w1\\.hconds01 h3 , #health_w1\\.hcond dd:nth-child(2) , #health_w1\\.hcond h3 , #health_w1\\.disdif dd:nth-child(2) , #health_w1\\.disdif h3 , #health_w1\\.health dd:nth-child(2) , #health_w1\\.health h3 , #health_w1\\.hlwtl dd:nth-child(2) , #health_w1\\.hlwtl h3 , h3+ dd , #health_w1\\.hlwte h3 , h3+ dd , #health_w1\\.hlwtk h3 , #health_w1\\.hlwtp h3 , #health_w1\\.hlwts h3 , #health_w1\\.hlwt dd:nth-child(2) , #health_w1\\.hlwt h3 , #health_w1\\.hlhtc h3 , #health_w1\\.hlhti h3 , #health_w1\\.hlhtf h3 , #health_w1\\.hlht dd:nth-child(2) , #health_w1\\.hlht h3 , #health_w1\\.sf7 h3 , #health_w1\\.sf6c h3 , #health_w1\\.sf6b h3 , #health_w1\\.sf6a h3 , #health_w1\\.sf5 dd:nth-child(2) , #health_w1\\.sf5 h3 , #health_w1\\.sf4b h3 , #health_w1\\.sf4a h3 , #health_w1\\.sf3b h3 , #health_w1\\.sf3a h3 , #health_w1\\.sf2b dd:nth-child(2) , #health_w1\\.sf2b h3 , #health_w1\\.sf2a dd:nth-child(2) , #health_w1\\.sf2a h3 , dd:nth-child(6) , #health_w1\\.sf1 h3 , #health_w1\\.healthts h3 , #health_w1\\.hconds08 h3'
householdgrid_node <- '#hhgrid_w1\\.lda h3 , #hhgrid_w1\\.gpcomp h3 , #hhgrid_w1\\.emboost h3 , #hhgrid_w1\\.numcivil h3 , #hhgrid_w1\\.nummpart h3 , #hhgrid_w1\\.numcpart h3 , #hhgrid_w1\\.numssex h3 , #hhgrid_w1\\.numchild h3 , #hhgrid_w1\\.numadult h3 , #hhgrid_w1\\.respm16 h3 , #hhgrid_w1\\.respf16 h3 , #hhgrid_w1\\.hgpart h3 , #hhgrid_w1\\.hgbiof h3 , #hhgrid_w1\\.hgbiom h3 , #hhgrid_w1\\.relationship h3 , #hhgrid_w1\\.apno h3 , #hhgrid_w1\\.epno h3 , #hhgrid_w1\\.relationships h3 , #hhgrid_w1\\.ethnic dd~ dd , #hhgrid_w1\\.ethnic h3 , #hhgrid_w1\\.lingua dd~ dd , #hhgrid_w1\\.lingua h3 , #hhgrid_w1\\.hhlang h3 , #hhgrid_w1\\.surnam h3 , #hhgrid_w1\\.fornam h3 , #hhgrid_w1\\.ttlx h3 , #hhgrid_w1\\.ttl h3 , #hhgrid_w1\\.employ dd~ dd , #hhgrid_w1\\.employ h3 , #hhgrid_w1\\.livewith h3 , #hhgrid_w1\\.livesp dd~ dd , #hhgrid_w1\\.livesp h3 , #hhgrid_w1\\.marstat h3 , #hhgrid_w1\\.absreasoth h3 , #hhgrid_w1\\.absreason h3 , #hhgrid_w1\\.dvage h3 , #hhgrid_w1\\.ageif h3 , #hhgrid_w1\\.birthd dd~ dd , #hhgrid_w1\\.birthd h3 , #hhgrid_w1\\.birthm dd~ dd , #hhgrid_w1\\.birthm h3 , #hhgrid_w1\\.birthy dd~ dd , #hhgrid_w1\\.birthy h3 , #hhgrid_w1\\.birth h3 , #hhgrid_w1\\.sex h3 , #hhgrid_w1\\.hhsize h3 , #hhgrid_w1\\.nametype h3 , #hhgrid_w1\\.absflag h3 , #hhgrid_w1\\.absent h3 , #hhgrid_w1\\.abseduc dd~ dd , dd:nth-child(6) , h3+ .dl-horizontal dd:nth-child(2) , #hhgrid_w1\\.absadd h3 , #hhgrid_w1\\.abseduc h3 , #hhgrid_w1\\.name h3 , #hhgrid_w1\\.pid h3 , #hhgrid_w1\\.pno h3 , #hhgrid_w1\\.region h3 , #hhgrid_w1\\.hhgrid h3 , #hhgrid_w1\\.hhsttime h3'
nodes <- c(selfemployment_node,caring_node,currentemployment_node,demographics_node,
           childcare_node,jobsatisfaction_node,nonemployment_node,secondjobs_node,
           householdfin_node,disability_node,ghq_node,employees_node,sf12_node,
           benefits_node,scasatisfaction_node,consents_node,contactdetails_node,
           discrimination_node,employmentstatus_node,environmentalbehavior_node,
           ethnicity_node,family_node,familynetworks_node,harrasment_node,fertility_node,
           initialconditions_node,language_node,migration_node,parentschildren_node,partnership_node,
           politics_node,religion_node,remitances_node,stablecontact_node,health_node,householdgrid_node) #Concatenation of nodes

#c. Number of options (manual coding due to unstructured webpages)
selfemployment_options <- c(NA,NA,NA,NA,NA,12,12,NA,NA,6,12,7,NA,NA,NA,NA,NA,11,3,12,2,11,2,2,NA,NA,11,3,3,3,3,2,2,2,2,2,2,3,2,2,2,10,NA,NA,NA)
caring_options <- c(2,10,16,7,7,3,3,2,NA)
currentemployment_options <- c(NA,NA,12,2,NA,NA,5,NA,2,3,8,2,2,2,2)
demographics_options <- c(NA,NA,NA,NA,16,4,NA,NA,12,NA,NA,NA,NA,2,30,14,9,2,9,2,2,2,2,2,2,2,NA,2,2,2,2,NA,NA,12,NA,NA,NA,12,2)
childcare_options <- c(NA,NA,2,3,17,17,17,17,2,5,NA,NA)
jobsatisfaction_options <- c(7)
nonemployment_options <- c(NA,2,2,7,12,NA,NA,4,2,2,2,2,3,11)
secondjobs_options <- c(2,NA,NA,NA,2)
householdfin_options <- c(NA,5,3,NA,2,2,2,2,2,2)
disability_options <- c(13,3,2,1)
ghq_options <- c(4,4,4,4,4,4,4,4,4,4,4,4)
employees_options <- c(2,2,2,NA,NA,NA,NA,NA,NA,5,11,NA,NA,NA,NA,NA,2,3,11,11,NA,16,3,16,2,16,NA,NA,NA,2,2,2,9,3,16,16,3,4,3,2,2,3,2,10,NA)
sf12_options <- c(5,5,5,3,3,5,5,5,5,5,5,5)
benefits_options <- c(5,9,11,9,9,NA,16,3,2,2,1,16,2,2,2,NA,NA,44)
scasatisfaction_options <- c(7,7,7,7,7)
consents_options <- c(NA,2,2,NA,2,2,3,2)
contactdetails_options <- c(NA,NA,NA,2,NA,2,NA)
discrimination_options <- c(NA,2,2,11,NA,2,11,2,11)
employmentstatus_options <- c(NA,2,13,NA,NA,NA,NA,2,2)
environmentalbehavior_options <- c(NA,6,6,6,6,6,6,6,6,6,6,6,2,NA,2,7,NA,NA)
ethnicity_options <- c(NA,7,NA,18,NA,17,NA,17,NA,NA)
family_options <- c(4,NA,4,NA,28,NA,2,NA,28,NA,2,NA,28,NA,28,NA,28,NA,28,NA,7,7,2,NA,8,4)
familynetworkds_options <- c(9,NA,NA,NA,NA,NA,NA,NA,NA,2,3,8,3,6,4,6,6,6,6,6,6,6,6,6,2,NA,5)
harrasment_options <- c(9,9,11,NA,9,9,11,9,9,11,9,9,11)
fertility_options <- c(NA,2,NA,NA,NA,12,NA,NA,2,NA,3,NA,NA,NA,2,NA,4,NA,2,NA,12,NA,NA,NA,2,2,2,NA,2,NA,NA,NA,2,3,NA,4,3,NA)
initialconditions_options <- c(6,6,6,NA,5,28,NA,NA,NA,NA,3,NA,16,2,16,3,NA,5,NA,2,NA,3,NA,5,4,5,10,5,6,6)
language_options <- c(2,2,4,2,3,2,4,2,3)
migration_options <- c(NA,2,NA,NA,NA,NA,2,NA,NA,NA,NA,2,2,NA,NA,NA,NA,NA,NA,6,2,5)
parentschildren_options <- c(NA,6,4,3,NA,3,NA,2,NA,NA,NA,2,6,4,4,4,4,4,4,4,4)
partnership_options <- c(NA,1,12,NA,NA,2,12,NA,12,NA,NA,1,NA,12,NA,2,12,NA,3,12,NA,2,12,NA,12,NA,2,NA,NA,12,NA,12,NA)
politics_options <- c(2,2,14,NA,13,NA,3,4)
religion_options <- c(NA,2,16,15,NA,2,17,16,5,4)
remitances_options <- c(NA,5,4,6,2,NA,NA,NA,3,NA)
stablecontact_options <- c(NA,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,8)
health_options <- c(2,NA,5,3,3,5,5,5,5,5,5,5,5,5,3,NA,NA,NA,3,NA,NA,NA,2,7,2,13,18,2,NA,2,NA,2,NA,2,NA,2,NA,2,NA,2,NA,NA,2,NA,2,NA,2,NA,2,NA,2,NA,2,NA,2,NA,2,NA,2,NA)
householdgrid_options <- c(NA,NA,NA,NA,NA,NA,2,2,2,2,3,NA,2,NA,NA,12,NA,NA,NA,10,NA,9,2,3,2,6,NA,NA,NA,2,10,15,NA,NA,NA,30,NA,NA,NA,2,2,NA,NA,NA,NA,NA,NA,2,2,2)
n_options <- list(selfemployment_options,caring_options,currentemployment_options,demographics_options,
                  childcare_options,jobsatisfaction_options,nonemployment_options,secondjobs_options,
                  householdfin_options,disability_options,ghq_options,employees_options,sf12_options,benefits_options,
                  scasatisfaction_options,consents_options,contactdetails_options,discrimination_options,employmentstatus_options,
                  environmentalbehavior_options,ethnicity_options,family_options,familynetworkds_options,harrasment_options,fertility_options,
                  initialconditions_options,language_options,migration_options,parentschildren_options,partnership_options,politics_options,
                  religion_options,remitances_options,stablecontact_options,health_options,householdgrid_options)

##### C. Web Scraping and structuration of information

#Function to read nodes, decompres, extract information in columns and add number of options
reading <- function(link,node,n_options){
  read <- read_html(link) %>%
    html_nodes(node) %>% 
    html_text() %>%
    accumulate(function(prev, current) {
      if (seq_along(prev) %% 2 == 1) {
        paste(prev, current)
      } else {
        current
      }
    }) %>% last() 
  read <- strsplit(read, pattern, perl = TRUE)[[1]]
  read <- read[!grepl("^\\s+$", read)]
  read <- data.frame(info = read)
  read <- data.frame(
    module = str_extract(read$info, "^[^_]+"),
    wave = str_extract(read$info, "_[^.]+\\."),
    code = str_extract(read$info, "(?<=\\.)[^A-Z]+"),
    description = str_extract(read$info, "(?=[A-Z])(.*?(?=\\bnumber\\b|\\bchoice\\b|\\bmultichoice\\b|\\bstring\\b|\\btext\\b|\\btime\\b))"),
    type = str_extract(read$info, "\\b(number|choice|multichoice|string|text|time)\\b"),
    text = str_extract(read$info, "(?<=\\b(number|choice|multichoice|string|text|time)\\b\\s)([A-Z].*)")
  )
  read$n_options <- as.integer(n_options)
  return(read)
}

#Loop to apply function to diferent webs and nodes.
datas_frames <- list()
for (i in 1:length(links)){
  df <- paste0('df_',i)
  datas_frames[[i]] <- reading(link = links[i],node = nodes[i],n_options = n_options[[i]])
} 

#This dataframe contains information of all question considered
merged_df <- do.call(rbind, datas_frames)

#Join considering just questions apply in the individual survey (some modules also considered questions that belongs to other surveys of the studio)
df_recurrent <- merge(merged_df, variables_w1_w12, by = "code")
df_recurrent <- df_recurrent %>% 
  arrange(-occurrences)

#################### III. Text analytics #######################

#Libraries
library(tm)
library(topicmodels)
library(tidytext)
library(udpipe) #udpipe package is a Natural Language Processing (NLP) toolkit in R for tokenization
library(wordcloud)
library(data.table)

#For those question without text - use the text given in the description column
df_recurrent <- df_recurrent %>% 
  mutate(text=ifelse(is.na(text),description,text))

##### A. Variables of LENGTH: number of characters, words and sentences

df_recurrent <- df_recurrent %>% 
  mutate(characters=nchar(text),
         words = str_count(text, "\\w+"),
         sentences = str_count(text, "[.!?]+"))

##### B. Transveral Domains

#Download nlp model
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

#Tokenization and reading of text data
text_df <- udpipe_annotate(ud_model, x = df_recurrent$text)
text_df <- as.data.frame(text_df) #Upos and feats are the columns most useful for posterior analysis (not just for domains)

#Tokenization and reading of description data
description_df <- udpipe_annotate(ud_model, x = df_recurrent$description)
description_df <- as.data.frame(description_df ) #Upos and feats are the columns most useful for posterior analysis (not just for domains)

#Definition of transversal domain: generation of a cloud of words with the most common nouns and verb of the description column

nouns_verbs <- description_df %>% 
  filter(upos %in% c('VERB','NOUN')) %>% 
  select(token) %>% 
  unlist() 

domain <- df_recurrent %>%
  unnest_tokens(output = "word", input = text, token = "words") %>%
  filter(!word %in% stopwords::stopwords("en") & !grepl("^\\d+$", word), word %in%  nouns_verbs) %>%
  count(word, sort = TRUE) %>% 
  filter(n>1)

wordcloud(domain$word,domain$n,random.order = FALSE)

#Once observed and analyzed, define the key words

time <- c('week','hours','time','overtime','months','weeks','year','hour','day','period','weekly','monthly')  
pattern1 <- paste0("\\b(", paste(time, collapse = "|"), ")\\b")
earns_expenses <- c('tax','deductions','deduction','deductions','provide','loan','loss','pay','paid','amount','income','business','working','profit','amount','dividends','Pay')
pattern2 <- paste0("\\b(", paste(earns_expenses, collapse = "|"), ")\\b")
family <- c('child','father','mother','cousin','sister','brother','childcare','parents','Child','family','marriage','marital','Father','Mother','children','kids','sisters','brothers','daughter','daughters')
pattern3 <- paste0("\\b(", paste(family, collapse = "|"), ")\\b")
job <- c('work','job','working','employment','Work','Employment','Job','worked','employee','Work')
pattern4 <- paste0("\\b(", paste(job, collapse = "|"), ")\\b")


#Apply: if the description column contains the keyword, then the transversal domain is present

df_recurrent <- df_recurrent %>%
  mutate(domain_time = ifelse(str_detect(description, pattern1), 1, 0),
         domain_earns_expenses = ifelse(str_detect(description, pattern2), 1, 0),
         domain_family=ifelse(str_detect(description,pattern3),1,0),
         domain_job=ifelse(str_detect(description,pattern4),1,0))

#### C. Reference Period

#Definition of reference period: generation of a cloud of words with the most common auxiliars of the text column

auxiliars <- text_df %>% 
  filter(upos %in% c('AUX')) %>% 
  select(token) %>% 
  unlist() 

reference_period <- df_recurrent %>%
  unnest_tokens(output = "word", input = text, token = "words") %>%
  filter(!grepl("^\\d+$", word), word %in%  auxiliars) %>%
  count(word, sort = TRUE)

wordcloud(reference_period$word,reference_period$n,random.order = FALSE)

#Once observed and analyzed, define the key words

past_keywords <- c("did",'Did', "were",'been','was','had','last','In the past','Have you recently','lived','started')
future_keywords <- c("will", "shall",'could choose')

#Apply: if the text column contains the keyword, then the specific time reference is assigned

df_recurrent <- df_recurrent %>%
  mutate(time_reference = if_else(is.na(text), NA_character_,
                                  if_else(grepl(paste(past_keywords, collapse = "|"), text),
                                          "past", 
                                          if_else(grepl(paste(future_keywords, collapse = "|"), text),"future",
                                                  'present'))))

#### D. Number of nouns and verbs

#Definition of nouns and assignment of number of nouns

nouns <- text_df %>% 
  filter(upos %in% c('NOUN')) %>% 
  select(token,sentence)

dt <- as.data.table(text_df)
dt[, id := .GRP, by = doc_id]
text_df_nouns <- as.data.frame(dt)

df_recurrent <- df_recurrent %>%
  mutate(id = row_number())

text_df_nouns <- merge(text_df_nouns, df_recurrent, by = "id")

n_nouns <- text_df_nouns %>% 
  group_by(id) %>% 
  mutate(n_nouns=sum(upos=='NOUN')) %>% 
  select(code,n_nouns)

n_nouns$id <- NULL

n_nouns <- distinct(n_nouns, code, n_nouns)

df_recurrent <- merge(df_recurrent, n_nouns, by = 'code',all = FALSE)
df_recurrent$id.x <- NULL
df_recurrent$id.y <- NULL

#Definition of nouns and assignment of number of nouns - We have to be more cautious with verbs because a simple list of words will confuse an auxiliar with a verb (have). The context is important
##Distinguish auxiliars from actual verbs (For example, 'Have' is an axuliar sometimes (not a verb))

verbs <- text_df %>% 
  filter(upos %in% c('VERB')) %>% 
  select(token,sentence) %>% 
  unlist()

verb_words <- df_recurrent %>%
  unnest_tokens(output = "word", input = text, token = "words") %>%
  filter(!grepl("^\\d+$", word), word %in%  verbs) %>%
  count(word, sort = TRUE)
wordcloud(verb_words$word,verb_words$n,random.order = FALSE) 

verbs <- text_df %>% 
  filter(upos %in% c('VERB')) %>% 
  select(token,sentence)

dt <- as.data.table(text_df)
dt[, id := .GRP, by = doc_id]
text_df_verbs <- as.data.frame(dt)

text_df_verbs <- merge(text_df_verbs, df_recurrent, by = "id")

n_verbs <- text_df_verbs %>% 
  group_by(id) %>% 
  mutate(n_verbs=sum(upos=='VERB')) %>% 
  select(code,n_verbs)

n_verbs$id <- NULL

n_verbs <- distinct(n_verbs, code, n_verbs)

df_recurrent <- merge(df_recurrent, n_verbs, by = 'code',all = FALSE)
df_recurrent$id.x <- NULL
df_recurrent$id.y <- NULL

#### E. Number of abstract nouns

#Definition of abstract nouns: generation of a cloud of words with the most common nouns of the text column

total_nouns <- text_df %>% 
  filter(upos %in% c('NOUN')) %>% 
  count(token, sort = TRUE) %>% 
  filter(n>1)

wordcloud(total_nouns$token,total_nouns$n,random.order = FALSE)

#Once observed and analyzed, define the key words

abstract_words <- c('time','work','job','activities','questions','amount','health','household','accounts',
                    'living','problems','help','things','way','answer','interest','kind','part','question',
                    'religion','rate','marriage','life','relationship','politics','lives')  

#Apply: if the text column contains the keyword, then the noun is counted as abstract

count_word_occurrences <- function(text, words) {
  word_count <- sapply(words, function(word) str_count(text, word))
  total_occurrences <- sum(word_count)
  return(total_occurrences)
}

# Count occurrences of selected abstract nouns in the text column

df_recurrent$abstractnouns <- sapply(df_recurrent$text, count_word_occurrences, words = abstract_words)

#### F. Usage of Wh word

wh_words <- data.frame(
  category = c("open"),
  words = I(list(
    c("who","which",'what','when','where','how','why',
      "Who","Which",'What','When','Where','How','Why')
  )))

wh_words <- wh_words %>% 
  unnest(words)

df_recurrent <- df_recurrent %>%
  mutate(wh_use = if_else(is.na(text), NA_character_,
                          if_else(grepl(paste(wh_words$words, collapse = "|"), text),
                                  "use_wh", "not_usewh")))

#### G. Concept

#Definition of conceptss: generation of a cloud of words with the most common auxiliars of the text column

concepts_cases <- text_df %>% 
  filter(upos %in% c('AUX')) %>%  #Determine which is the best possible upos
  select(token) %>% 
  unlist() #Conver to list

concept_df <- df_recurrent %>%
  unnest_tokens(output = "word", input = text, token = "words") %>%
  filter(!grepl("^\\d+$", word), word %in%  concepts_cases) %>%
  count(word, sort = TRUE)


wordcloud(concept_df$word,concept_df$n,random.order = FALSE)

#Once observed and analyzed, define the key words

tidy_data <- df_recurrent %>%
  unnest_tokens(word, text,drop = FALSE)

categories <- data.frame(
  category = c("opinions"),
  words = I(list(
    c("would you say", "Would you say", "how do you think",'describes your','if ',
      'would you like','Have you recently','feel','The income of your household.',
      'The amount of leisure time you have','Your life overall.','do you expect','unhappy',
      'recently felt','Have you recently been feeling','How difficult','difficulty','hard','easier','easy' )
  )))

#Apply: if the text column contains the keyword, then the question is classify in one concept

categories <- categories %>% 
  unnest(words)

df_recurrent <- df_recurrent %>%
  mutate(concept = if_else(is.na(text), NA_character_,
                           if_else(grepl(paste(categories$words, collapse = "|"), text),
                                   "opinion/perception", "behavior/practice")))

#### H. Direct and indirect requests

#Manual analysis. Definition of key phrases and application to dataframe

requests <- data.frame(
  category = c("open"),
  words = I(list(
    c("do you think that",'could you tell me','please tell me','do you agree',
      'does this extra work','Now thinking about','Looking ahead','Would you say',
      'Can I just check','Thinking about your','Even though you','On a scale of',
      'Leaving aside','After paying','If a job','Although you','If you could','The last time',
      'Your take home','The next questions','Here are some questions','you may not want',
      'Can IIwe just check ', 'Thank you for your help','I would like to ask','Before tax and other deductions,',
      'Last time we interviewed you','May I just check,','Just to check','If possible','Thinking','As you know','Now a few')
  )))

requests <- requests %>% 
  unnest(words)

df_recurrent <- df_recurrent %>%
  mutate(request = if_else(is.na(text), NA_character_,
                           if_else(grepl(paste(requests$words, collapse = "|"), text),
                                   "indirect request", "direct request")))


####  Cleaning of generated database before clustering

df_recurrent<- df_recurrent %>%
  mutate(type=if_else(!is.na(n_options),'choice',
                      if_else(type=='time','number',type))) %>%
  rename(subdomain=module) %>%
  select(code,wave,description,text,type,n_options,domain_time,domain_earns_expenses,domain_family,domain_job,subdomain,
         characters,words,sentences,time_reference,n_verbs,n_nouns,abstractnouns,wh_use,concept,request,occurrences) %>% 
  arrange(wave) %>% 
  select(-wave)

df_recurrent <- distinct(df_recurrent, code, .keep_all = TRUE) #There are three cases that are almost duplicated because they are in two different modules. In wave first they were in a different one. We keep their classification in the first wave

#################### IV. Clustering #######################

#Libraries
library(stats)
#install.packages("factoextra")
library(factoextra)
#install.packages('fpc')
library(fpc) #For sillohuete
#install.packages('mclust')
library(mclust)

#### A. Adequation of the dataset to unsupervised learning techniques requirements (encodings, standarization, etc.)

ulearning_df <- df_recurrent %>%
  select(-c(description,occurrences,text)) %>% 
  mutate(type=if_else(type=="choice", 1, 0),
         wh_use=ifelse(wh_use=='use_wh',1,0),
         concept=ifelse(concept=='behavior/practice',1,0),
         request=ifelse(request=='direct request',1,0),
         n_options=ifelse(is.na(n_options),0,n_options),
         subdomain = as.factor(subdomain)) 

ulearning_df <- spread(ulearning_df, key = subdomain, value = subdomain, sep = "_")
ulearning_df <- spread(ulearning_df, key = time_reference, value = time_reference, sep = "_")
ulearning_df <- ulearning_df %>%
  mutate(across(starts_with("subdomain"), ~ifelse(is.na(.), 0, 1)),
         across(starts_with('time_reference'),~ifelse(is.na(.),0,1))) 

row.names(ulearning_df) <- ulearning_df$code # Set the code as the id of each row

ulearning_df <- ulearning_df %>%  # Now that the code is the name of each row, remove duplicate code column
  select(-code)

ulearning_df <- scale(ulearning_df) #Scale (standarize data)

### B. K Means algorithm

# Create a data frame with the silhouette scores for different numbers of clusters

k_values <- 2:100  # Set the range of cluster numbers to explore
silhouette_data_km <- data.frame(K = k_values, Silhouette_Score = sapply(k_values, function(k) {
  km <- kmeans(ulearning_df, centers = k, nstart = 10)
  cluster.stats(dist(ulearning_df), km$cluster)$avg.silwidth
}))

# Create (and save) the elbow plot for the clusters in the range

kmelbow_plot_sillohuete <- ggplot(silhouette_data_km, aes(x = K, y = Silhouette_Score)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Clusters (K)", y = "Silhouette Score") +
  geom_vline(xintercept = 42, linetype = "dashed", color = "red",size=0.8)+
  geom_vline(xintercept = 25,linetype='dotted', color='blue',size=1,alpha=0.5)+
  geom_vline(xintercept = 43, linetype='dotted', color='blue',size=1,alpha=0.5)+
  theme_bw()

ggsave("R&A_elbowplotsillohette_km_plot16.jpg", plot = kmelbow_plot_sillohuete, width = 6, height = 4, dpi = 300)

# Once identified the ideal number of clusters, recreate the algorithm with that number. Also save the assigned cluster of each question as a new variable

k <- 42 #Ideal clusters
kmeans_model <- kmeans(ulearning_df, centers = k)

cluster_assignmentsKM <- kmeans_model$cluster # Get the cluster assignment for each data point

data_with_clusters_km <- cbind(ulearning_df, Cluster = cluster_assignmentsKM ) # Add the cluster assignments as a new variable to the data frame

data_with_clusters_km<- data.frame(data_with_clusters_km)

data_with_clusters_km <- data_with_clusters_km %>%
  rownames_to_column(var = "code") %>% 
  select(code,Cluster) %>% 
  rename(cluster_km=Cluster)

### C. GMM algorithm

# Create a data frame with the silhouette scores for different numbers of process (clusters)

calculate_silhouette <- function(k, data) { # Function to calculate the silhouette score for GMM with a given number of clusters
  model <- Mclust(data, G = k)
  cluster_assignments <- unname(model$classification)
  ss <- cluster.stats(dist(data), cluster_assignments)$avg.silwidth
  ss  # Return the mean silhouette width
}

k_values <- 2:100  # Set the range of process (clusters) numbers to explore
silhouette_data <- data.frame(K = k_values, Silhouette_Score = sapply(k_values, calculate_silhouette, data = ulearning_df))

# Create (and save) the elbow plot for the clusters in the range

gmm_elbow_plot <- ggplot(silhouette_data, aes(x = K, y = Silhouette_Score)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Clusters (K)", y = "Average Silhouette Score") +
  geom_vline(xintercept = 42,linetype = "dashed",color='red',size=0.8)+
  geom_vline(xintercept = 25, linetype='dotted', color='blue',size=1,alpha=0.5)+
  geom_vline(xintercept = 43, linetype='dotted', color='blue',size=1,alpha=0.5)+
  theme_bw()

ggsave("R&A_elbowplotsillohette_km_plot17.jpg", plot = gmm_elbow_plot, width = 6, height = 4, dpi = 300)

# Once identified the ideal number of clusters, recreate the algorithm with that number. Also save the assigned cluster of each question as a new variable

k <- 42 #Ideal number of process (clusters)
gmm_model <- Mclust(ulearning_df, G = k)

cluster_assignmentsGMM <- unname(gmm_model$classification) # Get the cluster assignment for each data point

data_with_clusters_gm <- cbind(ulearning_df, Cluster = cluster_assignmentsGMM) # Add the cluster assignments as a new variable to the data frame

data_with_clusters_gm <- data.frame(data_with_clusters_gm)

data_with_clusters_gm <- data_with_clusters_gm %>%
  rownames_to_column(var = "code") %>% 
  select(code,Cluster) %>% 
  rename(cluster_gmm=Cluster)

### D. Save both clusterization in the original dataframe constructed

df_recurrent <- merge(df_recurrent,data_with_clusters_km,by='code')
df_recurrent <- merge(df_recurrent,data_with_clusters_gm,by='code')

#################### V. Exploratory Data Analysis #######################

#This section contains the code to generate the univariate and bivariate plots

# 1. Domain

#Preprocessing to avoid one hot encoding plot

df_recurrent <- df_recurrent %>% 
  mutate(domain=if_else(domain_time == 1 & domain_earns_expenses == 1 & domain_family == 1 & domain_job==1,'Time,Earns/Expenses,Family & Job',
                        if_else(domain_time == 1 & domain_earns_expenses == 1 & domain_family == 1, 'Time,Earns/Expenses & Family',
                                if_else(domain_time == 1 & domain_earns_expenses == 1 & domain_job==1, 'Time,Earns/Expenses & Job',
                                        if_else(domain_time == 1 & domain_family == 1 & domain_job==1,'Time,Job & Family',
                                                if_else(domain_earns_expenses == 1 & domain_family == 1 & domain_job==1,'Earns/Expenses,Job & Family',
                                                        if_else(domain_time == 1 & domain_earns_expenses == 1,'Time & Earns/Expenses',
                                                                if_else(domain_time == 1 & domain_family == 1,'Time & Famiily',
                                                                        if_else(domain_time == 1 & domain_job == 1,'Time & Job',
                                                                                if_else(domain_earns_expenses == 1 & domain_family,'Earns/Expenses & Family',
                                                                                        if_else(domain_earns_expenses == 1 & domain_job == 1,'Earns/Expenses & Job',
                                                                                                if_else(domain_job == 1 & domain_family == 1,'Job & Family',
                                                                                                        if_else(domain_time == 1,'Time',
                                                                                                                if_else(domain_earns_expenses == 1,'Earns/Expenses',
                                                                                                                        if_else(domain_family==1,'Family',
                                                                                                                                if_else(domain_job==1,'Job','No domain'))))))))))))))))


domain_plot <- df_recurrent%>%
  group_by(domain) %>%
  summarise(n=n()) %>%
  ggplot(aes(y=reorder(domain,n),x=n)) +
  geom_bar(stat='identity',fill="black")+
  theme_bw()+
  labs(x=' ',y='Domain')

domain_plot

# 2. Subdomain

subdomain_plot <-df_recurrent %>%
  mutate(frequency=ifelse(occurrences>=6,'Recurrent (asked in 6 or more waves)','No recurrent (asked in less 6 waves)')) %>% 
  group_by(subdomain) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=reorder(subdomain,n),fill=frequency))+
  geom_bar()+
  scale_fill_manual(values = c("Recurrent (asked in 6 or more waves)" = "#2A2A28","No recurrent (asked in less 6 waves)" = "#B2AFAF"))+
  coord_flip()+
  scale_x_discrete(labels = c("employees" = "Employees",
                              "caring" = "Caring",
                              "benefits" = "Unearned Income and State Benefits",
                              "demographics" = "Demographics",
                              "childcare"="Childcare",
                              "householdfinances"="Household Finances",
                              "disability"="Disability",
                              "secondjobs"="Second Jobs",
                              "currentemployment"="Current Employment",
                              "nonemployment"="Non employment",
                              "jobsatisfaction"="Job Satisfaction",
                              "selfemployment"="Self-employment",
                              "scaghq"="General Health Questionarie",
                              "scasatisfaction"="Satisfaction",
                              'health'='Health and disability',
                              'familynetworks'='Family Networks',
                              'initialconditions'='Initial Conditions',
                              'hhgrid'='Household Grid',
                              'familybackground'='Family Background',
                              'environmentalbehaviour'='Environmental Behaviour',
                              'parentsandchildren'='Parents and children',
                              'scasf12'='Self-Completion',
                              'migrationhistory'='Migration History',
                              'partnershiphistory'='Partnership history',
                              'language'='Language',
                              'religion'='Religion',
                              'politics'='Politics',
                              'fertilityhistory'='Fertility History',
                              'ethnicityandnationalidentity'='Ethnicity and National Identity',
                              'discrimination'='Discrimination',
                              'consents'='Consents',
                              'stablecontact'='Stable contact',
                              'contactdetails'='Contact details',
                              'employmentstatushistory'='Employment Status History')) +
  theme_bw()+
  labs(x='Subdomain (Modules)',y='N questions',fill='')+
  theme(legend.position = c(0.65, 0.15),   # Adjust the position (x, y)
        legend.text = element_text(size = 8))

subdomain_plot

# 3. Concept

concept_plot <- df_recurrent%>%
  group_by(concept) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(concept,-n),y=n)) +
  geom_bar(stat='identity',fill="black")+
  theme_bw()+
  labs(y=' ',x='Concept')

concept_plot

# 4. Request

request_plot <- df_recurrent%>%
  group_by(request) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(request,-n),y=n)) +
  geom_bar(stat='identity',fill="black")+
  scale_x_discrete(labels = c("direct request" = "Direct request",
                              "indirect request" = "Indirect request"))+
  theme_bw()+
  labs(y=' ',x='Type of request')

request_plot

# 5. Time reference

time_plot <- df_recurrent%>%
  group_by(time_reference) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(time_reference,-n),y=n)) +
  geom_bar(stat='identity',fill="black")+
  scale_x_discrete(labels = c("present" = "Present",
                              "past" = "Past",
                              'future'='Future'))+
  theme_bw()+
  labs(y=' ',x='Time reference')

time_plot

# 6. Type

type_plot <- df_recurrent%>%
  group_by(type) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(type,-n),y=n)) +
  geom_bar(stat='identity',fill="black")+
  scale_x_discrete(labels = c("choice" = "Choice",
                              "number" = "Number",
                              'time'='Time'))+
  theme_bw()+
  labs(y=' ',x='Type')

type_plot

# 7. Number of options

n_options_plot <- df_recurrent %>% 
  drop_na(type) %>% 
  ggplot(aes(x=n_options))+
  geom_density(fill='black')+
  scale_x_continuous(breaks = seq(2, 30, 2)) +
  scale_y_continuous(labels = scales::percent_format())+
  theme_bw()+
  labs(x='Number of options',y='') #Universe is questions that are for choice selection

n_options_plot

# 8. Wh use

wh_plot <- df_recurrent%>%
  group_by(wh_use) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(wh_use,-n),y=n)) +
  geom_bar(stat='identity',fill="black")+
  scale_x_discrete(labels = c("not_usewh" = "Do not use Wh words",
                              "use_wh" = "Use Wh words"))+
  theme_bw()+
  labs(y=' ',x='Wh words') 

wh_plot

# 9. Characters

characters_plot <- df_recurrent %>%
  drop_na(characters) %>%
  ggplot(aes(y = characters, x = 0)) +
  geom_boxplot(width = 0.5) +
  xlim(-1,1) +
  ylim(0, 300)+
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = 'Number of characters',x='') 

characters_plot

# 10. Words

words_plot <- df_recurrent %>%
  drop_na(words) %>%
  ggplot(aes(y = words, x = 0)) +
  geom_boxplot(width = 0.5) +
  xlim(-1,1) +
  ylim(0, 65)+
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = 'Number of words',x='')

words_plot

# 11. Sentences

sentences_plot <- df_recurrent%>%
  mutate(sentences=ifelse(sentences==0,1,sentences)) %>% 
  drop_na(sentences) %>% 
  group_by(sentences) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(sentences,-n),y=n)) +
  geom_bar(stat='identity',fill="black")+
  theme_bw()+
  labs(y=' ',x='N of sentences')

sentences_plot

# 12. Nouns

nnouns_plot <- df_recurrent %>%
  drop_na(n_nouns) %>%
  ggplot(aes(y = n_nouns, x = 0)) +
  geom_boxplot(width = 0.5)+
  xlim(-1,1)+
  ylim(0, 20)+
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = 'Number of nouns',x='')

nnouns_plot

# 13. Verbs

nverbs_plot <-df_recurrent %>%
  drop_na(n_verbs) %>%
  ggplot(aes(y = n_verbs, x = 0)) +
  geom_boxplot(width = 0.5)+
  xlim(-1,1)+
  ylim(0, 15)+
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = 'Number of verbs',x='') +
  scale_fill_manual(values=c("#2A2A28", "#B2AFAF", "#044480"),labels=c("past" = "Past", "present" = "Present", "future" = "Future"))

nverbs_plot

# 14. Abstract nouns

abstractnouns_plot <- df_recurrent %>% 
  drop_na(abstractnouns) %>% 
  ggplot(aes(y=abstractnouns,x=0))+
  geom_boxplot(width = 0.5)+
  xlim(-1,1)+
  ylim(0, 15)+
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = 'Number of abstract nouns',x='')

abstractnouns_plot

# 15. Subdomain and concept

subdomainconcept_plot <- df_recurrent %>% 
  drop_na(concept) %>% 
  group_by(subdomain,concept) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=reorder(subdomain,n),fill= as.factor(concept),y=n))+
  geom_bar(stat='identity',position = 'identity')+
  coord_flip()+
  scale_x_discrete(labels = c("employees" = "Employees",
                              "caring" = "Caring",
                              "benefits" = "Unearned Income and State Benefits",
                              "demographics" = "Demographics",
                              "childcare"="Childcare",
                              "householdfinances"="Household Finances",
                              "disability"="Disability",
                              "secondjobs"="Second Jobs",
                              "currentemployment"="Current Employment",
                              "nonemployment"="Non employment",
                              "jobsatisfaction"="Job Satisfaction",
                              "selfemployment"="Self-employment",
                              "scaghq"="General Health Questionarie",
                              "scasatisfaction"="Satisfaction",
                              'health'='Health and disability',
                              'familynetworks'='Family Networks',
                              'initialconditions'='Initial Conditions',
                              'hhgrid'='Household Grid',
                              'familybackground'='Family Background',
                              'environmentalbehaviour'='Environmental Behaviour',
                              'parentsandchildren'='Parents and children',
                              'scasf12'='Self-Completion',
                              'migrationhistory'='Migration History',
                              'partnershiphistory'='Partnership history',
                              'language'='Language',
                              'religion'='Religion',
                              'politics'='Politics',
                              'fertilityhistory'='Fertility History',
                              'ethnicityandnationalidentity'='Ethnicity and National Identity',
                              'discrimination'='Discrimination',
                              'consents'='Consents',
                              'stablecontact'='Stable contact',
                              'contactdetails'='Contact details',
                              'employmentstatushistory'='Employment Status History')) +
  theme_bw()+
  labs(x='',y='',fill='')+
  theme(legend.title = element_text(size = 12), legend.position = "top")+
  scale_fill_manual(values=c("#2A2A28", "#B2AFAF"),labels=c("behavior/practice" = "Behavior/Practice", "opinion/perception" = "Opinion/Perception"))

subdomainconcept_plot

# 16. Subdomain and Time reference

df_recurrent$time_reference <-  factor(df_recurrent$time_reference, levels = c("past", "present", "future"))

subdomaintimereference_plot <- df_recurrent %>% 
  drop_na(time_reference) %>% 
  group_by(subdomain) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  drop_na(time_reference) %>% 
  ggplot(aes(x=reorder(subdomain,n),fill=time_reference))+
  geom_bar()+
  coord_flip()+
  scale_x_discrete(labels = c("employees" = "Employees",
                              "caring" = "Caring",
                              "benefits" = "Unearned Income and State Benefits",
                              "demographics" = "Demographics",
                              "childcare"="Childcare",
                              "householdfinances"="Household Finances",
                              "disability"="Disability",
                              "secondjobs"="Second Jobs",
                              "currentemployment"="Current Employment",
                              "nonemployment"="Non employment",
                              "jobsatisfaction"="Job Satisfaction",
                              "selfemployment"="Self-employment",
                              "scaghq"="General Health Questionarie",
                              "scasatisfaction"="Satisfaction",
                              'health'='Health and disability',
                              'familynetworks'='Family Networks',
                              'initialconditions'='Initial Conditions',
                              'hhgrid'='Household Grid',
                              'familybackground'='Family Background',
                              'environmentalbehaviour'='Environmental Behaviour',
                              'parentsandchildren'='Parents and children',
                              'scasf12'='Self-Completion',
                              'migrationhistory'='Migration History',
                              'partnershiphistory'='Partnership history',
                              'language'='Language',
                              'religion'='Religion',
                              'politics'='Politics',
                              'fertilityhistory'='Fertility History',
                              'ethnicityandnationalidentity'='Ethnicity and National Identity',
                              'discrimination'='Discrimination',
                              'consents'='Consents',
                              'stablecontact'='Stable contact',
                              'contactdetails'='Contact details',
                              'employmentstatushistory'='Employment Status History'))+
  theme_bw()+
  labs(x='',y='',fill='')+
  theme(legend.title = element_text(size = 12), legend.position = "top")+
  scale_fill_manual(values=c("#2A2A28", "#B2AFAF", "#ECC1B7"),labels=c("past" = "Past", "present" = "Present", "future" = "Future"))

subdomaintimereference_plot

# 17. Subdomain and Options

subdomainlenght_plot <- df_recurrent %>% 
  drop_na(n_options) %>% 
  group_by(subdomain) %>% 
  summarise(median=mean(n_options)) %>% 
  arrange(-median) %>% 
  ggplot(aes(x=reorder(subdomain,median),y=median))+
  geom_bar(stat='identity',fill='black')+
  coord_flip()+
  theme_bw()+
  labs(x=' ',y='Average number of options')+
  scale_x_discrete(labels = c("employees" = "Employees",
                              "caring" = "Caring",
                              "benefits" = "Unearned Income and State Benefits",
                              "demographics" = "Demographics",
                              "childcare"="Childcare",
                              "householdfinances"="Household Finances",
                              "disability"="Disability",
                              "secondjobs"="Second Jobs",
                              "currentemployment"="Current Employment",
                              "nonemployment"="Non employment",
                              "jobsatisfaction"="Job Satisfaction",
                              "selfemployment"="Self-employment",
                              "scaghq"="General Health Questionarie",
                              "scasatisfaction"="Satisfaction",
                              'health'='Health and disability',
                              'familynetworks'='Family Networks',
                              'initialconditions'='Initial Conditions',
                              'hhgrid'='Household Grid',
                              'familybackground'='Family Background',
                              'environmentalbehaviour'='Environmental Behaviour',
                              'parentsandchildren'='Parents and children',
                              'scasf12'='Self-Completion',
                              'migrationhistory'='Migration History',
                              'partnershiphistory'='Partnership history',
                              'language'='Language',
                              'religion'='Religion',
                              'politics'='Politics',
                              'fertilityhistory'='Fertility History',
                              'ethnicityandnationalidentity'='Ethnicity and National Identity',
                              'discrimination'='Discrimination',
                              'consents'='Consents',
                              'stablecontact'='Stable contact',
                              'contactdetails'='Contact details',
                              'employmentstatushistory'='Employment Status History'))

subdomainlenght_plot

# 18. Subdomain and lenght (characters)

subdomainlength2_plot <- df_recurrent %>% 
  drop_na(text) %>% 
  group_by(subdomain) %>% 
  summarise(n=n(),length=mean(characters)) %>% 
  ggplot(aes(x=reorder(subdomain,n),y=length))+
  geom_bar(stat='identity',fill='black')+
  geom_text(aes(label = paste('N :',n)), vjust = 0.3,position = position_stack(vjust = 0.5), color = 'white',size=2) +
  coord_flip()+
  theme_bw()+
  labs(x='',y='Average length (characters)')+
  scale_x_discrete(labels = c("employees" = "Employees",
                              "caring" = "Caring",
                              "benefits" = "Unearned Income and State Benefits",
                              "demographics" = "Demographics",
                              "childcare"="Childcare",
                              "householdfinances"="Household Finances",
                              "disability"="Disability",
                              "secondjobs"="Second Jobs",
                              "currentemployment"="Current Employment",
                              "nonemployment"="Non employment",
                              "jobsatisfaction"="Job Satisfaction",
                              "selfemployment"="Self-employment",
                              "scaghq"="General Health Questionarie",
                              "scasatisfaction"="Satisfaction",
                              'health'='Health and disability',
                              'familynetworks'='Family Networks',
                              'initialconditions'='Initial Conditions',
                              'hhgrid'='Household Grid',
                              'familybackground'='Family Background',
                              'environmentalbehaviour'='Environmental Behaviour',
                              'parentsandchildren'='Parents and children',
                              'scasf12'='Self-Completion',
                              'migrationhistory'='Migration History',
                              'partnershiphistory'='Partnership history',
                              'language'='Language',
                              'religion'='Religion',
                              'politics'='Politics',
                              'fertilityhistory'='Fertility History',
                              'ethnicityandnationalidentity'='Ethnicity and National Identity',
                              'discrimination'='Discrimination',
                              'consents'='Consents',
                              'stablecontact'='Stable contact',
                              'contactdetails'='Contact details',
                              'employmentstatushistory'='Employment Status History'))

subdomainlength2_plot

# 19. Clustering KMeans (Distribution of questions after ideal clusters 42)

distrbution_clusters_kmeans<- df_recurrent %>% 
  group_by(cluster_km) %>% 
  mutate(n=n()) %>% 
  ggplot(aes(x=reorder(cluster_km,n)))+
  geom_bar(fill='black')+
  coord_flip()+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 40, 5),limits = c(0,40))+
  labs(y='Number of questions',x='Cluster KMeans')

distrbution_clusters_kmeans

# 20. Clustering GMM (Distribution of questions after ideal clusters 42)

distribution_clusters_gmm <- df_recurrent %>% 
  group_by(cluster_gmm) %>% 
  mutate(n=n()) %>% 
  ggplot(aes(x=reorder(cluster_gmm,n)))+
  geom_bar(fill='black')+
  coord_flip()+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 40, 5),limits = c(0,40))+
  labs(y='Number of questions',x='Cluster GMM')

distribution_clusters_gmm

# 21. Distribution of questions per edition (wave) of Understanding Society (2009 - 2020)

df <- data.frame(
  Wave = c("Wave1", "Wave2","Wave3","Wave4","Wave5","Wave6","Wave7","Wave8","Wave9","Wave10","Wave11","Wave12"),
  Variables = c(1401, 1647, 3058,2109,2610,2062,2826,2152,3133,2461,3228,2561),
  Year=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
)

variables_df <- df %>% 
  ggplot(aes(x=reorder(Wave,Year),y=Variables))+
  geom_point()+
  geom_line(group=1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x='',y='N of Variables')

variables_df 

# 22. Number of occurences of gathered questions across the 12 edtions of the study

ocurrencies_plot <- df_recurrent %>% 
  group_by(occurrences) %>% 
  summarise(n=n()) %>% 
  mutate(occurrences=as.factor(occurrences)) %>% 
  ggplot(aes(x=occurrences,y=n))+
  geom_bar(stat='identity',fill='black')+
  geom_label(aes(label=n),size=3)+
  coord_flip()+
  theme_bw()+
  labs(x='Number of occurrences in waves',y='Number of questions')

ocurrencies_plot 

#################### VI. Final outputs #######################

#Unstandarized metadata 

metadata <- df_recurrent %>% 
  select(-c(domain_earns_expenses,domain_family,domain_time,domain_job)) %>% 
  select(code, description, text, type, domain, subdomain, everything())

export(metadata,'metadata.xlsx')

#Standarized metadata (for unsupervised learning)

metadata_ulearning <- ulearning_df

export(metadata_ulearning ,'metadata_ulearning .xlsx')

