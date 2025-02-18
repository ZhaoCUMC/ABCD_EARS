################################################################################
################################ EARS MANUAL 1 #################################
############################### Cleaned Version ################################
################################   01/27/2024  #################################
### Step 1: Data cleaning
# run ExtractCSV.R to get tt.appus.zip$id (n=495)
# 65 partcipants had more than one APPUSAGE file
# sub.nm <- unique(tt.appus.zip$id)
# then run EARSR5_cleaning_v1.APPUSAGE.R to get individual level daily apps usage data 
# use Google-Play-Scraper 1.2.4 to extract app information via unique app id: 
# https://pypi.org/project/google-play-scraper/
# merge with master.file from master.merge.R
# dt.ears495.master.long.csv was obtained after running EARSR5_cleaning_v1.APPUSAGE.R
### Step 2: Run analysis
################################################################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(lmerTest)
library(table1)
library(flextable)
library(officer)
library(car)
library(broom)
library(broom.mixed)
library(openxlsx)
library(ggpubr)
library(gridExtra)
library(forestplot)

### input data
ears495 <- read.csv("./Output/R5/earsR5/Appusage/dt.ears495.master.long.csv")
## recode demo variables
ears495$interview_age_yrs <- ears495$interview_age/12
ears495$parental_educ_highest_recode <- ifelse(ears495$parental_educ_highest_recode=="< HS Diploma" | ears495$parental_educ_highest_recode=="HS Diploma/GED", 
                                               "High School or Below", ears495$parental_educ_highest_recode)
ears495$parental_educ_highest_recode <- factor(ears495$parental_educ_highest_recode, 
                                               levels = c("Post Graduate Degree", "Bachelor", 
                                                          "Some College", "High School or Below"))
ears495$family_income_recode_l <- ifelse(ears495$family_income_recode_l=="[>=200k]" | ears495$family_income_recode_l=="[100-200k]", 
                                         "[>=100k]", ears495$family_income_recode_l)
ears495$family_income_recode_l <- factor(ears495$family_income_recode_l, 
                                         levels = c("[>=100k]", "[50-100k]", "[<50k]"))
ears495$race_ethnicity_recode <- factor(ears495$race_ethnicity_recode, 
                                        levels = c("White", "Black", "Hispanic", "Other"))
ears495$parent_married_l <- factor(ears495$parent_married_l, labels = c("Not Married", "Married"))
ears495$parent_married_l <- factor(ears495$parent_married_l, levels = c("Married", "Not Married"))
ears495$demo_sex_v2_recode <- factor(ears495$demo_sex_v2_recode, levels = c("Male", "Female"))
# year4
ears495.y4<-ears495[which(ears495$eventname==4),]

# full sample
master.file <- read.csv("./Output/R5/earsR5/Appusage/dt.ears.fullsample.long.csv")
## recode demo variables
master.file$interview_age_yrs <- master.file$interview_age/12
master.file$parental_educ_highest_recode <- ifelse(master.file$parental_educ_highest_recode=="< HS Diploma" | master.file$parental_educ_highest_recode=="HS Diploma/GED", 
                                               "High School or Below", master.file$parental_educ_highest_recode)
master.file$parental_educ_highest_recode <- factor(master.file$parental_educ_highest_recode, 
                                               levels = c("High School or Below", "Some College", 
                                                          "Bachelor", "Post Graduate Degree"))
master.file$family_income_recode_l <- ifelse(master.file$family_income_recode_l=="[>=200k]" | master.file$family_income_recode_l=="[100-200k]", 
                                         "[>=100k]", master.file$family_income_recode_l)
master.file$family_income_recode_l <- factor(master.file$family_income_recode_l, 
                                         levels = c("[<50k]", "[50-100k]", "[>=100k]"))
master.file$race_ethnicity_recode <- factor(master.file$race_ethnicity_recode, 
                                        levels = c("White", "Black", "Hispanic", "Other"))
master.file$parent_married_l <- factor(master.file$parent_married_l, labels = c("Not Married", "Married"))
# year4
master.file.y4 <- master.file[which(master.file$eventname==4),]

# EARS participants --> check how many participants in EARS and the proportion of gender
ears.sub.id <- read.csv("./Output/R5/earsR5/EARS_Total_File.csv") # n=1483
ears.1483.id <- unique(ears.sub.id$subid)
table(master.file.y4$demo_sex_v2_recode[which(master.file.y4$src_subject_id %in% ears.1483.id)])

################################## 1. Table 1 ##################################
pvalue <- function(x, ...) {
  #x <- x[-length(x)]  # Remove "overall" group --> !!!! comment out if don't need overall column
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # lmer: control for family id and site id
    data_df <- data.frame(y = y, g = g)
    
    subject_id <- vector()
    family_id <- vector()
    site_id <- vector()
    
    for (i in 1:length(x)) {
    #for (i in unique(dt$grp.index)) {
      subject_id <- c(subject_id, dt$src_subject_id[which(dt$grp.index==i)])
      family_id <- c(family_id, dt$rel_family_id[which(dt$grp.index==i)])
      site_id <- c(site_id, dt$site_id_l[which(dt$grp.index==i)])
    }
    
    data_df$subject_id <- subject_id
    data_df$family_id <- family_id
    data_df$site_id <- site_id
    
    # Fit a linear mixed-effects model
    formula_str <- paste("y ~ g + (1 | family_id) + (1 | site_id)")
    #formula_str <- paste("y ~ g + (1 | family_id)")
    temp <- lmer(formula_str, data = data_df)
    
    # Extract p-value for the fixed effect
    #p <- anova(temp)$`Pr(>F)`[1]
    p <- Anova(temp, type=3, test.statistic = "F")$`Pr(>F)`[2]
    
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value # won't count missing
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar(0.5, 0.5, 0.5, 0.5)
)

###### table1 configuration ######
behav.nm <- c("MH_cbcl_scr_syn_internal_r", "MH_cbcl_scr_syn_external_r", 
              "MPIQ.total.score", "SMAQ.total.score", "VGAQ.total.score",
              "nihtbx_picvocab_uncorrected", "nihtbx_picture_uncorrected")
dt <- master.file.y4

dt$grp.index <- with(dt, ifelse(src_subject_id %in% ears495.y4$src_subject_id, 1, 2))
# dt$grp.index <- factor(dt$grp.index, labels = c("People in EARS", "People Not in EARS"))
# check if the number is correct
table(dt$grp.index)

label(dt$interview_age_yrs) <- "Age (yrs)"
label(dt$demo_sex_v2_recode) <- "Gender"
label(dt$race_ethnicity_recode) <- "Race Ethnicity"
label(dt$family_income_recode_l) <- "Family Income"
label(dt$parental_educ_highest_recode) <- "Parental Education"
label(dt$parent_married_l) <- "Parental Marital Status"
label(dt$MH_cbcl_scr_syn_internal_r) <- "CBCL Internalizing"
label(dt$MH_cbcl_scr_syn_external_r) <- "CBCL Externalizing"
###### END configuration ######

tbl1 <- table1(~ interview_age_yrs + demo_sex_v2_recode + race_ethnicity_recode
               + family_income_recode_l + parental_educ_highest_recode + parent_married_l
               | factor(grp.index), 
               data=dt, 
               rowlabelhead = "", 
               caption = "Table 1. Demographic Characteristics between Participants with and without EARS Data", 
               digits = 4,
               overall = FALSE,
               extra.col=list(`P-value`=pvalue))
t1flex(tbl1) %>% 
  save_as_docx(path="./Output/R5/earsR5/EARS_manual1/table1.495.4259.final.v2.docx", pr_section = sect_properties)

### Other general information about EARS 
# number of active days
mean(ears495.y4$APPUSAGE_ndays_active_total)
sd(ears495.y4$APPUSAGE_ndays_active_total)
# number of apps used perday
mean(ears495.y4$APPUSAGE_napps_perday_all_mean)
sd(ears495.y4$APPUSAGE_napps_perday_all_mean)
# number of apps during 3-week period
mean(ears495.y4$APPUSAGE_napps_total)
sd(ears495.y4$APPUSAGE_napps_total)
# Did you change how much you used your smartphone after the EARS app was installed?
# 1 = Used it a lot more; 2 = Used it a little more; 3 = Not at all; 4 = Used it a little less; 5 = Used it a lot less
table(ears495.y4$ears_post_app_y)
prop.table(table(ears495.y4$ears_post_app_y))
# If we asked you to have the EARS app on your smartphone longer, would you do it?
# 1 = Yes; 0 = No;	
table(ears495.y4$ears_post_future_y)
prop.table(table(ears495.y4$ears_post_future_y))
# If the app asked you a few questions every day, would that be okay with you?
# 1 = Yes; 0 = No;
table(ears495.y4$ears_post_future_2_y)
prop.table(table(ears495.y4$ears_post_future_2_y))
# What percent of time did someone else use it?
# 1 = Rarely (just briefly); 2 = Some (about a quarter of the time; 3 = About half the time; 4 = More than half the time	
table(ears495.y4$ears_post_over_3wk_2_y)

################################## 2. proportion of each category on total usage  ##################################
###### proportion table of usage for individual items in full sample ######
### column-wise complete
item.seq <- c("total", "socialmedia", "text", "videochat", "browse", "game", "video")
# EARS APPUSAGE
dt.appusage <- master.file.y4[c("src_subject_id", "APPUSAGE_daily.usage_all.exl_mins_mean", "APPUSAGE_daily.usage_SOCIAL.v2_mins_mean",
                                "APPUSAGE_daily.usage_TEXT.v3_mins_mean", "APPUSAGE_daily.usage_VIDEOCHAT.v3_mins_mean",
                                "APPUSAGE_daily.usage_BROWSER.v3_mins_mean", "APPUSAGE_daily.usage_GAME.v2_mins_mean",
                                "APPUSAGE_daily.usage_VIDEO.v2_mins_mean")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "browse", "game", "video"))
dt.appusage[, 2:8] <- dt.appusage[, 2:8]/60
dt.appusage$data.source <- "EARS.APPUSAGE"
# EARS.POST.Y
dt.post.y <- master.file.y4[c("src_subject_id", "ears.post.y_total_typical_hrs_recode", "ears.post.y_social.media_typical_hrs_recode",
                              "ears.post.y_text_typical_hrs_recode", "ears.post.y_video.chat_typical_hrs_recode",
                              "ears.post.y_browse_typical_hrs_recode", "ears.post.y_video.game_wkdy_hrs_recode")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "browse", "game"))
dt.post.y$video <- NA
dt.post.y$data.source <- "EARS.post.y"
# STQ.PHONE --> browse all NA
dt.stq.phone <- master.file.y4[c("src_subject_id", "stq.y_phone_total.typical_typical_hr", "stq.y_phone_social.media_typical_hr",
                                 "stq.y_phone_text_typical_hr", "stq.y_phone_video.chat_typical_hr",
                                 "stq.y_phone_browse_typical_hr", "stq.y_phone_video.game_typical_hr", 
                                 "stq.y_phone_tv.video_typical_hr")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "browse", "game", "video"))
dt.stq.phone$data.source <- "stq.phone"
# STQ
dt.stq <- master.file.y4[c("src_subject_id", "stq_totaltime_typical.y", "stq_socialmedia_typical", 
                           "stq_text_typical", "stq_videochat_typical", "stq_videogame_typical", "stq_tv_typical")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "game", "video"))
dt.stq$browse <- NA
dt.stq$data.source <- "stq"

# rbind four data
dt.table2.full <- rbind(dt.appusage, dt.post.y, dt.stq.phone, dt.stq)
dt.table2.full$data.source <- factor(dt.table2.full$data.source, levels = c("EARS.APPUSAGE", "EARS.post.y", "stq.phone", "stq"))
dt.table2.prop <- dt.table2.full %>%
  rowwise() %>%
  mutate(across(c("socialmedia", "text", "videochat", "browse", "game", "video"), 
                list(prop = ~./total)))
dt.table2.prop[dt.table2.prop=="Inf"] <- NA
for (i in paste0(c("socialmedia", "text", "videochat", "browse", "game", "video"), "_prop")) {
  dt.table2.prop[[paste0(i, "_winsor")]] <- ifelse(dt.table2.prop[[i]]>1, 1, dt.table2.prop[[i]])
}
dt.table2.prop$sel.sub.idx <- apply(dt.table2.prop[grep("_winsor",colnames(dt.table2.prop))],
                                  1, function(x){sum(!is.na(x))})
table(dt.table2.prop$sel.sub.idx, dt.table2.prop$data.source)
dt.table2.prop <- dt.table2.prop[which(dt.table2.prop$sel.sub.idx>=5),]

######
tbl2 <- table1(~ socialmedia_prop_winsor + game_prop_winsor + browse_prop_winsor
               + video_prop_winsor + text_prop_winsor + videochat_prop_winsor
               | data.source, 
               data=dt.table2.prop, 
               #rowlabelhead = "Usage Time (hrs)", 
               caption = "Table 3: Proportion of Time Spent on Each Category of SMA on a Typical Day", 
               overall = FALSE,
               digits = 4,
               #fmt = "%.2f"
)
t1flex(tbl2) %>% 
  save_as_docx(path="./Output/R5/earsR5/EARS_manual1/eTable3_prop.over.total.docx", pr_section = sect_properties)

# get the number with prop>1
apply(dt.table2.prop[which(dt.table2.prop$data.source=="EARS.post.y"), 
                     colnames(dt.table2.prop)[grep("_prop$", colnames(dt.table2.prop))]], 
      2, function(x) length(x[x>1 & !is.na(x)]))
apply(dt.table2.prop[which(dt.table2.prop$data.source=="stq.phone"), 
                     colnames(dt.table2.prop)[grep("_prop$", colnames(dt.table2.prop))]], 
      2, function(x) length(x[x>1 & !is.na(x)]))
apply(dt.table2.prop[which(dt.table2.prop$data.source=="stq"), 
                     colnames(dt.table2.prop)[grep("_prop$", colnames(dt.table2.prop))]], 
      2, function(x) length(x[x>1 & !is.na(x)]))
stq.socialmedia.over1.sub <- unique(dt.table2.prop$src_subject_id[which(dt.table2.prop$data.source=="stq" & dt.table2.prop$socialmedia_prop>1)])
stq.game.over1.sub <- unique(dt.table2.prop$src_subject_id[which(dt.table2.prop$data.source=="stq" & dt.table2.prop$game_prop>1)])
length(intersect(stq.socialmedia.over1.sub, stq.game.over1.sub))
length(intersect(stq.socialmedia.over1.sub, stq.game.over1.sub))/4718

# lmer to get p-values
dt.table2.prop <- merge(dt.table2.prop, master.file.y4[c("src_subject_id", "rel_family_id")])
for (i in c("socialmedia_prop_winsor")) {
  print(i)
  temp_table <- list()
  for (j in c("EARS.post.y", "stq.phone", "stq")) {
    print(j)
    formula <- paste0(i, " ~ data.source + (1|rel_family_id)")
    mod <- lmer(formula, 
                data = dt.table2.prop[which(dt.table2.prop$data.source=="EARS.APPUSAGE" | dt.table2.prop$data.source==j),])
    #summary(mod)
    temp <- tidy(mod, conf.int = TRUE)
    temp_table[[j]] <- temp[grepl(j, temp$term), ]
    temp_table[[j]]$n <- nobs(mod)
  }
  lmer_table <- bind_rows(temp_table, .id = "data.source")
  print(lmer_table)
}


################################## 3. Table 2 ##################################
############################ focus on sample of 495 ############################

#### add correlation analysis here 01/25/2024
cor.test(ears495.y4$APPUSAGE_daily.usage_all.exl_mins_mean/60, ears495.y4$ears.post.y_total_typical_hrs_recode)
# data:  ears495.y4$APPUSAGE_daily.usage_all.exl_mins_mean/60 and ears495.y4$ears.post.y_total_typical_hrs_recode
# t = 9.6821, df = 375, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.3625764 0.5245118
# sample estimates:
#   cor 
# 0.4472013 
cor.test(ears495.y4$APPUSAGE_daily.usage_all.exl_mins_mean/60, ears495.y4$stq_totaltime_typical.y)
# data:  ears495.y4$APPUSAGE_daily.usage_all.exl_mins_mean/60 and ears495.y4$stq_totaltime_typical.y
# t = 4.0119, df = 492, p-value = 6.96e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.09119305 0.26208976
# sample estimates:
#   cor 
# 0.1779831 
cor.test(ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean/60, ears495.y4$ears.post.y_social.media_typical_hrs_recode)
# data:  ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean/60 and ears495.y4$ears.post.y_social.media_typical_hrs_recode
# t = 16.396, df = 394, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.5743449 0.6919653
# sample estimates:
#   cor 
# 0.636846 
cor.test(ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean/60, ears495.y4$stq_socialmedia_typical)
# data:  ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean/60 and ears495.y4$stq_socialmedia_typical
# t = 12.698, df = 492, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.4273124 0.5604630
# sample estimates:
#   cor 
# 0.4968056 
cor.test(ears495.y4$stq.y_phone_social.media_typical_hr, ears495.y4$stq_socialmedia_typical)
# data:  ears495.y4$stq.y_phone_social.media_typical_hr and ears495.y4$stq_socialmedia_typical
# t = 55.544, df = 351, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.9357174 0.9572532
# sample estimates:
#   cor 
# 0.9475503 
cor.test(ears495.y4$APPUSAGE_daily.usage_GAME.v2_mins_mean/60, ears495.y4$ears.post.y_video.game_typical_hrs_recode)
# data:  ears495.y4$APPUSAGE_daily.usage_GAME.v2_mins_mean/60 and ears495.y4$ears.post.y_video.game_typical_hrs_recode
# t = 1.6264, df = 394, p-value = 0.1047
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.01701874  0.17877197
# sample estimates:
#   cor 
# 0.08166445 
cor.test(ears495.y4$APPUSAGE_daily.usage_GAME.v2_mins_mean/60, ears495.y4$stq_videogame_typical)
# data:  ears495.y4$APPUSAGE_daily.usage_GAME.v2_mins_mean/60 and ears495.y4$stq_videogame_typical
# t = 1.5998, df = 492, p-value = 0.1103
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.01638799  0.15914993
# sample estimates:
#   cor 
# 0.071938
cor.test(ears495.y4$stq.y_phone_video.game_typical_hr, ears495.y4$stq_videogame_typical)
# data:  ears495.y4$stq.y_phone_video.game_typical_hr and ears495.y4$stq_videogame_typical
# t = 13.283, df = 424, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.4713294 0.6058709
# sample estimates:
#   cor 
# 0.5420647 

###### calculate proportion of time between smartphone and all devices in stq data ######
## social media apps --> almost all activities were on smartphone
## check this number: among 495 participants, and across full samples
ears495.y4$stq.phone.all.prop_socialmedia <- ears495.y4$stq.y_phone_social.media_typical_hr/ears495.y4$stq_socialmedia_typical
## text
ears495.y4$stq.phone.all.prop_text <- ears495.y4$stq.y_phone_text_typical_hr/ears495.y4$stq_text_typical
## video chat
ears495.y4$stq.phone.all.prop_videochat <- ears495.y4$stq.y_phone_video.chat_typical_hr/ears495.y4$stq_videochat_typical
## video game
ears495.y4$stq.phone.all.prop_videogame <- ears495.y4$stq.y_phone_video.game_typical_hr/ears495.y4$stq_videogame_typical
## video
ears495.y4$stq.phone.all.prop_video <- ears495.y4$stq.y_phone_tv.video_typical_hr/ears495.y4$stq_tv_typical
## total
ears495.y4$stq.phone.all.prop_total <- ears495.y4$stq.y_phone_total.typical_typical_hr/ears495.y4$stq_totaltime_typical.y
######

item.seq <- c("total", "socialmedia", "text", "videochat", "browse", "game", "video")
# EARS APPUSAGE
dt.appusage <- ears495.y4[c("src_subject_id", "APPUSAGE_daily.usage_all.exl_mins_mean", "APPUSAGE_daily.usage_SOCIAL.v2_mins_mean",
                            "APPUSAGE_daily.usage_TEXT.v3_mins_mean", "APPUSAGE_daily.usage_VIDEOCHAT.v3_mins_mean",
                            "APPUSAGE_daily.usage_BROWSER.v3_mins_mean", "APPUSAGE_daily.usage_GAME.v2_mins_mean",
                            "APPUSAGE_daily.usage_VIDEO.v2_mins_mean")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "browse", "game", "video"))
dt.appusage[, 2:8] <- dt.appusage[, 2:8]/60
dt.appusage$data.source <- "EARS.APPUSAGE"
# EARS.POST.Y
dt.post.y <- ears495.y4[c("src_subject_id", "ears.post.y_total_typical_hrs_recode", "ears.post.y_social.media_typical_hrs_recode",
                          "ears.post.y_text_typical_hrs_recode", "ears.post.y_video.chat_typical_hrs_recode",
                          "ears.post.y_browse_typical_hrs_recode", "ears.post.y_video.game_wkdy_hrs_recode")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "browse", "game"))
dt.post.y$video <- NA
dt.post.y$data.source <- "EARS.post.y"
# STQ.PHONE --> browse all NA
dt.stq.phone <- ears495.y4[c("src_subject_id", "stq.y_phone_total.typical_typical_hr", "stq.y_phone_social.media_typical_hr",
                             "stq.y_phone_text_typical_hr", "stq.y_phone_video.chat_typical_hr",
                             "stq.y_phone_browse_typical_hr", "stq.y_phone_video.game_typical_hr", 
                             "stq.y_phone_tv.video_typical_hr")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "browse", "game", "video"))
dt.stq.phone$data.source <- "stq.phone"
# STQ
dt.stq <- ears495.y4[c("src_subject_id", "stq_totaltime_typical.y", "stq_socialmedia_typical", "stq_text_typical", 
                       "stq_videochat_typical", "stq_videogame_typical", "stq_tv_typical")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "game", "video"))
dt.stq$browse <- NA
dt.stq$data.source <- "stq"
# stq.phone.all.prop
ears495.y4$stq.phone.all.prop_browse <- NA
dt.stq.phone.prop <- ears495.y4[c("src_subject_id", "stq.phone.all.prop_total", "stq.phone.all.prop_socialmedia", 
                                  "stq.phone.all.prop_text", "stq.phone.all.prop_videochat", "stq.phone.all.prop_browse",
                                  "stq.phone.all.prop_videogame", "stq.phone.all.prop_video")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "browse", "game", "video"))
dt.stq.phone.prop$data.source <- "stq.phone.all.prop"

# rbind five data
dt.table2 <- rbind(dt.appusage, dt.post.y, dt.stq.phone, dt.stq, dt.stq.phone.prop)
dt.table2$data.source <- factor(dt.table2$data.source, levels = c("EARS.APPUSAGE", "EARS.post.y", "stq.phone", "stq", "stq.phone.all.prop"))

# revised on 12/03/2023 proportion from this table was not reported
dt.table2.sub <- dt.table2[c("src_subject_id", "data.source", "total", "socialmedia", "game")] %>%
  filter(data.source != "stq.phone" & data.source != "stq.phone.all.prop")
sel.sub <- dt.table2.sub %>%
  group_by(src_subject_id) %>%
  summarise(index_sel_total = length(data.source[which(!is.na(total))]),
            index_sel_socialmedia = length(data.source[which(!is.na(socialmedia))]),
            index_sel_game = length(data.source[which(!is.na(game))]))

dt.table2.sub$total[which(dt.table2.sub$src_subject_id %in% sel.sub$src_subject_id[which(sel.sub$index_sel_total<3)])] <- NA
dt.table2.sub$socialmedia[which(dt.table2.sub$src_subject_id %in% sel.sub$src_subject_id[which(sel.sub$index_sel_socialmedia<3)])] <- NA
dt.table2.sub$game[which(dt.table2.sub$src_subject_id %in% sel.sub$src_subject_id[which(sel.sub$index_sel_game<3)])] <- NA

tbl1 <- table1(~ total + socialmedia + game | data.source, 
               data=dt.table2.sub, 
               rowlabelhead = "Usage Time (hrs)", 
               caption = "TABLE2: Difference in Usage Time between Objective and Subjective Measures", 
               overall = FALSE,
               digits = 4,
               #fmt = "%.2f"
)
t1flex(tbl1) %>% 
  save_as_docx(path="./Output/R5/earsR5/EARS_manual1/table2.obj.sub.rowwise.complete.docx", pr_section = sect_properties)

# sub.nm <- sel.sub$src_subject_id[which(sel.sub$index_sel_total==3)]
# dt.table2.sub.2 <- dt.table2.sub[which(dt.table2.sub$src_subject_id %in% sub.nm),]
# tbl1 <- table1(~ total + socialmedia + game | data.source, 
#                data=dt.table2.sub.2, 
#                rowlabelhead = "Usage Time (hrs)", 
#                caption = "TABLE2: Difference in Usage Time between Objective and Subjective Measures", 
#                overall = FALSE,
#                digits = 4,
#                #fmt = "%.2f"
# )

###### calculate pvalue from two sample t-test ######
dt.table2.wide <- dt.table2.sub %>%
  pivot_wider(
    id_cols = "src_subject_id",
    names_from = c("data.source"),
    #values_from = c("total", "socialmedia", "text", "videochat", "browse", "game", "video"),
    values_from = c("total", "socialmedia", "game"),
    names_glue = "{.value}_{data.source}"
  )

# total
t.test(dt.table2.wide$total_EARS.APPUSAGE, dt.table2.wide$total_EARS.post.y, paired = TRUE, na.rm = TRUE)
# data:  dt.table2.wide$total_EARS.APPUSAGE and dt.table2.wide$total_EARS.post.y
# t = 0.89273, df = 375, p-value = 0.3726
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.1993497  0.5308871
# sample estimates:
#   mean difference 
# 0.1657687 

t.test(dt.table2.wide$total_EARS.APPUSAGE, dt.table2.wide$total_stq, paired = TRUE, na.rm = TRUE)
# data:  dt.table2.wide$total_EARS.APPUSAGE and dt.table2.wide$total_stq
# t = -1.9437, df = 375, p-value = 0.05267
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.758517696  0.004378705
# sample estimates:
#   mean difference 
# -0.3770695 

# socialmedia
t.test(dt.table2.wide$socialmedia_EARS.APPUSAGE, dt.table2.wide$socialmedia_EARS.post.y, paired = TRUE, na.rm = TRUE)
# data:  dt.table2.wide$socialmedia_EARS.APPUSAGE and dt.table2.wide$socialmedia_EARS.post.y
# t = 0.20868, df = 394, p-value = 0.8348
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.1456877  0.1802890
# sample estimates:
#   mean difference 
# 0.01730064

t.test(dt.table2.wide$socialmedia_EARS.APPUSAGE, dt.table2.wide$socialmedia_stq, paired = TRUE, na.rm = TRUE)
# data:  dt.table2.wide$socialmedia_EARS.APPUSAGE and dt.table2.wide$socialmedia_stq
# t = 2.0936, df = 394, p-value = 0.03693
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.01221633 0.38856940
# sample estimates:
#   mean difference 
# 0.2003929 

# game
t.test(dt.table2.wide$game_EARS.APPUSAGE, dt.table2.wide$game_EARS.post.y, paired = TRUE, na.rm = TRUE)
# data:  dt.table2.wide$game_EARS.APPUSAGE and dt.table2.wide$game_EARS.post.y
# t = -12.884, df = 394, p-value < 2.2e-16
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -2.258369 -1.660408
# sample estimates:
#   mean difference 
# -1.959389 

t.test(dt.table2.wide$game_EARS.APPUSAGE, dt.table2.wide$game_stq, paired = TRUE, na.rm = TRUE)
# data:  dt.table2.wide$game_EARS.APPUSAGE and dt.table2.wide$game_stq
# t = -16.805, df = 394, p-value < 2.2e-16
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -3.217139 -2.543230
# sample estimates:
#   mean difference 
# -2.880184 

######

###### Proportion of STQ.PHONE/STQ in full sample (proportion from this table was reported) ######
## social media apps --> almost all activities were on smartphone
master.file.y4$stq.phone.all.prop_socialmedia <- master.file.y4$stq.y_phone_social.media_typical_hr/master.file.y4$stq_socialmedia_typical
## text
master.file.y4$stq.phone.all.prop_text <- master.file.y4$stq.y_phone_text_typical_hr/master.file.y4$stq_text_typical
## video chat
master.file.y4$stq.phone.all.prop_videochat <- master.file.y4$stq.y_phone_video.chat_typical_hr/master.file.y4$stq_videochat_typical
## video game
master.file.y4$stq.phone.all.prop_videogame <- master.file.y4$stq.y_phone_video.game_typical_hr/master.file.y4$stq_videogame_typical
## video
master.file.y4$stq.phone.all.prop_video <- master.file.y4$stq.y_phone_tv.video_typical_hr/master.file.y4$stq_tv_typical
## total
master.file.y4$stq.phone.all.prop_total <- master.file.y4$stq.y_phone_total.typical_typical_hr/master.file.y4$stq_totaltime_typical.y

# STQ.PHONE --> browse all NA
dt.stq.phone <- master.file.y4[c("src_subject_id", "stq.y_phone_total.typical_typical_hr", "stq.y_phone_social.media_typical_hr",
                             "stq.y_phone_text_typical_hr", "stq.y_phone_video.chat_typical_hr",
                             "stq.y_phone_browse_typical_hr", "stq.y_phone_video.game_typical_hr", 
                             "stq.y_phone_tv.video_typical_hr")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "browse", "game", "video"))
dt.stq.phone$data.source <- "stq.phone"
# STQ
dt.stq <- master.file.y4[c("src_subject_id", "stq_totaltime_typical.y", "stq_socialmedia_typical", "stq_text_typical", 
                       "stq_videochat_typical", "stq_videogame_typical", "stq_tv_typical")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "game", "video"))
dt.stq$browse <- NA
dt.stq$data.source <- "stq"
# stq.phone.all.prop
master.file.y4$stq.phone.all.prop_browse <- NA
dt.stq.phone.prop <- master.file.y4[c("src_subject_id", "stq.phone.all.prop_total", "stq.phone.all.prop_socialmedia", 
                                  "stq.phone.all.prop_text", "stq.phone.all.prop_videochat", "stq.phone.all.prop_browse",
                                  "stq.phone.all.prop_videogame", "stq.phone.all.prop_video")] %>%
  setNames(c("src_subject_id", "total", "socialmedia", "text", "videochat", "browse", "game", "video"))
dt.stq.phone.prop$data.source <- "stq.phone.all.prop"
dt.stq.phone.prop[dt.stq.phone.prop=="Inf"] <- NA
dt.stq.phone.prop[2:8][dt.stq.phone.prop[2:8]>1] <- 1

# rbind five data
dt.table2 <- rbind(dt.stq.phone, dt.stq, dt.stq.phone.prop)
dt.table2$data.source <- factor(dt.table2$data.source, levels = c("stq.phone", "stq", "stq.phone.all.prop"))

tbl1 <- table1(~ total + socialmedia + game | data.source, 
               data=dt.table2, 
               rowlabelhead = "Usage Time (hrs)", 
               caption = "Differences between objective and self-reported usage time on phone", 
               overall = FALSE,
               digits = 4,
               #fmt = "%.2f"
)
t1flex(tbl1) %>% 
  save_as_docx(path="./Output/R5/earsR5/EARS_manual1/table2_stq.phone_stq_prop_fullsample.docx", pr_section = sect_properties)


######

################################ 4. Table 3 obj-sub discrepancy ################################
# A2: what factors contribute to the discrepancy low agreement vs high agreement 

# (3 versions: ears - stq, ears - stq.phone, ears - post-sensing self-report)
## ears-stq
ears495.y4$diff_total_ears_stq <- ears495.y4$APPUSAGE_daily.usage_all.exl_mins_mean/60 - ears495.y4$stq_totaltime_typical.y
ears495.y4$diff_social.media.v2_ears_stq <- ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean/60 - ears495.y4$stq_socialmedia_typical
ears495.y4$diff_game.v2_ears_stq <- ears495.y4$APPUSAGE_daily.usage_GAME.v2_mins_mean/60 - ears495.y4$stq_videogame_typical

## ears-stq.phone
ears495.y4$diff_total_ears_stq.phone <- ears495.y4$APPUSAGE_daily.usage_all.exl_mins_mean/60 - ears495.y4$stq.y_phone_total.typical_typical_hr
ears495.y4$diff_social.media.v2_ears_stq.phone <- ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean/60 - ears495.y4$stq.y_phone_social.media_typical_hr
ears495.y4$diff_game.v2_ears_stq.phone <- ears495.y4$APPUSAGE_daily.usage_GAME.v2_mins_mean/60 - ears495.y4$stq.y_phone_video.game_typical_hr

## ears-ears.post.y
ears495.y4$diff_total_ears_post.y <- ears495.y4$APPUSAGE_daily.usage_all.exl_mins_mean/60 - ears495.y4$ears.post.y_total_typical_hrs_recode
ears495.y4$diff_social.media.v2_ears_post.y <- ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean/60 - ears495.y4$ears.post.y_social.media_typical_hrs_recode
ears495.y4$diff_game.v2_ears_post.y <- ears495.y4$APPUSAGE_daily.usage_GAME.v2_mins_mean/60 - ears495.y4$ears.post.y_video.game_typical_hrs_recode

### recode diff.score to categories
dt.tt <- ears495.y4[c("src_subject_id", "diff_social.media.v2_ears_stq", 
                      "family_income_recode_l", "parental_educ_highest_recode", "race_ethnicity_recode",
                      "nihtbx_picvocab_uncorrected", "nihtbx_picture_uncorrected", "interview_age_yrs", 
                      "demo_sex_v2_recode", "parent_married_l")]

# ### using baseline cognitive to predict y4 diff
# dt.tt <- ears495.y4[c("src_subject_id", "diff_social.media.v2_ears_stq", 
#                       "family_income_recode_l", "parental_educ_highest_recode", "race_ethnicity_recode",
#                       "interview_age_yrs", 
#                       "demo_sex_v2_recode", "parent_married_l",
#                       "nihtbx_picvocab_uncorrected", "nihtbx_picture_uncorrected")]
# dt.tt <- merge(dt.tt, 
#                master.file[which(master.file$eventname==0), 
#                            c("src_subject_id", "nihtbx_picvocab_uncorrected", "nihtbx_picture_uncorrected",
#                              "nihtbx_fluidcomp_uncorrected", "nihtbx_cryst_uncorrected")] %>%
#                  setNames(c("src_subject_id", "nihtbx_picvocab_uncorrected.y0", "nihtbx_picture_uncorrected.y0",
#                             "nihtbx_fluidcomp_uncorrected.y0", "nihtbx_cryst_uncorrected.y0")),
#                all.x = TRUE)

#dt.tt <- dt.tt[complete.cases(dt.tt),]
for (i in c("social.media.v2")) {
  print(i)
  for (cutoff in c(0.5)) {
    print(paste0("cutoff=", cutoff))
    for (j in c("stq")) {
      #print(j)
      est <- list()
      mod <- list()
      for (sel in c("Over_reporting", "Under_reporting")) {
        print(sel)
        #dt <- ears495.y4
        dt <- dt.tt
        if (sel=="Over_reporting") {
          dt$grp.index <- ifelse(dt[[paste0("diff_", i, "_ears_", j)]] < -cutoff, 1,
                                 ifelse((dt[[paste0("diff_", i, "_ears_", j)]]>= -cutoff & dt[[paste0("diff_", i, "_ears_", j)]]<= cutoff), 0, NA))
        } else if (sel=="Under_reporting") {
          dt$grp.index <- ifelse(dt[[paste0("diff_", i, "_ears_", j)]] > cutoff, 1,
                                 ifelse((dt[[paste0("diff_", i, "_ears_", j)]]>= -cutoff & dt[[paste0("diff_", i, "_ears_", j)]]<= cutoff), 0, NA))
        } else if (sel=="Total") {
          dt$grp.index <- ifelse(dt[[paste0("diff_", i, "_ears_", j)]] > cutoff | dt[[paste0("diff_", i, "_ears_", j)]] < -cutoff, 1,
                                 ifelse((dt[[paste0("diff_", i, "_ears_", j)]]>= -cutoff & dt[[paste0("diff_", i, "_ears_", j)]]<= cutoff), 0, NA))
        }
        print(table(dt$grp.index))
        dt <- dt %>% filter(!is.na(grp.index))
        
        formula <- paste0("factor(grp.index) ~ nihtbx_picvocab_uncorrected + interview_age_yrs + demo_sex_v2_recode + race_ethnicity_recode + family_income_recode_l + parental_educ_highest_recode + parent_married_l")
        mod[[sel]] <- glm(formula, 
                          data = dt, family = binomial())
        print(summary(mod[[sel]]))
        est[[sel]] <- data.frame(summary(mod[[sel]])$coefficient[, c("Estimate", "Pr(>|z|)")]) %>%
          setNames(paste0(c("Est", "Pvalue"), "_", sel))
        est[[sel]]$pred <- rownames(est[[sel]])
      }
    }
  }
}
final.est.0.5 <- Reduce(merge, est)
final.est.1 <- Reduce(merge, est)
write.csv(final.est.0.5, file = "./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/diff.social.y4_cryst.y0_0.5hr.csv", row.names = FALSE)
write.csv(final.est.1, file = "./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/diff.social.y4_cryst.y0_1hr.csv", row.names = FALSE)

# > tidy(mod$Under_reporting, conf.int = TRUE)
# # A tibble: 13 × 7
# term                                             estimate std.error statistic p.value conf.low conf.high
# <chr>                                               <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#   1 (Intercept)                                       -2.29      2.70     -0.850   0.395  -7.60      2.99   
# 2 nihtbx_picvocab_uncorrected                       -0.0348    0.0165   -2.11    0.0345 -0.0676   -0.00287
# 3 interview_age_yrs                                  0.351     0.175     2.00    0.0451  0.00979   0.698  
# 4 demo_sex_v2_recodeFemale                           0.104     0.243     0.429   0.668  -0.374     0.581  
# 5 race_ethnicity_recodeBlack                         0.466     0.558     0.834   0.404  -0.620     1.59   
# 6 race_ethnicity_recodeHispanic                      0.0317    0.328     0.0966  0.923  -0.619     0.671  
# 7 race_ethnicity_recodeOther                        -0.514     0.390    -1.32    0.188  -1.31      0.227  
# 8 family_income_recode_l[50-100k]                   -0.344     0.312    -1.10    0.270  -0.966     0.262  
# 9 family_income_recode_l[<50k]                      -0.152     0.414    -0.368   0.713  -0.976     0.653  
# 10 parental_educ_highest_recodeBachelor              -0.196     0.330    -0.594   0.552  -0.847     0.449  
# 11 parental_educ_highest_recodeSome College           0.616     0.374     1.65    0.0994 -0.113     1.36   
# 12 parental_educ_highest_recodeHigh School or Below   0.371     0.562     0.660   0.509  -0.739     1.47   
# 13 parent_married_lNot Married                        0.500     0.308     1.63    0.104  -0.106     1.10 

temp <- tidy(mod$Under_reporting, conf.int = TRUE)
temp$OR <- exp(temp$estimate)
temp$OR_conf.low <- exp(temp$conf.low)
temp$OR_conf.high <- exp(temp$conf.high)
temp$cutoff <- 0.5
temp$grp <- "Over_reporting"
write.csv(temp, file = "./Output/R5/earsR5/EARS_manual1/Draft/EARS-check/tt.csv", row.names = FALSE)


### bivariate test in table1
tbl1 <- list()
df.lst <- list()
#for (i in c("total", "social.media.v2", "game.v2")) {
for (i in c("social.media.v2")) {
  print(i)
  for (cutoff in c(0.25, 0.5, 1)) {
    print(cutoff)
    for (j in c("stq")) {
      print(j)
      for (sel in c("Over_reporting", "Under_reporting")) {
        print(sel)
        dt <- ears495.y4
        if (sel=="Over_reporting") {
          dt$grp.index <- ifelse(dt[[paste0("diff_", i, "_ears_", j)]] < -cutoff, 2,
                                 ifelse((dt[[paste0("diff_", i, "_ears_", j)]]>= -cutoff & dt[[paste0("diff_", i, "_ears_", j)]]<= cutoff), 1, NA))
        } else if (sel=="Under_reporting") {
          dt$grp.index <- ifelse(dt[[paste0("diff_", i, "_ears_", j)]] > cutoff, 2,
                                 ifelse((dt[[paste0("diff_", i, "_ears_", j)]]>= -cutoff & dt[[paste0("diff_", i, "_ears_", j)]]<= cutoff), 1, NA))
        }
        print(table(dt$grp.index))
        dt <- dt %>% filter(!is.na(grp.index))
        
        tbl1[[paste0("diff_", i, "_ears_", j, "_", sel, "_", cutoff)]] <-
          table1(~ interview_age_yrs + demo_sex_v2_recode + race_ethnicity_recode
                 + family_income_recode_l + parental_educ_highest_recode + parent_married_l
                 + MH_cbcl_scr_syn_internal_r + MH_cbcl_scr_syn_external_r
                 + nihtbx_picvocab_uncorrected + nihtbx_picture_uncorrected
                 + APPUSAGE_daily.usage_SOCIAL.v2_mins_mean
                 | factor(grp.index), data = dt, rowlabelhead = "", caption = "",
                 #overall = "Total",
                 digits = 5,
                 overall = FALSE,
                 extra.col=list(`P-value`=pvalue))
        df.lst[[paste0("diff_", i, "_ears_", j, "_", sel, "_", cutoff)]] <- as.data.frame(tbl1[[paste0("diff_", i, "_ears_", j, "_", sel, "_", cutoff)]])
        colnames(df.lst[[paste0("diff_", i, "_ears_", j, "_", sel, "_", cutoff)]]) <- paste0(i, "_EARS_", j, c(1:4))
        insert.dt <- data.frame(x1 = "Outcome", x2 = "Consistent", x3 = sel, x4 = "p-value") %>%
          setNames(paste0(i, "_EARS_", j, c(1:4)))
        df.lst[[paste0("diff_", i, "_ears_", j, "_", sel, "_", cutoff)]] <- rbind(insert.dt, df.lst[[paste0("diff_", i, "_ears_", j, "_", sel, "_", cutoff)]])
      }
    }
  }
}
# for (i in 1:length(df.lst)) {
#   df.lst[[i]] <- df.lst[[i]][which(df.lst[[i]][[1]] != "  Missing"),]
# }
for (i in 2:length(df.lst)) {
  df.lst[[i]] <- df.lst[[i]][, -1]
}
for (i in c(2,4)) {
  df.lst[[i]] <- df.lst[[i]][, -1]
}
combined_df <- do.call(cbind, df.lst)
write.xlsx(combined_df, "./Output/R5/earsR5/EARS_manual1/A2/summary_tbl_stq_social_0.5_1.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#write.xlsx(combined_df, "./Output/R5/earsR5/EARS_manual1/A2/summary_tbl_stq.phone_2hr.cutoff.xlsx", sheetName = "Sheet1", rowNames = FALSE)

write.xlsx(combined_df, "./Output/R5/earsR5/EARS_manual1/A2/summary_tbl_stq_social_0.25_0.5_1.xlsx", sheetName = "Sheet1", rowNames = FALSE)

### Figure1: scatter plot
dt <- ears495.y4
behav.nm <- c("nihtbx_picvocab_uncorrected", "nihtbx_picture_uncorrected")
behav.nm.recode <- c("Picture Vocabulary", "Picture Sequencing Memory")
columns_indices <- match(behav.nm, colnames(dt))
colnames(dt)[columns_indices] <- behav.nm.recode

# diff.nm <- c("diff_total_ears_stq", "diff_social.media.v2_ears_stq", "diff_game.v2_ears_stq")
# diff.nm <- c("diff_total_ears_post.y", "diff_social.media.v2_ears_post.y", "diff_game.v2_ears_post.y")
diff.nm <- c("diff_social.media.v2_ears_stq")
diff.nm.recode <- c("Absolute Discrepancies in Social Media")
# diff.nm.recode <- c("Absolute Discrepancies in Total", "Absolute Discrepancies in Social Media", "Absolute Discrepancies in Game")
columns_indices <- match(diff.nm, colnames(dt))
colnames(dt)[columns_indices] <- diff.nm.recode

p <- list()
n <- 1
for (i in behav.nm.recode) {
  print(i)
  for (j in diff.nm.recode) {
    print(j)
    dt.sub <- dt[which(!is.na(dt[[j]]) & !is.na(dt[[i]])),]
    #dt.sub <- dt[which(dt[[j]]<0 & !is.na(dt[[i]])),]
    dt.sub[[j]] <- abs(dt.sub[[j]])
    ## winsorize extreme values
    # cutoff <- mean(dt.sub[[j]]) + sd(dt.sub[[j]])*3
    # dt.sub[[j]][which(dt.sub[[j]]>cutoff)] <- cutoff
    #dt.sub$index <- ifelse(dt.sub[[j]]>=0, "Under", "Over")
    col1 <- ifelse(i=="Picture Vocabulary", "red", "blue")
    loc.x <- ifelse(j=="Absolute Discrepancies in Social Media", 10, 15)
    p[[n]] <- ggscatter(dt.sub, x = j, y = i,
                        #color = "race_ethnicity_recode", palette = "jco",
                        #shape = 21, size = 2, # Points color, shape and size
                        add = "reg.line",  # Add regressin line
                        add.params = list(color = col1, fill = "lightgray"), # Customize reg. line
                        conf.int = TRUE, # Add confidence interval
                        cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                        cor.coeff.args = list(#aes(color = index),
                                              method = "pearson", label.x = loc.x, #label.y = 150, 
                                              label.sep = "\n",
                                              p.accuracy = 0.001, r.accuracy = 0.01)
    )
    n <- n+1
  }
}

# Save the arranged plots to a PDF file
pdf(file.path(paste0("./Output/R5/earsR5/EARS_manual1/Draft/Table and Figures/Single_Files/A2.scatterplot.stq.pdf")), 
    width = 8, height = 3, onefile = FALSE)
# print(grid.arrange(p[[1]], p[[2]], p[[3]],
#                    p[[4]], p[[5]], p[[6]], 
#                    ncol = 3))
print(grid.arrange(p[[1]], p[[2]], ncol = 2))
dev.off()

################################ 5. association between behav measures and SMA ################################
# A3: whether self-report subjective and objective screentime were differentially associated with 
#     clinically relevant measures related to psychopathology and sleep disturbance. 

dt <- ears495.y4
# outcome
behav.nm <- c("MH_cbcl_scr_syn_internal_r", "MH_cbcl_scr_syn_external_r", 
              "SMAQ.total.score", "VGAQ.total.score")
behav.nm.recode <- c("CBCL_INTERNAL", "CBCL_EXTERNAL", 
                     "SMAQ_Total_Score", "VGAQ_Total_Score")
columns_indices <- match(behav.nm, colnames(dt))
colnames(dt)[columns_indices] <- behav.nm.recode

# predictors
stq.add.nm <- c("APPUSAGE_daily.usage_all.exl_mins_mean", "ears.post.y_total_typical_hrs_recode", "stq_totaltime_typical.y",
                "APPUSAGE_daily.usage_SOCIAL.v2_mins_mean", "ears.post.y_social.media_typical_hrs_recode", "stq_socialmedia_typical",
                "APPUSAGE_daily.usage_GAME.v2_mins_mean", "ears.post.y_video.game_typical_hrs_recode", "stq_videogame_typical")
stq.nm.recode <- c("EARS_Total", "POST_Total", "STQ_Total",
                   "EARS_SocialMedia", "POST_SocialMedia", "STQ_SocialMedia",
                   "EARS_Game", "POST_Game", "STQ_Game")
# Find the indices of the columns to be relabeled
columns_indices <- match(stq.add.nm, colnames(dt))
# Update the column names with the new labels
colnames(dt)[columns_indices] <- stq.nm.recode
# change mins to hrs for EARS measurements
dt[stq.nm.recode[grep("EARS", stq.nm.recode)]] <- dt[stq.nm.recode[grep("EARS", stq.nm.recode)]]/60

for (i in behav.nm.recode) {
  require(lmerTest)
  require(dplyr)
  print(i)
  dt.new <- dt
  dt.new[[i]] <- scale(dt.new[[i]])
  temp_table <- list()
  mod <- list()
  for (j in stq.nm.recode) {
    print(j)
    formula <- paste0(i, " ~ ", j,  " + interview_age + factor(demo_sex_v2_recode) + factor(race_ethnicity) + (1|rel_family_id)")
    mod[[j]] <- lmer(formula, data=dt.new, REML=FALSE)
    temp1 <- tidy(mod[[j]], conf.int = TRUE)
    temp_table[[j]] <- temp1[grepl(j, temp1$term), ]
    temp_table[[j]]$n <- nobs(mod[[j]])
    temp_table[[j]]$Outcome <- i
  }
  lmer_table <- bind_rows(temp_table, .id = "Predictors")
  lmer_table <- lmer_table[order(lmer_table$p.value, decreasing = FALSE),]
  lmer_table$p.adj <- p.adjust(lmer_table$p.value)
  
  out.path <- paste0("./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3_lmer_results/", i, "_lmer_table.csv")
  write.csv(lmer_table, file=out.path, row.names = FALSE)
}
# summary table
in.path<-"./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3_lmer_results"
tbl.fn <- list.files(path=in.path, pattern="*lmer_table.csv", full.names=T, include.dirs=T)
tbl.lst <- lapply(tbl.fn, read.csv, stringsAsFactors=FALSE)
names(tbl.lst) <- sapply(strsplit(tbl.fn, "/"), function(x) x[9]) 
sel.row <- c("EARS_Total", "POST_Total", "STQ_Total",
             "EARS_SocialMedia", "POST_SocialMedia", "STQ_SocialMedia", 
             "EARS_Game", "POST_Game", "STQ_Game")
for (i in names(tbl.lst)) {
  new.nm <- sub("_lmer_table.csv", "", i)
  tbl.lst[[i]] <- tbl.lst[[i]][which(tbl.lst[[i]]$Predictors %in% sel.row),]
  tbl.lst[[i]]$p.value.recode <- ifelse(tbl.lst[[i]]$p.value<0.001, "<0.001", paste0("=", sprintf("%.3f", tbl.lst[[i]]$p.value)))
  tbl.lst[[i]][paste0("estimate_", new.nm)] <- paste0(sprintf("%.3f", tbl.lst[[i]]$estimate), " (", sprintf("%.3f", tbl.lst[[i]]$conf.low), ", ", sprintf("%.3f", tbl.lst[[i]]$conf.high), ")")
  tbl.lst[[i]] <- tbl.lst[[i]][which(tbl.lst[[i]]$p.value<0.05), c("Predictors", paste0("estimate_", new.nm))]
}
tbl.comb <- Reduce(function(x, y) merge(x, y, all=TRUE),
                   tbl.lst)
tbl.comb <- merge(data.frame(Predictors=sel.row), tbl.comb, all.x = TRUE) %>%
  arrange(Predictors)
write.csv(tbl.comb, file = "./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3.summary.table.csv", row.names = FALSE, na="")

### Include both subjective and objective measures into the same model
comb.nm <- c("EARS_Total + STQ_Total",
             "EARS_SocialMedia + STQ_SocialMedia",
             "EARS_Game + STQ_Game")
comb.nm <- c("EARS_Total + POST_Total",
             "EARS_SocialMedia + POST_SocialMedia",
             "EARS_Game + POST_Game")
for (i in behav.nm.recode) {
  print(i)
  
  dt.new <- dt
  dt.new[[i]] <- scale(dt.new[[i]])
  
  temp_table <- list()
  mod <- list()
  for (j in comb.nm) {
    require(lmerTest)
    require(dplyr)
    print(j)
    formula <- paste0(i, " ~ ", j, " + interview_age + factor(demo_sex_v2_recode) + factor(race_ethnicity) + (1|rel_family_id)")
    mod[[j]] <- lmer(formula, data=dt.new)
    temp1 <- tidy(mod[[j]], conf.int = TRUE)
    temp_table[[j]] <- temp1[grepl("EARS_|STQ_|POST_", temp1$term), ]
    temp_table[[j]]$n <- nobs(mod[[j]])
    temp_table[[j]]$Outcomes <- i
  }
  lmer_table <- bind_rows(temp_table, .id = "Combination")
  #lmer_table <- lmer_table[order(lmer_table$p.value, decreasing = FALSE),]
  lmer_table$p.adj <- p.adjust(lmer_table$p.value)
  
  out.path <- paste0("./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3_combined_lmer_results/", i, "_sub.obj.comb_lmer_table.csv")
  write.csv(lmer_table, file=out.path, row.names = FALSE)
}
# summary table
in.path<-"./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3_combined_lmer_results"
tbl.fn <- list.files(path=in.path, pattern="*_sub.obj.comb_lmer_table.csv", full.names=T, include.dirs=T)
tbl.lst <- lapply(tbl.fn, read.csv, stringsAsFactors=FALSE)
names(tbl.lst) <- sapply(strsplit(tbl.fn, "/"), function(x) x[9]) 
tt <- tbl.lst$CBCL_EXTERNAL_sub.obj.comb_lmer_table.csv[c("Combination", "term")]
for (i in names(tbl.lst)) {
  print(i)
  new.nm <- sub("_sub.obj.comb_lmer_table.csv", "", i)
  tbl.lst[[i]]$p.value.recode <- ifelse(tbl.lst[[i]]$p.value<0.001, "<0.001", paste0("=", sprintf("%.3f", tbl.lst[[i]]$p.value)))
  tbl.lst[[i]]$estimate <- paste0(sprintf("%.3f", tbl.lst[[i]]$estimate), " (p", tbl.lst[[i]]$p.value.recode, ")")
  # tbl.lst[[i]] <- tbl.lst[[i]][which(tbl.lst[[i]]$p.value<0.05), c("Outcomes", "term", "estimate")] %>%
  #   spread(key = term, value = estimate)
  # colnames(tbl.lst[[i]]) <- c("Outcomes", paste0("Est.", new.nm, "_", colnames(tbl.lst[[i]])[2:3]))
  tbl.lst[[i]] <- tbl.lst[[i]][which(tbl.lst[[i]]$p.value<0.05), c("Combination", "term", "estimate")]
  colnames(tbl.lst[[i]]) <- c("Combination", "term", paste0("Est.", new.nm))
}
tbl.comb <- Reduce(function(x, y) merge(x, y, all=TRUE),
                   tbl.lst)
tbl.comb <- merge(tt, tbl.comb, all.x = TRUE, sort = FALSE)
tbl.comb <- merge(tt, tbl.comb, sort = FALSE)

write.csv(tbl.comb, file = "./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3_summary_comb.post.ears_table.csv", row.names = FALSE, na="")



### summary plot
in.path<-"./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3_lmer_results"
csv.fn <- list.files(path=in.path, pattern="*lmer_table.csv", full.names=T, include.dirs=T)
csv.lst <- lapply(csv.fn, read.csv, stringsAsFactors=FALSE)
names(csv.lst) <- sapply(strsplit(csv.fn, "/"), function(x) x[9])

plot.dt <- bind_rows(csv.lst, .id = "File.Name")
plot.dt$model <- paste0(plot.dt$Outcome, "x", plot.dt$Predictors)

plot.dt$Group <- ifelse(plot.dt$p.value<0.05, "Sig", "Non-Sig")
plot.dt$Group <- factor(plot.dt$Group, levels = c("Sig", "Non-Sig"))

plot.dt$estimate.v2 <- paste0(sprintf("%.2f", plot.dt$estimate), " [", sprintf("%.2f", plot.dt$conf.low), ", ", sprintf("%.2f", plot.dt$conf.high), "]")
plot.dt$p.value.v2 <- ifelse(plot.dt$p.value<0.001, "<0.001", sprintf("%.3f", plot.dt$p.value))

plot.dt$conf.low.v2 <- ifelse(plot.dt$conf.low < -1, -1, plot.dt$conf.low)
plot.dt$conf.high.v2 <- ifelse(plot.dt$conf.high > 1, 1, plot.dt$conf.high)
#write.csv(plot.dt, file = "./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3.plot.dt.csv", row.names = FALSE)

plot.dt_ordered <- plot.dt[order(factor(plot.dt$Outcome, levels = c("CBCL_INTERNAL", "CBCL_EXTERNAL", 
                                                                    "SMAQ_Total_Score", "VGAQ_Total_Score")),
                                 factor(plot.dt$Predictors, levels = c("EARS_Total", "EARS_SocialMedia", "EARS_Game",
                                                                       "POST_Total", "POST_SocialMedia", "POST_Game",
                                                                       "STQ_Total", "STQ_SocialMedia", "STQ_Game"))),]
plot.dt_ordered <- plot.dt_ordered[c("model", "Outcome", "Predictors", "estimate", "conf.low.v2", "conf.high.v2",
                                     "estimate.v2", "p.value.v2", "Group")]
write.csv(plot.dt_ordered, file = "./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3.plot.dt_ordered.csv", row.names = FALSE)


# forestplot
plot.dt <- read.csv("./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3.plot.dt_ordered.csv")
# remove the rows we don't want
#plot.dt <- subset(plot.dt, !grepl("Gender|Female|Male|Edu|school|degree|college|Health|dequate|Age", plot.dt$term))
tt <- plot.dt

tt <- plot.dt[grep("SocialMedia|CBCL|SMAQ|VGAQ", plot.dt$term),]
tt$term <- sub(" - SocialMedia", "", tt$term)
# tt <- tt %>%
#   mutate(box_color = ifelse(Group == "Sig", "blue", ifelse(Group == "Non-Sig", "darkred", "")),
#          line_color = ifelse(Group == "Sig", "blue", ifelse(Group == "Non-Sig", "darkred", "")))
tabletext <- data.frame(Outcome=tt$term, 
                        Est=tt$estimate.v2,
                        p.value=tt$p.value.v2)
### full 
col.lst <- list()
col.lst[[1]] <- gpar(col = "white")
fill.lst <- list()
fill.lst[[1]] <- gpar(fill = "white")
n <- 1
for (i in 1:nrow(tt)) {
  print(i)
  n <- n+1
  tt.col <- ifelse(tt[i, "Group"]=="", "white", 
               ifelse(tt[i, "Group"]=="Sig", "red", "black"))
  col.lst[[n]] <- gpar(col = tt.col)
  fill.lst[[n]] <- gpar(fill = tt.col)
}
styles <- fpShapesGp(
  lines = col.lst,
  box = fill.lst
)

pdf(file.path(paste0("./Output/R5/earsR5/EARS_manual1/Draft/Add_May2024/A3.figure_socialmedia.pdf")), width = 8, height = 6, onefile = FALSE)
print(tt |> 
        #group_by(Group) |>
        forestplot(labeltext=tabletext, graph.pos=4, 
                   mean=estimate, 
                   lower=conf.low.v2, upper=conf.high.v2,
                   title="", xlab="Estimate", 
                   # col = fpColors(box = tt$box_color, 
                   #                lines = tt$line_color, 
                   #                zero = "gray50"),
                   col=fpColors(box="white", lines="white", zero = "gray50"),
                   zero=0, cex=0.9, lineheight = "auto", boxsize=0.3, colgap=unit(6,"mm"),
                   lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.1,
                   shapes_gp = styles
        ) |> 
        fp_add_header(Outcome = c("Outcome"),
                      Est = c("Estimate (95%CI)"),
                      p.value = c("p-value")
        ) |> 
        fp_set_zebra_style("#EFEFEF") |> 
        fp_add_lines() |> 
        fp_set_style(#box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555")),
                     align = "lrr",
                     txt_gp = fpTxtGp(ticks = gpar(cex = 0.8),
                                      xlab  = gpar(cex = 0.8))
        ))
dev.off()

### sig only
tt2 <- tt[which(tt$Group==""|tt$Group=="Sig"),]
tabletext <- data.frame(Outcome=tt2$term, 
                        Est=tt2$estimate.v2,
                        p.value=tt2$p.value.v2)

pdf(file.path(paste0("./Output/R5/earsR5/EARS_manual1/Draft/Table and Figures/A3.forestplot.sig.pdf")), width = 8, height = 8, onefile = FALSE)
print(tt2 |> 
        forestplot(labeltext=tabletext, graph.pos=4, 
                   # hrzl_lines = list("5" = gpar(lwd=100, lineend="butt", columns=c(1:4), col = "lightgray"),
                   #                   "10" = gpar(lwd=120, lineend="butt", columns=c(1:4), col = "lightgray"),
                   #                   "15" = gpar(lwd=20, lineend="butt", columns=c(1:4), col = "lightgray")),
                   mean=estimate, 
                   lower=conf.low.v2, upper=conf.high.v2,
                   title="", xlab="Estimate", 
                   col=fpColors(box="black", lines="black", zero = "gray50"),
                   zero=0, cex=0.9, lineheight = "auto", boxsize=0.3, colgap=unit(6,"mm"),
                   lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2
        ) |> 
        fp_add_header(Outcome = c("Outcome"),
                      Est = c("Estimate (95%CI)"),
                      p.value = c("p-value")
        ) |> 
        fp_set_zebra_style("#EFEFEF") |> 
        fp_add_lines() |> 
        fp_set_style(align = "lrr",
                     txt_gp = fpTxtGp(ticks = gpar(cex = 0.8),
                                      xlab  = gpar(cex = 0.8))
        ))
dev.off()

################################ 6. Problematic Technology Usage ################################
cor.test(ears495.y4$APPUSAGE_daily.usage_all.exl_mins_mean, ears495.y4$MPIQ.total.score)
# data:  ears495.y4$APPUSAGE_daily.usage_all.exl_mins_mean and ears495.y4$MPIQ.total.score
# t = 4.9672, df = 483, p-value = 9.44e-07
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.1340497 0.3035354
# sample estimates:
#   cor 
# 0.220456 
cor.test(ears495.y4$stq_totaltime_typical.y, ears495.y4$MPIQ.total.score)
# data:  ears495.y4$stq_totaltime_typical.y and ears495.y4$MPIQ.total.score
# t = 2.5825, df = 483, p-value = 0.0101
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.02795701 0.20362563
# sample estimates:
#   cor 
# 0.116704 
cor.test(ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean, ears495.y4$MPIQ.total.score)
# data:  ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean and ears495.y4$MPIQ.total.score
# t = 5.9107, df = 483, p-value = 6.445e-09
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.1747186 0.3408709
# sample estimates:
#   cor 
# 0.2597158 
cor.test(ears495.y4$stq_socialmedia_typical, ears495.y4$MPIQ.total.score)
# data:  ears495.y4$stq_socialmedia_typical and ears495.y4$MPIQ.total.score
# t = 5.8724, df = 483, p-value = 7.998e-09
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.1730889 0.3393848
# sample estimates:
#   cor 
# 0.258148 
cor.test(ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean, ears495.y4$SMAQ.total.score)
# data:  ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean and ears495.y4$SMAQ.total.score
# t = 3.0842, df = 382, p-value = 0.002189
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.05668225 0.25201852
# sample estimates:
#   cor 
# 0.1558739 
cor.test(ears495.y4$stq_socialmedia_typical, ears495.y4$SMAQ.total.score)
# data:  ears495.y4$stq_socialmedia_typical and ears495.y4$SMAQ.total.score
# t = 6.8866, df = 382, p-value = 2.36e-11
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.2402378 0.4184821
# sample estimates:
#   cor 
# 0.3323239 
cor.test(ears495.y4$stq_totaltime_typical.y, ears495.y4$VGAQ.total.score)
# data:  ears495.y4$stq_totaltime_typical.y and ears495.y4$VGAQ.total.score
# t = 4.5069, df = 427, p-value = 8.504e-06
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.1208587 0.3016861
# sample estimates:
#   cor 
# 0.2130965 
cor.test(ears495.y4$stq_videogame_typical, ears495.y4$VGAQ.total.score)
# data:  ears495.y4$stq_videogame_typical and ears495.y4$VGAQ.total.score
# t = 10.236, df = 427, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.3645417 0.5168521
# sample estimates:
#   cor 
# 0.4438974 

### pairwise correlations among these variables (screentime usage, EARs related measures, and addiction scores)
add.var <- c("MPIQ.total.score", "SMAQ.total.score", "VGAQ.total.score")
screen.time.var <- c("APPUSAGE_daily.usage_all.exl_mins_mean", "APPUSAGE_daily.usage_SOCIAL.v2_mins_mean", "APPUSAGE_daily.usage_GAME.v2_mins_mean",
                     "stq_totaltime_typical.y", "stq_socialmedia_typical", "stq_videogame_typical",
                     "ears.post.y_total_typical_hrs_recode", "ears.post.y_social.media_typical_hrs_recode", "ears.post.y_video.game_typical_hrs_recode")
full.lst <- c(add.var, screen.time.var)
dt <- ears495.y4
full.lst.recode <- c("MPIQ_Total_Score", "SMAQ_Total_Score", "VGAQ_Total_Score",
                     "EARS_Total_MEAN", "EARS_SocialMedia_MEAN", "EARS_Game_MEAN",
                     "STQ_Total_Mean", "STQ_SocialMedia_Mean", "STQ_Game_Mean",
                     "POST_Total_MEAN", "POST_SocialMedia_MEAN", "POST_Game_MEAN")
# Find the indices of the columns to be relabeled
columns_indices <- match(full.lst, colnames(dt))
# Update the column names with the new labels
colnames(dt)[columns_indices] <- full.lst.recode

# Create empty matrices to store correlation coefficients and p-values
cor_matrix <- matrix(NA, nrow = length(full.lst.recode), ncol = length(full.lst.recode))
# Loop through pairs of variables and calculate correlation coefficients and p-values
# Significance level
alpha <- 0.05
# Loop through pairs of variables and calculate correlation coefficients and p-values
for (i in seq_along(full.lst.recode)) {
  for (j in seq_along(full.lst.recode)) {
    result <- cor.test(dt[[full.lst.recode[i]]], dt[[full.lst.recode[j]]])
    
    # If the p-value is less than the significance level, store the correlation coefficient
    # Otherwise, assign NA
    if (result$p.value < alpha) {
      cor_matrix[i, j] <- paste0(round(result$estimate, 2), " (", ifelse(result$p.value<0.001, "p<0.001", paste0("p=", round(result$p.value, 3))), ")")
    } else {
      cor_matrix[i, j] <- NA
    }
  }
}
# Convert the matrices to a data frame for better printing
cor_matrix <- as.data.frame(t(cor_matrix))
rownames(cor_matrix) <- full.lst.recode
colnames(cor_matrix) <- full.lst.recode
write.csv(cor_matrix, file = "./Output/R5/earsR5/EARS_manual1/A3_figure/addiction_final/pairwise_correlation.csv", na="")

### modeling
dt <- ears495.y4
behav.nm <- c("MPIQ.total.score", "SMAQ.total.score", "VGAQ.total.score")

# predictors
stq.add.nm <- c("APPUSAGE_daily.usage_all.exl_mins_mean", "ears.post.y_total_typical_hrs_recode", "stq_totaltime_typical.y",
                "APPUSAGE_daily.usage_SOCIAL.v2_mins_mean", "ears.post.y_social.media_typical_hrs_recode", "stq_socialmedia_typical",
                "APPUSAGE_daily.usage_GAME.v2_mins_mean", "ears.post.y_video.game_typical_hrs_recode", "stq_videogame_typical")
stq.nm.recode <- c("EARS_Total", "POST_Total", "STQ_Total",
                   "EARS_SocialMedia", "POST_SocialMedia", "STQ_SocialMedia",
                   "EARS_Game", "POST_Game", "STQ_Game")
# Find the indices of the columns to be relabeled
columns_indices <- match(stq.add.nm, colnames(dt))
# Update the column names with the new labels
colnames(dt)[columns_indices] <- stq.nm.recode
# change mins to hrs for EARS measurements
dt[stq.nm.recode[grep("EARS", stq.nm.recode)]] <- dt[stq.nm.recode[grep("EARS", stq.nm.recode)]]/60

for (i in behav.nm) {
  require(lmerTest)
  require(dplyr)
  print(i)
  dt.new <- dt
  dt.new[[i]] <- scale(dt.new[[i]])
  temp_table <- list()
  mod <- list()
  for (j in c(stq.nm.recode)) {
    print(j)
    formula <- paste0(i, " ~ ", j,  " + interview_age + factor(demo_sex_v2_recode) + factor(race_ethnicity) + (1|rel_family_id)")
    mod[[j]] <- lmer(formula, data=dt.new, REML=FALSE)
    temp1 <- tidy(mod[[j]], conf.int = TRUE)
    temp_table[[j]] <- temp1[grepl(j, temp1$term), ]
    temp_table[[j]]$n <- nobs(mod[[j]])
    temp_table[[j]]$Outcome <- i
  }
  lmer_table <- bind_rows(temp_table, .id = "Predictors")
  lmer_table <- lmer_table[order(lmer_table$p.value, decreasing = FALSE),]
  lmer_table$p.adj <- p.adjust(lmer_table$p.value)
  
  out.path <- paste0("./Output/R5/earsR5/EARS_manual1/A3_figure/addiction_final/", i, "_lmer_table.csv")
  write.csv(lmer_table, file=out.path, row.names = FALSE)
}
# summary table
in.path<-"./Output/R5/earsR5/EARS_manual1/A3_figure/addiction_final"
tbl.fn <- list.files(path=in.path, pattern="*total.score_lmer_table.csv", full.names=T, include.dirs=T)
tbl.lst <- lapply(tbl.fn, read.csv, stringsAsFactors=FALSE)
names(tbl.lst) <- sapply(strsplit(tbl.fn, "/"), function(x) x[8]) 
sel.row <- c("EARS_Total", "POST_Total", "STQ_Total",
             "EARS_SocialMedia", "POST_SocialMedia", "STQ_SocialMedia", 
             "EARS_Game", "POST_Game", "STQ_Game")
for (i in names(tbl.lst)) {
  new.nm <- sub("_lmer_table.csv", "", i)
  tbl.lst[[i]] <- tbl.lst[[i]][which(tbl.lst[[i]]$Predictors %in% sel.row),]
  tbl.lst[[i]]$p.value.recode <- ifelse(tbl.lst[[i]]$p.value<0.001, "<0.001", paste0("=", sprintf("%.3f", tbl.lst[[i]]$p.value)))
  tbl.lst[[i]][paste0("estimate_", new.nm)] <- paste0(sprintf("%.3f", tbl.lst[[i]]$estimate), " (", sprintf("%.3f", tbl.lst[[i]]$conf.low), ", ", sprintf("%.3f", tbl.lst[[i]]$conf.high), ")")
  tbl.lst[[i]] <- tbl.lst[[i]][which(tbl.lst[[i]]$p.value<0.05), c("Predictors", paste0("estimate_", new.nm))]
}
tbl.comb <- Reduce(function(x, y) merge(x, y, all=TRUE),
                   tbl.lst)
tbl.comb <- merge(data.frame(Predictors=sel.row), tbl.comb, all.x = TRUE) %>%
  arrange(Predictors)
write.csv(tbl.comb, file = "./Output/R5/earsR5/EARS_manual1/A3_figure/addiction_final/summary_table.csv", row.names = FALSE, na="")


### Include both subjective and objective measures into the same model
comb.nm <- c("EARS_Total + STQ_Total",
             "EARS_SocialMedia + STQ_SocialMedia",
             "EARS_Game + STQ_Game")
for (i in c("MH_cbcl_scr_syn_internal_r", "MH_cbcl_scr_syn_external_r")) {
#for (i in behav.nm) {
  print(i)
  
  dt.new <- dt
  dt.new[[i]] <- scale(dt.new[[i]])
  
  temp_table <- list()
  mod <- list()
  for (j in comb.nm) {
    require(lmerTest)
    require(dplyr)
    print(j)
    formula <- paste0(i, " ~ ", j, " + interview_age + factor(demo_sex_v2_recode) + factor(race_ethnicity) + (1|rel_family_id)")
    mod[[j]] <- lmer(formula, data=dt.new)
    temp1 <- tidy(mod[[j]], conf.int = TRUE)
    temp_table[[j]] <- temp1[grepl("EARS_|STQ_", temp1$term), ]
    temp_table[[j]]$n <- nobs(mod[[j]])
    temp_table[[j]]$Outcomes <- i
  }
  lmer_table <- bind_rows(temp_table, .id = "Combination")
  #lmer_table <- lmer_table[order(lmer_table$p.value, decreasing = FALSE),]
  lmer_table$p.adj <- p.adjust(lmer_table$p.value)
  
  out.path <- paste0("./Output/R5/earsR5/EARS_manual1/A3_figure/addiction_final/", i, "_sub.obj.comb_lmer_table.csv")
  write.csv(lmer_table, file=out.path, row.names = FALSE)
}
# summary table
in.path<-"./Output/R5/earsR5/EARS_manual1/A3_figure/addiction_final"
tbl.fn <- list.files(path=in.path, pattern="*.total.score_sub.obj.comb_lmer_table.csv", full.names=T, include.dirs=T)
tbl.lst <- lapply(tbl.fn, read.csv, stringsAsFactors=FALSE)
names(tbl.lst) <- sapply(strsplit(tbl.fn, "/"), function(x) x[8]) 
tt <- tbl.lst$MPIQ.total.score_sub.obj.comb_lmer_table.csv[c("Combination", "term")]
for (i in names(tbl.lst)) {
  new.nm <- sub("_sub.obj.comb_lmer_table.csv", "", i)
  tbl.lst[[i]]$p.value.recode <- ifelse(tbl.lst[[i]]$p.value<0.001, "<0.001", paste0("=", sprintf("%.3f", tbl.lst[[i]]$p.value)))
  tbl.lst[[i]]$estimate <- paste0(sprintf("%.3f", tbl.lst[[i]]$estimate), " (p", tbl.lst[[i]]$p.value.recode, ")")
  # tbl.lst[[i]] <- tbl.lst[[i]][which(tbl.lst[[i]]$p.value<0.05), c("Outcomes", "term", "estimate")] %>%
  #   spread(key = term, value = estimate)
  # colnames(tbl.lst[[i]]) <- c("Outcomes", paste0("Est.", new.nm, "_", colnames(tbl.lst[[i]])[2:3]))
  tbl.lst[[i]] <- tbl.lst[[i]][which(tbl.lst[[i]]$p.value<0.05), c("Combination", "term", "estimate")]
  colnames(tbl.lst[[i]]) <- c("Combination", "term", paste0("Est.", new.nm))
}
tbl.comb <- Reduce(function(x, y) merge(x, y, all=TRUE),
                   tbl.lst)
tbl.comb <- merge(tt, tbl.comb, all.x = TRUE, sort = FALSE)
write.csv(tbl.comb, file = "./Output/R5/earsR5/EARS_manual1/A3_figure/addiction_final/summary_comb_table.csv", row.names = FALSE, na="")

#----------------additional checking------------------------------
quantile(ears495.y4$stq.recode_socialmedia_typical.hrs.perwk, c(0.2,0.8), na.rm=T)
# 50% 80% 
# 5.75 15.4 
cor(ears495.y4$stq.recode_socialmedia_typical.hrs.perwk[which(ears495.y4$stq.recode_socialmedia_typical.hrs.perwk>15.4)],
    ears495.y4$stq.recode_videogame_typical.hrs.perwk[which(ears495.y4$stq.recode_socialmedia_typical.hrs.perwk>15.4)], use="na.or.complete")
# [1] 0.3328228
cor(ears495.y4$stq.recode_socialmedia_typical.hrs.perwk[which(ears495.y4$stq.recode_socialmedia_typical.hrs.perwk<5.75)],
    ears495.y4$stq.recode_videogame_typical.hrs.perwk[which(ears495.y4$stq.recode_socialmedia_typical.hrs.perwk<5.75)], use="na.or.complete")
# [1] 0.08746239

cor(ears495.y4$stq.recode_socialmedia_typical.hrs.perwk[which(ears495.y4$SMAQ.mean.score>=2)],
    ears495.y4$stq.recode_videogame_typical.hrs.perwk[which(ears495.y4$SMAQ.mean.score>=2)], use="na.or.complete")

cor(ears495.y4$stq.recode_socialmedia_typical.hrs.perwk[which(ears495.y4$SMAQ.mean.score==1)],
    ears495.y4$stq.recode_videogame_typical.hrs.perwk[which(ears495.y4$SMAQ.mean.score==1)], use="na.or.complete")
