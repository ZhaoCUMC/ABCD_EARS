################################################################################
################################## ABCD EARS ###################################
##############################  Code for revison ###############################
#################################   Feb 2025  ##################################

setwd("/Users/xueweihan/Desktop/ABCD_EARS_Manu1_revision/")

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
# cleaned EARS data
#ears495 <- read.csv("./Output/R5/earsR5/Appusage/dt.ears495.master.y4.csv")
ears495 <- read.csv("./Data/dt.ears495.master.y4.csv")

# full sample
#master.file <- read.csv("../Master_File/Output/master.file.csv")
master.file <- read.csv("./Data/master.file.csv")
## recode demo variables
master.file$interview_age_yrs <- master.file$interview_age/12
master.file$parental_educ_highest_recode <- ifelse(master.file$parental_educ_highest_recode=="< HS Diploma" | master.file$parental_educ_highest_recode=="HS Diploma/GED", 
                                               "High School or Below", master.file$parental_educ_highest_recode)
master.file$parental_educ_highest_recode <- factor(master.file$parental_educ_highest_recode, 
                                               levels = c("Post Graduate Degree", "Bachelor", 
                                                          "Some College", "High School or Below"))
master.file$parental_educ_highest_recode_l <- ifelse(master.file$parental_educ_highest_recode_l=="< HS Diploma" | master.file$parental_educ_highest_recode_l=="HS Diploma/GED", 
                                                   "High School or Below", master.file$parental_educ_highest_recode_l)
master.file$parental_educ_highest_recode_l <- factor(master.file$parental_educ_highest_recode_l, 
                                                   levels = c("Post Graduate Degree", "Bachelor", 
                                                              "Some College", "High School or Below"))
master.file$family_income_recode_l <- ifelse(master.file$family_income_recode_l=="[>=200k]" | master.file$family_income_recode_l=="[100-200k]", 
                                         "[>=100k]", master.file$family_income_recode_l)
master.file$family_income_recode_l <- factor(master.file$family_income_recode_l, 
                                         levels = c("[>=100k]", "[50-100k]", "[<50k]"))
master.file$race_ethnicity_recode <- factor(master.file$race_ethnicity_recode, 
                                        levels = c("White", "Black", "Hispanic", "Other"))
master.file$parent_married_l <- factor(master.file$parent_married_l, labels = c("Not Married", "Married"))
master.file$parent_married_l <- factor(master.file$parent_married_l, levels = c("Married", "Not Married"))
master.file$demo_sex_v2_recode <- factor(master.file$demo_sex_v2_recode, levels = c("Male", "Female"))

### create a new pds variable for additional tests
master.file$PH_pds.p_y_cat <- ifelse(is.na(master.file$PH_pds.p_male_female_cat_2_recode) & !is.na(master.file$PH_pds.y_male_female_cat_2_recode),
                                master.file$PH_pds.y_male_female_cat_2_recode, master.file$PH_pds.p_male_female_cat_2_recode)
table(master.file$PH_pds.p_y_cat, master.file$eventname)
master.file$PH_pds.p_y_cat_recode <- ifelse(master.file$PH_pds.p_y_cat<=2, 2,
                                            ifelse(master.file$PH_pds.p_y_cat>=4, 4, 3))
table(master.file$PH_pds.p_y_cat_recode, master.file$eventname)
master.file$PH_pds.p_y_cat_recode <- factor(master.file$PH_pds.p_y_cat_recode, levels = 4:2)

sum(table(master.file$PH_pds.p_y_cat_recode[which(master.file$eventname==4& master.file$src_subject_id %in% ears495$src_subject_id)]))

######### fill NA's in family_income_recode_l, PH_pds.p_y_cat_recode, parent_married_l, and parental_educ_highest_recode_l
var.nm <- c("family_income_recode_l", "PH_pds.p_y_cat_recode", 
            "parent_married_l", "parental_educ_highest_recode_l")
tt <- master.file[c("src_subject_id", "eventname", var.nm)] %>%
  arrange(src_subject_id, eventname) %>%
  group_by(src_subject_id) %>%
  mutate(last_obs_pds_b4.y4 = last(PH_pds.p_y_cat_recode[which(eventname<4 & !is.na(PH_pds.p_y_cat_recode))])) %>%
  fill(all_of(var.nm), .direction = "downup") %>%
  rename_with(~ paste0(.x, "_filled"), all_of(var.nm)) %>%  # Rename filled variables
  ungroup()
tt <- merge(master.file[c("src_subject_id", "eventname", var.nm)],
            tt)
tt.y4 <- tt[which(tt$eventname==4),]
tt.y4$PH_pds.p_y_cat_recode_filled[which(is.na(tt.y4$PH_pds.p_y_cat_recode) & tt.y4$last_obs_pds_b4.y4==2)] <- NA

# year4
master.file.y4 <- master.file[which(master.file$eventname==4),]
master.file.y4 <- merge(master.file.y4, tt.y4)
ears495.y4 <- merge(ears495[c("src_subject_id", colnames(ears495)[grep("APPUSAGE_|ears.post.y", colnames(ears495))])], 
                    master.file.y4, all.x = TRUE)
# longitudinal version
master.file <- merge(master.file, tt)
# check missings after imputation
missing_counts <- ears495.y4 %>%
  group_by(src_subject_id) %>%
  summarise(missing_count_pds = sum(is.na(PH_pds.p_y_cat_recode_filled)),
            missing_count_income = sum(is.na(family_income_recode_l_filled)),
            missing_count_married = sum(is.na(parent_married_l_filled)),
            missing_count_edu = sum(is.na(parental_educ_highest_recode_l_filled)),
            missing_count_pds_income = sum(is.na(PH_pds.p_y_cat_recode_filled) | is.na(family_income_recode_l_filled)))  # Check missing values in filled data

apply(missing_counts[,-1], 2, table)

################################## Calculate Cronbach’s Alpha ##################################
### psychometric properties of the scales for the effective sample to the description of respective scales
### https://rforhr.com/cronbachsalpha.html
vgaq.nm <- c("screentime_vgaq1","screentime_vgaq2","screentime_vgaq3","screentime_vgaq4","screentime_vgaq5","screentime_vgaq6") #freq:1|2|3|4|5|6
smaq.nm <- c("screentime_smqa1","screentime_smqa2","screentime_smqa3","screentime_smqa4","screentime_smqa5","screentime_smqa6") #freq:1|2|3|4|5|6

library(psych)
# library(ltm)
alpha.lst <- list()
a1 <- alpha(ears495.y4[vgaq.nm])
alpha.lst[["ears_vgaq"]] <- a1$total
# tt <- ears495.y4[vgaq.nm]
# tt <- tt[complete.cases(tt),]
# cronbach.alpha(tt)

a2 <- alpha(ears495.y4[smaq.nm])
alpha.lst[["ears_smaq"]] <- a2$total

a3 <- alpha(master.file.y4[vgaq.nm])
alpha.lst[["y4_vgaq"]] <- a3$total

a4 <- alpha(master.file.y4[smaq.nm])
alpha.lst[["y4_smaq"]] <- a4$total

alpha.final <- bind_rows(alpha.lst, .id = "data")

# reported omega in the Revision 02/01/2025
o1 <- omega(ears495.y4[vgaq.nm])$omega.tot
o2 <- omega(ears495.y4[smaq.nm])$omega.tot
o3 <- omega(master.file.y4[vgaq.nm])$omega.tot
o4 <- omega(master.file.y4[smaq.nm])$omega.tot

alpha.final$omega <- c(o1, o2, o3, o4)
write.csv(alpha.final, file = "./Output/cronbach_alpha_vgaq_smaq_total.csv", row.names = FALSE)

################################## pairwise correlations ##################################
### pairwise correlations among these variables (screentime usage, EARs related measures, and addiction scores)
dt <- ears495.y4
dt$APPUSAGE_daily.usage_all.exl_hrs_mean <- dt$APPUSAGE_daily.usage_all.exl_mins_mean/60
dt$APPUSAGE_daily.usage_SOCIAL.v2_hrs_mean <- dt$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean/60
dt$APPUSAGE_daily.usage_GAME.v2_hrs_mean <- dt$APPUSAGE_daily.usage_GAME.v2_mins_mean/60

dt$diff_social.media.v2_ears_stq <- dt$APPUSAGE_daily.usage_SOCIAL.v2_hrs_mean - dt$stq_socialmedia_typical
#cutoff <- 0.5
#cutoff <- 1
dt$grp.index <- ifelse(abs(dt$diff_social.media.v2_ears_stq) < 0.5, "accurate", "biased")
# dt$grp.index <- ifelse(dt$diff_social.media.v2_ears_stq <= -cutoff, "over",
#                        ifelse(dt$diff_social.media.v2_ears_stq>= cutoff, "under", "accurate"))
dt$grp.index <- factor(dt$grp.index)
table(dt$grp.index)

add.var <- c("SMAQ.total.score", "VGAQ.total.score")
#mh.var <- c("MH_cbcl_scr_syn_internal_r", "MH_cbcl_scr_syn_external_r")
screen.time.var <- c("APPUSAGE_daily.usage_all.exl_hrs_mean", "APPUSAGE_daily.usage_SOCIAL.v2_hrs_mean", "APPUSAGE_daily.usage_GAME.v2_hrs_mean",
                     "stq_totaltime_typical.y", "stq_socialmedia_typical", "stq_videogame_typical",
                     "ears.post.y_total_typical_hrs_recode", "ears.post.y_social.media_typical_hrs_recode", "ears.post.y_video.game_typical_hrs_recode")
full.lst <- c(add.var, screen.time.var)

full.lst.recode <- c("SMAQ Total Score", "VGAQ Total Score",
                     #"CBCL Internalizing", "CBCL Externalizing",
                     "EARS Total", "EARS SocialMedia", "EARS Gaming",
                     "SR(12-mos) Total", "SR(12-mos) SocialMedia", "SR(12-mos) Gaming",
                     "SR(3-wks) Total", "SR(3-wks) SocialMedia", "SR(3-wks) Gaming")
# Find the indices of the columns to be relabeled
columns_indices <- match(full.lst, colnames(dt))
# Update the column names with the new labels
colnames(dt)[columns_indices] <- full.lst.recode

### all
dt.all <- dt[full.lst.recode]

library(corrplot)
library(RColorBrewer)
M = cor(dt.all, use = "pairwise.complete.obs")
testRes = cor.mtest(dt.all, conf.level = 0.95, use = "pairwise.complete.obs")

png("./Output/correlation_plot_all_v2.png", width = 8, height = 8, units = "in", res = 300)  # Open a PDF device
# corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig = 'blank',
#          addCoef.col = 'black', number.cex = 0.8, #order = 'AOE', 
#          order = "original", diag = FALSE)
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'upper', insig='blank',
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.9,
         number.cex = 0.8,
         col = rev(COL2('RdBu', 200)),
         #sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         order = 'original', diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, sprintf("%.2f", p1$corr))
dev.off()

### accurate
dt.accurate <- dt[which(dt$grp.index=="accurate"), full.lst.recode]

M = cor(dt.accurate, use = "pairwise.complete.obs")
testRes = cor.mtest(dt.accurate, conf.level = 0.95, use = "pairwise.complete.obs")
library(corrplot)
png("./Output/correlation_plot_accurate_v2.png", width = 8, height = 8, units = "in", res = 300)  # Open a PDF device
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'upper', insig='blank',
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.9,
         number.cex = 0.8,
         col = rev(COL2('RdBu', 200)),
         #sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         order = 'original', diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, sprintf("%.2f", p1$corr))
dev.off()

### biased
dt.biased <- dt[which(dt$grp.index=="biased"), full.lst.recode]

M = cor(dt.biased, use = "pairwise.complete.obs")
testRes = cor.mtest(dt.biased, conf.level = 0.95, use = "pairwise.complete.obs")
library(corrplot)
png("./Output/correlation_plot_biased_v2.png", width = 8, height = 8, units = "in", res = 300)  # Open a PDF device
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'upper', insig='blank',
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.9,
         number.cex = 0.8,
         col = rev(COL2('RdBu', 200)),
         #sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         order = 'original', diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, sprintf("%.2f", p1$corr))
dev.off()

### combine
library(magick)
library(grid)
library(cowplot)

fig1 <- png::readPNG("./Output/correlation_plot_accurate_v2.png")
fig2 <- png::readPNG("./Output/correlation_plot_biased_v2.png")

p1 <- ggdraw() + draw_image(fig1)
p2 <- ggdraw() + draw_image(fig2)

combined_plot <- plot_grid(
  p1, p2,              # Figures
  labels = c("A", "B"),   # Labels
  ncol = 1,                         # Number of columns
  align = "hv",                     # Align horizontally and vertically
  axis = "tblr",                    # Align axis (top, bottom, left, right)
  rel_widths = c(1, 1),             # Adjust relative widths
  rel_heights = c(1, 1)             # Adjust relative heights
)

# Set white background (optional)
combined_plot <- combined_plot +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
ggsave(
  filename = "./Output/correlation_plot_accurate.vs.biased_v2.pdf", 
  plot = combined_plot, 
  width = 5, 
  height = 8, 
  dpi = 1000,          # High resolution
  limitsize = FALSE   # Allow saving larger sizes
)

################################ 4. Table 3 obj-sub discrepancy ################################
## ears-stq
ears495.y4$diff_social.media.v2_ears_stq <- ears495.y4$APPUSAGE_daily.usage_SOCIAL.v2_mins_mean/60 - ears495.y4$stq_socialmedia_typical

### multinomial logistic regression (results in Table 3)
library(nnet)

cog.nm <- c("nihtbx_picvocab_uncorrected", "nihtbx_list_uncorrected",
            "nihtbx_picture_uncorrected", "nihtbx_reading_uncorrected")

cutoff <- 0.5
#cutoff <- 1
ears495.y4$grp.index <- ifelse(ears495.y4$diff_social.media.v2_ears_stq <= -cutoff, "over",
                               ifelse(ears495.y4$diff_social.media.v2_ears_stq>= cutoff, "under", "accurate"))
ears495.y4$grp.index <- factor(ears495.y4$grp.index)
table(ears495.y4$grp.index)

mod <- list()
coef_table <- list()
for (i in cog.nm) {
  print(i)
  label <- unlist(strsplit(i, "_"))[2]
  # dt <- ears495.y4[c("grp.index", i,
  #                    "interview_age_yrs", "demo_sex_v2_recode", "race_ethnicity_recode",
  #                    "family_income_recode_l", "parental_educ_highest_recode",
  #                    "parent_married_l", #"PH_pds.y_male_female_cat_2_recode"#,
  #                    "PH_pds.p_y_cat_recode"
  #                    )]
  dt <- ears495.y4[c("grp.index", i,
                     "interview_age_yrs", "demo_sex_v2_recode", "race_ethnicity_recode",
                     "family_income_recode_l_filled", "parental_educ_highest_recode_l_filled",
                     "parent_married_l_filled"#, "PH_pds.p_y_cat_recode_filled"
  )]
  dt <- dt[complete.cases(dt),]
  table(dt$grp.index)
  
  mod[[i]] <- multinom(grp.index ~ ., data = dt)
  # summary(mod[[i]])
  temp <- tidy(mod[[i]], exponentiate = TRUE, conf.int = TRUE)
  temp$est <- paste0(sprintf("%.2f", temp$estimate), " [", sprintf("%.2f", temp$conf.low), "-", sprintf("%.2f", temp$conf.high), "]")
  temp$p <- paste0(ifelse(temp$p.value<0.001, "<0.001", sprintf("%.3f", temp$p.value)))
  temp <- temp[c("y.level", "term", "est", "p")] %>%
    pivot_wider(
      names_from = y.level,
      values_from = c(est, p)
    )
  temp$term[which(temp$term==i)] <- "cog_function"
  
  temp <- temp[c("term", "est_over", "p_over", "est_under", "p_under")] %>%
    setNames(c("term", paste0(label, "_", c("est_over", "p_over", "est_under", "p_under"))))
  n.grp <- data.frame(table(dt$grp.index))
  temp <- rbind(temp, n=c("n", paste(n.grp$Freq[1:2], collapse = ","), paste(n.grp$Freq[1:2], collapse = ","),
                          paste(n.grp$Freq[c(1,3)], collapse = ","), paste(n.grp$Freq[c(1,3)], collapse = ",")))
  coef_table[[i]] <- temp
}
final_tbl <- Reduce(function(x, y) merge(x, y, sort=FALSE), 
                    coef_table)
final_tbl_over <- final_tbl[c("term", colnames(final_tbl)[grep("over", colnames(final_tbl))])]
final_tbl_under <- final_tbl[c("term", colnames(final_tbl)[grep("under", colnames(final_tbl))])]
write.csv(final_tbl_over, file = paste0("./Output/cog_multinom_over_", cutoff, "_no.pds_filled.csv"), row.names = FALSE)
write.csv(final_tbl_under, file = paste0("./Output/cog_multinom_under_", cutoff, "_no.pds_filled.csv"), row.names = FALSE)


### binary version: logistic regression
ears495.y4$grp.index <- ifelse(abs(ears495.y4$diff_social.media.v2_ears_stq) < 0.5, 0, 1)
#ears495.y4$grp.index <- ifelse(abs(ears495.y4$diff_social.media.v2_ears_stq) < 1, 0, 1)

table(ears495.y4$grp.index)

mod <- list()
coef_table <- list()
for (i in cog.nm) {
  print(i)
  label <- unlist(strsplit(i, "_"))[2] # change back to 2 if no NC_ in front of nihtbx_
  # dt <- ears495.y4[c("grp.index", i,
  #                    "interview_age_yrs", "demo_sex_v2_recode", "race_ethnicity_recode",
  #                    "family_income_recode_l", "parental_educ_highest_recode",
  #                    "parent_married_l", #"PH_pds.y_male_female_cat_2_recode"#,
  #                    "PH_pds.p_y_cat_recode"
  # )]
  dt <- ears495.y4[c("grp.index", i,
                     "interview_age_yrs", "demo_sex_v2_recode", "race_ethnicity_recode",
                     "family_income_recode_l_filled", "parental_educ_highest_recode_l_filled",
                     "parent_married_l_filled"#, "PH_pds.p_y_cat_recode_filled"
  )]
  dt <- dt[complete.cases(dt),]
  table(dt$grp.index)
  
  mod[[i]] <- glm(grp.index ~ ., data = dt, family = binomial(link="logit"))
  # summary(mod[[i]])
  temp <- tidy(mod[[i]], exponentiate = TRUE, conf.int = TRUE)
  temp$est <- paste0(sprintf("%.2f", temp$estimate), " [", sprintf("%.2f", temp$conf.low), "-", sprintf("%.2f", temp$conf.high), "]")
  temp$p <- paste0(ifelse(temp$p.value<0.001, "<0.001", sprintf("%.3f", temp$p.value)))
  temp <- temp[c("term", "est", "p")]
  temp$term[which(temp$term==i)] <- "cog_function"
  
  temp <- temp[c("term", "est", "p")] %>%
    setNames(c("term", paste0(label, "_", c("est", "p"))))
  n.grp <- data.frame(table(dt$grp.index))
  temp <- rbind(temp, n=c("n", paste(n.grp$Freq[1:2], collapse = ","), paste(n.grp$Freq[1:2], collapse = ",")))
  coef_table[[i]] <- temp
}

final_tbl <- Reduce(function(x, y) merge(x, y, sort=FALSE), 
                    coef_table)
write.csv(final_tbl, file = paste0("./Output/cog.y4_logistic_", 1, "_no.pds_filled.csv"), row.names = FALSE)

### lmer models: cognitive functions as outcomes
ears495.y4$grp.index <- ifelse(abs(ears495.y4$diff_social.media.v2_ears_stq) < 0.5, 0, 1)
#ears495.y4$grp.index <- ifelse(abs(ears495.y4$diff_social.media.v2_ears_stq) < 1, 0, 1)

table(ears495.y4$grp.index)

mod <- list()
coef_table <- list()
for (i in cog.nm) {
  print(i)
  label <- unlist(strsplit(i, "_"))[2] # change back to 2 if no NC_ in front of nihtbx_
  # dt <- ears495.y4[c("grp.index", i,
  #                    "interview_age_yrs", "demo_sex_v2_recode", "race_ethnicity_recode",
  #                    "family_income_recode_l", "parental_educ_highest_recode",
  #                    "parent_married_l", #"PH_pds.y_male_female_cat_2_recode"#,
  #                    "PH_pds.p_y_cat_recode"
  # )]
  dt <- ears495.y4[c("grp.index", i,
                     "interview_age_yrs", "demo_sex_v2_recode", "race_ethnicity_recode",
                     "family_income_recode_l_filled", "parental_educ_highest_recode_l_filled",
                     "parent_married_l_filled", "PH_pds.p_y_cat_recode_filled", "rel_family_id"
  )]
  dt <- dt[complete.cases(dt),]
  table(dt$grp.index)
  
  formula <- paste0(i, " ~ grp.index + interview_age_yrs + demo_sex_v2_recode + race_ethnicity_recode + family_income_recode_l_filled + parental_educ_highest_recode_l_filled + parent_married_l_filled + PH_pds.p_y_cat_recode_filled + (1|rel_family_id)")
  mod[[i]] <- lmer(formula, data = dt, REML=FALSE)
  # summary(mod[[i]])
  temp <- tidy(mod[[i]], conf.int = TRUE)
  temp$est <- paste0(sprintf("%.2f", temp$estimate), " [", sprintf("%.2f", temp$conf.low), "-", sprintf("%.2f", temp$conf.high), "]")
  temp$p <- paste0(ifelse(temp$p.value<0.001, "<0.001", sprintf("%.3f", temp$p.value)))
  temp <- temp[c("term", "est", "p")]
  
  temp <- temp[c("term", "est", "p")] %>%
    setNames(c("term", paste0(label, "_", c("est", "p"))))
  n.grp <- data.frame(table(dt$grp.index))
  temp <- rbind(temp, n=c("n", paste(n.grp$Freq[1:2], collapse = ","), paste(n.grp$Freq[1:2], collapse = ",")))
  coef_table[[i]] <- temp
}

final_tbl <- Reduce(function(x, y) merge(x, y, sort=FALSE), 
                    coef_table)

### line plots
ears495.y4$grp.index <- ifelse(abs(ears495.y4$diff_social.media.v2_ears_stq) < 0.5, "Accurate", "Biased")
dt <- merge(master.file, ears495.y4[c("src_subject_id", "grp.index")])
dt.plot <- dt[which(!is.na(dt$grp.index) & dt$eventname %in% c(0,2,4)),] %>%
  group_by(eventname, grp.index) %>%
  summarize(mean_cognitive_functions = mean(nihtbx_reading_uncorrected, na.rm = TRUE)) %>%
  ungroup()

# Create the line plot using the summarized data
p <- ggplot(dt.plot, aes(x = factor(eventname), y = mean_cognitive_functions, group = factor(grp.index), color = factor(grp.index))) +
  geom_line() +  # Add lines
  geom_point() +  # Add points for better visualization
  labs(
    x = "Event Name", 
    y = "Mean Reading", 
    color = "Group Index", 
    title = "Line Plot of Mean Cognitive Functions by Event Name"
  ) +
  theme_bw()
ggsave("./Output/lineplot_reading.png", p, width = 6, height = 4)


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

###### table1 configuration ######

dt <- master.file.y4

dt$grp.index <- with(dt, ifelse(src_subject_id %in% ears495.y4$src_subject_id, 1, 2))
# dt$grp.index <- factor(dt$grp.index, labels = c("People in EARS", "People Not in EARS"))
# check if the number is correct
table(dt$grp.index)

label(dt$interview_age_yrs) <- "Age (yrs)"
label(dt$demo_sex_v2_recode) <- "Sex at Birth"
label(dt$race_ethnicity_recode) <- "Race Ethnicity"
label(dt$family_income_recode_l) <- "Family Income (US$)"
label(dt$parental_educ_highest_recode) <- "Parental Education"
label(dt$parent_married_l) <- "Parental Marital Status"
label(dt$PH_pds.p_y_cat_recode) <- "Pubertal Development"
###### END configuration ######

tbl1 <- table1(~ interview_age_yrs + demo_sex_v2_recode + race_ethnicity_recode
               + family_income_recode_l + parental_educ_highest_recode + parent_married_l + PH_pds.p_y_cat_recode
               | factor(grp.index), 
               data=dt, 
               rowlabelhead = "", 
               caption = "Table 1. Demographic Characteristics between Participants with and without EARS Data", 
               digits = 4,
               overall = FALSE,
               extra.col=list(`P-value`=pvalue))
t1flex(tbl1) %>% 
  save_as_docx(path="./Output/table1.495.4259.final.v2.docx")

tbl1 <- table1(~ interview_age_yrs + demo_sex_v2_recode + race_ethnicity_recode
               + family_income_recode_l_filled + parental_educ_highest_recode_l_filled + parent_married_l_filled
               + PH_pds.p_y_cat_recode_filled
               | factor(grp.index), 
               data=dt, 
               rowlabelhead = "", 
               caption = "Table 1. Demographic Characteristics between Participants with and without EARS Data", 
               digits = 4,
               overall = FALSE,
               extra.col=list(`P-value`=pvalue))
t1flex(tbl1) %>% 
  save_as_docx(path="./Output/table1.495.4259.final.filled.docx")
