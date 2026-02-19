#Code for analyses of illustrations feedback
#written by Audrey Arner

setwd("~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/")
rm(list=ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(corrplot)
library(emmeans)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
library(lme4)
library(lmerTest)
library(ggmap)
library(googlesheets4)

############################################################
###      OA read in data and clean                       ###
############################################################
interviews=read.csv('~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/Illustrations2025_DATA_2025-09-04_1400.csv')

traditional=read.csv('~/Library/CloudStorage/Box-Box/Lea Lab/Audrey_Arner/Lab/Malaysia/data/old/oahelp/data/traditional_lifestyle.csv')
medical=read.csv('~/Library/CloudStorage/Box-Box/Lea Lab/Audrey_Arner/Lab/Malaysia/data/old/oahelp/data/medical.csv')
personal=read.csv('~/Library/CloudStorage/Box-Box/Lea Lab/Audrey_Arner/Lab/Malaysia/data/old/oahelp/data/personal_information.csv')
urb_score=read.csv('~/Library/CloudStorage/Box-Box/Audrey/Lab/RNAseq/OAHeLP_location_urbanicity_scores_2026-02-18.txt', sep = "\t")

oa_data = left_join(medical, personal, by = "rid", suffix = c(".medical", ".personal")) #join data
oa_data = left_join(traditional, oa_data, by = "rid") #join data

#get age for metadata
oa_data$age=as.numeric(as.Date(oa_data$med_date)-as.Date(oa_data$date_of_birth))/365

#take out individuals with NA for TID
oa_data = oa_data[!is.na(oa_data$tid), ]

#merge only with values I want
interview_meta = interviews %>%
  left_join(
    oa_data %>% dplyr::select(rid, highest_education_stage, date_of_birth, ),
    by = c("rid_illus_malay" = "rid")
  )

#calculate age from birth date
interview_meta$age = as.numeric(
  as.Date(interview_meta$date_illus_malay, format = "%m/%d/%y") -
    as.Date(interview_meta$date_of_birth, format = "%Y-%m-%d")) / 365

#make illustration interview age into a single column
interview_meta = interview_meta %>%
  mutate(age_illus_interview = coalesce(age_illust_malay, age_estimated_malay))

#look into age matching
interview_meta$age_match=ifelse(interview_meta$age_illus_interview == floor(interview_meta$age), 1, 0)
table(interview_meta$age_match)
table(interview_meta$know_age_illust_malay, interview_meta$age_match) #is it only people who report not knowing their age?

#we ultimately want to use the age that is "correct", in our case using birth date from medical
interview_meta$age_final = ifelse(is.na(interview_meta$age)==F, interview_meta$age, interview_meta$age_illus_interview)

#check how reported highest education aligns
interview_meta$education_match = ifelse(is.na(interview_meta$highest_education_stage_illus_malay) ==F & interview_meta$highest_education_stage_illus_malay == interview_meta$highest_education_stage, 1, 0)
table(interview_meta$education_match) #what is the match and mismatch 
table(interview_meta$highest_education_stage_illus_malay, interview_meta$highest_education_stage)

#if don't have education level, grab from interview
interview_meta$highest_education_stage_illus_malay = ifelse(is.na(interview_meta$highest_education_stage_illus_malay) == T & is.na(interview_meta$highest_education_stage)==F, interview_meta$highest_education_stage, interview_meta$highest_education_stage_illus_malay)

#does it match where people who say they have 0 years of schooling also put down no school?
table(interview_meta$highest_education_stage, interview_meta$years_schooling_illust_malay) #yes

table(interview_meta$age, interview_meta$years_schooling_illust_malay) #yes

#know this individual has 0 years of schooling but right now it is empty
interview_meta$years_schooling_illust_malay[interview_meta$rid == 97] = 0

#give location places to Laba & Melala
interview_meta$interview_location_illus_malay=ifelse(interview_meta$location_other_malay == "Melela", 46, interview_meta$interview_location_illus_malay)
interview_meta$interview_location_illus_malay=ifelse(interview_meta$location_other_malay == "Laba", 48, interview_meta$interview_location_illus_malay)

#add urbanicity score to meta data
interview_meta$urb_score = urb_score$urb_score[match(interview_meta$interview_location_illus_malay, urb_score$village_id)]
############################################################
###      demographics                                    ###
############################################################
#what was the sex and gender breakdown?
table(interview_meta$sex_illust_malay)
interview_meta %>% 
  mutate(age_bins = cut(age_final, breaks = seq(0, 100, by = 10), 
                        labels = c("0-10","10-20","20-30","30-40","40-50",
                                   "50-60","60-70","70-80","80-90","90-100"), 
                        include.lowest = TRUE),
         sex_illust_malay = stringr::str_to_title(sex_illust_malay),
         age_bins = if_else(age_bins %in% c("0-10","10-20","20-30"), "18-30", 
                            if_else(age_bins %in% c("80-90","90-100"), "80+", age_bins))) %>% 
  filter(!age<18, 
         !is.na(age), 
         !is.na(sex_illust_malay)) %>% 
  ggplot(aes(x = age_bins, fill = sex_illust_malay)) + 
  geom_bar(color = "black", position = position_dodge(width = 0.5, preserve = 'single')) + 
  scale_fill_manual(values = c("#C7CB85","#7EA172"), labels = c("female", "male")) + 
  theme_classic(base_size = 24) + 
  theme(axis.text.x = element_text(size = 20)) + 
  labs(x = "Age (deciles)", y = "Count", fill = "Sex") + guides(fill = guide_legend(title = NULL))
ggsave(filename="illustrations_age_gender.png", width=9, height=10)

#breakdown by schooling level
table(interview_meta$highest_education_stage_illus_malay)

ggplot(interview_meta, aes(x = as.factor(highest_education_stage_illus_malay))) + 
  geom_bar(color = "black", fill = "#A8B97D") + 
  theme_classic(base_size = 24) + 
  scale_x_discrete(labels = c(
      "0" = "None",
      "1" = "Primary\nschool",
      "2" = "Secondary\nschool",
      "3" = "College" )) +
  theme(axis.text.x = element_text(size = 20)) + 
  labs(x = "Highest education level", y = "Count")  

ggsave(filename="illustrations_education_level.png", width=8, height=5)
############################################################
###      map                                             ###
############################################################
#orang Asli village register with locations
register = read.delim("~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/oa_village_register.csv", sep = ",")

#names of villages, year visited for this project, phase of project
villages=c() #redacted
year = c('2025', '2025', '2024', '2024', '2023', "2023", "2025", "2025", "2025", "2025") 
phase = c('2', '2', '1', '1', '1', '1', '2', '2', '2', '2')
village_info = data.frame(villages, year, phase)

#subset to only villages for this project
register = subset(register, name %in% villages)

register=merge(register, village_info, by.x="name", by.y="villages")
colnames(register)[2] = "name"

register=select(register, c("name", "phase", "year", "lat", "long"))

if ("package:plyr" %in% search()) {
  detach("package:plyr", unload = TRUE)
}

s = "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&size=480x360"

#center map in median latitude and longitude of villages
medianlat = median(register$lat, na.rm=T)
medianlong = median(register$long, na.rm = T)
map = get_googlemap(center=c(102,4), zoom = 7, scale = 1, style = s)
m= ggmap(map)

# Plot with corrections
plot = m + geom_jitter(data = register, aes(x = long, y = lat, color = year, shape = phase), alpha = 0.65, size=7) + 
  labs(x = 'Longitude', y = 'Latitude') + 
  theme_bw(22)  + scale_shape_manual(values=c(17, 19)) +
  scale_color_manual(values = c("2023" = "#db7f34", "2024" = "#7EA172", "2025"= "#485696")) +
  xlim(101, 102.8) + ylim(3.5, 6) +
  theme(legend.title.align = 0.5, legend.text = element_text(size = 16), legend.title = element_text(size=18)) 

ggsave(plot, file="~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/location_map.pdf", height=6, width =5)


#turkana map
register = read_sheet("https://docs.google.com/spreadsheets/d/1TdIlHADHbO1LBehozhbN5AearUIWWsQsmPPzwWLnQj0/edit?gid=1116356671#gid=1116356671", sheet=1)

villages=c() #redacted
year = c('2022', '2023', '2022', '2022') 
phase = c('1', '1', '1', '1')
village_info = data.frame(villages, year, phase)

register = subset(register, Sampling_location %in% villages)

register=merge(register, village_info, by.x="Sampling_location", by.y="villages")
colnames(register)[1] = "name"

register=select(register, c("name", "phase", "year", "Y_latitude", "X_longitude"))

if ("package:plyr" %in% search()) {
  detach("package:plyr", unload = TRUE)
}

s = "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&size=480x360"

#center map in median latitude and longitude of villages
medianlat = median(register$`Y_latitude`, na.rm=T)
medianlong = median(register$`X_longitude`, na.rm = T)
map = get_googlemap(center=c(medianlong,medianlat), zoom = 7, scale = 1, style = s)

m= ggmap(map)

# Plot with corrections
plot = m + geom_jitter(data = register, aes(x = X_longitude, y = Y_latitude, color = year, shape = phase), alpha = 0.65, size=7) + 
  labs(x = 'Longitude', y = 'Latitude') + 
  theme_bw(22)  + scale_shape_manual(values=c(17, 19)) +
  scale_color_manual(values = c("2023" = "#db7f34", "2022" = "#8D0801")) + xlim(34,38) + ylim(-1,4.5) +
  theme(legend.title.align = 0.5, legend.text = element_text(size = 16), legend.title = element_text(size=18)) 

ggsave(plot, file="~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/kenya_location_map.pdf", height=6, width =5)

############################################################
###     general pre questions sample-wide trends         ###
############################################################
# survey questions
questions = c("want_know_more_malay", "health_info_in_blood_malay")

#how many people said yes for each of these questions
survey_long = interview_meta %>%
  dplyr::select(all_of(questions)) %>%
  pivot_longer(cols = everything(),
               names_to = "question",
               values_to = "response")
question_labels = c(
  "want_know_more_malay" = "Would you like to know\n more about the work\n we are doing?",
  "health_info_in_blood_malay" = "Do you think there is\n information about health\n in your blood?")

# Make stacked proportional bar plot
ggplot(survey_long, aes(x = question, fill = factor(response))) +
  geom_bar(color = "black", position = "fill") +
  scale_fill_manual(values = c("#7EA172", "#C7CB85"),
                    labels = c("No", "Yes")) +
  scale_x_discrete(labels = question_labels) +  
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(x = "Question", y = "Proportion", fill = NULL) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
ggsave(filename="pre_pres_responses.png", width=8, height=6)

############################################################
###     general favorites and questions                  ###
############################################################
#Which illustrations were the favorites?
table(interview_meta$best_image_malay)
ggplot(interview_meta, aes(x = as.character(best_image_malay))) +
  geom_bar(fill = "#485696", color = "black") + scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F", "G", "H")) +
  labs(x = "Favorite image", y = "Count") + theme_classic(base_size=24) + ylim(0,45)
ggsave(filename="favorite_image.pdf", width=19, height=6)

#how many images did people find confusing
#make a new column summing the number of illustrations individuals find confusing
interview_meta = interview_meta %>%
  dplyr::mutate(confusing_count = rowSums(dplyr::select(., starts_with("which_confusing_malay___")) == 1, na.rm = TRUE))

table(interview_meta$confusing_count)

ggplot(interview_meta, aes(x = as.character(confusing_count))) +
  geom_bar(fill = "#485696", color = "black") +
  labs(x = "Number of confusing images", y = "Count") + theme_classic(base_size=24) + ylim(0,65) + scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F", "G", "H")) 
ggsave(filename="number_confusing_images.pdf", width=4, height=3)

#which images did people find confusing
confusing_long = interview_meta %>%
  dplyr::select(starts_with("which_confusing_malay___")) %>%
  pivot_longer(cols = everything(),
               names_to = "question",
               values_to = "response")

# Count how many 1s per question
confusing_counts = confusing_long %>%
  filter(response == 1) %>%
  group_by(question) %>%
  summarise(count = n())

# Create a mapping for nicer x-axis labels
question_labels = setNames(as.character(1:8), 
                            paste0("which_confusing_malay___", 1:8))

# Plot
ggplot(confusing_counts, aes(x = question, y = count)) +
  geom_bar(stat = "identity", fill = "#485696", color = "black") +
  scale_x_discrete(labels = question_labels) +
  labs(x = "Number confusing item", y = "Number of individuals") +
  theme_classic(base_size = 24) + ylim(0,45) + scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F", "G", "H")) 
ggsave(filename="which_confusing_images.pdf", width=6, height=3)

#how many images did people have questions about
interview_meta = interview_meta %>%
  dplyr::mutate(learn_more_count = rowSums(dplyr::select(., starts_with("learn_more_malay___")) == 1, na.rm = TRUE))
#if learn_more_malay___9==1, then that means "none"
interview_meta$learn_more_count=ifelse(interview_meta$learn_more_malay___9==1, 0,interview_meta$learn_more_count)

table(interview_meta$learn_more_count)
ggplot(interview_meta, aes(x = as.character(learn_more_count))) +
  geom_bar(fill = "#485696", color = "black") +
  labs(x = "Number reported", y = "Count") + theme_classic(base_size=24) + ylim(0,65)
ggsave(filename="number_want_learn_more.pdf", width=4, height=3)

#what images would people want to learn more about?
learn_more_long = interview_meta %>%
  dplyr::select(starts_with("learn_more_malay___")) %>%
  pivot_longer(cols = everything(),
               names_to = "question",
               values_to = "response")

#get rid of none (9)
learn_more_long=subset(learn_more_long, question != "learn_more_malay___9")

# Count how many 1s per question
learn_more_counts = learn_more_long %>%
  filter(response == 1) %>%
  group_by(question) %>%
  summarise(count = n())

# Create a mapping for nicer x-axis labels
question_labels = setNames(as.character(1:8), 
                            paste0("learn_more_malay___", 1:8))

# Plot
ggplot(learn_more_counts, aes(x = question, y = count)) +
  geom_bar(stat = "identity", fill = "#485696", color = "black") +
  scale_x_discrete(labels = question_labels) +
  labs(x = "Number learn_more item", y = "Count") +
  theme_classic(base_size = 24) + ylim(0,45)
ggsave(filename="learn_more_images.pdf", width=6, height=3)

#what do you still have questions about?
questions_long = interview_meta %>%
  dplyr::select(starts_with("other_questions_malay___")) %>%
  pivot_longer(cols = everything(),
               names_to = "question",
               values_to = "response")

# Count how many 1s per question
questions_counts = questions_long %>%
  filter(response == 1) %>%
  group_by(question) %>%
  summarise(count = n())

#this means no questions
questions_counts=subset(questions_counts, question != "other_questions_malay___6")
# Create a mapping for nicer x-axis labels
question_labels = setNames(
  c("Health and disease", "Relatedness", "Uniqueness of DNA", "DNA similarity to\nother populations", "Other questions"),
  paste0("other_questions_malay___", 1:5))

# Plot
ggplot(questions_counts, aes(x = question, y = count)) +
  geom_bar(stat = "identity", fill = "#485696", color = "black") +
  scale_x_discrete(labels = question_labels) +
  labs(x = "Question category", y = "Number of individuals") +
  theme_classic(base_size = 24) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename="questions.pdf", width=8, height=6)

############################################################
###     yes/no questions                                 ###
############################################################
questions = c("could_explain_malay", "know_more_malay", "help_understand_malay", "why_study_malay",
               "hard_understand_malay", "watch_again_malay", "tell_friend_malay", "want_learn_more_malay")

#want yes/no for each question
survey_long = interview_meta %>%
  dplyr::select(all_of(questions)) %>%
  pivot_longer(cols = everything(),
               names_to = "question",
               values_to = "response")
question_labels = c(
  "know_more_malay"="1",
  "help_understand_malay"="2", 
  "why_study_malay"="3",
  "could_explain_malay"="4",
  "watch_again_malay"="5", 
  "tell_friend_malay"="6", 
  "want_learn_more_malay"="7",
  "hard_understand_malay"="8")

survey_long = survey_long %>%
  mutate(question = factor(question, levels = names(question_labels), labels = question_labels))


# Make stacked proportional bar plot
ggplot(survey_long, aes(x = question, fill = factor(response))) +
  geom_bar(color = "black", position = "fill") +
  scale_fill_manual(values = c("#C7CB85", "#7EA172"),
                    labels = c("No", "Yes")) +
  scale_x_discrete(labels = question_labels) +   # rename x-axis ticks
  scale_y_continuous(labels = scales::percent_format()) +  # y-axis as %
  labs(x = "Question", y = "Proportion", fill = NULL) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(hjust = 1))
ggsave(filename="yes_no_post_responses.pdf", width=14, height=)

#table of percent yes & no for each question
survey_summary = survey_long %>%
  group_by(question, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(question) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  pivot_wider(
    names_from = response,
    values_from = c(n, percent),
    names_prefix = ""
  )

############################################################
###     binomial analyses and correlation                ###
############################################################
#do binomial models for whether proportion yes is significantly different from 1
results = survey_long %>%
  group_by(question) %>%
  do({
    model = glm(response ~ 1, data = ., family = binomial)
    tidy(model, conf.int = TRUE)  
  }) %>%
  ungroup()

results$fdr = p.adjust(results$p.value, method = "fdr")

#correlation in question responses
survey_cols = c(
  "know_more_malay",
  "help_understand_malay",
  "why_study_malay",
  "watch_again_malay",
  "could_explain_malay",
  "tell_friend_malay",
  "want_learn_more_malay",
  "hard_understand_malay"
)

# run correlation
cor_matrix = cor(interview_meta[survey_cols], use = "pairwise.complete.obs")

cor_matrix

col_palette = colorRampPalette(c("#db7f34", "white", "#59764f"))(200)

# Rename rows/columns
rownames(cor_matrix) = c("1", "2", "3", "4", "5", "6", "7", "8")
colnames(cor_matrix) = c("1", "2", "3", "4", "5", "6", "7", "8")

# Save to PDF
pdf("~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/questions_corrplot_output.pdf", width = 9, height = 8)
corrplot(cor_matrix,
         type = "upper",
         order = "original",
         method = "ellipse",
         tl.col = "black",
         tl.srt = 1.5,
         tl.cex = 1.5, 
         addCoef.col = "black",
         col = col_palette)
dev.off()

############################################################
###     multiple correspondance analysis                 ###
############################################################
#self-estimated urb_score, potentially revise later
interview_meta$urb_score[interview_meta$interview_location_illus_malay == 46] = 21.92
interview_meta$urb_score[interview_meta$interview_location_illus_malay == 48] = 19.645

interview_mca=interview_meta[,c("age_final", "sex_illust_malay", "urb_score",
                                "highest_education_stage_illus_malay",
                                "could_explain_malay",
                                  "know_more_malay",
                                  "help_understand_malay",
                                  "why_study_malay",
                                  "hard_understand_malay",
                                  "watch_again_malay",
                                  "tell_friend_malay",
                                  "want_learn_more_malay"
                                  )]
#how many individuals have NAs for these columns
sum(apply(is.na(interview_mca),1,any)) #3
interview_mca = na.omit(interview_mca) #get rid of those with NAs

#prepare for mca with only rows wanted
interview_mca[ , 5:ncol(interview_mca)] = lapply(interview_mca[ , 5:ncol(interview_mca)], 
                                                  function(x) factor(x, levels = c(0,1), labels = c("No","Yes")))

# Check what kind of variables each row is
str(interview_mca)

#MCA function
res.mca = MCA(interview_mca, quali.sup = 2:5, quanti.sup = 1)

#look at how many contributions there are 
fviz_screeplot(res.mca, barfill = "#485696",barcolor = "black", addlabels = TRUE, ylim = c(0, 35)) + theme_classic(base_size=20) +
  labs(x = "Principal dimension", y = "% of explained variance", title = "")
ggsave(file="~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/scree_mce.png", height=8, width =7)

#visualize biplot all variables, don't use in final
fviz_mca_biplot(res.mca,
                repel = TRUE,
                label = "all",        
                col.var = "#59764f",
                col.ind = "gray50",
                ggtheme = theme_classic(base_size = 18))

#look at where individuals fall on biplot
fviz_mca_ind(res.mca, 
             label = "all", 
             repel=T,
             habillage = "sex_illust_malay", 
             palette = c("#7EA172", "#db7f34"),
             addEllipses = TRUE, ellipse.type = "confidence",
             pointsize=2,
             mean.point=F,
             ggtheme = theme_bw()) 

# Extract MCA coordinates
ind_coords = as.data.frame(res.mca$ind$coord)
ind_coords$sex = factor(interview_mca$sex_illust_malay,
                         levels = c(0,1),
                         labels = c("Female", "Male"))

#analysis on each individual's mca loadings 
interview_mca_clean = interview_mca
interview_mca_clean$Dim1 = res.mca$ind$coord[,1]
interview_mca_clean$Dim2 = res.mca$ind$coord[,2]

#linear model to see if loadings are predicted by different demographics
#dimension1
m1_dim1 = glm(Dim1 ~ age_final + sex_illust_malay + highest_education_stage_illus_malay + urb_score, data=interview_mca_clean)
dim1=as.data.frame(coef(summary(m1_dim1)))
dim1$fdr=p.adjust(dim1$`Pr(>|t|)`, method = "fdr")


#dimension2
m1_dim2 = glm(Dim2 ~ age_final + sex_illust_malay + highest_education_stage_illus_malay + urb_score, data=interview_mca_clean)
dim2=as.data.frame(coef(summary(m1_dim2)))
dim2$fdr=p.adjust(dim2$`Pr(>|t|)`, method = "fdr")


#multiple correspondance analysis without covariates
interview_mca=interview_meta[,c("could_explain_malay",
                                "know_more_malay",
                                "help_understand_malay",
                                "why_study_malay",
                                "hard_understand_malay",
                                "watch_again_malay",
                                "tell_friend_malay",
                                "want_learn_more_malay")]

#how many individuals have NAs for these columns
sum(apply(is.na(interview_mca),1,any)) #3
interview_mca = na.omit(interview_mca)

# Check
str(interview_mca)
interview_mca[, 1:8] = lapply(interview_mca[, 1:8], as.factor)
res.mca = MCA(interview_mca)

#eigenvalues
eig.val=get_eigenvalue(res.mca)

#look at how many contributions there are 
fviz_screeplot(res.mca, barfill = "#485696",barcolor = "black", addlabels = TRUE, ylim = c(0, 35)) + theme_classic(base_size=20) +
  labs(x = "Principal dimension", y = "% of explained variance", title = "")

#visualize pc
fviz_mca_var(res.mca, choice = "mca.cor", col.var="#485696", shape.var = 15,
             repel = TRUE,
             ggtheme = theme_classic(base_size=18))

#see contributions of levels
var_contrib = data.frame(res.mca$var$contrib)
var_contrib$Category = rownames(var_contrib)

# Get mapping of each dummy variable (level) to its parent variable
var_map = res.mca$call$X %>% names()

# Combine and sum contributions by parent variable
var_contrib$Variable = sub("_[^_]+$", "", var_contrib$Category)
var_contrib_summary = var_contrib %>%
  group_by(Variable) %>%
  summarise(across(starts_with("Dim."), sum))

# Plot total contribution for a dimension (e.g., Dim 1)
ggplot(var_contrib_summary, aes(x=reorder(Variable, Dim.1), y=Dim.1)) +
  geom_col(fill="#485696") +
  coord_flip() + 
  scale_x_discrete(labels = c(
    "know_more_malay"="1",
    "help_understand_malay"="2", 
    "why_study_malay"="3",
    "could_explain_malay"="4",
    "watch_again_malay"="5", 
    "tell_friend_malay"="6", 
    "want_learn_more_malay"="7",
    "hard_understand_malay"="8")) +
  theme_classic(base_size=16) +
  labs(x="Question", y="Contribution to dimension 1 (%)")

ggsave(file="~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/dimension1_contrib.pdf", height=4, width =7.5)

#dimension 2
ggplot(var_contrib_summary, aes(x=reorder(Variable, Dim.2), y=Dim.2)) +
  geom_col(fill="#485696") +
  coord_flip() + 
  scale_x_discrete(labels = c(
    "know_more_malay"="1",
    "help_understand_malay"="2", 
    "why_study_malay"="3",
    "could_explain_malay"="4",
    "watch_again_malay"="5", 
    "tell_friend_malay"="6", 
    "want_learn_more_malay"="7",
    "hard_understand_malay"="8"))+
  theme_classic(base_size=16) +
  labs(x="Question", y="Contribution to dimension 2 (%)")
ggsave(file="~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/dimension2_contrib.pdf", height=4, width =7)

############################################################
###     predictors that impact response                  ###
############################################################
survey_cols = c(
  "know_more_malay",
  "help_understand_malay",
  "why_study_malay",
  "watch_again_malay",
  "could_explain_malay",
  "tell_friend_malay",
  "want_learn_more_malay",
  "hard_understand_malay"
)

#binomial model with all potential variables in the plot
#first make sure all columns are correct type
interview_meta$sex_illust_malay=as.factor(interview_meta$sex_illust_malay)
interview_meta$highest_education_stage_illus_malay=as.integer(interview_meta$highest_education_stage_illus_malay)
interview_meta$interview_location_illus_malay=as.factor(interview_meta$interview_location_illus_malay)

pvals = do.call(rbind, lapply(survey_cols, function(col) {
  formula = as.formula(paste0(col, " ~ age_final + (sex_illust_malay) + (highest_education_stage_illus_malay) + (urb_score) "))
  model = lm(formula, data = interview_meta)
  coefs = summary(model)$coefficients
  coefs = coefs[rownames(coefs) != "(Intercept)", , drop = FALSE]  # exclude intercept
  data.frame(
    question = col,
    variable = rownames(coefs),
    beta = coefs[, "Estimate"],
    p_value = coefs[, "Pr(>|t|)"],
    st_error=coefs[,"Std. Error"],
    row.names = NULL
  )
}))
pvals

#make an fdr
pvals$fdr = p.adjust(pvals$p_value, method = "fdr")

#significance column for graph
pvals$Significant = ifelse(
  pvals$fdr < 0.05, "FDR<0.05",
  ifelse(pvals$p_value < 0.05, "p<0.05", "n.s.")
)

#factor so outputs as like in graph
pvals$Significant = factor(pvals$Significant, levels = c("FDR<0.05", "p<0.05", "n.s."))
pvals$question = factor(pvals$question, levels = c( "hard_understand_malay", "want_learn_more_malay","tell_friend_malay","watch_again_malay", "could_explain_malay","why_study_malay","help_understand_malay","know_more_malay"))

ggplot(pvals, aes(x = beta, y = question, xmin = beta -(1.96*st_error), xmax = beta + (1.96*st_error), color = Significant)) +
  geom_vline(xintercept = 0, size = 1.5, linetype = "dashed") +
  facet_wrap(~variable, nrow=1, scales = "free_x", labeller = as_labeller(c(
    age_final = "Age", sex_illust_malay1 = "Sex", highest_education_stage_illus_malay = "Highest\neducation", urb_score = "Urbanicity"))) +
  geom_point(size = 6, position = position_dodge(width = 0.4)) +  # offset points
  geom_errorbar(width = 0.1, size = 1.3, position = position_dodge(width = 0.4)) +
  labs(x = "Effect size", y = "Question") +
  scale_color_manual(values = c("FDR<0.05"="#7EA172",  
                                "p<0.05"="#C7CB85",
                                "n.s."="gray"))+
  theme_bw(base_size = 22) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size=16),
        legend.text = element_text(size = 16)) +
 # guides(color = guide_legend(override.aes = list(size = 4))) +
  scale_y_discrete(labels = c(
     "know_more_malay"="1",
  "help_understand_malay"="2", 
  "why_study_malay"="3",
  "could_explain_malay"="4",
  "watch_again_malay"="5", 
  "tell_friend_malay"="6", 
  "want_learn_more_malay"="7",
  "hard_understand_malay"="8"))+
  labs(color=NULL)

ggsave(file="model_effects_answers_19Feb26.pdf", height=5, width =14)

############################################################
###     all covariates in model, binary education        ###
############################################################
#binomial model with all potential variables in the plot
interview_meta$binary_education=ifelse(interview_meta$highest_education_stage_illus_malay==0,0,1)

pvals = do.call(rbind, lapply(survey_cols, function(col) {
  formula = as.formula(paste0(col, " ~ age_final + (sex_illust_malay) + (binary_education) + urb_score "))
  model = glm(formula, data = interview_meta, family = binomial)
  coefs = summary(model)$coefficients
  coefs = coefs[rownames(coefs) != "(Intercept)", , drop = FALSE]  # exclude intercept
  data.frame(
    question = col,
    variable = rownames(coefs),
    beta = coefs[, "Estimate"],
    p_value = coefs[, "Pr(>|z|)"],
    st_error=coefs[,"Std. Error"],
    row.names = NULL
  )
}))

pvals
#make an fdr
pvals$fdr = p.adjust(pvals$p_value, method = "fdr")


pvals$Significant = ifelse(
  pvals$fdr < 0.05, "FDR<0.05",
  ifelse(pvals$p_value < 0.05, "p<0.05", "n.s.")
)

pvals$Significant = factor(pvals$Significant, levels = c("FDR<0.05", "p<0.05", "n.s."))
pvals$question = factor(pvals$question, levels = c( "hard_understand_malay", "want_learn_more_malay","tell_friend_malay","watch_again_malay", "could_explain_malay","why_study_malay","help_understand_malay","know_more_malay"))

ggplot(pvals, aes(x = beta, y = question, xmin = beta -(1.96*st_error), xmax = beta + (1.96*st_error), color = Significant)) +
  geom_vline(xintercept = 0, size = 1.5, linetype = "dashed") +
  facet_wrap(~variable, nrow=1, scales = "free_x", labeller = as_labeller(c(
    age_final = "Age", sex_illust_malay1 = "Sex", binary_education = "Any education", urb_score = "Urbanicity"))) +
  geom_point(size = 6, position = position_dodge(width = 0.4)) +  # offset points
  geom_errorbar(width = 0.1, size = 1.3, position = position_dodge(width = 0.4)) +
  labs(x = "Effect size", y = "Question") +
  scale_color_manual(values = c("FDR<0.05"="#7EA172",  
                                "p<0.05"="#C7CB85",
                                "n.s."="gray"))+
  theme_bw(base_size = 22) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size=16),
        legend.text = element_text(size = 16)) +
  # guides(color = guide_legend(override.aes = list(size = 4))) +
  scale_y_discrete(labels = c(
    "know_more_malay"="1",
    "help_understand_malay"="2", 
    "why_study_malay"="3",
    "could_explain_malay"="4",
    "watch_again_malay"="5", 
    "tell_friend_malay"="6", 
    "want_learn_more_malay"="7",
    "hard_understand_malay"="8"))+
  labs(color=NULL)



############################################################
###     look at covariates across interview locs         ###
############################################################
#age
p=ggplot(interview_meta, aes(x = interview_location_illus_malay, y = age_final)) +
  geom_violin() + geom_jitter(size = 3) +
  labs(y = "Age", x = "Interview location") +
  theme_bw(base_size = 22) 
ggsave(p, file="~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/location_ages.png", height=6, width =8)

#years of schooling
p=ggplot(interview_meta, aes(x = interview_location_illus_malay, y = years_schooling_illust_malay)) +
  geom_violin() + geom_jitter(size = 3) +
  labs(y = "Years of schooling", x = "Interview location") +
  theme_bw(base_size = 22) 

ggsave(p, file="~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/location_schooling.png", height=6, width =8)

#sex proportion
# Calculate proportions
prop_data = interview_meta %>%
  group_by(interview_location_illus_malay, sex_illust_malay) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(interview_location_illus_malay) %>%
  mutate(prop = n / sum(n))

p=ggplot(prop_data, aes(x = interview_location_illus_malay, y = prop, fill = sex_illust_malay)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Interview Location", y = "Sex proportion", fill = "Sex") +
  theme_bw(base_size = 30) 

ggsave(p, file="~/Library/CloudStorage/Box-Box/Audrey/Lab/genetics_illustrations/location_sex_proportion.png", height=6, width =8)
