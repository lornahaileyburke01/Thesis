library(apaTables)
library(car)
library(lme4)
library(lmerTest)  
library(emmeans)
library(QuantPsyc)
library(ggplot2)
library(sjPlot)
library(effectsize)
library(effsize)
library(tidyr) 
library(ez) 
library(schoRsch)
library(forcats) 
library(stringr) 
library(glmmTMB) 
library(GGally)
library(scales)
library(dplyr)




#######DEBRIEF RATINGS ################
###!!!! TASK T: rename the variables with more intuitive names
#incontro: I think I have met the other
#feedback 1: how much your relied on visual feedback
#feedback 2: how much your relied on auditory feedback
debr= read.csv("Debrief_23-Jun-2025.csv", stringsAsFactors = T)
d <- dplyr::select(debr, ParticipantPublicID, Response, QuestionKey, cond)
colnames(d) = c("sub", "response", "question",  "cond")
d$dyad = as.factor(substr(d$sub, start = 1, stop = 5))
length(unique(d$sub))
summary(d$question)

d$question = as.character(d$question)

d$question[d$question == "instructions"] = "Istruzioni"
d$question[d$question == "meeting"] = "incontro"
d$question[d$question == "fun"] = "Divertimento"
d$question[d$question == "embarassment"] = "imbarazzo"
d$question[d$question == "synch"] = "sincronia"
d$question[d$question == "difficulty"] = "Difficoltà"
d$question[d$question == "enjoyment"] = "gradimento"
d$question = as.factor(d$question)
summary(d)

# zscore
d = d %>% 
  group_by(question) %>%
  mutate(responsez = (response - mean(response, na.rm=T))/sd(response, na.rm=T))
dd <- d %>% filter(!question %in% c("anni", "yearTrainingInstrument-quantised"))
summary(dd)

#z-scored
ggplot(dd, aes(x = question, y = responsez, color = question)) +
  #geom_jitter(width = 0.2, alpha = 0.2, size = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 4, position = position_dodge(0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_grid(~cond) +
  labs(
    title = "Z-Scored Debrief Responses (post-pre)",
    x = "", y = "Z-Score",
    color = ""
  ) +
  theme_minimal(base_size = 14)

#sig testing
leveneTest(responsez ~ cond * question, data = dd)
anova_model <- lmer(responsez ~ cond * question + (1 | dyad/sub), data = dd,
                    contrasts = list(cond = "contr.sum", question = "contr.sum"))
Anova(anova_model, type = 3)
emm <- emmeans(anova_model, ~ cond | question)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
eta_squared(anova_model)

emm2 = as.data.frame(emmeans(anova_model, ~ cond * question))
emm2$cond <- factor(emm2$cond, levels = c("silent", "isot", "mus"))

dodge <- position_dodge(width = 0.4)

ggplot(emm2, aes(x = cond, y = emmean, color = question)) +
  geom_line(position = dodge, linewidth = 1) + 
  geom_point(position = dodge, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                position = dodge, width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Subjective Experience Ratings",
    x = "",
    y = "Z-Scored",
    color = "Question"
  ) +
  theme_minimal(base_size = 14) 




#######SVO - Social value orientation ############
# individualistic angle is between 7.82 and -7.82
# prosocial angle is between 37.09 and 52.9
svo = read.csv("SVO_new_Scores_23-Jun-2025.csv", stringsAsFactors = T)
colnames(svo) = c("sub", "cond", "prepost", "angle", "type") #"type" (1=Altruistic, 2=Prosocial, 3=Individualistic, 4=Competitive)
svo$dyad <- as.factor(substr(as.character(svo$sub), 1, 5))
svo$type = as.factor(svo$type)
svo$type = fct_recode(svo$type, "Altruistic" = "1","Prosocial" = "2","Individualistic" = "3","Competitive" = "4")
length(unique(svo$sub))

svo = svo %>%
  mutate(prepost = as.factor(ifelse(prepost==1, "Pre", "Post"))) #check if pre == 1 e post == 2
summary(svo)
svo$prepost <- factor(svo$prepost, levels = c("Pre", "Post"))
svo$anglenew = svo$angle + abs(min(svo$angle))+1
summary(svo)
xtabs(~sub+prepost, svo)

hist(svo$angle)
hist(svo$anglenew)

#new graph
agg <- svo[,c("sub", "prepost","cond", "anglenew", "dyad")] %>%
  pivot_wider(names_from = prepost, values_from = anglenew) 

agg <- agg %>%
  mutate(deltaangle = Post - Pre) 
head(agg[order(agg$deltaangle),])

ggplot(agg, aes(x = cond, y = deltaangle, fill = cond, color = cond)) +
  theme_classic() +
  stat_summary(fun = mean, geom = "point", size = 4) +
  geom_point(alpha = 0.3, position = position_nudge(x = 0)) +  
  stat_summary(fun.data = 'mean_cl_boot', geom = "errorbar", width = 0.2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("Post - Pre SVO Angle")

agg$cond <- factor(agg$cond)
agg$sub <- factor(agg$sub)  

anova_model <- lm(deltaangle ~ cond, data = agg, 
                  contrasts = list(cond = "contr.sum"))
Anova(anova_model, type = 3)
anova_model <- lmer(deltaangle ~ cond + (1 | dyad), data = agg,
                    contrasts = list(cond = "contr.sum"))
Anova(anova_model, type = 3)

anova_model <- lmer(anglenew ~ cond * prepost + (1 | dyad/sub), data = svo, 
                    contrasts = list(cond = contr.sum, prepost = contr.sum))
Anova(anova_model, type = 3)

anova_model <- lm(anglenew ~ cond * prepost, data = svo, 
                    contrasts = list(cond = contr.sum, prepost = contr.sum))
Anova(anova_model, type = 3)

confint(anova_model)
emm <- emmeans(anova_model, ~ cond)
pairs(emm, adjust = "tukey", infer = c(TRUE, TRUE))
eta_squared(anova_model)
emm <- emmeans(anova_model, pairwise ~ cond)
summary(emm)

emm <- emmeans(anova_model, pairwise ~ cond, infer= c(T,T))

emm2 = as.data.frame(emmeans(anova_model, ~ cond))
emm2$cond <- factor(emm2$cond, levels = c("silent", "isot", "mus"))

ggplot(emm2, aes(x = cond, y = emmean, col= cond)) +
  geom_line() + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(limits = c(min(emm2$lower.CL) - 25, max(emm2$upper.CL) + 25)) +
  labs(
    title = "SVO",
    x = "Condition",
    y = "SVO Angles (Post-Pre)",
    color = "cond"
  )+
  theme_minimal() +
  stat_summary(fun.data = mean_cl_boot)

df_svo = svo


############COORD GAME ##########
d = read.csv("GabrisTask_23-Jun-2025.csv", stringsAsFactors = T)
colnames(d) = c("sub", "time", "game", "trial", "rt", "sp", "resp", "cond")
d$dyad = as.factor(substr(d$sub, start = 1, stop = 5))
d = d %>%
  mutate(prepost = as.factor(ifelse(time==1, "Pre", "Post")), 
         resp.i = ifelse(resp ==1,0,1),
         sp.mc = scale(sp, scale =F))
length(unique(d$sub))  # N = 108




#Payoff info
d$payoff2 <- NA

# Adding payoff-related info
d$payoff2[d$resp.i==0]=d$sp[d$resp.i==0] # If resp.i = 0, payoff is always identical to the sure payoff value, across conditions. 
d$payoff2[d$resp.i==1 & d$game=='lottery']=7.5

for (i in c(1:nrow(d))){
  if(d$resp.i[i]==1 & d$game[i] =="coordination"){
    other = d$resp.i[d$game=='coordination' & d$dyad==d$dyad[i] & d$sub != d$sub[i] & d$prepost == d$prepost[i] & 
                       d$sp == d$sp[i]]
    d$payoff2[i]= 15 * mean(other)
  }
}
summary(d)

other
i
d[5,]
d[d$game=='coordination' & d$dyad==d$dyad[i] & d$sub != d$sub[i] & d$prepost == d$prepost[i] & 
    d$sp == d$sp[i],]
xtabs(~sub + sp + prepost, d[d$game=="coordination",])


d$cond <- factor(d$cond, levels = c("silent", "isot", "mus"))
ggplot(d, aes(sp, payoff2, group = prepost, color = prepost, fill = prepost)) +theme_light() +
  stat_summary(fun = mean, geom = "line", alpha = 0.8) + facet_grid(game ~cond) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, colour = NA, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", alpha = 0.8, size = 1) +
  xlab("SP Value") + ylab("Payoff") + theme(legend.position = "bottom") 

d$sp.c=scale(d$sp)

logit_model <- glmmTMB(payoff2 ~ sp.c * game + prepost * cond * game + (prepost + sp.c | sub), 
                       contrasts= list(game= "contr.sum", cond= "contr.sum", prepost= "contr.sum"), 
                       data = d)
Anova(logit_model, type = 3)

emm <- emmeans(logit_model, ~ prepost | cond * game) #use this for graph
pairs(emm, adjust = "none", infer = c(TRUE, TRUE))

emm <- emmeans(logit_model, ~ cond | game) #use this for graph
pairs(emm, adjust = "none", infer = c(TRUE, TRUE))

emm <- emmeans(logit_model, ~ game)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emt = emtrends(logit_model, pairwise ~ game, var = "sp.c")
summary(emt, infer = c(TRUE, TRUE))


summary(lil)
lil$bonf=lil$p.value*nrow(lil)
lil

emm <- emmeans(logit_model, ~ prepost |cond * game)
diffs <- contrast(emm, method = "revpairwise", by = c("cond", "game"))
res   <- contrast(diffs, method = "pairwise", by = "game", adjust = "bonferroni")
ci_link <- summary(res, infer = c(TRUE, TRUE))   # adds lower.CL / upper.CL
ci_link

emm <- emmeans(logit_model, ~ cond | game)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm4 <- data.frame(pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE)))
summary(emm4)
emm4$cond <- factor(emm4$cond, levels = c("silent", "isot", "mus"))

ggplot(emm4, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~game) +
  labs(
    title = "Payoff",
    x = "Condition",
    y = "Contrast Estimate",
    color = ""
  ) +
  theme_minimal(base_size = 14)

logit_model_coord <- glmmTMB(payoff2 ~ sp.c * prepost * cond + (prepost * sp.c | sub), 
                             contrasts= list(cond= "contr.sum", prepost= "contr.sum"), 
                             data = d [d$game=="coordination",])
Anova(logit_model_coord, type = 3)


#Risk rates 
d$cond <- factor(d$cond, levels = c("silent", "isot", "mus"))
ggplot(d, aes(sp, resp.i, group = prepost, color = prepost, fill = prepost)) +theme_light() +
  stat_summary(fun = mean, geom = "line", alpha = 0.8) + facet_grid(game ~cond) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, colour = NA, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", alpha = 0.8, size = 1) +
  xlab("SP Value") + ylab("Risk") + theme(legend.position = "bottom") 



d$sp.c=scale(d$sp)
View(d)
logit_model <- glmmTMB(resp.i ~ sp.c * game + prepost * cond * game + (game * prepost + sp.c | sub), 
                       contrasts= list(game= "contr.sum", cond= "contr.sum", prepost= "contr.sum"), 
                       data = d, family = "binomial")
Anova(logit_model, type = 3)

emm <- emmeans(logit_model, ~ game)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emt = emtrends(logit_model, pairwise ~ 1, var = "sp.c")
summary(emt, infer = c(TRUE, TRUE))

emt = emtrends(logit_model, pairwise ~ game, var = "sp.c")
summary(emt, infer = c(TRUE, TRUE))


emm <- emmeans(logit_model, ~ prepost | cond * game)
diffs <- contrast(emm, method = "revpairwise", by = c("cond", "game"))
contrast(diffs, method = "pairwise", by = "game",adjust = "bonferroni")


emm3 <- data.frame(pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE)))
summary(emm3)
emm3$cond <- factor(emm3$cond, levels = c("silent", "isot", "mus"))

ggplot(emm3, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~game) +
  labs(
    title = "Risk",
    x = "Condition",
    y = "Contrast Estimate",
    color = "Question"
  ) +
  theme_minimal(base_size = 14)




logit_model_coord <- glmmTMB(resp.i ~ sp.c * prepost * cond + (prepost * sp.c | sub), 
                             contrasts= list(cond= "contr.sum", prepost= "contr.sum"), 
                             data = d [d$game=="coordination",], family = "binomial")
Anova(logit_model_coord, type = 3)





#Coordination rates 
d$coord <- NA

d$coord[d$game == "lottery"] <- NA

for (i in c(1:nrow(d))) {
  if (d$game[i] == "coordination") {
    other <- d$resp.i[
      d$game == "coordination" &
        d$dyad == d$dyad[i] &
        d$sub != d$sub[i] &
        d$prepost == d$prepost[i] &
        d$sp == d$sp[i]]
    
    {d$coord[i] <- ifelse(d$resp.i[i]==1,mean(other),1-mean(other))
    } 
  }
}

View(d)
summary(d)

ggplot(d, aes(sp, coord, group = prepost, color = prepost, fill = prepost)) +theme_light() +
  stat_summary(fun = mean, geom = "line", alpha = 0.8) + facet_wrap(~cond ) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, colour = NA, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", alpha = 0.8, size = 1) +
  xlab("SP Value") + ylab("coord rate") + theme(legend.position = "bottom")


d$sp.c=scale(d$sp)
xtabs(~ coord + sp, d)
d$sp.med=as.factor(ifelse(d$sp<median(d$sp),"low", "high"))

logit_model <- glmmTMB(coord ~ sp.c * prepost * cond + (prepost * sp.c | sub), 
                       contrasts= list(cond= "contr.sum", prepost= "contr.sum"), 
                       data = d)
Anova(logit_model, type = 3)
logit_model_med <- glmmTMB(coord ~ sp.med * prepost * cond + (prepost * sp.med | sub), 
                           contrasts= list(cond= "contr.sum", prepost= "contr.sum", sp.med= "contr.sum"), 
                           data = d[d$game=="coordination",])
Anova(logit_model_med, type = 3)
emm <- emmeans(logit_model, ~ prepost | sp.c * cond)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm <- emmeans(logit_model, ~ prepost | cond * sp.c)
diffs <- contrast(emm, method = "revpairwise", by = c("cond", "sp.c"))
contrast(diffs, method = "pairwise", by = "sp.c",adjust = "bonferroni")
res   <- contrast(diffs, method = "pairwise", by = "sp.c", adjust = "bonferroni")
ci_link <- summary(res, infer = c(TRUE, TRUE))   # adds lower.CL / upper.CL
ci_link



logit_model <- glmmTMB(coord ~ sp.c * prepost * cond + (prepost * sp.c | sub), 
                       contrasts= list(cond= "contr.sum", prepost= "contr.sum"), 
                       data = d[d$game=="coordination",])
Anova(logit_model, type = 3)
emm <- emmeans(logit_model, ~ prepost | cond)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm3 <- data.frame(pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE)))
summary(emm3)
emm3$cond <- factor(emm3$cond, levels = c("silent", "isot", "mus"))

ggplot(emm3, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Coordination",
    x = "Condition",
    y = "Contrast Estimate",
    color = ""
  ) +
  theme_minimal(base_size = 14)





#######SAVAGE RATINGS ################
savage= read.csv("Savage_Scores_20-May-2025.csv", stringsAsFactors = T)
d <- dplyr::select(savage, ParticipantPublicID, Response, Question, prePost, cond)
colnames(d) = c("sub", "response", "question", "prepost", "cond")
d$dyad = as.factor(substr(d$sub, start = 1, stop = 5))
length(unique(d$sub))

d = d %>%
  mutate(prepost = as.factor(ifelse(prepost==1, "Pre", "Post")))
d$prepost = as.factor(d$prepost)
d$prepost <- factor(d$prepost, levels = c("Pre", "Post"))
# All data seems there: 
xtabs(~sub+prepost, d)

#visualize the difference between pre and post
d_scaled <- d %>%
  group_by(question) %>%
  mutate(z_response = scale(response)) %>%
  ungroup()


ggplot(d_scaled, aes(x = prepost, y = z_response, color = question)) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.5), size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.5), width = 0.1, alpha = 0.6) +
  facet_wrap(~cond, scales = "free_y") +
  labs(
    title = "Change in Savage Scores by Condition and Question",
    x = "Session",
    y = "Z-Score Response",
    color = "Condition"
  ) +
  theme_minimal(base_size = 14)

#points instead of boxes but combines average of pre/post
prepost_diff <- d_scaled %>%
  group_by(sub, cond, question, prepost, dyad) %>%
  summarise(mean_z = mean(z_response, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = prepost, values_from = mean_z) %>%
  mutate(diff = Post - Pre)



#graph with average change in z score (one point)
group_avg_diff <- prepost_diff %>%
  group_by(question, cond) %>%
  summarise(
    mean_diff = mean(diff, na.rm = TRUE),
    se = sd(diff, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
summary(prepost_diff)
#graph z-score post-pre
prepost_diff$cond <- factor(prepost_diff$cond, levels = c("silent", "isot", "mus"))
ggplot(prepost_diff, aes(x = cond, y = diff, color = question)) +
  stat_summary(fun=mean, geom="point", size=4, position = position_dodge(width = 1)) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width=0, alpha=0.6, position = position_dodge(width = 1)) + 
  #geom_errorbar(aes(ymin = mean_diff - se, ymax = mean_diff + se),
  #width = 0.15) + 
  #position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Average Change in Savage Scores (Post – Pre)",
    x = "Condition",
    y = "Mean Z-Score Difference",
    color = "Question"
  ) +
  theme_minimal(base_size = 14)

#significance testing 
leveneTest(diff ~ cond * question, data = prepost_diff)
anova_model <- lmer(diff ~ cond * question + (1|dyad/sub), data = prepost_diff,
                    contrasts = list(cond = "contr.sum", question = "contr.sum"))
Anova(anova_model, type = 3)
emm <- emmeans(anova_model, ~ cond| question)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm <- emmeans(anova_model, ~ cond)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm <- emmeans(anova_model, ~ question)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

eta_squared(anova_model)

emm <- emmeans(anova_model, pairwise ~ cond, infer= c(T,T))
summary(emm)
confint(emm)

emm2 = as.data.frame(emmeans(anova_model, ~ cond * question))
emm2$cond <- factor(emm2$cond, levels = c("silent", "isot", "mus"))
summary(emm2)

dodge <- position_dodge(width = 0.4)

ggplot(emm2, aes(x = cond, y = emmean, color = question)) +
  geom_line(position = dodge, size = 1) + 
  geom_point(position = dodge, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                position = dodge, width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Self-reported attitudinal prosociality",
    x = "",
    y = "Ratings (post-pre)",
    color = "question"
  ) +
  theme_minimal(base_size = 14) 




#since there was no signicant interaction see which cond changed most
overall_diff <- prepost_diff %>%
  group_by(sub, cond, dyad) %>%
  summarise(mean_diff = mean(diff, na.rm = TRUE), .groups = "drop")

summary_stats <- overall_diff %>%
  group_by(cond) %>%
  summarise(
    mean_diff = mean(mean_diff, na.rm = TRUE),
    se = sd(mean_diff, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(summary_stats, aes(x = cond, y = mean_diff, color = cond)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean_diff - se, ymax = mean_diff + se), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Overall Change in Savage Scores (Post – Pre)",
    x = "Condition",
    y = "Mean Z-Score Change"
  ) +
  theme_minimal(base_size = 14)

#significant testing for cond
leveneTest(mean_diff ~ cond, data = overall_diff)
anova_model <- lm(mean_diff ~ cond, data = overall_diff,
                  contrasts = list(cond = "contr.sum"))
Anova(anova_model, type = 3)
emm <- emmeans(anova_model, ~ cond)
pairs(emm, adjust = "tukey", infer = c(TRUE, TRUE))
eta_squared(anova_model)
emm <- emmeans(anova_model, pairwise ~ cond)
summary(emm)


emm2 = as.data.frame(emmeans(anova_model, ~ cond))
emm2$cond <- factor(emm2$cond, levels = c("silent", "isot", "mus"))

ggplot(emm2, aes(x = cond, y = emmean, color = cond)) +
  geom_line(position = dodge, size = 1) + 
  geom_point(position = dodge, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                position = dodge, width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Self-reported attitudinal prosociality scores",
    x = "",
    y = "Ratings (post-pre)",
    color = "cond"
  ) +
  theme_minimal(base_size = 14) 


df_ratings = d
summary(df_ratings)


#######MUSIC BOX############
## INDIVIDUAL MEASURES: SD of velocity of rotation how much stable they play, the smaller the more stable, it's a proxy of adaptation
## DYADIC MEASURES: 
# 1) PLV phase locking values (0 = different , 1 perfectly aligned) period matching regardless of phase and temporal line
# 2) phase_diff_unwrap. phase matching (unwrap data e substract one from the other) __ are they in the same bar and same note
# 3) phase_diff_signed. who is leading? subtract one from the other. if positive a is leading, if negative b is leading. but it makes 
#    sense to extract individual category of leader or not. averaging across dyads doesn't make sense.
# 4) phase_diff_abs. similar to PLV period matching just how different is the angle regardless of where they are in the phase
# 5) phase_diff_std. std of phase difference, if low SD they are not variable, yet they might be synchronized 
# 6) phase_diff_unwr_abs, phase_diff_unwr_std
# check /remove outlier trials on the most sensitive measure (phase_diff_unwrap, plv, phase_diff_std)
# check their relations
emb = read.csv(paste0("file_info_with_measures_2025-07-14_switched_reslength.csv"), stringsAsFactors = T)
emb$dyad = as.factor(substr(emb$fileName, start = 8, stop = 14))
emb$dyad <- as.factor(gsub("_d", "", emb$dyad))
emb <- dplyr::select(emb, !fileName)
#add dyad info taken from debrief df (not in debrief- in savage)
maps = unique(df_ratings[,c('cond','dyad')])
emb = merge(emb, maps, all.x=T)
#emb <- emb %>%
  #select(-mean_eMB1, -mean_eMB2, -trial_counter, -timestmp)  # Replace with actual column names you want to remove
emb <- dplyr::select(emb, -mean_eMB1, -mean_eMB2, -trial_counter, -timestmp)
# Reshape the dataframe by creating sub column while keeping all original columns
df_emb <- emb %>%
  pivot_longer(cols = c(velrot_emb1, velrot_emb2, std_velrot_emb1, std_velrot_emb2), 
               names_to = "name", values_to = "value") %>%
  mutate(
    type = case_when(
      grepl("std_velrot_emb", name) ~ "SDvelrotEMB",  # Check for std_velrot_emb first
      grepl("velrot_emb", name) ~ "velrotEMB",
      TRUE ~ NA_character_  # Ensure all cases are covered
    ),
    name = str_extract(name, "\\d+$"),  # Extract only the last digit (1 or 2)
    name = ifelse(name == "1", "a", "b"),  # Map 1 -> "a", 2 -> "b"
    dyad = str_remove(dyad, "_$"),  # Remove trailing underscore from dyad (if exists)
    sub = paste(dyad, name, sep = "_")  # Combine dyad and new name with underscore
  ) %>%
  dplyr:: select(sub, type, everything())  # Keep all columns and classify type
length(unique(emb$dyad))
summary(df_emb)
# if there is a mismatch of sub between the gorilla and the emb files, NA are generated
df_emb[!complete.cases(df_emb), ]  
# check <- df_emb %>% 
#   filter(dyad == "FM_3") %>% 
#   select(cond, plv, sub, dyad)
df_emb <- df_emb %>% drop_na(cond)

# plot single subject measures
ggplot(df_emb, aes(trial, value, group = cond, color = cond, fill = cond)) +theme_light() +
  stat_summary(fun = mean, geom = "line", alpha = 0.8) + facet_wrap(~type) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, colour = NA, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", alpha = 0.8, size = 1) +
  xlab("trials") + ylab("Value") + theme(legend.position = "bottom") 

df_emb$trial.c=scale(df_emb$trial)

leveneTest(value ~ cond * type, data = df_emb)
anova_model <- lmer(value ~ cond * trial * type + (1 | sub), data = df_emb,
                    contrasts = list(
                      cond = "contr.sum",
                      type = "contr.sum" ))
Anova(anova_model, type = 3)
emm <- emmeans(anova_model, ~ cond | type)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
eta_squared(anova_model, partial = TRUE)
summary(df_emb)

# plot dyadic measures
ag <-with(df_emb, aggregate(cbind(plv, phase_diff_unwrap, phase_diff_abs, phase_diff_unwr_abs, phase_diff_std, phase_diff_unwr_std, phase_diff_signed, resultant_length, phase_diff_wrapped_deg)~dyad+cond, FUN="mean"))
ag2<-with(df_emb, aggregate(cbind(plv, phase_diff_abs, phase_diff_std, phase_diff_wrapped_deg)~dyad+cond, FUN="mean"))
ggplot(ag, aes(x=cond, y= phase_diff_unwr_abs,  fill = cond, col = cond)) +theme_classic()+
  stat_summary(fun=mean, geom="point", size=4)+
  geom_jitter(aes(group = dyad),  alpha = 0.2, width = 0.1, height = 0.02) +
  stat_summary(fun.data = 'mean_se', geom = "errorbar", width=0, alpha=0.6)+
  xlab("") + ylab("value")+ggtitle('Phase Diff Unwrapped Abs')

summary(ag)

leveneTest(phase_diff_unwr_std ~ cond, data = ag) #if the p-value is greater than 0.05, it means there is no evidence to suggest that the variances are different across groups. 
kruskal.test(phase_diff_unwr_abs ~ cond, data = ag) # no significant difference between cond
wilcox.test(phase_diff_unwr_abs ~ cond, data = ag %>% filter(cond %in% c("isot", "mus")))
wilcox.test(phase_diff_unwr_abs ~ cond, data = ag %>% filter(cond %in% c("silent", "mus")))
wilcox.test(phase_diff_unwr_abs ~ cond, data = ag %>% filter(cond %in% c("isot", "silent")))

anova_model <- lm(phase_diff_unwr_abs ~ cond, data = ag, 
                  contrasts = list(cond = "contr.sum"))
Anova(anova_model, type = 3)
emm <- emmeans(anova_model, ~ cond)
summary(emm)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
eta_squared(anova_model, partial = TRUE)

emm_df <- as.data.frame(emm)

emm2 = as.data.frame(emmeans(anova_model, ~ cond))

emm2$cond <- factor(emm2$cond, levels = c("silent", "isot", "mus"))

ggplot(emm2, aes(x = cond, y = emmean, col= cond)) +
  geom_line() + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(limits = c(min(emm2$lower.CL) - 20, max(emm2$upper.CL) + 20)) +
  labs(
    title = "Phase Diff Unwrapped Absolute",
    y = "Value",
    x = ""
  ) +
  theme_minimal() +
  stat_summary(fun.data = mean_cl_boot)



#phase_diff_unwr
ggplot(emm_df, aes(x = cond, y = emmean, fill = cond, color = cond)) +
  geom_point(size = 4) +  # Means
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, alpha = 0.7) +
  scale_y_continuous(limits = c(min(emm_df$lower.CL) - 15, max(emm_df$upper.CL) + 30)) +  # dynamic padding
  theme_classic() +
  labs(
    title = "Phase Diff Absolute",
    y = "Value",
    x = ""
  ) +
  theme(legend.position = "none")


#PLV
ggplot(emm_df, aes(x = cond, y = emmean, fill = cond, color = cond)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, alpha = 0.6) +
  geom_point(data = ag, aes(x = cond, y = plv, group = dyad),
             alpha = 0, color = "black", shape = 16, size = 2, inherit.aes = FALSE) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.01)) +  # full PLV range
  theme_classic() +
  labs(
    title = "PLV",
    x = "",
    y = "Phase Locking Value"
  )



# find leader / follower. if positive a is leading, if negative b is leading.
df_leader <-with(df_emb, aggregate(cbind(phase_diff_signed)~sub+cond+dyad, FUN="mean"))
df_leader <- df_leader %>%
  group_by(dyad) %>%
  mutate(leader = case_when(
    phase_diff_signed > 0 & grepl("_a$", sub) ~ "leader",
    phase_diff_signed < 0 & grepl("_b$", sub) ~ "leader",
    TRUE ~ "follower"
  ))
xtabs( ~ cond+leader, df_leader )
xtabs( ~ cond, df_leader )





#######CONNECTING THE VARIABLES#######

#risk by follower/leader
leader_roles <- df_leader %>%
  dplyr::select(sub, leader, dyad) %>%
  distinct()

coord_leader <- d %>%
  filter(game == "coordination") %>%
  left_join(leader_roles, by = "sub") %>%
  filter(!is.na(leader))

summary(coord_leader)

ggplot(coord_leader, aes(x = sp, y = payoff2, colour = cond, linetype = prepost)) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_grid(leader ~cond) +
  labs(
    title = "Payoff",
    x = "sp value",
    y = "Payoff value",
    linetype = "Session"
  ) +
  theme_minimal(base_size = 14)

ggplot(coord_leader, aes(x = sp, y = payoff2, colour = leader)) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~leader) +
  labs(
    title = "Payoff",
    x = "sp value",
    y = "Payoff value",
    color = "Roles"
  ) +
  theme_minimal(base_size = 14)


coord_leader$sp.c=scale(coord_leader$sp)

logit_model <- glmmTMB(resp.i ~ sp.c * leader + prepost * cond * leader + (prepost + sp.c | sub), 
                       contrasts= list(leader= "contr.sum", cond= "contr.sum", prepost= "contr.sum"), 
                       data = coord_leader, family = "binomial")
Anova(logit_model, type = 3)


logit_model <- glmmTMB(payoff2 ~ sp.c * leader + prepost * cond * leader + (prepost + sp.c | sub), 
                       contrasts= list(leader= "contr.sum", cond= "contr.sum", prepost= "contr.sum"), 
                       data = coord_leader)
Anova(logit_model, type = 3)

emm <- emmeans(logit_model, ~ leader)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
contrast(emm)


emt = emtrends(logit_model, pairwise ~ leader, var = "sp.c")
summary(emt, infer = c(TRUE, TRUE))

emm   <- emmeans(logit_model, ~ prepost | cond * leader)
diffs <- contrast(emm, method = "revpairwise", by = c("cond","leader"))
res   <- contrast(diffs, method = "pairwise", by = "leader", adjust = "bonferroni")
ci_link <- summary(res, infer = c(TRUE, TRUE))   # adds lower.CL / upper.CL
ci_link


#risk graph
emm <- emmeans(logit_model, ~ leader)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
contrast(emm)

emm <- emmeans(logit_model, pairwise~ leader,  infer = c(TRUE, TRUE), type = "response")
summary(emm)
emm2 = as.data.frame(emm <- emmeans(logit_model, ~ leader, type = "response"))

emm3 <- data.frame(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
summary(emm3)

ggplot(emm2, aes(x = leader, y = prob, color= leader)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  scale_y_continuous(limits = c(min(emm2$asymp.LCL) - .05, max(emm2$asymp.UCL) + 0)) +
  #facet_wrap(~ leader) +
  labs(
    title = "Risk",
    x = "Role",
    y = "Risk Probability",
    color = "Roles"
  ) +
  theme_minimal(base_size = 14)

emm <- emmeans(logit_model, pairwise~ leader,  infer = c(TRUE, TRUE), type = "response")
summary(emm)
emm2 = as.data.frame(emm <- emmeans(logit_model, ~ leader, type = "response"))
emm2$cond <- factor(emm2$cond, levels = c("silent", "isot", "mus"))
ggplot(emm2, aes(x = cond, y = emmean, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) + # We do not need the dashed line as here it is meaningless.
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~phase_bini) +
  labs(
    title = "Self-reported prosocialiaty",
    x = "Condition",
    y = "Ratings",
    color = "Cond"
  ) +
  theme_minimal(base_size = 14)

#payoff graph
emt = emtrends(logit_model, pairwise ~ leader, var = "sp.c")
summary(emt, infer = c(TRUE, TRUE))
emt_df <- as.data.frame(emt$emtrends)# pull the slope estimates
summary(emt_df)

emt_df1 <- as.data.frame(emt)
summary(emt_df1)

ggplot(emt_df, aes(x = leader, y = sp.c.trend, color = leader)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(x = "Role", 
       y = "Slope of SP",
       color = "Roles",
       title = "Payoff") +
  theme_minimal(base_size = 14)


emm <- emmeans(logit_model, ~ leader| cond)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm3 <- data.frame(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
summary(emm3)

ggplot(emm3, aes(x = cond, y = emmean, color= cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  facet_wrap(~ leader) +
  labs(
    title = "Risk",
    x = "Condition",
    y = "Emmean",
    color = "Cond"
  ) +
  theme_minimal(base_size = 14)


emm2 = as.data.frame(emmeans(logit_model, ~ leader| cond))
summary(emm2)


ggplot(emm2, aes(x = cond, y = emmean, col= cond)) +
  geom_line() + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(limits = c(min(emm2$lower.CL) - 20, max(emm2$upper.CL) + 20)) +
  labs(
    title = "Phase Diff Unwrapped Absolute",
    y = "Value",
    x = ""
  ) +
  theme_minimal() +
  stat_summary(fun.data = mean_cl_boot)

logit_model_coord <- glmmTMB(resp.i ~ sp.c * prepost * cond + (prepost * sp.c | sub), 
                             contrasts= list(cond= "contr.sum", prepost= "contr.sum"), 
                             data = coord_leader, family = "binomial")
Anova(logit_model_coord, type = 3)

emm <- emmeans(logit_model, ~ prepost | leader * cond)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm3 <- data.frame(pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE)))

summary(emm3)

ggplot(emm3, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~leader) +
  labs(
    title = "Risk",
    x = "Condition",
    y = "Contrast Estimate",
    color = "Question"
  ) +
  theme_minimal(base_size = 14)

emm3 <- data.frame(pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE)))
summary(emm3)

ggplot(emm3, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~type) +
  labs(
    title = "Payoff",
    x = "Condition",
    y = "Contrast Estimate",
    color = "Question"
  ) +
  theme_minimal(base_size = 14)



#risk by SVO type#risk by SVO temmeanype
svo_type_df <- svo %>%
  filter(!is.na(type)) %>%
  group_by(sub) %>%
  summarise(type = first(type), .groups = "drop")  # Keep only unique subject-type pairs

coord_svo <- d %>%
  filter(game == "coordination") %>%
  left_join(svo_type_df, by = "sub")


ggplot(coord_svo, aes(x = sp, y = payoff2, colour = cond, linetype = prepost)) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_grid(rows = vars(type), cols = vars(cond)) +
  labs(
    title = "SVO and risk", 
    x = "sp value",
    y = "risk value",
    linetype = "Session"
  ) +
  theme_minimal(base_size = 14)


coord_svo$sp.c=scale(coord_svo$sp)

logit_model1 <- glmmTMB(resp.i ~ sp.c * type + prepost * cond * type + (prepost + sp.c | sub), 
                       contrasts= list(type= "contr.sum", cond= "contr.sum", prepost= "contr.sum"), 
                       data = coord_svo, family = "binomial")
Anova(logit_model1, type = 3)

emt = emtrends(logit_model, pairwise ~ type, var = "sp.c")
summary(emt, infer = c(TRUE, TRUE))

logit_model2 <- glmmTMB(payoff2 ~ sp.c * type + prepost * cond * type + (prepost + sp.c | sub), 
                       contrasts= list(type= "contr.sum", cond= "contr.sum", prepost= "contr.sum"), 
                       data = coord_svo)
Anova(logit_model2, type = 3)

emm <- emmeans(logit_model1, ~ type)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm <- emmeans(logit_model1, ~ cond | type)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm3 <- data.frame(pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE)))
summary(emm3)

ggplot(emm3, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~type) +
  labs(
    title = "Payoff",
    x = "Condition",
    y = "Contrast Estimate",
    color = "Question"
  ) +
  theme_minimal(base_size = 14)

emm2 = as.data.frame(emmeans(logit_model2, ~ type))

summary(emm2)



ggplot(emm2, aes(x = type, y = emmean, color = type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  #facet_wrap(~leader)+
  labs(
    title = "Payoff",
    x = "Type",
    y = "Emmean",
    color = "Type"
  ) +
  theme_minimal(base_size = 14)


logit_model_coord <- glmmTMB(resp.i ~ sp.c * prepost * cond + (prepost * sp.c | sub), 
                             contrasts= list(cond= "contr.sum", prepost= "contr.sum"), 
                             data = coord_leader, family = "binomial")
Anova(logit_model_coord, type = 3)

emm <- emmeans(logit_model1, ~ prepost | cond * type)
diffs <- contrast(emm, method = "revpairwise", by = c("cond", "type"))
res   <- contrast(diffs, method = "pairwise", by = "type", adjust = "bonferroni")
ci_link <- summary(res, infer = c(TRUE, TRUE))   # adds lower.CL / upper.CL
ci_link

df_diffs <- as.data.frame(summary(diffs, infer = c(TRUE, TRUE)))
summary(df_diffs)

ggplot(df_diffs, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~type)+
  labs(
    title = "Risk",
    x = "Condition",
    y = "Contrast Estimate"
  ) +
  theme_minimal(base_size = 14)



#music box measures w coordination 
agg = d %>%
  group_by(sub, dyad, game, prepost, cond, sp) %>%
  summarise(mean.risk = mean(resp.i), mean.coord = mean(coord, na.rm = TRUE), mean.payoff=mean(payoff2, na.rm = TRUE), .groups = "drop")

agg = d %>%
  group_by(sub, dyad, game, prepost, cond, sp, resp.i, payoff2)

ag <-with(df_emb, aggregate(cbind(plv, phase_diff_unwrap, phase_diff_abs, phase_diff_unwr_abs, phase_diff_std, phase_diff_signed)~dyad+cond+sub, FUN="mean"))
ag2<-with(df_emb, aggregate(cbind(plv, phase_diff_unwr_abs, phase_diff_wrapped_deg)~dyad+cond+sub, FUN="mean"))
summary(ag2)
length(unique(agg$sub))
agg =merge(agg, ag[,c("phase_diff_abs", "phase_diff_signed","plv", "phase_diff_std", "sub", "dyad")], all.x=T)
agg2 =merge(agg, ag2[,c("phase_diff_unwr_abs", "phase_diff_wrapped_deg","plv", "sub", "dyad")])
summary(agg2)
View(agg)
agg_post <- agg2 %>%
  filter(game == "coordination")

write.csv(agg_post, "agg_post.csv", row.names = FALSE)
write.csv(agg_post, "data/agg_post.csv", row.names = FALSE)

summary(agg_post)
length(unique(agg_post$dyad))
nrow(agg_post)
View(agg_post)

ggplot(agg_post, aes(phase_diff_unwr_abs, mean.risk, color = cond, fill = cond)) +theme_light() +
  #stat_summary(fun = mean, geom = "line", alpha = 0.8) + 
  geom_smooth() + 
  facet_wrap(~interaction(cond, game)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, colour = NA, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", alpha = 0.8, size = 1) +
  scale_y_continuous(
    limits = c(min(agg$mean.risk, na.rm = TRUE) - 0.05, 
               max(agg$mean.risk, na.rm = TRUE) + 0.05),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Phase diff wrapped deg unwrapped on risk rates",
    x = "Phase_diff_wrapped_deg",
    y = "Risk"
  ) + theme(legend.position = "bottom") 



ggplot(agg_post, aes(plv, mean.risk, color = cond, fill = cond)) +theme_light() +
  #stat_summary(fun = mean, geom = "line", alpha = 0.8) + 
  geom_smooth(method="lm") + 
  facet_wrap(~interaction(cond, game)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, colour = NA, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", alpha = 0.8, size = 1) +
  scale_y_continuous(
    limits = c(min(agg$mean.risk, na.rm = TRUE) - 0.05, 
               max(agg$mean.risk, na.rm = TRUE) + 0.05),
    breaks = seq(0, 1, 0.25)
  ) +
  xlab("") + ylab("Risk") + theme(legend.position = "bottom") 


agg_post=agg_post[!is.na(agg_post$phase_diff_unwr_abs),]
ggplot(agg_post[agg_post$phase_diff_unwr_abs> 10,], aes(phase_diff_unwr_abs, mean.risk, color = cond, fill = cond)) +theme_light() +
  #stat_summary(fun = mean, geom = "line", alpha = 0.8) + 
  geom_smooth(method="lm") + 
  facet_wrap(~interaction(cond, game),scales="free_x") +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, colour = NA, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", alpha = 0.8, size = 1) +
  scale_y_continuous(
    limits = c(min(agg$mean.risk, na.rm = TRUE) - 0.05, 
               max(agg$mean.risk, na.rm = TRUE) + 0.05),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Phase Diff Unwrapped Absolut on risk rates",
    x = "Phase Diff Unwrapped Absolute",
    y = "Risk"
  ) + theme(legend.position = "bottom") 





maps=unique(agg_post[,c("dyad", "plv")])
nrow(maps)
hist(maps$plv)



risk_model <- glmmTMB(mean.payoff ~ phase_diff_unwr_abs * cond * game + (1 | dyad/sub), data = agg_post,
  contrasts = list(cond = "contr.sum", game = "contr.sum"))

Anova(risk_model, type = 3)
emm <- emmeans(risk_model, ~ phase_diff_unwr_abs * cond | game)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm2 = as.data.frame(emmeans(risk_model, ~ cond * game))
emm2=as.data.frame(emm)
summary(emm2)

ggplot(emm2, aes(x = cond, y = emmean, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~game) +
  labs(
    title = "Payoff",
    x = "Condition",
    y = "Contrast Estimate",
    color = "Question"
  ) +
  theme_minimal(base_size = 14)

  

  
##Use the models for results graph
  agg_post$sp.c=scale(agg_post$sp)
  
agg_post$phase_diff_unwr_abs.med=as.factor(ifelse(agg_post$phase_diff_unwr_abs<median(agg_post$phase_diff_unwr_abs),"low", "high"))

agg_post$phase_bini <- factor(
  ifelse(agg_post$phase_diff_unwr_abs < median(agg_post$phase_diff_unwr_abs), "low", "high"),
  levels = c("low", "high")
)
  
logit_model <- glmmTMB(resp.i ~ sp.c * phase_bini + prepost * cond * phase_bini + (prepost + sp.c | sub), 
                         contrasts= list(cond= "contr.sum", prepost= "contr.sum", phase_bini= "contr.sum"), 
                         data = agg_post, family = "binomial")
Anova(logit_model, type = 3)
  
  
logit_model <- glmmTMB(payoff2 ~ sp.c * phase_bini + prepost * cond * phase_bini + (prepost + sp.c | sub), 
                         contrasts= list(cond= "contr.sum", prepost= "contr.sum", phase_bini= "contr.sum"), 
                         data = agg_post)
Anova(logit_model, type = 3)

emt = emtrends(logit_model, pairwise ~ 1, var = "sp.c")
summary(emt, infer = c(TRUE, TRUE))


emm <- emmeans(logit_model, ~ prepost | phase_bini * cond)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm <- emmeans(logit_model, ~ phase_bini | cond)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm <- emmeans(logit_model, ~ cond)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm2 = as.data.frame(emm <- emmeans(logit_model, ~ phase_bini | cond))
emm2 = as.data.frame(pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE)))

summary(emm2)
emm_df <- as.data.frame(emm)
summary(emm_df)
emm_df$cond <- factor(emm_df$cond, levels = c("silent", "isot", "mus"))

ggplot(emm2, aes(x = cond, y = emmean, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
 facet_wrap(~phase_bini) +
  labs(
    title = "Payoff",
    x = "Condition",
    y = "Emmean",
    color = "Question"
  ) +
  theme_minimal(base_size = 14)

emm <- emmeans(logit_model, pairwise~ phase_bini | cond,  infer = c(TRUE, TRUE), type = "response")
summary(emm)
emm2 = as.data.frame(emm <- emmeans(logit_model, ~ phase_bini | cond, type = "response"))
emm2$cond <- factor(emm2$cond, levels = c("silent", "isot", "mus"))
ggplot(emm2, aes(x = cond, y = prob, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  # geom_hline(yintercept = 0, linetype = “dashed”, color = “gray50") + # We do not need the dashed line as here it is meaningless.
  facet_wrap(~phase_bini) +
  labs(
    title = "Risk",
    x = "Condition",
    y = "Risk Probability",
    color = "Cond"
  ) +
  theme_minimal(base_size = 14)

emm <- emmeans(logit_model, ~ prepost | cond * phase_bini)
diffs <- contrast(emm, method = "revpairwise", by = c("cond", "phase_bini"))
res   <- contrast(diffs, method = "pairwise", by = "phase_bini", adjust = "bonferroni")
ci_link <- summary(res, infer = c(TRUE, TRUE))   # adds lower.CL / upper.CL
ci_link

df_diffs <- as.data.frame(summary(diffs, infer = c(TRUE, TRUE)))
summary(df_diffs)

ggplot(df_diffs, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~phase_bini)+
  labs(
    title = "Payoff",
    x = "Condition",
    y = "Contrast Estimate"
  ) +
  theme_minimal(base_size = 14)

emt = emtrends(logit_model, pairwise ~ cond, var = "phase_bini")
summary(emt, infer = c(TRUE, TRUE))


##Leader/follower on savage
leader_roles <- df_leader %>%
  dplyr::select(sub, leader) %>%
  distinct()

df_leader <- d_scaled %>%
  left_join(leader_roles, by = "sub") %>%
  filter(!is.na(leader))
summary(df_leader)

ggplot(df_leader, aes(x = cond, y = z_response, color = cond)) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.5), size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.5), width = 0.1, alpha = 0.6) +
  facet_wrap(~leader) +
  labs(
    title = "Savage Scores by Leader/Follower Roles",
    x = "",
    y = "Z-Score Response",
    color = "Condition"
  ) +
  theme_minimal(base_size = 14)

leveneTest(z_response ~ cond, data = overall_diff)
anova_model2 <- lmer(z_response ~ leader * cond * prepost + (1 | dyad/sub),data = df_leader,
                    contrasts = list(prepost = "contr.sum", cond    = "contr.sum", leader  = "contr.sum"))
Anova(anova_model2, type = 3)
emm <- emmeans(anova_model2, ~ leader)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
eta_squared(anova_model)
emm <- emmeans(anova_model, pairwise ~ cond)
summary(emm2)

emm <- emmeans(anova_model2, ~ leader)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
contrast(emm)

emm3 <- data.frame(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
summary(emm3)

emm <- emmeans(anova_model2, pairwise~ leader,  infer = c(TRUE, TRUE), type = "response")
summary(emm)
emm2 = as.data.frame(emm <- emmeans(anova_model2, ~ leader, type = "response"))

ggplot(emm2, aes(x = leader, y = emmean, color= leader)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  #facet_wrap(~ leader) +
  labs(
    title = "Self-reported prosociality",
    x = "Role",
    y = "Ratings",
    color = "Roles"
  ) +
  theme_minimal(base_size = 14)


emm2 = as.data.frame(emmeans(anova_model2, pairwise ~ leader))

summary(emm2)



ggplot(emm2, aes(x = leader, y = emmean, color = leader)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  #facet_wrap(~leader)+
  labs(
    title = "Savage",
    x = "Role",
    y = "Emmean",
    color = "Roles"
  ) +
  theme_minimal(base_size = 14)

emm <- emmeans(anova_model2, ~  prepost | leader * cond)
diffs <- contrast(emm, method = "revpairwise", by = c("cond", "leader"))
res   <- contrast(diffs, method = "pairwise", by = "leader", adjust = "bonferroni")
ci_link <- summary(res, infer = c(TRUE, TRUE))   # adds lower.CL / upper.CL
ci_link

df_diffs <- as.data.frame(emm <- emmeans(anova_model2, ~ leader| cond))
summary(df_diffs)

ggplot(df_diffs, aes(x = cond, y = emmean, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~leader)+
  labs(
    title = "Savage",
    x = "Condition",
    y = "Contrast Estimate"
  ) +
  theme_minimal(base_size = 14)

##SVO on savage

svo_type_df <- svo %>%
  filter(!is.na(type)) %>%
  group_by(sub) %>%
  summarise(type = first(type), .groups = "drop")

df_type <- d_scaled %>%
  left_join(svo_type_df, by = "sub") %>%
  filter(!is.na(type))
summary(df_type)


ggplot(df_type, aes(x = cond, y = z_response, color = cond)) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.5), size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.5), width = 0.1, alpha = 0.6) +
  facet_wrap(~type) +
  labs(
    title = "Savage Scores by SVO",
    x = "",
    y = "Z-Score Response",
    color = "Condition"
  ) +
  theme_minimal(base_size = 14)

anova_model <- lmer(z_response ~ type * cond * prepost + (1 | dyad/sub),data = df_type,
                    contrasts = list(prepost = "contr.sum", cond    = "contr.sum", type  = "contr.sum"))
Anova(anova_model, type = 3)
emm <- emmeans(anova_model, ~ type)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
eta_squared(anova_model)
emm <- emmeans(anova_model, pairwise ~ cond)
summary(emm2)

emm <- emmeans(anova_model, ~ type)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))

emm3 <- data.frame(pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE)))
summary(emm3)

emm2 = as.data.frame(pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE)))

ggplot(emm3, aes(x = cond, y = emmean, color = cond)) +
  geom_line(position = dodge, size = 1) + 
  geom_point(position = dodge, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                position = dodge, width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~type) +
  labs(
    title = "Savage",
    x = "Condition",
    y = "Contrast Estimate",
    color = "cond"
  ) +
  theme_minimal(base_size = 14)

emm2 = as.data.frame(emmeans(anova_model, ~ type))

summary(emm2)



ggplot(emm2, aes(x = type, y = emmean, color = type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  #facet_wrap(~leader)+
  labs(
    title = "Savage",
    x = "Type",
    y = "Emmean",
    color = "Type"
  ) +
  theme_minimal(base_size = 14)

emm <- emmeans(anova_model, ~ prepost | cond * type)
diffs <- contrast(emm, method = "revpairwise", by = c("cond", "type"))
res   <- contrast(diffs, method = "pairwise", by = "type", adjust = "bonferroni")
ci_link <- summary(res, infer = c(TRUE, TRUE))   # adds lower.CL / upper.CL
ci_link

df_diffs <- as.data.frame(summary(diffs, infer = c(TRUE, TRUE)))
summary(df_diffs)

ggplot(df_diffs, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~type)+
  labs(
    title = "Savage",
    x = "Condition",
    y = "Contrast Estimate"
  ) +
  theme_minimal(base_size = 14)


##Phase Diff Unwr Abs on Savage

df_phase = d_scaled %>%
  group_by(sub, dyad, response, prepost, cond, z_response)

ag2<-with(df_emb, aggregate(cbind(plv, phase_diff_unwr_abs)~dyad+cond+sub, FUN="mean"))

df_phase2 =merge(df_phase, ag2[,c("phase_diff_unwr_abs", "sub", "dyad")])
summary(df_phase2)

df_phase2$phase_bini <- factor(
  ifelse(df_phase2$phase_diff_unwr_abs < median(df_phase2$phase_diff_unwr_abs), "low", "high"),
  levels = c("low", "high")
)

anova_model1 <- lmer(z_response ~ phase_bini * cond * prepost + (1 | dyad/sub),data = df_phase2,
                    contrasts = list(prepost = "contr.sum", cond= "contr.sum", phase_bini= "contr.sum"))
Anova(anova_model1, type = 3)
emm <- emmeans(anova_model1, ~ phase_bini| cond * prepost)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
emm <- emmeans(anova_model1, ~ cond| prepost)
pairs(emm, adjust = "bonferroni", infer = c(TRUE, TRUE))
eta_squared(anova_model)
emm <- emmeans(anova_model, pairwise ~ cond)
summary(emm2)


emm2 = as.data.frame(emm <- emmeans(anova_model1, ~ prepost| cond * phase_bini))
emm2$cond <- factor(emm2$cond, levels = c("silent", "isot", "mus"))


ggplot(emm2, aes(x = cond, y = emmean, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~phase_bini)+
  labs(
    title = "Savage",
    x = "Condition",
    y = "Contrast Estimate"
  ) +
  theme_minimal(base_size = 14)

emm <- emmeans(anova_model1, pairwise~ phase_bini| cond,  infer = c(TRUE, TRUE), type = "response")
summary(emm)
emm2 = as.data.frame(emm <- emmeans(anova_model1, ~ phase_bini| cond, type = "response"))
emm2$cond <- factor(emm2$cond, levels = c("silent", "isot", "mus"))
ggplot(emm2, aes(x = cond, y = emmean, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) + # We do not need the dashed line as here it is meaningless.
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~phase_bini) +
  labs(
    title = "Self-reported prosocialiaty",
    x = "Condition",
    y = "Ratings",
    color = "Cond"
  ) +
  theme_minimal(base_size = 14)


emm <- emmeans(anova_model1, ~ prepost | cond* phase_bini)
diffs <- contrast(emm, method = "revpairwise", by = c("cond", "phase_bini"))
res   <- contrast(diffs, method = "pairwise", by = "phase_bini", adjust = "bonferroni")
ci_link <- summary(res, infer = c(TRUE, TRUE))   # adds lower.CL / upper.CL
ci_link

df_diffs <- as.data.frame(summary(diffs, infer = c(TRUE, TRUE)))
summary(df_diffs)

ggplot(df_diffs, aes(x = cond, y = estimate, color = cond, group = cond)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~phase_bini)+
  labs(
    title = "Savage",
    x = "Condition",
    y = "Contrast Estimate"
  ) +
  theme_minimal(base_size = 14)



#####ggpairs
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}



maps=unique(agg_post[,c("dyad", "plv", "phase_diff_wrapped_deg", "phase_diff_unwr_abs")])
maps=maps[!is.na(maps$plv),]
g = ggpairs(maps[maps$plv>.6,],columns = 2:4, lower = list(continuous = my_fn))
g
summary(maps)
nrow(maps)

g = ggpairs(maps[maps$plv>.6,],columns = 2:4, lower = list(continuous = my_fn))
g

g = ggpairs(df_emb[df_emb$plv>.6,],columns = 6:14, lower = list(continuous = my_fn))
g

summary(df_emb)
