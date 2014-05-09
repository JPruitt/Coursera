load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/evals.RData"))

summary(evals$score)
boxplot(evals$score)

plot(evals$bty_avg ~ evals$age)
boxplot(evals$age ~ evals$gender)
mosaicplot(evals$rank ~ evals$gender)

plot(evals$score ~ evals$bty_avg)

plot(evals$score ~ jitter(evals$bty_avg))

m_bty<-lm(score ~ bty_avg, data = evals)
plot(evals$score ~ jitter(evals$bty_avg))
abline(m_bty)

names(evals)

plot(evals$bty_avg ~ evals$bty_f1lower)
correlation<-cor(evals$bty_avg, evals$bty_f1lower)
correlation

plot(evals[, 13:19])

plot(m_bty)

m_bty_gen<-lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

plot(evals$score ~ evals$bty_avg + evals$gender)
multiLines(m_bty_gen)

m_bty_rank<-lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)

m_full = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m_full)

m_new = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_credits + bty_avg, data = evals)
summary(m_new)

# The full model:
m_full = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m_full)$adj.r.squared

# Remove rank:
m1 = lm(score ~ ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m1)$adj.r.squared

# Remove ethnicity:
m2 = lm(score ~ rank + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m2)$adj.r.squared

# Remove gender:
m3 = lm(score ~ rank + ethnicity + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m3)$adj.r.squared

# Remove language:
m4 = lm(score ~ rank + ethnicity + gender + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m4)$adj.r.squared

# Remove age:
m5 = lm(score ~ rank + ethnicity + gender + language + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m5)$adj.r.squared

# Remove cls_perc_eval:
m6 = lm(score ~ rank + ethnicity + gender + language + age + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m6)$adj.r.squared

# Remove cls_students:
m7 = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m7)$adj.r.squared

# Remove cls_level:
m8 = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_profs + cls_credits + bty_avg, data = evals)
summary(m8)$adj.r.squared

# Remove cls_students:
m9 = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m9)$adj.r.squared

# Remove cls_level:
m10 = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_profs + cls_credits + bty_avg, data = evals)
summary(m10)$adj.r.squared

# Remove cls_profs:
m11 = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_credits + bty_avg, data = evals)
summary(m11)$adj.r.squared

# Remove cls_credits:
m12 = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + bty_avg, data = evals)
summary(m12)$adj.r.squared

# Remove bty_avg:
m13 = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits, data = evals)
summary(m13)$adj.r.squared

