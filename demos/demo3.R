rm(list=ls())
library(lme4)
library(arm)
library(MASS)
library(tidyverse)

data(nlschools)

M0 <- lmer(lang ~ 1 + (1 | class), REML=FALSE, data=nlschools)
summary(M0)

# Extract random effects and compute CIs
reDF <- tibble(
    est = ranef(M0)$class[,1],
    se = se.ranef(M0)$class[,1],
    `2.5 %` = est - 1.96*se,
    `97.5 %` = est + 1.96*se) %>%
    mutate(trueID = 1:n()) %>%
    arrange(-est) %>%
    mutate(id=1:n()) %>%
    mutate(Significant=(0 > `97.5 %` & 0 >`2.5 %`) | (0 < `97.5 %` & 0 <`2.5 %`)) %>%
    mutate(model="Random Effects")

# Compare with the fixed effects estimates 
parEst <- lm(
    lang ~ class, data=nlschools, contrasts=list(class = contr.sum))

# calc the stats for the last coefficient
muLast <- -sum(coefficients(parEst)[-1])
sterrLast <- sqrt(sum(vcov(parEst)[-1,-1]))


feDF <- confint(parEst) %>%
    as_tibble %>%
    mutate(coef=names(parEst$coefficients)) %>%
    mutate(est=unname(parEst$coefficients)) %>%
    filter(coef != "(Intercept)") %>%
    rbind(tibble(
        `2.5 %` = muLast - 1.98*sterrLast,
        `97.5 %` = muLast + 1.98*sterrLast,
        coef = paste0("schoolid", length(unique(nlschools$class))),
        est = muLast
    )) %>%
    mutate(trueID=1:n()) %>%
    arrange(-est) %>%
    mutate(id=1:n()) %>%
    mutate(Significant=(0 > `97.5 %` & 0 >`2.5 %`) | (0 < `97.5 %` & 0 <`2.5 %`)) %>%
    mutate(model="Fixed Effects")

bind_rows(reDF, feDF) %>%
    ggplot(aes(x=id, y=est, ymin=`2.5 %`, ymax=`97.5 %`, color=Significant)) +
    coord_flip() +
    theme_classic() +
    geom_point() +
    geom_errorbar() +
    labs(x="School Effects", y="Parameter Estimates") +
    theme(
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
    geom_hline(yintercept = 0, linetype=2) +
    ggtitle("School Effects As a Difference From the Mean(w/ 95% CI)") +
    facet_wrap(~model)

left_join(reDF, feDF,by="trueID") %>%
    ggplot(aes(est.x, est.y)) +
    geom_point() +
    theme_classic()

# check
nlschools %>%
    group_by(class) %>%
    summarise(n=n(), sd=sd(lang), mu=mean(lang)) %>%
    mutate(st.err=sd/sqrt(n))


