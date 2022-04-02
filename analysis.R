
# MAC study

# Setup -------------------------------------------------------------------

# Required packages
pacs <- c("tidyverse", "haven", "tables", "lme4", "lmerTest", "emmeans")
sapply(pacs, require, character.only = TRUE)

# Function to search variables
search_var <- function(df, name) {
  data.frame(num=grep(name, names(df), ignore.case=TRUE),
             var=grep(name, names(df), ignore.case=TRUE, value=TRUE))
}

# Path to data file
setwd("C:\\Users\\koda\\Dropbox\\Nutrition\\Julie Jones")

# Read data ---------------------------------------------------------------

# Read data, n obs = 70
# mac0 <- read_sav("MAC Endpoint data with baseline values.sav")
mac0 <- read_sav("MAC Endpoint data with baseline values 102121.sav")

# From n = 35 subjects (one for control, one for mac)
n_distinct(mac0$ID)

# Data including all subjects
mac <- mac0 %>% 
  mutate(ID = factor(ID),
         Treatment = factor(Treatment, labels = c("Control", "Mac")),
         Treatment = relevel(Treatment, ref = "Mac"),
         Gender = factor(Gender),
         Group = factor(Group, labels = c("Mac-Control", "Control-Mac")),
         Phase = factor(Phase),
         BaseBMI = ifelse(BMIB >= 30, 1, 0),
         BaseBMI = factor(BaseBMI, labels = c("<30", ">=30")),
         BaseWC = ifelse(WCbaseline >= 108, 1, 0),
         BaseWC = factor(BaseWC, labels = c("<108", ">=108")),
         BaseBF = ifelse(PBFPercentBodyFat >= 43, 1, 0),
         BaseBF = factor(BaseBF, labels = c("<43", ">=43")))

# Number of subjects for each sequence group
mac %>% 
  filter(Phase == 1) %>% 
  group_by(Group) %>% 
  tally()

# Female only (for sub-group analysis)
# Changing cut-off values for WC and body fat
mac_fem <- mac %>% 
  filter(Gender == "F") %>% 
  mutate(BaseWC = ifelse(WCbaseline >= 100, 1, 0),
         BaseWC = factor(BaseWC, labels = c("<100", ">=100")),
         BaseBF = ifelse(PBFPercentBodyFat >= 44, 1, 0),
         BaseBF = factor(BaseBF, labels = c("<44", ">=44")))

# Insulin resistance variables (Descriptive) ------------------------------

# Histograms
mac %>% 
  select(GlucosemgdL, InsulinuIUml, HOMA_IR) %>% 
  pivot_longer(GlucosemgdL:HOMA_IR, names_to = "var", values_to = "value") %>% 
  mutate(var = factor(var, levels = c("GlucosemgdL", "InsulinuIUml", "HOMA_IR"))) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~ var, scales = "free")

# Profile plots by sequence group
# pdf("plot by seq.pdf", height = 8, width = 6)
mac %>% 
  select(ID, Group, Phase, GlucosemgdL, InsulinuIUml, HOMA_IR) %>% 
  pivot_longer(GlucosemgdL:HOMA_IR, names_to = "variable", values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("GlucosemgdL", "InsulinuIUml", "HOMA_IR"))) %>% 
  ggplot(aes(x = Phase, y = value, group = ID, color = Group)) +
  geom_line() +
  facet_grid(variable ~ Group, scales = "free") +
  scale_x_discrete(expand = c(0, .2)) +
  theme(legend.position = "none")
# dev.off()

# Descriptive means
# Data including baseline values as obs
mac_with_base <- mac %>%
  filter(Phase == 1) %>% 
  mutate(Treatment = "Baseline") %>% 
  select(ID, Treatment, GlucosemgdLB, InsulinuIUmlB) %>% 
  rename(GlucosemgdL = GlucosemgdLB, InsulinuIUml = InsulinuIUmlB) %>% 
  bind_rows(select(mac, ID, Treatment, GlucosemgdL, InsulinuIUml, HOMA_IR))
  
# Mean (SD) by treatment
vars <- c("GlucosemgdL", "InsulinuIUml", "HOMA_IR")
options(pillar.sigfig = 4)
mac_with_base %>% 
  pivot_longer(any_of(vars), names_to = "var", values_to = "value") %>% 
  mutate(var = factor(var, levels = vars)) %>% 
  group_by(var, Treatment) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(!is.na(mean))

# Mean (SD) by sequence and treatment
Mean <- function(x) mean(x, na.rm = TRUE)
SD <- function(x) sd(x, na.rm = TRUE)
tabular((GlucosemgdL + InsulinuIUml + HOMA_IR) * (Mean + SD) * Format(digits = 2)  ~  
          Heading() * Group * Heading() * Treatment, data = mac) %>% 
  suppressWarnings()

# Mixed model for whole cohort --------------------------------------------

# Functions for output
emm_options(opt.digits = FALSE)

tidy_output <- function(emm){
  
  delta <- confint(pairs(emm)) %>% 
    rename(Treatment = contrast, emmean = estimate)
  
  pval <- c(NA, NA, as_tibble(pairs(emm))$p.value) %>% 
    finalfit::p_tidy(digits = 4, prefix = NULL) %>%
    as_tibble_col(column_name = "pval")
  
  as.data.frame(emm) %>% 
    bind_rows(delta) %>% 
    select(-df, -SE) %>% 
    bind_cols(pval)
}

tidy_output2 <- function(lmer, emm){
  as.data.frame(emm) %>% 
    select(-df) %>% 
    bind_rows(as.data.frame(confint(pairs(emm, adjust = "none"))) %>% 
                slice(1, 6) %>% 
                mutate(Treatment = "mac - control") %>% 
                rename(emmean = estimate) %>% 
                select(-df, -contrast) %>% 
                bind_cols(as.data.frame(pairs(emm, adjust = "none")) %>%
                            slice(1, 6) %>% 
                            select(p.value) %>% round(4))) %>% 
    slice(1:2, 5, 3:4, 6) %>% 
    bind_cols(c(rep(NA, 5), coef(summary(lmer)) %>% 
                  as.data.frame() %>% 
                  slice(n()) %>% 
                  pull("Pr(>|t|)")) %>% round(5)) %>%
    rename(intx.p = ...8) %>% 
    mutate_at(3:6, round, 2)
}

# Base model
x_vars <- c("Treatment", "Group", "Phase")
RHS <- paste(x_vars, collapse = " + ")
write_model <- function(y, RHS) as.formula(paste0(y, " ~ ", RHS, "+ (1|ID)"))

# Insulin resistance variables
# Glucose
glu_mod1 <- write_model(y = "GlucosemgdL", RHS) %>% 
  lmer(data = mac)

glu_mod1 %>% 
  emmeans(~ Treatment) %>% 
  tidy_output() %>% 
  mutate_at(2:4, round, 2) %>% 
  print(row.names = FALSE)

ggResidpanel::resid_panel(glu_mod1, plots = "all")

# Insulin (log-transformed)
ins_mod1 <- write_model(y = "log(InsulinuIUml)", RHS) %>% 
  lmer(data = mac)

ins_mod1 %>% 
  emmeans(~ Treatment) %>% 
  tidy_output() %>% 
  mutate_at(2:4, exp) %>% 
  mutate_at(2:4, round, 2) %>% 
  print(row.names = FALSE)

ggResidpanel::resid_panel(ins_mod1, plots = "all")

# HOMA-IR (log-transformed)
homa_mod1 <- write_model(y = "log(HOMA_IR)", RHS) %>% 
  lmer(data = mac)

homa_mod1 %>% 
  emmeans(~ Treatment) %>% 
  tidy_output() %>% 
  mutate_at(2:4, exp) %>% 
  mutate_at(2:4, round, 2) %>% 
  print(row.names = FALSE)

ggResidpanel::resid_panel(homa_mod1, plots="all")

# Inflammatory/oxidative variables


# Checking for interactions -----------------------------------------------

# Interaction with baseline BMI, dichotomous
# Glucose
glu_mod2 <- lmer(GlucosemgdL ~ Treatment * BaseBMI + Group + Phase + (1|ID), data = mac)
summary(glu_mod2)
glu_emm <- emmeans(glu_mod2, ~ Treatment + BaseBMI)
tidy_output2(glu_mod2, glu_emm)

# Insulin
ins_mod2 <- lmer(log(InsulinuIUml) ~ Treatment * BaseBMI + Group + Phase + (1|ID), data = mac)
summary(ins_mod2)
ins_emm <- emmeans(ins_mod2, ~ Treatment + BaseBMI)
tidy_output2(ins_mod2, ins_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE)

# HOMA-IR
homa_mod2 <- lmer(log(HOMA_IR) ~ Treatment * BaseBMI + Group + Phase + (1|ID), data = mac)
summary(homa_mod2)
homa_emm <- emmeans(homa_mod2, ~ Treatment + BaseBMI)
tidy_output2(homa_mod2, homa_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE)

# Interaction with baseline WC, dichotomous
# Glucose
glu_mod3 <- lmer(GlucosemgdL ~ Treatment * BaseWC + Group + Phase + (1|ID), data = mac)
summary(glu_mod3)
glu_emm <- emmeans(glu_mod3, ~ Treatment + BaseWC)
tidy_output2(glu_mod3, glu_emm) %>% 
  select(-SE) %>% print(row.names = FALSE)

# Insulin
ins_mod3 <- lmer(log(InsulinuIUml) ~ Treatment * BaseWC + Group + Phase + (1|ID), data = mac)
summary(ins_mod3)
ins_emm <- emmeans(ins_mod3, ~ Treatment + BaseWC)
tidy_output2(ins_mod3, ins_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE) %>% print(row.names = FALSE)

# HOMA-IR
homa_mod3 <- lmer(log(HOMA_IR) ~ Treatment * BaseWC + Group + Phase + (1|ID), data = mac)
summary(homa_mod3)
homa_emm <- emmeans(homa_mod3, ~ Treatment + BaseWC)
tidy_output2(homa_mod3, homa_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE) %>% print(row.names = FALSE)

# Interaction with baseline % body fat, dichotomous
# Glucose
glu_mod4 <- lmer(GlucosemgdL ~ Treatment * BaseBF + Group + Phase + (1|ID), data = mac)
summary(glu_mod4)
glu_emm <- emmeans(glu_mod4, ~ Treatment + BaseBF)
tidy_output2(glu_mod4, glu_emm) %>% 
  select(-SE) %>% print(row.names = FALSE)

# Insulin
ins_mod4 <- lmer(log(InsulinuIUml) ~ Treatment * BaseBF + Group + Phase + (1|ID), data = mac)
summary(ins_mod4)
ins_emm <- emmeans(ins_mod4, ~ Treatment + BaseBF)
tidy_output2(ins_mod4, ins_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE) %>% print(row.names = FALSE)

# HOMA-IR
homa_mod4 <- lmer(log(HOMA_IR) ~ Treatment * BaseBF + Group + Phase + (1|ID), data = mac)
summary(homa_mod4)
homa_emm <- emmeans(homa_mod4, ~ Treatment + BaseBF)
tidy_output2(homa_mod4, homa_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE) %>% print(row.names = FALSE)

# Subgroup analysis, women only -------------------------------------------

# Interaction with baseline BMI, dichotomous
# Glucose
glu_mod2 <- lmer(GlucosemgdL ~ Treatment * BaseBMI + Group + Phase + (1|ID), data = mac_fem)
summary(glu_mod2)
glu_emm <- emmeans(glu_mod2, ~ Treatment + BaseBMI)
tidy_output2(glu_mod2, glu_emm) %>% 
  select(-SE)

# Insulin
ins_mod2 <- lmer(log(InsulinuIUml) ~ Treatment * BaseBMI + Group + Phase + (1|ID), data = mac_fem)
summary(ins_mod2)
ins_emm <- emmeans(ins_mod2, ~ Treatment + BaseBMI)
tidy_output2(ins_mod2, ins_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE)

# HOMA-IR
homa_mod2 <- lmer(log(HOMA_IR) ~ Treatment * BaseBMI + Group + Phase + (1|ID), data = mac_fem)
summary(homa_mod2)
homa_emm <- emmeans(homa_mod2, ~ Treatment + BaseBMI)
tidy_output2(homa_mod2, homa_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE)

# Interaction with baseline WC, dichotomous
# Glucose
glu_mod3 <- lmer(GlucosemgdL ~ Treatment * BaseWC + Group + Phase + (1|ID), data = mac_fem)
summary(glu_mod3)
glu_emm <- emmeans(glu_mod3, ~ Treatment + BaseWC)
tidy_output2(glu_mod3, glu_emm) %>% 
  select(-SE) %>% print(row.names = FALSE)

# Insulin
ins_mod3 <- lmer(log(InsulinuIUml) ~ Treatment * BaseWC + Group + Phase + (1|ID), data = mac_fem)
summary(ins_mod3)
ins_emm <- emmeans(ins_mod3, ~ Treatment + BaseWC)
tidy_output2(ins_mod3, ins_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE) %>% print(row.names = FALSE)

# HOMA-IR
homa_mod3 <- lmer(log(HOMA_IR) ~ Treatment * BaseWC + Group + Phase + (1|ID), data = mac_fem)
summary(homa_mod3)
homa_emm <- emmeans(homa_mod3, ~ Treatment + BaseWC)
tidy_output2(homa_mod3, homa_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE) %>% print(row.names = FALSE)

# Interaction with baseline % body fat, dichotomous
# Glucose
glu_mod4 <- lmer(GlucosemgdL ~ Treatment * BaseBF + Group + Phase + (1|ID), data = mac_fem)
summary(glu_mod4)
glu_emm <- emmeans(glu_mod4, ~ Treatment + BaseBF)
tidy_output2(glu_mod4, glu_emm) %>% 
  select(-SE) %>% print(row.names = FALSE)

# Insulin
ins_mod4 <- lmer(log(InsulinuIUml) ~ Treatment * BaseBF + Group + Phase + (1|ID), data = mac_fem)
summary(ins_mod4)
ins_emm <- emmeans(ins_mod4, ~ Treatment + BaseBF)
tidy_output2(ins_mod4, ins_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE) %>% print(row.names = FALSE)

# HOMA-IR
homa_mod4 <- lmer(log(HOMA_IR) ~ Treatment * BaseBF + Group + Phase + (1|ID), data = mac_fem)
summary(homa_mod4)
homa_emm <- emmeans(homa_mod4, ~ Treatment + BaseBF)
tidy_output2(homa_mod4, homa_emm) %>% 
  mutate_at(3:6, exp) %>% 
  mutate_at(3:6, round, 2) %>% 
  select(-SE) %>% print(row.names = FALSE)

# Checking previous results -----------------------------------------------

# Lipids
tc_mod1 <- lmer(CholmgdL ~ Treatment + Group + Phase + (1|ID), data = mac)
summary(tc_mod1)
homa_emm <- emmeans(tc_mod1, ~ Treatment)
tidy_output(homa_emm)

ldl_mod1 <- lmer(LDLmgdL ~ Treatment + Group + Phase + (1|ID), data = mac)
summary(ldl_mod1)
ldl_emm <- emmeans(ldl_mod1, ~ Treatment)
tidy_output(ldl_emm)

tc_mod2 <- lmer(CholmgdL ~ Treatment * BaseBMI + Group + Phase + (1|ID), data = mac)
summary(tc_mod2)
tc_emm <- emmeans(tc_mod2, ~ Treatment + BaseBMI)
tidy_output2(tc_mod2, tc_emm)

tc_mod3 <- lmer(CholmgdL ~ Treatment * BaseBMI + Group + Phase + (1|ID), data = mac_fem)
summary(tc_mod3)
tc_emm <- emmeans(tc_mod3, ~ Treatment + BaseBMI)
tidy_output2(tc_mod3, tc_emm)

tc_mod4 <- lmer(LDLmgdL ~ Treatment * BaseBF + Group + Phase + (1|ID), data = mac_fem)
summary(tc_mod4)
tc_emm <- emmeans(tc_mod4, ~ Treatment + BaseBF)
tidy_output2(tc_mod4, tc_emm)

# Inflammatory/oxidative variables ----------------------------------------

# Variable names
c("crp", "eselec", "il6", "tnf", "icam", "vcam", "8iso", "mda") %>% 
  map(function(x) grep(x, names(mac0), value = TRUE, ignore.case = TRUE))
