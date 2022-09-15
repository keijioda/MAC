
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
setwd("C:\\Users\\keiji\\Dropbox\\Nutrition\\Julie Jones")

# Read data ---------------------------------------------------------------

# Read data, n obs = 70
mac0 <- read_sav("MAC Endpoint data with baseline values.sav")
# mac0 <- read_sav("MAC Endpoint data with baseline values 102121.sav")

# Fix variable names
fixthis <- mac0 %>% search_var("8iso") %>% pull(num)
names(mac0)[fixthis] <- c("isopgf2pgmL", "isopgf2pgmLB")

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
  select(GlucosemgdL, InsulinuIUml, HOMA2_IR) %>% 
  pivot_longer(GlucosemgdL:HOMA2_IR, names_to = "var", values_to = "value") %>% 
  mutate(var = factor(var, levels = c("GlucosemgdL", "InsulinuIUml", "HOMA2_IR"))) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~ var, scales = "free")

# Profile plots by sequence group
# pdf("plot by seq.pdf", height = 8, width = 6)
mac %>% 
  select(ID, Group, Phase, GlucosemgdL, InsulinuIUml, HOMA2_IR) %>% 
  pivot_longer(GlucosemgdL:HOMA2_IR, names_to = "variable", values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("GlucosemgdL", "InsulinuIUml", "HOMA2_IR"))) %>% 
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
  select(ID, Treatment, GlucosemgdLB, InsulinuIUmlB, HOMA2_IRB) %>% 
  rename(GlucosemgdL = GlucosemgdLB, InsulinuIUml = InsulinuIUmlB, HOMA2_IR = HOMA2_IRB) %>% 
  bind_rows(select(mac, ID, Treatment, GlucosemgdL, InsulinuIUml, HOMA2_IR)) %>% 
  mutate(Treatment = factor(Treatment))
  
Mean <- function(x) mean(x, na.rm = TRUE)
SD <- function(x) sd(x, na.rm = TRUE)

# Mean (SD) by treatment
tabular((GlucosemgdL + InsulinuIUml + HOMA2_IR) * (Mean + SD) * Format(digits = 2)  ~  
          Heading() * Treatment, data = mac_with_base) %>% 
  suppressWarnings()

# Mean (SD) by sequence and treatment
tabular((GlucosemgdL + InsulinuIUml + HOMA2_IR) * (Mean + SD) * Format(digits = 2)  ~  
          Heading() * Group * Heading() * Treatment, data = mac) %>% 
  suppressWarnings()

# Mixed model for whole cohort --------------------------------------------

# Functions for output
emm_options(opt.digits = FALSE)

tidy_output <- function(emm){
  
  delta <- confint(pairs(emm)) %>% 
    rename(Treatment = contrast, emmean = estimate)
  
  pval <- emm %>% pairs() %>% summary() %>% pull(p.value) %>% 
    finalfit::p_tidy(digits = 4, prefix = NULL) %>% 
    c("", "", .) %>% 
    as_tibble_col(column_name = "pval")
  
  as.data.frame(emm) %>% 
    bind_rows(delta) %>% 
    select(-df) %>% 
    bind_cols(pval)
}

show_result <- function(emm, log = FALSE){
  emm <- emm %>% tidy_output()
  if(log) emm <- emm %>% mutate_at(c(2, 4:5), exp)
  emm <- emm %>% 
    mutate_at(2:5, round, 2)
}

# Base model: adjusts for sequence and phase
write_model <- function(y, trt_var = "Treatment") as.formula(paste0(y, " ~ ", trt_var, "+ Group + Phase + (1|ID)"))

# Insulin resistance variables
# Glucose
glu_mod1 <- write_model(y = "GlucosemgdL") %>% lmer(data = mac)
glu_mod1 %>% emmeans(~Treatment) %>% show_result()
ggResidpanel::resid_panel(glu_mod1, plots = "all")

# Insulin (log-transformed)
ins_mod1 <- write_model(y = "log(InsulinuIUml)") %>% lmer(data = mac)
ins_mod1 %>% emmeans(~Treatment) %>% show_result(log = TRUE)
ggResidpanel::resid_panel(ins_mod1, plots = "all")

# HOMA-IR (log-transformed)
homa_mod1 <- write_model(y = "log(HOMA2_IR)") %>% lmer(data = mac)
homa_mod1 %>% emmeans(~Treatment) %>% show_result(log = TRUE)
ggResidpanel::resid_panel(homa_mod1, plots="all")

# All together
insresis_results <- vector(mode = "list", length = 3)

insresis_results[[1]] <- write_model(y = "GlucosemgdL") %>% 
  lmer(data = mac) %>% emmeans(~Treatment) %>% show_result()

insresis_results[[2]] <- write_model(y = "log(InsulinuIUml)") %>% 
  lmer(data = mac) %>% emmeans(~Treatment) %>% show_result(log = TRUE)

insresis_results[[3]] <- write_model(y = "log(HOMA2_IR)") %>% 
  lmer(data = mac) %>% emmeans(~Treatment) %>% show_result(log = TRUE)

names(insresis_results) <- c("GlucosemgdL", "log(InsulinuIUml)", "log(HOMA2_IR)")
insresis_results %>% print(row.names = FALSE)

# Checking for interactions -----------------------------------------------

tidy_output2 <- function(emm, lmer){
  
  flevels <- summary(emm) %>% select(2) %>% distinct()
  
  pval <- summary(pairs(emm, adjust = "none")) %>% 
    pull(p.value) %>% 
    finalfit::p_tidy(digits = 4, prefix = NULL) %>% 
    as_tibble_col(column_name = "pval")
  
  intxp <- coef(summary(lmer)) %>% as_tibble() %>%
    slice(n()) %>% pull(-1) %>% 
    finalfit::p_tidy(digits = 4, prefix = NULL) %>% 
    c(rep("", 5), .) %>% 
    as_tibble_col(column_name = "intx.P")
  
  delta <- confint(pairs(emm, adjust = "none")) %>% 
    mutate(Treatment = "Mac - Ctrl") %>% 
    rename(emmean = estimate) %>% 
    bind_cols(pval) %>% slice(1, 6) %>% 
    bind_cols(flevels)
  
  summary(emm) %>% 
    mutate(pval = "") %>% 
    bind_rows(delta) %>% 
    select(-contrast, -df, -SE) %>% 
    arrange(.[2]) %>%
    bind_cols(intxp)
}

show_result2 <- function(emm, lmer, log = FALSE){
  emm <- emm %>% tidy_output2(lmer)
  if(log) emm <- emm %>% mutate_at(3:5, exp)
  emm %>% 
    mutate_at(3:5, round, 2) 
    # print(row.names = FALSE)
}

# Interaction with baseline BMI, dichotomous
# Glucose
glu_mod2 <- write_model("GlucosemgdL", "Treatment * BaseBMI") %>% lmer(data = mac)
glu_mod2 %>% 
  emmeans(~ Treatment + BaseBMI) %>% 
  show_result2(glu_mod2)

# Insulin
ins_mod2 <- write_model("log(InsulinuIUml)", "Treatment * BaseBMI") %>% lmer(data = mac)
ins_mod2 %>% 
  emmeans(~ Treatment + BaseBMI) %>% 
  show_result2(ins_mod2, log = TRUE)

# HOMA-IR
homa_mod2 <- write_model("log(HOMA2_IR)", "Treatment * BaseBMI") %>% lmer(data = mac)
homa_mod2 %>% 
  emmeans(~ Treatment + BaseBMI) %>% 
  show_result2(homa_mod2, log = TRUE)

# All together
insresis_results[[1]] <- glu_mod2  %>% emmeans(~Treatment + BaseBMI) %>% show_result2(glu_mod2)
insresis_results[[2]] <- ins_mod2  %>% emmeans(~Treatment + BaseBMI) %>% show_result2(ins_mod2, log = TRUE)
insresis_results[[3]] <- homa_mod2 %>% emmeans(~Treatment + BaseBMI) %>% show_result2(homa_mod2, log = TRUE)
names(insresis_results) <- c("GlucosemgdL", "log(InsulinuIUml)", "log(HOMA2_IR)")
insresis_results %>% print(row.names = FALSE)

# Interaction with baseline WC, dichotomous
# Glucose
glu_mod3 <- write_model("GlucosemgdL", "Treatment * BaseWC") %>% lmer(data = mac)
glu_mod3 %>% 
  emmeans(~ Treatment + BaseWC) %>% 
  show_result2(glu_mod3)

# Insulin
ins_mod3 <- write_model("log(InsulinuIUml)", "Treatment * BaseWC") %>% lmer(data = mac)
ins_mod3 %>% 
  emmeans(~ Treatment + BaseWC) %>% 
  show_result2(ins_mod3, log = TRUE)

# HOMA-IR
homa_mod3 <- write_model("log(HOMA2_IR)", "Treatment * BaseWC") %>% lmer(data = mac)
homa_mod3 %>% 
  emmeans(~ Treatment + BaseWC) %>% 
  show_result2(homa_mod3, log = TRUE)

# All together
insresis_results[[1]] <- glu_mod3  %>% emmeans(~Treatment + BaseWC) %>% show_result2(glu_mod3)
insresis_results[[2]] <- ins_mod3  %>% emmeans(~Treatment + BaseWC) %>% show_result2(ins_mod3, log = TRUE)
insresis_results[[3]] <- homa_mod3 %>% emmeans(~Treatment + BaseWC) %>% show_result2(homa_mod3, log = TRUE)
names(insresis_results) <- c("GlucosemgdL", "log(InsulinuIUml)", "log(HOMA2_IR)")
insresis_results %>% print(row.names = FALSE)

# Interaction with baseline % body fat, dichotomous
# Glucose
glu_mod4 <- write_model("GlucosemgdL", "Treatment * BaseBF") %>% lmer(data = mac)
glu_mod4 %>% 
  emmeans(~ Treatment + BaseBF) %>% 
  show_result2(glu_mod4)

# Insulin
ins_mod4 <- write_model("log(InsulinuIUml)", "Treatment * BaseBF") %>% lmer(data = mac)
ins_mod4 %>% 
  emmeans(~ Treatment + BaseBF) %>% 
  show_result2(ins_mod4, log = TRUE)

# HOMA-IR
homa_mod4 <- write_model("log(HOMA2_IR)", "Treatment * BaseBF") %>% lmer(data = mac)
homa_mod4 %>% 
  emmeans(~ Treatment + BaseBF) %>% 
  show_result2(homa_mod4, log = TRUE)

# All together
insresis_results[[1]] <- glu_mod4  %>% emmeans(~Treatment + BaseBF) %>% show_result2(glu_mod4)
insresis_results[[2]] <- ins_mod4  %>% emmeans(~Treatment + BaseBF) %>% show_result2(ins_mod4, log = TRUE)
insresis_results[[3]] <- homa_mod4 %>% emmeans(~Treatment + BaseBF) %>% show_result2(homa_mod4, log = TRUE)
names(insresis_results) <- c("GlucosemgdL", "log(InsulinuIUml)", "log(HOMA2_IR)")
insresis_results %>% print(row.names = FALSE)

# Checking previous results -----------------------------------------------

# Lipids
lipid_results <- function(var){
  write_model(var) %>% 
  lmer(data = mac) %>% 
  suppressWarnings() %>% 
  emmeans(~ Treatment) %>% 
  tidy_output() %>% 
  mutate_at(c(2, 4:5), round, 2)
}

yvars <- c("CholmgdL", "LDLmgdL")
all_lipid_results <- yvars %>% map(lipid_results)
names(all_lipid_results) <- yvars
print(all_lipid_results, row.names = FALSE)

# Inflammatory/oxidative variables ----------------------------------------

# Variable names
c("crp", "eselec", "il6", "tnf", "icam", "vcam", "iso", "mda") %>% 
  map(function(x) grep(x, names(mac0), value = TRUE, ignore.case = TRUE))

inflam_vars <- c("crp", "eselec", "il6", "tnf", "icam", "vcam", "iso", "mda") %>% 
  map(function(x) grep(x, names(mac0), value = TRUE, ignore.case = TRUE)) %>% 
  sapply("[[", 1)

inflamB_vars <- paste0(inflam_vars, "B")

# Descriptive means
# Data including baseline values as obs
mac_with_base <- mac %>%
  filter(Phase == 1) %>% 
  mutate(Treatment = "Baseline") %>% 
  select(ID, Treatment, all_of(inflamB_vars)) %>% 
  rename_with(function(x) substr(x, 1, nchar(x) - 1), all_of(inflamB_vars)) %>% 
  bind_rows(select(mac, ID, Treatment, all_of(inflam_vars))) %>% 
  mutate(Treatment = factor(Treatment))

# Mean (SD) by treatment
tabular((CRPmgdL + ESelectinngdL + IL6pgmL + TNFapgmL + sICAM1pgmL + sVCAm1pgmL + isopgf2pgmL + MDAnmolmL) * (Mean + SD) * Format(digits = 2)  ~  
          Heading() * Treatment, data = mac_with_base) %>% 
  suppressWarnings()

# Mean (SD) by sequence and treatment
tabular((CRPmgdL + ESelectinngdL + IL6pgmL + TNFapgmL + sICAM1pgmL + sVCAm1pgmL + isopgf2pgmL + MDAnmolmL) * (Mean + SD) * Format(digits = 2)  ~  
          Heading() * Group * Heading() * Treatment, data = mac) %>% 
  suppressWarnings()

# CRP
crp_mod1 <- write_model(y = "log(CRPmgdL)") %>% lmer(data = mac)
crp_mod1 %>% 
  emmeans(~ Treatment) %>% 
  show_result(log = TRUE)

ggResidpanel::resid_panel(crp_mod1, plots = "all")

# E-selectin
esel_mod1 <- write_model(y = "log(ESelectinngdL)") %>% lmer(data = mac)
esel_mod1 %>% 
  emmeans(~ Treatment) %>% 
  show_result(log = TRUE)

ggResidpanel::resid_panel(esel_mod1, plots = "all")

# IL-6
il6_mod1 <- write_model(y = "log(IL6pgmL)") %>% lmer(data = mac)
il6_mod1 %>% 
  emmeans(~ Treatment) %>% 
  show_result(log = TRUE)

ggResidpanel::resid_panel(il6_mod1, plots = "all")

# TNF-a
tnf_mod1 <- write_model(y = "log(TNFapgmL)") %>% lmer(data = mac)
tnf_mod1 %>% 
  emmeans(~ Treatment) %>% 
  show_result(log = TRUE)

ggResidpanel::resid_panel(tnf_mod1, plots = "all")

# ICAM
icam_mod1 <- write_model(y = "log(sICAM1pgmL)") %>% lmer(data = mac)
icam_mod1 %>% 
  emmeans(~ Treatment) %>% 
  show_result(log = TRUE)

ggResidpanel::resid_panel(icam_mod1, plots = "all")

# VCAM
vcam_mod1 <- write_model(y = "log(sVCAm1pgmL)") %>% lmer(data = mac)
vcam_mod1 %>% 
  emmeans(~ Treatment) %>% 
  show_result(log = TRUE)

ggResidpanel::resid_panel(vcam_mod1, plots = "all")

# 8-iso-PGF
pgf_mod1 <- write_model(y = "log(isopgf2pgmL)") %>% lmer(data = mac)
pgf_mod1 %>% 
  emmeans(~ Treatment) %>% 
  show_result(log = TRUE)

ggResidpanel::resid_panel(pgf_mod1, plots = "all")

# MDA
mda_mod1 <- write_model(y = "log(MDAnmolmL)") %>% lmer(data = mac)
mda_mod1 %>% 
  emmeans(~ Treatment) %>% 
  show_result(log = TRUE)

ggResidpanel::resid_panel(mda_mod1, plots = "all")

# All together
inflam_results <- function(var){
  write_model(y = var) %>% 
    lmer(data = mac) %>% 
    suppressWarnings() %>% 
    emmeans(~ Treatment) %>% 
    show_result(log = TRUE)
}

yvars <- c("log(CRPmgdL)", "log(ESelectinngdL)", "log(IL6pgmL)", "log(TNFapgmL)", 
           "log(sICAM1pgmL)", "log(sVCAm1pgmL)", "log(isopgf2pgmL)", "log(MDAnmolmL)")
all_inflam_results <- yvars %>% map(inflam_results)
names(all_inflam_results) <- yvars

print(all_inflam_results, row.names = FALSE)

# Interaction with baseline BMI, dichotomous

inflam_results <- function(var){
  mod <- write_model(y = var, "Treatment * BaseBMI") %>% 
    lmer(data = mac) %>% 
    suppressWarnings()
  mod %>% 
    emmeans(~ Treatment + BaseBMI) %>% 
    show_result2(mod, log = TRUE)
}
all_inflam_results2 <- yvars %>% map(inflam_results)
names(all_inflam_results2) <- yvars
all_inflam_results2 %>% 
  print(row.names = FALSE)

# Interaction with baseline WC, dichotomous

inflam_results <- function(var){
  mod <- write_model(y = var, "Treatment * BaseWC") %>% 
    lmer(data = mac) %>% 
    suppressWarnings()
  mod %>% 
    emmeans(~ Treatment + BaseWC) %>% 
    show_result2(mod, log = TRUE)
}
all_inflam_results3 <- yvars %>% map(inflam_results)
names(all_inflam_results3) <- yvars
all_inflam_results3 %>% 
  print(row.names = FALSE)

# Interaction with baseline % body fat, dichotomous

inflam_results <- function(var){
  mod <- write_model(y = var, "Treatment * BaseBF") %>% 
    lmer(data = mac) %>% 
    suppressWarnings()
  mod %>% 
    emmeans(~ Treatment + BaseBF) %>% 
    show_result2(mod, log = TRUE)
}
all_inflam_results4 <- yvars %>% map(inflam_results)
names(all_inflam_results4) <- yvars
all_inflam_results4 %>% 
  print(row.names = FALSE)

# Check Table 4
adip_vars <- c("Weight", 
               "BMI", 
               "WC", 
               "TBWTotalBodyWater", 
               "DLMDryLeanMass", 
               "LBMLeanBodyMass", 
               "SMMSkeletalMuscleMass", 
               "BFMBodyFatMass", 
               "PBFPercentBodyFat")

tab4func <- \(x){
  write_model(y = x) %>% 
    lmer(data = mac) %>% 
    emmeans(~Treatment) %>% 
    show_result()
}

results <- adip_vars %>% 
  lapply(tab4func) %>% 
  setNames(adip_vars) %>% 
  lapply(\(x) slice(x, 3)) %>% 
  do.call(rbind, .)

results$var <- c("Weight (kg)", "BMI", "WC (cm)", "TBW (L)", "DLM (kg)", "LBM (kg)", "SMM (kg)", "Fat mass (kg)", "Body fat (%)")

pdf("Fig for Tab4.pdf", width = 5, height = 5)
results %>% 
  ggplot(aes(x = var, y = emmean)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  geom_point(shape = 15, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.25) +
  ylim(min = -1, max = 1) + 
  coord_flip() + 
  theme(legend.position = "none") +
  labs(x = "", y = "Estimated mean difference (Mac - Control)")
dev.off()

