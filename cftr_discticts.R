library(binom)

# Estimation of CI for AFs - RUSeq healthy and all
# F508del
# RUSeq healthy
binom.confint(27, 3342, method='exact', conf.level = 0.9)
binom.confint(27, 3342, method='exact', conf.level = 0.95)
# RUSeq all
binom.confint(81, 12902, method='exact', conf.level = 0.9)
# Abramov et al.
binom.confint(15, 2000, method='exact', conf.level = 0.9)

# F508 RUSeq vs infertile
# All infertile
f508_if_ac = round(0.0152 * 12066, digits=0)
f508_ruseq_inf <- matrix(c(27, 3342, f508_if_ac, 12066 - f508_if_ac), 
                         nrow=2, byrow=T)
f508_ruseq_inf
chisq.test(f508_ruseq_inf)

# Non-CBAVD
f508_ncbavd_ac = round(0.0113 * 10196, digits=0)
f508_ruseq_ncbavd <- matrix(c(27, 3342, f508_ncbavd_ac, 10196 - f508_ncbavd_ac), 
                            nrow=2, byrow=T)
f508_ruseq_ncbavd
chisq.test(f508_ruseq_ncbavd)

# CFTRdele2,3 
dele23_if_ac = round(0.0017 * 12066, digits=0)
dele23_petrova_inf <- matrix(c(0.0004*2*1327,2*1327 - 0.0004*2*1327, f508_if_ac, 12066 - f508_if_ac), 
                             nrow=2, byrow=T)

chisq.test(dele23_petrova_inf)


### Testing the regional differences (in different combinations)
# F508 pairwise Yugra and SPb
f508_yugra_sob <- matrix(c(43, 65, 2846, 1978), nrow=2, byrow=T)
f508_yugra_sob
chisq.test(f508_yugra_sob)
chisq.test(f508_yugra_sob)$residual


# CFTRdele2,3 
# Pairwise Yugra vs. SPb
dele23_yugra_sob <- matrix(c(5, 103, 178, 4646), nrow=2, byrow=T)
dele23_yugra_sob
chisq.test(dele23_yugra_sob)
chisq.test(dele23_yugra_sob)$residual


# F508 and CFTRdele2,3 combined test
#### More regions
f508_dele23_reg_all <- matrix(c(839,	295,	357,	73,	571,	264,	401,	157,
                                126,	26,	41,	5,	59,	24,	46, 20,
                                645,	205,	248,	262,	502,	202,	277,	115), 
                              ncol=3, byrow=F)
rownames(f508_dele23_reg_all) <- c('Central', 'North-West', 'South', 'Caucasus',
                                   'Volga', 'Ural', 'Siberia', 'Far East')
colnames(f508_dele23_reg_all) <- c('F509del', 'CFTRdele2,3', 'other')
f508_dele23_reg_all
chisq.test(f508_dele23_reg_all)
chisq.test(f508_dele23_reg_all)$residual