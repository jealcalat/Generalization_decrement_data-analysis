## -------------------------- Data preparation --------------------------------

## Source wrapper ----
## WARNING: wrapper erase all data in memory, run it with at the beggining;
## also, it will load several custum functions without which ANYTHING below
## will work as expected.

source("~/Documentos/2019A/paper_gen_decrement/analysis/r/wrapper.r")

# Paths for processed data. Check raw2processed_proc.r ----

path_read.general = "~/Documentos/2019A/paper_gen_decrement/Data/"
path_read.BL = path_read.general + "processed/baseline"
path_read.EXP = path_read.general + "processed/experimental"

# Paths for results of analysis (data_results) ----

path_save.general = "~/Documentos/2019A/paper_gen_decrement/analysis/"
path_save.data = path_save.general + "data_results/"

# ETC
## vector of subjects numbers, used in several functions below
subjects_vec = 326:333
## Names of phases
ph_names = c("Baseline", "Experimental")
# Path of BL and EXP to read
paths = c(path_read.BL, path_read.EXP)
# ==============================================================================
# All data ----
# Data of peak trials ----  

peak_Baseline <- bind_all_Ss(subjects_vec,
                             n = 40 ,
                             peak = 1,
                             path_read = paths[1])
peak_Baseline$phase <- ph_names[1]

peak_Experimental <- bind_all_Ss(subjects_vec,
                                 n = 30,
                                 peak = 1,
                                 path_read = paths[2])

peak_Experimental$phase <- ph_names[2]

all_data_peak <- bind_rows(peak_Baseline, peak_Experimental) %>%
  select(cum_dt, evento, sujeto, sesion, cde, trial, phase)

# Phase and cde (the component) as factors

all_data_peak$phase <- factor(all_data_peak$phase)
all_data_peak$cde <- factor(all_data_peak$cde)

# Data of multiple fixed interval ----

IF_Baseline <- bind_all_Ss(subjects_vec,
                           n = 40 ,
                           peak = 0,
                           path_read = paths[1])
IF_Baseline$phase <- ph_names[1]

IF_Experimental <- bind_all_Ss(subjects_vec,
                               n = 30,
                               peak = 0,
                               path_read = paths[2])

IF_Experimental$phase <- ph_names[2]

all_data_IF <- bind_rows(IF_Baseline, IF_Experimental) %>%
  select(cum_dt, evento, sujeto, sesion, cde, trial, phase)

all_data_IF$phase <- factor(all_data_IF$phase)
all_data_IF$cde <- factor(all_data_IF$cde)

save(all_data_IF, all_data_peak,
     file = path_save.data + "all_data_IF_&_peak.RData")
# ==============================================================================
# Overall rate ----

ov_rate_BL <- 
  ov_rate(subjects_vec, path=paths[1]) %>%
  filter(evento %in%  c(1,11,13,14)) %>%
  group_by(sujeto,sesion,cde) %>%
  summarise(rate = length(evento)/max(t) * 60) # convert to resp per minute

ov_rate_BL$phase <- "Baseline"

ov_rate_Exp <- 
  ov_rate(subjects_vec, path=paths[2]) %>%
  filter(evento %in%  c(1,11)) %>%
  group_by(sujeto,sesion,cde) %>%
  summarise(rate = length(evento)/max(t) * 60) # convert to resp per minute

ov_rate_Exp$phase <- "Experimental"

ov_rate2ph <- bind_rows(ov_rate_BL,ov_rate_Exp) %>%
  as.data.frame()
ov_rate2ph$cde <- factor(ov_rate2ph$cde)

save(ov_rate2ph, file = path_save.data + "overall_rate_data.RData")
write.csv(ov_rate2ph,path_save.data + "overall_rate_data_resp_per_minute.csv")
# ==============================================================================
# Individual trial analysis using Church low-high-low algorithm ---------------

df_p_lhl <- ind_ana_lhl(
  list_ss = subjects_vec,
  phases_names = ph_names,
  paths = paths,
  sessions = 20,
  max_trials = 30,
  trial_duration = 180
) %>%
  as.data.frame()

# Filter if rate in high rate is higher than the other states
df_p_lhl <- df_p_lhl %>%
  filter(r1 < hrate & hrate > r3)
# ==============================================================================
# FWHM data ----

resp_times_Baseline <- 
  bind_all_Ss(
    subjects_vec,
    n = 25 ,
    peak = 1,
    path_read = paths[1]) %>%
  resp_times(., 30) %>%
  as.data.frame()

resp_times_Baseline$phase <- ph_names[1]

resp_times_Experimental <- 
  bind_all_Ss(
    subjects_vec,
    n = 25 ,
    peak = 1,
    path_read = paths[2]) %>%
  resp_times(., 30) %>%
  as.data.frame()

resp_times_Experimental$phase <- ph_names[2]

resp_times_df <-
  bind_rows(resp_times_Experimental, resp_times_Baseline)

fwhm_2phases <- resp_times_df %>%
  group_by(sujeto, cde, phase, sesion) %>%
  filter(length(bins) > 1) %>%
  group_by(sujeto, cde, phase, sesion) %>%
  summarise(fwhm =
              {
                rt <- bins
                rt <- rt[rt < 180]
                d  <- data.frame(x = density(rt)$x,
                                 y = density(rt)$y)
                fwhm <- fwhm(d$x, d$y)$fwhm # custom function
                if (length(fwhm) == 0) {
                  NA
                } else {
                  fwhm
                }
              },
            peak =
              {
                rt <- bins
                rt <- rt[rt < 180]
                d  <- data.frame(x = density(rt)$x,
                                 y = density(rt)$y)
                mfw <- fwhm(d$x, d$y)$peak # custom function
                if (length(mfw) == 0) {
                  NA
                } else {
                  mfw
                }
              }) %>% na.omit()

fwhm_2phases$cde <- fwhm_2phases$cde %>% as.factor

save(df_p_lhl, fwhm_2phases, 
     file = path_save.data + "start_stop_fwhm_data.RData")
# ==============================================================================
#Correlation coefficient by session ----
# IF_baseline and IF_experimebtal from above

# Delayed phase ----
rho_BL_delayed <- IF_Baseline %>%
  ungroup() %>%
  filter(cde == 1) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(cde,sesion) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
              {
                t_ev <- .
                rho <- rho.rR(t_ev,1,2,1000,200)
                rho
              })

rho_BL_immediate <- IF_Baseline %>%
  ungroup() %>%
  filter(cde == 2) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(sesion,cde) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
              {
                t_ev <- .
                rho <- rho.rR(t_ev,11,21,1000,200)
                rho
              })

rho_BL_2comp <- bind_rows(rho_BL_delayed, rho_BL_immediate)
rho_BL_2comp$phase <- "Baseline"

# Experimental phase ----

rho_EXP_delayed <- IF_Experimental %>%
  ungroup() %>%
  filter(cde == 1) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(cde,sesion) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
              {
                t_ev <- .
                rho <- rho.rR(t_ev,1,2,1000,200)
                rho
              })

rho_EXP_immediate <- IF_Experimental %>%
  ungroup() %>%
  filter(cde == 2) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(cde,sesion) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
              {
                t_ev <- .
                rho <- rho.rR(t_ev,11,21,1000,200)
                rho
              })

rho_EXP_2comp <- bind_rows(rho_EXP_delayed,rho_EXP_immediate)
rho_EXP_2comp$phase <- "Experimental"
# Join both phases 
rho_2ph <- bind_rows(rho_BL_2comp,rho_EXP_2comp)
rho_2ph$phase <- factor(rho_2ph$phase)
# Save data

save(rho_2ph,file = path_save.data + "correlation_data.RData")
# ==============================================================================
# Data for qqplot of IRIs ------

df_IRI_BL <- IF_Baseline %>%
  filter(evento %in% c(2, 21)) %>%
  group_by(sujeto,cde,cum_trial,phase) %>%
  summarise(iri = cum_dt) %>%
  ungroup() %>%
  filter(iri <= quantile(iri, 0.95),
         iri > quantile(iri, 0.05)) %>%
  spread(cde,iri) %>%
  na.omit() %>%
  filter(cum_trial <= 30) 

colnames(df_IRI_BL)[c(4,5)] <- c("Delay","Immediate")

qq_BL <- df_IRI_BL %>% 
  as.data.frame() %>%
  na.omit() %>%
  `[`(c("Delay","Immediate")) %>%
  {
    xy <- .
    qq <- qq.xy(xy)
    qq$phase <- "Baseline"
    qq
  }


df_IRI_EXP  <- IF_Experimental %>%
  filter(evento %in% c(2, 21)) %>%
  group_by(sujeto,cde,cum_trial,phase) %>%
  summarise(iri = cum_dt) %>%
  ungroup() %>%
  filter(iri <= quantile(iri, 0.95),
         iri > quantile(iri, 0.05)) %>%
  spread(cde,iri) %>%
  na.omit() %>%
  filter(cum_trial <= 30) 

colnames(df_IRI_EXP)[c(4,5)] <- c("Delay","Immediate")

qq_EXP <- df_IRI_EXP %>% 
  as.data.frame() %>%
  na.omit() %>%
  `[`(c("Delay","Immediate")) %>%
  {
    xy <- .
    qq <- qq.xy(xy)
    qq$phase <- "Experimental"
    qq
  }


qq_2ph <- bind_rows(qq_BL,qq_EXP)

save(qq_2ph,file = path_save.data + "qqIRI_data.RData")
# ==============================================================================
# DKL: Kullback-Leibler divergence ----
# Use peak_{Phase} data from above

df_BL <- peak_Baseline %>%
  group_by(sujeto, sesion, cde, trial) %>%
  filter(sum(evento == 1) > 9 |
           sum(evento == 11) > 9,
         cum_dt <= 180,
         cum_trial <= 30)

df_EXP <- peak_Experimental %>%
  group_by(sujeto, sesion, cde, trial) %>%
  filter(sum(evento == 1) > 9 |
           sum(evento == 11) > 9,
         cum_dt <= 180,
         cum_trial <= 30)

df_2ph <- bind_rows(df_BL, df_EXP)

s_vector <- unique(df_2ph$sujeto)

d <- lapply(s_vector,function(k){
  
  df.s <- df_2ph[df_2ph$sujeto == k, ]
  v_ph <- unique(df.s$phase)
  
  phse <- lapply(v_ph,function(f){
    
    df.s.f <- df.s[df.s$phase == f,]
    ses <- unique(df.s.f$sesion)
    
    sesion <- lapply(ses, function(s){
      
      df.s.f.s <- df.s.f[df.s.f$sesion == s,]
      P <- df.s.f.s$cum_dt[df.s.f.s$evento == 1] 
      Q <- df.s.f.s$cum_dt[df.s.f.s$evento == 11] 
      
      # Length of nonzero bins
      lP = length(P[P > 0])
      lQ = length(Q[Q > 0])
      
      if(lP > 6 & lQ > 6){
        kld.s <- kld_d(P,Q)
      } else {
        kld.s <- NA
      }
      
      data.frame(s = k,kld = kld.s,sesion = s,phase = f)
      
    }
    ) %>% bind_rows()
    sesion
  }
  ) %>% bind_rows()
  phse
}
) %>% bind_rows()

d %<>% arrange(phase)

dsum <- d %>% group_by(phase) %>% summarise(kld = median(kld,na.rm = T))
save(d, dsum,
     file = path_save.data + 'dkl.RData')
# ==============================================================================
# Normalized response rate ----
# Normalization with Z = [x(i) - min(x)]/[max(x) - min(x)]
# which is a unit-based normalization, or feature scaling normalization

norm_rate_BL <- peak_Baseline %>% 
  group_by(sujeto,sesion,cde,trial) %>%
  filter(sum(evento == 1) > 9 |
           sum(evento == 11) > 9) %>%
  as.data.frame() %>%
  resp_rate_df(.,1,30) %>%
  group_by(sujeto,cde) %>%
  mutate(resp_norm = (resp - min(resp))/(max(resp) - min(resp)),
         ec_norm = (ec_resp - min(ec_resp))/(max(ec_resp) - min(ec_resp)))

norm_rate_Exp <- peak_Experimental %>% 
  group_by(sujeto,sesion,cde,trial) %>%
  filter(sum(evento == 1) > 9 |
           sum(evento == 11) > 9) %>%
  as.data.frame() %>%
  resp_rate_df(.,1,30) %>%
  group_by(sujeto,cde) %>%
  mutate(resp_norm = (resp - min(resp))/(max(resp) - min(resp)),
         ec_norm = (ec_resp - min(ec_resp))/(max(ec_resp) - min(ec_resp)))

save(norm_rate_BL, norm_rate_Exp,
     file = path_save.data +  "rate_norm.RData")
# ==============================================================================
## ---- Clustering by k medoids, pam function that is robust to outliers ------

df_p = pooled_ind_ana_km_df(
  list_ss = subjects_vec,
  paths = paths,
  sessions = 8,
  max_trials = 30
) %>%
  as.data.frame()

write.csv(df_p, path_save.data + 'kmedoids.csv', row.names = F)
