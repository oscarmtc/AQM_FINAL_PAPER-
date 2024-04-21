# The following code replicates the analysis in: Eva Anduiza & Guillem Rico, 
# "Sexism and the far-right vote: The individual dynamics of gender backlash." 
# American Journal of Political Science.
# 
# This code uses the data previously prepared and exported using the do-file 
# replication_code_1.do.
# 
# Analysis carried out using R version 4.2.1 (64-bit) in Windows 10 x64.




# If not already set, uncomment and set the folder where the data produced by 
# replication_code_1.do is stored as the working directory
# setwd("path/to/data")

# Load required packages (install using `install.packages()`)
library(tidyverse)
library(ragg)
library(ggridges)
library(ggrepel)
library(brglm2)
library(stargazer)
library(marginaleffects)
library(survey)




#################
### Main text ###
#################

# FIGURE 1

## Reads the data
data_fig1 <- read_delim("data_figure_1.txt", delim = "\t", escape_double = FALSE,
  col_names = c("year", "Sexism", "ll", "ul"), col_types = cols(year = col_integer()),
  trim_ws = TRUE, skip = 2)

## Adds specific date (at approx. midway of fieldwork period)
data_fig1 <- add_column(data_fig1, date = as.Date(c("2017-06-07", "2018-05-24", "2019-06-06", "2020-05-10")))

## Defines events' dates 
events <- data.frame(dates = as.Date(c("2018-03-08", "2017-10-15", "2018-12-02", "2019-04-28", "2019-11-10", "2020-03-11")), labels = c("Women's Day\n(March 8)", "#MeToo", "Andalusian\nelection", "General\nelection I", "General\nelection II", "COVID-19"), y = -Inf, yend = 0.42)

## Produces plot
figure_1 <- data_fig1 %>% 
  ggplot(aes(date, Sexism)) +
  annotate(geom = "text", x = events$dates, y = 0.425, label = events$labels, size = 2.6, lineheight = 0.7) +
  geom_pointrange(aes(ymin = ll, ymax = ul), color = "dodgerblue4") +
  geom_line(color = "dodgerblue4") +
  theme_classic() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2017-01-01", "2020-12-31"))) +
  scale_y_continuous(limits = c(0.326, 0.428), n.breaks = 6) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major = element_line(), panel.grid.minor = element_blank(), axis.line = element_line(size = .25), 
        axis.ticks = element_line(size = .25)) + 
  geom_segment(data = events, aes(x = dates, xend = dates, y = y, yend = yend), linetype = "dashed", color = 2)

## Saves plot
ggsave(plot = figure_1, filename ="figure_1.png", height = 3.5, width = 6.9, dpi = 600, units = "in", device = agg_png)



# FIGURE 2

## Reads and prepares the data
data_fig2 <- read_csv("data_figure_2.csv")
data_fig2 <- data_fig2 %>% 
  mutate(year = as.character(year), 
         yrchange = fct_recode(year, NULL = "2017", "2017-2018" = "2018", "2018-2019" = "2019", "2019-2020" = "2020"))

## Produces plot
figure_2 <- data_fig2 %>% 
  filter(year != "2017") %>% 
  ggplot(aes(x = difmsex, y = fct_relevel(yrchange, "2019-2020", "2018-2019"), fill = stat(x))) + 
  geom_density_ridges_gradient(stat = "binline", bins = 21, scale = 0.9) + 
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal()

## Saves plot (as file figure_2.png)
ggsave(plot = figure_2, filename = "figure_2.png", width = 4.5, height = 4.6, dpi = 600, units = "in", bg = "white", device = agg_png)



# FIGURE 3

## Reads, merges, and prepares the data
files <- c("mx_cohort.txt", "mx_education.txt", "mx_female.txt", "mx_ideology.txt", "mx_income.txt", "mx_interest.txt", "mx_partner.txt", "mx_party.txt")
names(files) <- c("Cohort", "Education", "Sex", "Ideological identification", "Income", "Interest", "Lives with partner", "Partisanship")

mixed <- files %>% 
  map_dfr(read_delim, delim = "\t", skip = 2, col_names = c("margin", "b", "cilow", "cihigh"), .id = "variable") %>%
  separate(margin, into = c("year", "category"), sep = " # ", convert = T)

mixed <- mixed %>% 
  mutate(category = fct_recode(category, "Cs" = "Ciudadanos"))

femob <- read_delim("mx_8m.txt", delim = "\t", skip = 2, col_names = c("margin", "b", "cilow", "cihigh")) %>% 
  separate(margin, into = c(NA, "year"), sep = " # ", convert = T) %>% 
  mutate(category = rep(c("None", "0.25", "0.5", "0.75", "Highest"), each = 4),
         variable = "Women's Day protest engagement")

mixed <- bind_rows(mixed, femob) %>% 
  mutate(variable = fct_relevel(variable, "Sex", "Cohort", "Education", "Income", "Lives with partner", "Interest", "Ideological identification", "Partisanship", "Women's Day protest engagement"),
         category = fct_relevel(category, "Female", "Male", "Lower 2ry", "Upper 2ry", "3ry", "Far left", "Left", "Center", "Right", "Far right", "Low", "Mid", "High", "Yes", "No", "Podemos", "Others", "PSOE", "Cs", "PP", "Highest", "0.75", "0.5", "0.25", "None"),
         label = case_when(category %in% c("0.25", "0.5", "0.75") ~ "",
                           TRUE ~ as.character(category))) %>%  
  group_by(variable) %>% 
  mutate(color = as.character(factor(category, labels = seq(from = 1, to = 5, length.out = n_distinct(category)))),
         color2 = as.numeric(color)) %>% 
  ungroup()

## Produces plot
figure_3 <- mixed %>% 
  filter(variable %in% c("Sex", "Ideological identification", "Partisanship", "Women's Day protest engagement")) %>% 
  ggplot(aes(year, b, group = category, color = color2)) +
  geom_pointrange(aes(ymin = cilow, ymax = cihigh), size = 0.3, position = position_dodge(width = 0.11)) +
  geom_line(position = position_dodge(width = 0.1)) +
  facet_wrap(~ variable, scales = "free_y") +
  guides(color = "none") +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  scale_colour_gradient(low = "lightblue3", high = "dodgerblue4") +
  scale_x_continuous(expand = expansion(add = c(0.2, 0.9))) +
  geom_text_repel(data = . %>% filter(year == 2020), aes(label = label), hjust = 0, nudge_x = 0.3, direction = "y", color = 1, size = 2.6, seed = 1234)

## Saves plot
ggsave(plot = figure_3, filename = "figure_3.png", width = 6, height = 5, dpi = 600, units = "in", device = agg_png)



# TABLE 3

## Reads the data
data_tbl3 <- read_csv("data_table_3.csv")

## Column 1 (model 1, 2019)
t3m1 <- glm(vim_vox ~ female + age + edu3_2 + edu3_3 + dhincome_all + livingpartner + intpol + l2vim_vox + l2authoritarian + l2ideol + l2nativism + l2orgterr + l2pop6amz + l2msexism + lsvim_vox + lsauthoritarian + lsideol + lsnativism + lsorgterr + lspop6amz + lsmsexism, family = binomial(logit), method = "brglmFit", data = data_tbl3 %>% filter(year == 2019))

## Column 2 (model 2, 2020)
t3m2 <- glm(vim_vox ~ female + age + edu3_2 + edu3_3 + dhincome_all + livingpartner + intpol + l2vim_vox + l2authoritarian + l2ideol + l2nativism + l2orgterr + l2pop6amz + l2msexism + lsvim_vox + lsauthoritarian + lsideol + lsnativism + lsorgterr + lspop6amz + lsmsexism, family = binomial(logit), method = "brglmFit", data = data_tbl3 %>% filter(year == 2020))

## Column 3 (model 3, 2019)
t3m3 <- update(t3m1, ~. - lsmsexism + posmsex + negmsex)

## Column 4 (model 4, 2020)
t3m4 <- update(t3m2, ~. - lsmsexism + posmsex + negmsex)

## Prints Table 3
stargazer(t3m1, t3m2, t3m3, t3m4, type = "text", dep.var.caption = "", dep.var.labels.include = F, column.labels = c("2019", "2020", "2019", "2020"), keep.stat = "n", star.char = c("+", "*", "**"), star.cutoffs = c(0.1, 0.05, 0.01),  notes = c("+ p<0.1; * p<0.05; ** p<0.01"), covariate.labels = c("Female", "Age", "Upper secondary", "Tertiary", "Income", "Lives with partner", "Interest in politics", "Vox intention (t-2)", "Authoritarianism (t-2)", "Ideological identification (t-2)", "Nativism (t-2)", "Territorial preference (t-2)", "Populism (t-2)", "Sexism (t-2)", "Vox intention (t-1 minus t-2)", "Authoritarianism (t-1 minus t-2)", "Ideological identification (t-1 minus t-2)", "Nativism (t-1 minus t-2)", "Territorial preference (t-1 minus t-2)", "Populism (t-1 minus t-2)", "Sexism (t-1 minus t-2)", "Increase in sexism (t-1 minus t-2)", "Decrease in sexism (t-1 minus t-2)"), notes.append = F, no.space = T, title = "Table 3. Effect of prior change in attitudes and vote intention on intended vote for Vox")

## Predicted probability of supporting Vox if change in sexism at 5th and 
## 95th percentile (-0.26 and 0.20 points respectively)
predt3m1 <- model.frame(t3m1) %>% 
  summarise(quants = quantile(lsmsexism, probs = c(.05, .95))) %>% 
  pull()

predictions(t3m1, newdata = datagrid(lsmsexism = c(predt3m1), grid.type = "counterfactual")) %>%
  group_by(lsmsexism) %>%
  summarise(across(predicted:std.error, mean))

## Predicted probability of supporting Vox if change in sexism is zero and 
## 0.20 points increase
predictions(t3m3, newdata = datagrid(posmsex = c(0, predt3m1[2]), negmsex = 0, grid.type = "counterfactual")) %>%
  group_by(posmsex, negmsex) %>%
  summarise(across(predicted:std.error, mean))



##############################
### Supporting information ###
##############################

# TABLE A4

## Reads the data
data_tbla4 <- read_csv("data_table_a4.csv")

## Prepares data for models 1 and 3
w789msex <- drop_na(data_tbla4, w789) %>% 
  filter(year == 2019)
w789msex <- svydesign(~ 1, weights = ~ w789, data = w789msex)

## Prepares data for models 2 and 4
w890msex <- drop_na(data_tbla4, w890) %>% 
  filter(year == 2020)
w890msex <- svydesign(~ 1, weights = ~ w890, data = w890msex)

## The following should issue informative warnings
## (not an error, see ?svyglm)
## Column 1 (2019)
ta4m1 <- svyglm(vim_vox ~ female + age + edu3_2 + edu3_3 + dhincome_all + livingpartner + intpol + l2vim_vox + l2authoritarian + l2ideol + l2nativism + l2orgterr + l2pop6amz + l2msexism + lsvim_vox + lsauthoritarian + lsideol + lsnativism + lsorgterr + lspop6amz + lsmsexism, family = binomial(logit), method = "brglmFit", design =  w789msex)

## Column 2 (2020)
ta4m2 <- svyglm(vim_vox ~ female + age + edu3_2 + edu3_3 + dhincome_all + livingpartner + intpol + l2vim_vox + l2authoritarian + l2ideol + l2nativism + l2orgterr + l2pop6amz + l2msexism + lsvim_vox + lsauthoritarian + lsideol + lsnativism + lsorgterr + lspop6amz + lsmsexism, family = binomial(logit), method = "brglmFit", design =  w890msex)

## Column 3 (2019)
ta4m3 <- update(ta4m1, ~. - lsmsexism + posmsex + negmsex)

## Column 4 (2020)
ta4m4 <- update(ta4m2, ~. - lsmsexism + posmsex + negmsex)

## Prints Table A4
stargazer(ta4m1, ta4m2, ta4m3, ta4m4, type = "text", dep.var.caption = "", dep.var.labels.include = F, column.labels = c("2019", "2020", "2019", "2020"), keep.stat = "n", star.char = c("+", "*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001), notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), covariate.labels = c("Female", "Age", "Upper secondary", "Tertiary", "Income", "Lives with partner", "Interest in politics", "Vox intention (t-2)", "Authoritarianism (t-2)", "Ideological identification (t-2)", "Nativism (t-2)", "Territorial preference (t-2)", "Populism (t-2)", "Sexism (t-2)", "Vox intention (t-1 minus t-2)", "Authoritarianism (t-1 minus t-2)", "Ideological identification (t-1 minus t-2)", "Nativism (t-1 minus t-2)", "Territorial preference (t-1 minus t-2)", "Populism (t-1 minus t-2)", "Sexism (t-1 minus t-2)", "Increase in sexism (t-1 minus t-2)", "Decrease in sexism (t-1 minus t-2)"), notes.append = F, no.space = T, title = "Table A4. Effect of prior change in attitudes on intended vote for Vox, weighted by the inverse probability of panel survival")



# FIGURE A1

## Reads the data
data_figa1 <- read_csv("data_figure_a1.csv")

## Produces plot
figure_a1 <- data_figa1 %>% 
  mutate(yearfct = fct_rev(as.factor(year))) %>% 
  ggplot(aes(x = msexism, y = yearfct, fill = stat(x))) + 
  geom_density_ridges_gradient(stat = "binline", bins = 20, scale = 0.9, alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.25)) + 
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(x = NULL, y = NULL) +
  guides(fill = "none") +
  theme_minimal()
ggsave(plot = figure_a1, filename = "figure_a1.png", width = 4.5, height = 6, dpi = 600, units = "in", bg = "white", device = agg_png)



# FIGURE A2

## The necessary data was already imported to produce Figure 3
## (object `mixed` in main text section)

## Produces plot
figure_a2 <- mixed %>% 
  filter(!variable %in% c("Sex", "Ideological identification", "Partisanship", "Women's Day protest engagement")) %>% 
  ggplot(aes(year, b, group = category, color = color2)) +
  geom_pointrange(aes(ymin = cilow, ymax = cihigh), size = 0.3, position = position_dodge(width = 0.11)) +
  geom_line(position = position_dodge(width = 0.1)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  guides(color = "none") +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  scale_colour_gradient(low = "lightblue3", high = "dodgerblue4") +
  scale_x_continuous(expand = expansion(add = c(0.2, 0.9))) + 
  geom_text_repel(data = . %>% filter(year == 2020), aes(label = label), hjust = 0, nudge_x = 0.25, direction = "y", color = 1, size = 2.6, seed = 123)

ggsave(plot = figure_a2, filename = "figure_a2.png", width = 6.5, height = 7.5, dpi = 600, units = "in", device = agg_png)



# TABLE A9

## The necessary data was already imported above to produce Table 3 
## (object `data_tbl3` in main text section)

## Column 1 (model 1, 2019)
ta9m1 <- glm(vim_vox ~ female + age + edu3_2 + edu3_3 + dhincome_all + livingpartner + intpol + l2vim_vox + l2authoritarian + l2ideol + l2nativism + l2orgterr + l2pop6amz + l2swim_msex + lsvim_vox + lsauthoritarian + lsideol + lsnativism + lsorgterr + lspop6amz + lsswim_msex, family = binomial(logit), method = "brglmFit", data = data_tbl3 %>% filter(year == 2019))

## Column 2 (model 2, 2020)
ta9m2 <- glm(vim_vox ~ female + age + edu3_2 + edu3_3 + dhincome_all + livingpartner + intpol + l2vim_vox + l2authoritarian + l2ideol + l2nativism + l2orgterr + l2pop6amz + l2swim_msex + lsvim_vox + lsauthoritarian + lsideol + lsnativism + lsorgterr + lspop6amz + lsswim_msex, family = binomial(logit), method = "brglmFit", data = data_tbl3 %>% filter(year == 2020))

## Column 3 (model 3, 2019)
ta9m3 <- update(ta9m1, ~. - lsswim_msex + posswim + negswim)

## Column 4 (model 4, 2020)
ta9m4 <- update(ta9m2, ~. - lsswim_msex + posswim + negswim)

## Prints results in Table A9
table_a9 <- stargazer(ta9m1, ta9m2, ta9m3, ta9m4, type = "text", dep.var.caption = "", dep.var.labels.include = F, column.labels = c("2019", "2020", "2019", "2020"), keep.stat = "n", star.char = c("+", "*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),  notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), notes.append = F, covariate.labels = c("Female", "Age", "Upper secondary", "Tertiary", "Income", "Lives with partner", "Interest in politics", "Vox intention (t-2)", "Authoritarianism (t-2)", "Ideological identification (t-2)", "Nativism (t-2)", "Territorial preference (t-2)", "Populism (t-2)", "Sexism (t-2)", "Vox intention (t-1 minus t-2)", "Authoritarianism (t-1 minus t-2)", "Ideological identification (t-1 minus t-2)", "Nativism (t-1 minus t-2)", "Territorial preference (t-1 minus t-2)", "Populism (t-1 minus t-2)", "Sexism (t-1 minus t-2)", "Increase in sexism (t-1 minus t-2)", "Decrease in sexism (t-1 minus t-2)"), no.space = T, title = "Table A9. Effect of prior change in attitudes and vote intention on intended vote for Vox, using the original modern sexism scale")


