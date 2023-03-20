# fish businesses

rdat <- read_csv("data/cfbus.csv")
# dates provided in col 11 (expected logical), rows 79472:79476

dat <- rdat[-c(79472:79476),]
glimpse(dat)

(temp <- 
    dat %>% 
    filter(IssueYear >= 2010) %>% 
    group_by(IssueYear, FBUSLic2) %>% 
    summarise(n = n()))

temp <-
  temp %>% 
  mutate(year = as.factor(IssueYear),
         licence = as.factor(FBUSLic2),
         .keep = "unused",
         .before = n) %>% 
  ungroup() %>% 
  select(-IssueYear)

temp %>% 
  ggplot(aes(x = year, y = n, fill = licence)) +
  geom_col() +
  theme(axis.text.x = 
          element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Fish Business Licenses") +
  ylab("number issued") +
  xlab("") +
  scale_fill_viridis_d()
