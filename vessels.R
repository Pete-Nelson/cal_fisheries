# commercial fishing vessels

rdat <- read_csv("data/cvslreg.csv")
print(head(rdat), width = Inf)
glimpse(rdat)
dim(rdat)

dat <- 
  rdat %>% 
  drop_na(RegIssueDate, DOB, YearBuilt) %>% 
  mutate(DOB = ymd(DOB),
         RegIssueDate = ymd(RegIssueDate),
         vsl_age = year(RegIssueDate) - YearBuilt)
dim(dat)
print(head(dat), width = Inf)

# SCRATCH ####################################################



# from Bri
#### Vessel Characteristics ####
## LENGTH
# only count vessels once over the 21 yr time period (group by vesselid)
length <- cvslreg %>% filter(VesselYear %in% "1998":"2018") %>% group_by(Length,VesselID) %>% tally() %>% tally()
length <- length %>% filter(!Length %in% 0 & !Length > 100)  #This takes out 0 values and anything above 100...this is not methodical but rather somehwat arbitrary but leave for now. 
# rounded up
length$Length<-ceiling(length$Length)
length <- length %>% group_by(Length) %>% summarise(n = sum(n))
length_plot <-
  ggplot(length, aes(x = as.factor(Length), y = n)) + 
  geom_bar(stat = "identity") +
  theme(
    axis.text = element_text(
      angle = 45,
      hjust = 1,
      size = 7
    )) + labs(x = "Vessel Length", y = "Number of Vessels") + ggtitle("Vessel Length Distribution (1998-2018)")
print(length_plot)