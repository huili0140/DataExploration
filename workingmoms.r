### Dataset
library(data.table)
cols <- c("PAOC", "ST", "AGEP", "MAR", "ESR", "COW", "SCHL", "WAGP")
pusa <- fread("ss13pusa.csv", select = cols)
pusb <- fread("ss13pusb.csv", select = cols)
pus <- subset(rbind(pusa, pusb), !is.na(PAOC))
save(pus, file = "pus.Rdata")

library(ggplot2)
library(dplyr)
library(gridExtra)
library(maps)
load("pus.Rdata")

pus$MAR <- factor(pus$MAR)
levels(pus$MAR) <- c("Married", "Widowed", "Divorced", "Separated", "Never married")

pus$ESR <- factor(pus$ESR)
levels(pus$ESR) <- c("Employed", "Employed, not at work", "Unemployed", "Employed", "Employed, not at work", "Not in labor force")
pus$ESRG <- ifelse(pus$ESR == "Employed", 1, 0)

pus$COW <- factor(pus$COW)
levels(pus$COW) <- c("Private profit", "Private non-profit", "Local government", "State government", "Federal government", "Self-employed", "Self-employed", "Working without pay", "Unemployed")

pus$PAOC <- factor(pus$PAOC)
levels(pus$PAOC) <- c("Children under 6", "Children 6 to 17", "Children under 6 and 6 to 17", "No children")
pus$PAOCG <- ifelse(pus$PAOC == "No children", 0, 1)

pus$SCHL <- ifelse(pus$SCHL <= 16, 16, pus$SCHL)
pus$SCHL <- ifelse(pus$SCHL >= 17 & pus$SCHL <= 19, 19, pus$SCHL)
pus$SCHL <- factor(pus$SCHL)
levels(pus$SCHL) <- c("High school or lower", "Some college", "Associate's degree", "Bachelor's degree", "Master's degree", "Professional degree", "Doctorate degree")

am <- subset(pus, PAOCG == 1)

# Overview of the American Moms
### Age
ggplot(pus, aes(AGEP, group = PAOC)) + 
  geom_bar(binwidth = 1, aes(colour = PAOC, fill = PAOC), alpha = 0.3) +
  xlab("Age") + 
  ylab("Count") + 
  ggtitle("Women by Age")
prop.table(table(pus$PAOC))
by(pus$AGEP, pus$PAOCG, summary)

### Marital Status
```{r echo=TRUE, message=FALSE, warning=FALSE}
data <- as.data.frame(prop.table(table(am$AGEP, am$MAR)))
data$margin <- prop.table(table(am$AGEP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(am$AGEP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Age", y = "Frequency", title = "Marital Status of the Moms") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels=c("16", "27", "31", "33", "36", "38", "41", "43", "46", "50", "93"))

### Education Level
data <- as.data.frame(prop.table(table(am$AGEP, am$SCHL)))
data$margin <- prop.table(table(am$AGEP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(am$AGEP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Age", y = "Frequency", title = "Education Level of the Moms") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels=c("16", "27", "31", "33", "36", "38", "41", "43", "46", "50", "93"))

# The Working Moms
### Employment Status
data <- as.data.frame(prop.table(table(am$AGEP, am$ESR)))
data$margin <- prop.table(table(am$AGEP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(am$AGEP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Age", y = "Frequency", title = "Employment Status of the Moms") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels=c("16", "27", "31", "33", "36", "38", "41", "43", "46", "50", "93"))

### Marital Status vs Employment Status
chisq.test(table(am$MAR, am$ESR))
data <- as.data.frame(prop.table(table(am$MAR, am$ESR), margin = 1))
ggplot(data, aes(x = Var1, y = Freq, group = Var2)) + 
  geom_bar(stat = "identity", aes(colour = Var2, fill = Var2), alpha = 0.3) + 
  labs(x = "Marital Status", y = "Frequency", title = "Marital Status vs Employment Status")

### Education vs Employment Status
chisq.test(table(am$SCHL, am$ESR))
data <- as.data.frame(prop.table(table(am$SCHL, am$ESR), margin = 1))
ggplot(data, aes(x = Var1, y = Freq, group = Var2)) + 
  geom_bar(stat = "identity", aes(colour = Var2, fill = Var2), alpha = 0.3) + 
  labs(x = "Education", y = "Frequency", title = "Education vs Employment Status") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

### Field of the Moms's Work
wm <- subset(am, ESR == "Employed" & WAGP > 1000)
data <- as.data.frame(prop.table(table(wm$AGEP, wm$COW)))
data$margin <- prop.table(table(wm$AGEP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(wm$AGEP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Age", y = "Frequency", title = "Field of the Moms' Work") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels=c("16", "27", "31", "33", "36", "38", "41", "43", "46", "50", "93"))

### Wages of the Working Mom
wm$AGEG <- cut(wm$AGEP, breaks = quantile(wm$AGEP))
ggplot(na.omit(wm), aes(x = AGEG, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = AGEG), alpha = 0.5) + 
  labs(x = "Age Group", y = "Wage on Log10 Scale", title = "Wages vs Age Groups")

ggplot(na.omit(wm), aes(x = COW, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = COW), alpha = 0.5) + 
  labs(x = "Field of Work", y = "Wage on Log10 Scale", title = "Wage vs Field of Work") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)
ggplot(subset(wm, AGEP <= 56 & AGEP >= 21), aes(x = factor(AGEP), y = log10(WAGP))) + 
  stat_summary(fun.y=mean, aes(colour = COW), geom="point", size = 3) +
  stat_summary(fun.y=mean, aes(group=COW, colour = COW), geom="line") + 
  labs(x="Age", y="Wage on Log10 Scale", title="Wage vs Age, Grouped by Field of Work") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(na.omit(wm), aes(x = MAR, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = MAR), alpha = 0.5) + 
  labs(x = "Marital Status", y = "Wage on Log10 Scale", title = "Wage vs Marital Status") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)
ggplot(subset(wm, AGEP <= 56 & AGEP >= 21), aes(x = factor(AGEP), y = log10(WAGP))) + 
  stat_summary(fun.y=mean, aes(colour = MAR), geom="point", size = 3) +
  stat_summary(fun.y=mean, aes(group=MAR, colour = MAR), geom="line") + 
  labs(x="Age", y="Wage on Log10 Scale", title="Wage vs Age, Grouped by Marital Status") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(na.omit(wm), aes(x = SCHL, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = SCHL), alpha = 0.5) + 
  labs(x = "Education", y = "Wage on Log10 Scale", title = "Wage vs Education") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)
ggplot(subset(wm, AGEP <= 56 & AGEP >= 21), aes(x = factor(AGEP), y = log10(WAGP))) + 
  stat_summary(fun.y=mean, aes(colour = SCHL), geom="point", size = 3) +
  stat_summary(fun.y=mean, aes(group=SCHL, colour = SCHL), geom="line") + 
  labs(x="Age", y="Wage on Log10 Scale", title="Wage vs Age, Grouped by Education") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Working Moms across the States
data <- as.data.frame(prop.table(table(am$ST, am$ESRG), margin = 1))
data <- subset(data, Var2 == 1)
data$state <- c('alabama','alaska','arizona','arkansas','california','colorado','connecticut','delaware','district of columbia','florida','georgia','hawaii','idaho','illinois','indiana','iowa','kansas','kentucky','louisiana','maine','maryland','massachusetts','michigan','minnesota','mississippi','missouri','montana','nebraska','nevada','new hampshire','new jersey','new mexico','new york','north carolina','north dakota','ohio','oklahoma','oregon','pennsylvania','rhode island','south carolina','south dakota','tennessee','texas','utah','vermont','virginia','washington','west virginia','wisconsin','wyoming')

all_states <- map_data("state")
all_states$freq <- data$Freq[match(all_states$region, data$state)]*100
ggplot(all_states, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill = freq),colour="gray") + 
  ggtitle("Percentage of Working Moms across the States")

head(arrange(data, -Freq), 5) # The States with the highest employment rate of moms
tail(arrange(data, -Freq), 5) # The States with the lowest employment rate of moms

wm_grouped <- group_by(wm, ST)
data <- summarise(wm_grouped, wage = mean(WAGP))
data$state <- c('alabama','alaska','arizona','arkansas','california','colorado','connecticut','delaware','district of columbia','florida','georgia','hawaii','idaho','illinois','indiana','iowa','kansas','kentucky','louisiana','maine','maryland','massachusetts','michigan','minnesota','mississippi','missouri','montana','nebraska','nevada','new hampshire','new jersey','new mexico','new york','north carolina','north dakota','ohio','oklahoma','oregon','pennsylvania','rhode island','south carolina','south dakota','tennessee','texas','utah','vermont','virginia','washington','west virginia','wisconsin','wyoming')

all_states <- map_data("state")
all_states$wage <- data$wage[match(all_states$region, data$state)]
ggplot(all_states, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill = wage),colour="gray") + 
  ggtitle("Wage of Working Moms across the States")

head(arrange(data, -wage), 5) # The States with the highest wage of working moms
tail(arrange(data, -wage), 5) # The States with the lowest wage of workign moms
