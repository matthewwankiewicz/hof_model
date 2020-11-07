library(Lahman)
library(tidyverse)

# bring in HOF data
hof <- HallOfFame

# use master for names
names <- Master
names <- names %>% 
  mutate(name = paste(nameFirst, nameLast, sep = " ")) %>% 
  select(playerID, name)

# bring in hitting data
hitters <- Batting

# filter out players who played after 2012,
# those players likely aren't HOF eligible according to the data
hitters <- hitters %>% 
  filter(yearID < 2013)
  
hitters[is.na(hitters)] <- 0

career_totals <- hitters %>% 
  group_by(playerID) %>% 
  summarise(G = sum(G),
            AB = sum(AB),
            R = sum(R),
            H = sum(H),
            X2B = sum(X2B),
            X3B = sum(X3B),
            HR = sum(HR),
            RBI = sum(RBI),
            SB = sum(SB),
            CS = sum(CS),
            BB = sum(BB),
            SO = sum(SO),
            IBB = sum(IBB),
            HBP = sum(HBP),
            SF = sum(SF),
            SH = sum(SH),
            GIDP = sum(GIDP))

hof <- hof %>% 
  filter(inducted == "Y",
         category == "Player") %>% 
  select(playerID, inducted)

career_totals <- career_totals %>% 
  mutate(hof = ifelse(playerID %in% hof$playerID, 1, 0))
career_totals <- career_totals  %>% 
  filter(G >= 1000)

# make variables

career_totals <- career_totals %>% 
  mutate(X1B = H - X2B - X3B - HR,
         AVG = round(H/AB, digits = 3),
         wOBA = round((.69*(BB - IBB) + .72*HBP + .89*X1B + 1.27*X2B + 1.62*X3B + 2.1*HR)/(AB + BB - IBB + SF + HBP), 
                      digits = 3),
         BABIP = round((H - HR)/(AB - SO - HR + SF), digits = 3))

career_totals <- career_totals %>% 
  filter(H > 500)

career_totals <- inner_join(career_totals, names, by = "playerID")

write_rds(career_totals, "outputs/career_data.rds")



