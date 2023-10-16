##----Set-WD----
#setwd("D:/MYaseen208_B0-22-7A-E9-23-9B/Consultancy/Dr._Zahid_Asghar_QAU/2023-06-19")


##----Reading-Packages----
suppressMessages(source("001LoadingRPackages.R"))


##----Reading-Data----
# LFS21 <-
#   readRDS("docs/data/LFS21.rds") %>%
#   as.data.table()
# 
# save(LFS21, file = "docs/data/LFS21.RData")
# 
# load("docs/data/LFS21.RData")
# 
# 
# LFS21 |> glimpse()
# 
# LFS21 |> select(contains("S7"))
# 
# ##----Analysis----
# LFS21 <- 
#   LFS21 %>%
#   fmutate(
#     S7C33_52 = S7C33*52
#     , S7C43_12 = S7C43*12
#   ) %>%
#   fmutate(total_income= rowSums(fselect(., c(S7C33_52, S7C43_12, S7C53, S7C7)), na.rm = TRUE)) %>%  
#   fsubset(total_income > 0) 
# 
# LFS21 |> glimpse()
# 
# LFS_20 <- 
#   LFS21 |> 
#     fmutate(
#     edu_level =
#       nif(
#         S4C9 == 1,  "never attend school"
#       , S4C9 == 2,  "nursery below K.G"
#       , S4C9 == 3,  "K.G below primary"
#       , S4C9 == 4,  "primary below middle"
#       , S4C9 == 5,  "middle below matric"
#       , S4C9 == 6,  "matric below inter"
#       , S4C9 == 7,  "inter below degree"
#       , S4C9 == 8,  "degree eng"
#       , S4C9 == 9,  "degree medi"
#       , S4C9 == 10, "degree comp"
#       , S4C9 == 11, "degree agri"
#       , S4C9 == 12, "degree other"
#       , S4C9 == 13, "M.A/M.Sc"
#       , S4C9 == 14, "M.Phil"
#       , S4C9 == 15, "Ph.D"
#       )
#       ) %>% 
#   fmutate(edu_level = as_factor(edu_level))
# 
# 
# LFS_20 |> select(S5C8) |> head()
# 
# LFS_20 <- LFS_20 |>  fmutate(new = str_extract(S5C8, "\\d{1}")) |>
#   fmutate(
#     main_occupation = nif(
#       new == 1, "Managers",
#       new == 2,"Professionals",
#       new == 3, "Technicians"
#       , new == 4,"Clerk"
#       , new == 5,"Sale_worker"
#       , new == 6,"agriculture_fishry"
#       , new == 7,"trade_worker"
#       ,new == 8,"palnt_operator"
#       ,new == 9,"elementary_occupation"
#       , new == 01,
#       "armed forces"
#     )
#   )
# 
# 
# names(LFS_20)
# LFS_20 |> group_by(RSex) |> filter(main_occupation=="Clerk") |>  
#   summarise(avg_income=mean(total_income),n=n()) |> 
#   arrange(-avg_income)
# 
# 
# 
# 
# names(LFS_20)
# 
# 
# LFS_20 |> group_by(RSex) |> filter(main_occupation=="Clerk") |>  
#   summarise(avg_income=mean(total_income),n=n()) |> 
#   arrange(-avg_income)
# 
# LFS_20 |> group_by(edu_level)  |>  summarise(avg_income=mean(total_income),n=n()) |> 
#   arrange(-avg_income)
# 
# 
# 
# LFS_20 |> group_by(S5C10)  |> filter(main_occupation=="Professionals") |> summarise(avg_income=mean(total_income),n=n()) |> 
#   arrange(-avg_income)
# ## Age Distributioin
# 
# names(LFS_20)
# LFS_20 <- LFS_20 |> fmutate(age_group = cut(S4C6, breaks = c(0,1, 2, 4, 9, 14, 19,24,29,34,39,44,49,54,59,64,Inf), 
#                                   labels = c("Less than 1 Year", "1-2", "02-04", "04-09",
#                                              "10-14", "15-19","20-24","25-29",
#                                              "30-34","35-39","40-44",
#                                              "45-49","50-54","55-59",
#                                              "60-64","65+")))
#save(LFS_20,file="docs/data/LFS_20.RData")
load("docs/data/LFS_20.RData")

LFS_20 |> glimpse()
# library(survey)
# LFS_20W <- svydesign(ids=~1,weights =LFS_20$Weights, data = LFS_20)
# LFS_20W
LFS_20 |> group_by(age_group) |> summarise(percentage=n(),)




LFS_20 |> group_by(main_occupation, Province) |>
  filter(main_occupation == "palnt_operator")  |>
  summarise(avg_inc = mean(total_income), n = n()) |>
  arrange(-avg_inc)
LFS_20 |> group_by(main_occupation)  |>
  summarise(avg_inc = mean(total_income), n = n()) |>
  arrange(-avg_inc) -> avg_income

avg_income_20 <- as_tibble(avg_income) |> mutate(year=2020)

avg_income_09 <- as_tibble(avg_income_09) |> mutate(year=2009)

occ_inc <- bind_rows(avg_income_09,avg_income_20)
occ_inc


library(gt)
library(gtExtras)
occ_inc |> gt()


## Gender wise comparison of earnings

LFS_20 |> filter(RSex==1|RSex==2) |> 
ggplot()+
  aes(x=as_factor(RSex),y=log(total_income))+geom_boxplot()


LFS_20 |> 
  ggplot()+aes(log(total_income),color="Province")+
  geom_histogram()

ggplot(LFS_20)+
  aes(x=as_factor(edu_level),y=log(total_income))+geom_boxplot()+
  coord_flip()

LFS_20 |> glimpse()
ggplot(LFS_20)+
  aes(x=as_factor(S4C9),y=log(S7C43_12))+geom_boxplot()+
  coord_flip()


ggplot(LFS_20)+
  aes(x=as_factor(edu_level),y=log(S7C43_12), na.omit=TRUE)+geom_boxplot()+
  coord_flip()

LFS_20 |> filter(S7C43_12>0) |> 
ggplot()+
  aes(x=as_factor(S4C9),y=log(S7C43_12))+geom_point()
+
  coord_flip()


