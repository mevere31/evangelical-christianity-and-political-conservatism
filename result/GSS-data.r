#INFO 610 Stats Project
##initializing r file from GSS
library(foreign)
  read.dct <- function(dct, labels.included = "yes") {
      temp <- readLines(dct)
      temp <- temp[grepl("_column", temp)]
      switch(labels.included,
             yes = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
                 classes <- c("numeric", "character", "character", "numeric", "character")
                 N <- 5
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
             },
             no = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
                 classes <- c("numeric", "character", "character", "numeric")
                 N <- 4
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
             })
      temp_metadata <- setNames(lapply(1:N, function(x) {
          out <- gsub(pattern, paste("\\", x, sep = ""), temp)
          out <- gsub("^\\s+|\\s+$", "", out)
          out <- gsub('\"', "", out, fixed = TRUE)
          class(out) <- classes[x] ; out }), NAMES)
      temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
      temp_metadata
  }

  read.dat <- function(dat, metadata_var, labels.included = "yes") {
      read.table(dat, col.names = metadata_var[["ColName"]])
  }
  
#set working directory
getwd()
setwd("/Users/mikalaeverett/Downloads/result")

GSS_metadata <- read.dct("GSS.dct")
GSS_ascii <- read.dat("GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii

#Part 1: Setting up data to understand what is happening with the variables/data
##loading libraries
library(dplyr)
library(lsr)
library(stringr)
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(dygraphs)
library(lubridate)
library(corrplot)
#only pull white responses and create a new data frame
##I will be comparing responses from white conservative respondents versus white liberal respondents  
white <- GSS[GSS$RACE == "1",]

#finding and identifying the data types (all integers)
sapply(white, class)

#looking at frequency of conservative vs liberal responders 
## 1-3 slightly to extremely liberal; 5-7 slightly conservative to extremely conservative
frequency <- table(white$POLVIEWS)
print(frequency) #3,884 resondents on liberal end of spectrum and 4,828 respondents on liberal end of spectrum

#now looking at frequency of evangelical christian respondents
frequency <- table(white$DENOM)
print(frequency) #baptist and other denomination highest number of respondents after "not applicable"


#creating visualization of where respondents reside
region <- white %>%
  group_by(REGION) %>%
  summarize(mean_dbh = mean(REGION))
barplot(region$mean_dbh, names.arg = region$REGION) #1 = Northeast, 2 = Midwest, 3 = South, 4 = West

#Part 2: Understanding relationships between variables and testing correlation
##creating two data frames: conservative and liberal
conservative <- white %>% filter(POLVIEWS %in% c(5,6,7))
liberal <- white %>% filter(POLVIEWS %in% c(1,2,3))


##now I will look at changes over time in different variables 
###checking that R will read years as time variable
str(white)

#changing year to numeric variable
white$YEAR <- as.numeric(white$YEAR)
conservative$YEAR <- as.numeric(conservative$YEAR)

#changes in conservative respondents
hist(conservative$YEAR)
ggplot(conservative,aes(x=YEAR)) +
  geom_histogram() +
  theme_gray()

frequency <- table(conservative$YEAR)
print(frequency)

#########Mikala maybe add other denominations
##splitting the denominations into separate frames to evaluate each (all evangelical just different denominations)
baptist <- white[white$DENOM == "1",]
other <- white[white$DENOM == "6",]
nondenomchristian <- white[white$DENOM == "7",]

#changes to evangelical christian respondents over the years
hist(baptist$YEAR)
ggplot(baptist,aes(x=YEAR)) +
  geom_histogram() +
  theme_gray()

frequency <- table(baptist$YEAR)
print(frequency)

hist(other$YEAR)
ggplot(other,aes(x=YEAR)) +
  geom_histogram() +
  theme_gray()

frequency <- table(other$YEAR)
print(frequency)

hist(nondenomchristian$YEAR)
ggplot(nondenomchristian,aes(x=YEAR)) +
  geom_histogram() +
  theme_gray()

frequency <- table(nondenomchristian$YEAR)
print(frequency)

#Part 3: Correlation Tests 
##Part 3A: Conservative Variables Correlation
##creating scatterplots to view relationships between certain variables and conservatives
variables <- c("LIBHOMO","GUNLAW", "PRAYER", "FEPOL", "ABANY", "HOMOSEX", "SEXEDUC")

##creating a pivot table
long_table <- conservative %>%
  select(POLVIEWS, all_of(variables)) %>%
  pivot_longer(cols = all_of(variables),
               names_to = "Views",
               values_to = "Responses") %>%
  mutate(PoliticalViews = case_when(
    Views == "LIBHOMO" ~ "Gay Books in Library",
    Views == "GUNLAW" ~ "In Favor of Gun Control",
    Views == "PRAYER" ~ "Prayer in Schools",
    Views == "FEPOL" ~ "Females in Politics",
    Views == "ABANY" ~ "Abortion for any Reason",
    Views == "HOMOSEX" ~ "Same Sex Relationships",
    Views == "SEXEDUC" ~ "Sex Education in Schools"
  ))

###scatterplot matrix
ggplot(long_table, aes(x = Responses, y = Views)) +
  geom_point(color = "darkred", alpha = 0.7) +
  facet_wrap(~PoliticalViews, scales = "free") +
  labs(title = "Conservative Responses to American Societal Issues",
       x = NULL,
       y = "Views") +
  theme_minimal() 



##Part 3B: Liberal Correlation
##creating scatterplots to view relationships between certain variables and liberals
##creating a pivot table
lib_table <- liberal %>%
  select(POLVIEWS, all_of(variables)) %>%
  pivot_longer(cols = all_of(variables),
               names_to = "Views",
               values_to = "Responses") %>%
  mutate(PoliticalViews = case_when(
    Views == "LIBHOMO" ~ "Gay Books in Library",
    Views == "GUNLAW" ~ "In Favor of Gun Control",
    Views == "PRAYER" ~ "Prayer in Schools",
    Views == "FEPOL" ~ "Females in Politics",
    Views == "ABANY" ~ "Abortion for any Reason",
    Views == "HOMOSEX" ~ "Same Sex Relationships",
    Views == "SEXEDUC" ~ "Sex Education in Schools"
  ))

###scatterplot matrix
ggplot(lib_table, aes(x = Responses, y = Views)) +
  geom_point(color = "darkblue", alpha = 0.7) +
  facet_wrap(~PoliticalViews, scales = "free") +
  labs(title = "Conservative Responses to American Societal Issues",
       x = NULL,
       y = "Views") +
  theme_minimal() 

###correlations and derivative metrics
variables_to_exclude <- c("-100", "-99", "-98", "-97", "1", "2", "3", "4")

new_white <- white %>% 
  filter(!PARTYID %in% variables_to_exclude) 
###denomination
cor.test(new_white$LIBHOMO, new_white$DENOM, method = "kendall")
cor.test(new_white$GUNLAW, new_white$DENOM, method = "kendall")
cor.test(new_white$PRAYER, new_white$DENOM, method = "kendall")
cor.test(new_white$FEPOL, new_white$DENOM, method = "kendall")
cor.test(new_white$ABANY, new_white$DENOM, method = "kendall")
cor.test(new_white$HOMOSEX, new_white$DENOM, method = "kendall")
cor.test(new_white$SEXEDUC, new_white$DENOM, method = "kendall")


##correlation matrix
views <- select(new_white, DENOM, POLVIEWS, LIBHOMO, GUNLAW, PRAYER, FEPOL, 
                ABANY, HOMOSEX, SEXEDUC)
matrix <- cor(views, use = "complete.obs")
print(matrix)
corrplot(matrix)

###heatmap
long_matrix <- matrix %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

### create a heatmap
ggplot(long_matrix, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", mid = "darkorange", high = "darkred",
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  labs(title = "Correlation Heatmap",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())


#ordinal logistic regression - we're modeling our outcome variable (political views)
##our main predictor is denomination and covariates will be the respondent variables above

# Evangelical denominations: Baptist (1), Other Denomination (6), Non-denominational (7)
white$evangelical <- ifelse(white$DENOM %in% c(1, 6, 7), 1, 0)

# Convert to factor
white$evangelical <- factor(white$evangelical, 
                            labels = c("Not_Evangelical", "Evangelical"))
white$POLVIEWS <- factor(white$POLVIEWS,
                         levels = c(1,2,3,4,5,6,7),
                         ordered = TRUE)

library(MASS)

model <- polr(POLVIEWS ~ evangelical + ABANY + GUNLAW + PRAYER +
                HOMOSEX + FEPOL + SEXEDUC + YEAR,
              data = white, 
              Hess = TRUE)

summary(model)

# Odds ratios
exp(coef(model))

# 95% confidence intervals for the odds ratios
exp(confint(model))
ctable <- coef(summary(model))
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p_values)
ctable

library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)

# Extract odds ratios and CI
or_df <- data.frame(
  term = names(exp(coef(model))),
  OR = exp(coef(model)),
  conf.low = exp(confint(model)[,1]),
  conf.high = exp(confint(model)[,2])
)

# Remove YEAR (NA CI)
or_df <- or_df |> filter(!is.na(conf.low))

or_df$term <- fct_reorder(or_df$term, or_df$OR)

# Plot
ggplot(or_df, aes(x = term, y = OR)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(title = "Odds Ratios with 95% Confidence Intervals",
       x = "", y = "Odds Ratio") +
  theme_minimal(base_size = 14)

library(tidyverse)
library(broom)

# Get tidy model output (coefficients only)
coef_df <- broom::tidy(model) %>%
  filter(!str_detect(term, "\\|")) %>%   # remove intercepts
  mutate(term = fct_reorder(term, estimate))

# Coefficient plot
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error,
                    ymax = estimate + std.error),
                width = 0) +
  coord_flip() +
  labs(title = "Ordinal Logistic Regression Coefficients (log-odds)",
       x = "", y = "Coefficient (log-odds)") +
  theme_minimal(base_size = 14)





