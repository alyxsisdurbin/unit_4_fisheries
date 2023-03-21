# Haley Durbin
# 2023-03-21
# glm

source("build_collapse_table.R")
head(fish)
head(collapse)

load('data/RAMLDB v4.495/R Data/DBdata[asmt][v4.495].RData')

# we can use logistic regression for anything that is a binary variable where there is only two possible outcomes

# logistic regression 
  #can we see how likely a fishery is to collapse based on the type it is
model_data = collapse %>%
  group_by(stockid) %>%
  summarize(ever_collapsed = any(current_collapse)) %>%
  ungroup() %>%
  left_join(stock) %>%
  left_join(metadata) %>%
  mutate(FisheryType = as.factor(FisheryType)) %>%
  filter(!is.na(FisheryType))
head(model_data)

  # build/run a logistic regression
model_l = glm(ever_collapsed ~ FisheryType, data = model_data, family = "binomial") #have to tell it what kind og glm to run, this is bionomial
summary(model_l)

model_data %>% distinct(FisheryType) %>% arrange(FisheryType)

FisheryType = model_data %>%
  distinct(FisheryType)
model_l_predict = predict(model_l, newdata=FisheryType, se.fit=TRUE, type="response")
head(model_l_predict)

collapse_fishery_type_predictions = cbind(FisheryType, model_l_predict) # combining or data

# Plot predictions and SE bars
ggplot(aes(x=FisheryType, y=fit, fill=FisheryType), data=collapse_fishery_type_predictions) +
  geom_bar(stat="identity", show.legend = FALSE) + # stat="count" is default (like histogram)
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=0.2) +
  coord_flip() +
  ylab("Probability of stock collapse")

# Poisson Model
  # U is the harvest and Umsy is the max sust. yield. if gthe divising of this is greatger than 1 means we are overfishing
  
u_summary = timeseries_values_views %>%
  filter(!is.na(UdivUmsypref),
         !is.na(BdivBmsypref)) %>%
  group_by(stockid) %>%
  summarize(yrs_data=n(),
            ratio_yrs_overfished = sum(UdivUmsypref > 1)/yrs_data,
            ratio_yrs_low_stock = sum(BdivBmsypref < 1)/yrs_data)%>%
  select(-yrs_data)

collapse_summary = collapse %>%
  group_by(stockid) %>%
  summarize(yrs_data = n(),
            yrs_collapsed = sum(current_collapse)) %>%
  inner_join(u_summary, by="stockid") # only keep stocks that have collapse data AND u_summary data

head(collapse_summary)
hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)  

collapse_summary_zero_trunc = collapse_summary %>% filter(yrs_collapsed>0)
table(collapse_summary_zero_trunc$yrs_collapsed)

# Build posson model
model_p = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock, 
              offset(log(yrs_data)), 
              data=collapse_summary_zero_trunc, family="poisson") 
summary(model_p)

library(AER)
AER::dispersiontest(model_p)

model_qp = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock , offset(log(yrs_data)), data=collapse_summary_zero_trunc, family="quasipoisson") 
summary(model_qp)
