require(readr)
require(dplyr)
require(magrittr)

require(scales)
require(formattable)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

swag <- read_csv("GitHub/articles/world_series_swag/swag.csv")

require(ggplot2)

swag %>% group_by(Year,Team,Outcome,Title_Drought,Pennant_Drought)  %>% 
  tally() %>% ungroup() -> counts

counts %>% ggplot(aes(x=Year,y=n,color=Outcome)) + geom_point() +
  ggtitle("World Series Related Giveaways, by Year")

ggsave("GitHub/articles/world_series_swag/timeplot1.png")

z <- lm(n ~ Year,data=counts %>% filter(Outcome=="Won"))

counts %>% filter(Outcome=='Won') %>% ggplot(aes(x=Year,y=n)) + geom_point(color="#00BFC4") +
  ggtitle("World Series Related Giveaways, by Year\nWinning Teams Only") + 
  geom_smooth(method=lm,se=FALSE) +
  annotate("text",x = 2007,y=15,label=paste0("Adj R^2 = ",round(summary(z)$adj.r.squared,2),
                                             "\np = ",round(summary(z)$coefficients[2,4],2),
                                             "\ncoef=",round(summary(z)$coefficients[2,1],2)))

ggsave("GitHub/articles/world_series_swag/regressionplot.png")

# counts %>% filter(Outcome=='Lost') %>% ggplot(aes(x=Year,y=n)) + geom_point(color="#00BFC4") +
#   ggtitle("World Series Related Giveaways, by Year") + geom_smooth(method=lm,se=FALSE)

set.seed(100)

counts %>% ggplot(aes(x=Outcome,y=n,color=Outcome)) + geom_boxplot() + 
  geom_jitter() + guides(color=FALSE) + 
  ggtitle("Distribution of Number of World Series-Related\n Giveaways by Outcome")

ggsave("GitHub/articles/world_series_swag/boxplot.png")

# counts %>% ggplot(aes(x=Title_Drought,y=n,color=Outcome)) + geom_jitter() +
#   ggtitle("World Series Related Giveaways, by Length of Title Drought")
# 
# counts %>% ggplot(aes(x=Pennant_Drought,y=n,color=Outcome)) + geom_jitter() +
#   ggtitle("World Series Related Giveaways, by Length of Pennant Drought")

swag %<>% mutate(plot_date = 
                   as.Date(paste0("2017-",substring(Date_String,1,1),"-",
                                  substring(Date_String,3,5))),
                 dec_month = Month + (as.numeric(substring(Date_String,3,5))-1)/30 
                 )

# swag %>% ggplot(aes(x=plot_date,color=Outcome)) + geom_density()

# swag %>% ggplot() + 
#   geom_dotplot(aes(x=plot_date,fill=Outcome),binwidth=7) + facet_grid(Outcome~.)

swag %>% ggplot(aes(x=Year,y=dec_month)) + geom_point(aes(color=Outcome)) + geom_smooth(se=F) +
  ggtitle("Dates of Giveaways by Year With Smoother") + ylab("Time of Year (4.0 = April 1)") +
  scale_y_continuous(breaks=pretty_breaks())
  
ggsave("GitHub/articles/world_series_swag/dateyearplot.png")


# swag %>% ggplot(aes(factor(Year),dec_month)) + geom_boxplot() + geom_point() + geom_smooth()
# 
# swag %>% filter(Team != 'SFG' | Year != '2014') %>% ggplot() + 
#   geom_dotplot(aes(x=plot_date,fill=Outcome),binwidth=7) + facet_grid(Outcome~.)

################################

swag %>% group_by(Object) %>% tally() %>% ungroup() %>% arrange(-n) -> objs

# swag %>% select(Year,Team,Object,Outcome) %>% #unique() %>% 
#   ggplot(aes(x=reorder(Object,Object,
#                               function(x) length(x)))) + geom_bar() + coord_flip() +
#   facet_grid(.~Outcome)


swag %>% select(Year,Team,Object,Outcome) %>% unique() %>% 
  ggplot(aes(x=reorder(Object,Object,
                       function(x) length(x)))) + geom_bar() + coord_flip() +
  facet_grid(.~Outcome) + ylab("Number of Teams") + xlab("Item") +
  ggtitle("Items Given Out By Outcome") + scale_y_continuous(breaks=pretty_breaks())

ggsave("GitHub/articles/world_series_swag/gifttypes.png",width=10,height=8)

swag %>% select(Year,Team,Object) %>% # unique() %>% 
  ggplot(aes(x=reorder(Object,Object,
                       function(x) length(x)))) + geom_bar() + coord_flip()

mean(ifelse(is.na(swag$Notes),'',swag$Notes) == "Children's")

swag %>% group_by(Object) %>% mutate(ct = n()) %>% ungroup() %>%
  mutate(solo = ct == 1) %>% group_by(Year,Team) %>% 
  summarize(total = n(),solo = sum(solo)) %>% ungroup() %>% arrange(-solo)

t.test(data=counts,n~Outcome)

median1 = numeric(1000)
median2 = numeric(1000)
mean1 = numeric(1000)
mean2 = numeric(1000)


temp <- counts

obswonmed <- median(counts[counts$Outcome=="Won",]$n)
obslostmed <- median(counts[counts$Outcome=="Lost",]$n)
obswonmean <- mean(counts[counts$Outcome=="Won",]$n)
obslostmean <- mean(counts[counts$Outcome=="Lost",]$n)


set.seed(13)

for (k in 1:10000){
  temp$rand = sample(temp$Outcome,length(temp$Outcome))
  median1[k] <- median(temp[temp$rand=="Won",]$n)
  median2[k] <- median(temp[temp$rand=="Lost",]$n)
  mean1[k] <- mean(temp[temp$rand=="Won",]$n)
  mean2[k] <- mean(temp[temp$rand=="Lost",]$n)
}

mean(median1 == median2)

diffs <- median1 - median2

mean(abs(diffs)>=(obswonmed-obslostmed))
mean(diffs>=(obswonmed-obslostmed))

mean(abs(mean1-mean2)>=abs(obswonmean-obslostmean))
mean(mean1-mean2>=(obswonmean-obslostmean))

diffs %>% as_data_frame() %>% ggplot(aes(x=diffs)) + geom_histogram(binwidth=1)

options(digits = 2)

counts %>% group_by(Outcome) %>% mutate(group_mn = mean(n)) %>% ungroup() %>%
  mutate(LAA = n-group_mn) -> laa

laa %>% arrange(-LAA) %>% select(Year,Team,Outcome,n,LAA) %>% print.data.frame(row.names=F)

require(tibble)

wts <- tribble(
  ~Object,~Weight,
  'Ring',4,        
  'Trophy',4,
  'Poster',2,
  'Cap',2,
  'Flag',3,
  'Pennant',3,
  'Shirt',2,
  'Baseball',2,
  'Jersey',2,
  'Banner',3,
  'Snow Globe',2,
  'Bobblehead',3
)
  
swag %>% left_join(wts,by='Object') %>% group_by(Year,Team,Object) %>% mutate(rn = row_number()-1) %>%
  ungroup() %>% 
  mutate(ModWeight = pmax(coalesce( Weight-rn*.5 ,1),coalesce(Weight,2)/2) *
           ifelse(coalesce(Notes,'') == "Children's",0.75,1)) -> weighted

weighted %>% group_by(Year,Team,Outcome) %>% summarize(SWAG = sum(ModWeight),Avg=mean(ModWeight),Count=n() ) %>%
  ungroup() %>% mutate(#CombValue = sqrt(SWAG*Avg),Mult = Count * CombValue,
                       SWAGGER = SWAG - (pmin(Count,5)*.5 + pmin(Count,5)*(pmin(Count,5)-1)/2*.1
                                        + (pmax(Count,5)-5)*1 )) -> team_swag

options(digits = 3)

team_swag %>% arrange(-SWAG) %>% select(Year,Team,Outcome,Count,SWAG,Avg) %>% 
  print.data.frame(row.names=F)

team_swag %>% arrange(-SWAGGER) %>% select(Year,Team,Outcome,Count,SWAG,Avg,SWAGGER) %>% 
  write.table("clipboard",sep='\t')