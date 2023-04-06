library("tidyverse")
hotel_bookings <- read_csv("../Downloads/hotel_bookings.csv")
head(hotel_bookings)

library("ggplot2")

ggplot(data=hotel_bookings) +
  geom_point(mapping = aes(x= stays_in_weekend_nights, y=children))


library("palmerpenguins")

ggplot(data=penguins) +
  geom_point(mapping = aes(x=flipper_length_mm,y=body_mass_g, shape=species), color="purple") +
  geom_smooth(mapping = aes(x=flipper_length_mm,y=body_mass_g,linetype=species))

ggplot(data=penguins) +
  geom_jitter(mapping = aes(x=flipper_length_mm,y=body_mass_g, shape=species), color="purple")

ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut,fill=clarity))

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(aes(color=species)) + facet_grid(~sex)

ggplot(data=diamonds,aes(x=color)) +
  geom_bar(aes(fill=cut)) + facet_wrap(~cut)
penguins %>% 
  select(-species)

penguins %>% 
  rename(island_new = island)

rename_with(penguins,tolower)

clean_names(penguins)


trimmed_df <- bookings_df %>% 
  select("hotel","is_canceled","lead_time") %>% 
  rename("hotel_type" = hotel)

example_df <- bookings_df %>% 
  select(arrival_date_year, arrival_date_month) %>% 
  unite(arrival_month_year, c("arrival_date_month","arrival_date_year"), sep = " ")

example_df <- bookings_df %>% 
  mutate(guests = adults + children + babies) %>% 
  select(guests)

head(example_df)

example_df <- bookings_df %>% 
  summarize(number_canceled = sum(is_canceled), average_lead_time = mean(lead_time))


quartet %>% 
  group_by(set) %>% 
  summarize(mean(x), sd(x), mean(y), sd(y),cor(x,y))

ggplot(quartet, aes(x,y)) + geom_point() + geom_smooth(method=lm, se=FALSE)

hotel_bookings <- read_csv("hotel_bookings.csv")

hotel_bookings_v2 <- arrange(hotel_bookings, desc(lead_time))

mean(hotel_bookings_v2$lead_time)

colnames(hotel_bookings)

ggplot(data=penguins) + geom_point(mapping=aes(x = flipper_length_mm, y = body_mass_g))

library("ggplot2")
hotel_bookings <- read_csv("../Downloads/hotel_bookings.csv")

ggplot(data = hotel_bookings) +
  geom_bar(aes(x = distribution_channel, fill=deposit_type)) + facet_wrap(~deposit_type~market_segment) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data=hotel_bookings) +
  geom_point(aes(x=lead_time,y=children))

ggplot(data=hotel_bookings) +
  geom_bar(aes(x=hotel,fill = market_segment)) +
  facet_wrap(~market_segment)

onlineta_city_hotel <- filter(hotel_bookings, hotel =="City Hotel" &
                                hotel_bookings$market_segment=="Online TA")

onlineta_city_hotel_v2 <- hotel_bookings %>% 
  filter(hotel=="City Hotel") %>% 
  filter(market_segment=="Online TA")

ggplot(data=onlineta_city_hotel) +
  geom_point(aes(x=lead_time,y=children))


ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(aes(color=species)) +
  labs(title="Palmer Penguins: Body Mass vs. Flipper Length", subtitle="Sample of Three Penguins Species", caption = "data collected by Dr.")+
  annotate("text",x=220,y=3500,label="The Gentoos are the largest", color="purple",fontface="bold", size = 4.5, angle = 25)

p <- ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(aes(color=species)) +
  labs(title="Palmer Penguins: Body Mass vs. Flipper Length", subtitle="Sample of Three Penguins Species", caption = "data collected by Dr.")

p + annotate("text",x=220,y=3500,label="The Gentoos are the largest", color="purple",fontface="bold", size = 4.5, angle = 25)

ggsave("Three Penguin Species.png")

ggplot(data=hotel_bookings) +
  geom_bar(aes(x=market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle =45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       caption = paste0("Data from: ",mindate, " to ", maxdate),
       x="Market Segment", y="Number of Bookings")

mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)
