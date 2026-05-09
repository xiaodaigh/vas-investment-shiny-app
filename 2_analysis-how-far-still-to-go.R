meh = b7 %>% 
  mutate(higest_ever = cummax(close)) %>% 
  arrange(desc(date)) %>% 
  mutate(lowest_will_go = cummin(close)) %>% 
  arrange(date) %>% 
  mutate(biggest_ever_drop = 1-lowest_will_go/higest_ever)

biggest_ever_drop_global = meh$biggest_ever_drop %>% max

meh1 = meh %>% 
  mutate(how_far_have_i_dropped = 1-close/higest_ever) %>% 
  mutate(still_to_go_worse = biggest_ever_drop_global - how_far_have_i_dropped) %>% 
  tail


View(meh1)


meh2 = meh %>% 
  filter(lowest_will_go == close) %>% 
  filter(year(date) < 2020) %>% 
  mutate(d = c(NA, diff(date))) %>% 
  filter(d != 1) %>% 
  mutate(date = date + lubridate::days(365)) %>% 
  select(date, close) %>% 
  left_join(select(meh, date, close2 = close), by = "date") %>% 
  filter(!is.na(close2)) %>% 
  mutate(inc = close2/close-1) %>% 
  summarise(mean(inc))
