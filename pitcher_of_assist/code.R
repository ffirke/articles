library(RMySQL)
require(readr)
require(dplyr)


foo<-dbConnect(MySQL(), host="localhost", dbname="retrosheet",user="root",port=3306)

rs = dbSendQuery(foo, "select game_id,inning,batting_team,left_field,center_field,right_field,
fielder_with_first_assist,
fielder_with_second_assist,
fielder_with_third_assist,
fielder_with_fourth_assist,
fielder_with_fifth_assist

from events where fielder_with_first_assist in (7,8,9)
or fielder_with_second_assist in (7,8,9)
or fielder_with_third_assist  in (7,8,9)
or fielder_with_fourth_assist  in (7,8,9)
or fielder_with_fifth_assist              in (7,8,9)    
                 ")

data = fetch(rs, n=-1)

tall <- bind_rows(data %>% filter(fielder_with_first_assist==9) %>% select(game_id,inning,batting_team,right_field) %>% rename(fielder=right_field),
          data %>% filter(fielder_with_second_assist==9) %>% select(game_id,inning,batting_team,right_field) %>% rename(fielder=right_field),
          data %>% filter(fielder_with_third_assist==9) %>% select(game_id,inning,batting_team,right_field) %>% rename(fielder=right_field),
          data %>% filter(fielder_with_fourth_assist==9) %>% select(game_id,inning,batting_team,right_field) %>% rename(fielder=right_field),
          data %>% filter(fielder_with_fifth_assist==9) %>% select(game_id,inning,batting_team,right_field) %>% rename(fielder=right_field),
          data %>% filter(fielder_with_first_assist==8) %>% select(game_id,inning,batting_team,center_field) %>% rename(fielder=center_field),
          data %>% filter(fielder_with_second_assist==8) %>% select(game_id,inning,batting_team,center_field) %>% rename(fielder=center_field),
          data %>% filter(fielder_with_third_assist==8) %>% select(game_id,inning,batting_team,center_field) %>% rename(fielder=center_field),
          data %>% filter(fielder_with_fourth_assist==8) %>% select(game_id,inning,batting_team,center_field) %>% rename(fielder=center_field),
          data %>% filter(fielder_with_fifth_assist==8) %>% select(game_id,inning,batting_team,center_field) %>% rename(fielder=center_field),
          data %>% filter(fielder_with_first_assist==7) %>% select(game_id,inning,batting_team,left_field) %>% rename(fielder=left_field),
          data %>% filter(fielder_with_second_assist==7) %>% select(game_id,inning,batting_team,left_field) %>% rename(fielder=left_field),
          data %>% filter(fielder_with_third_assist==7) %>% select(game_id,inning,batting_team,left_field) %>% rename(fielder=left_field),
          data %>% filter(fielder_with_fourth_assist==7) %>% select(game_id,inning,batting_team,left_field) %>% rename(fielder=left_field),
          data %>% filter(fielder_with_fifth_assist==7) %>% select(game_id,inning,batting_team,left_field) %>% rename(fielder=left_field))

require(Lahman)

m <- Master
p <- Pitching

p %>% group_by(playerID) %>% summarize(outs=sum(IPouts)) %>% ungroup() %>% 
  left_join(m %>% select(playerID,retroID,nameFirst,nameLast)) -> did_pitch

did_pitch %>% inner_join(tall,by=c("retroID"="fielder")) %>% filter(outs >= 100,playerID != "ankieri01") %>% 
  mutate(year=substr(game_id,4,7)) %>% arrange(year) %>% write_csv()