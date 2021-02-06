# How does shot selection impact winning thus far? 06/02/2021

library(nbastatR)
library(tidyverse)
library(gt)
library(paletteer)


logo <-c("C:\\logos\\nba\\atl.png","C:\\logos\\nba\\bos.png","C:\\logos\\nba\\bkn.png",
         "C:\\logos\\nba\\cha.png","C:\\logos\\nba\\chi.png","C:\\logos\\nba\\cle.png",
         "C:\\logos\\nba\\dal.png","C:\\logos\\nba\\den.png","C:\\logos\\nba\\det.png",
         "C:\\logos\\nba\\gsw.png","C:\\logos\\nba\\hou.png","C:\\logos\\nba\\ind.png",
         "C:\\logos\\nba\\lac.png","C:\\logos\\nba\\lal.png","C:\\logos\\nba\\mem.png",
         "C:\\logos\\nba\\mia.png","C:\\logos\\nba\\mil.png","C:\\logos\\nba\\min.png",
         "C:\\logos\\nba\\nop.png","C:\\logos\\nba\\nyk.png","C:\\logos\\nba\\okc.png",
         "C:\\logos\\nba\\orl.png","C:\\logos\\nba\\phi.png","C:\\logos\\nba\\phx.png",
         "C:\\logos\\nba\\por.png","C:\\logos\\nba\\sac.png","C:\\logos\\nba\\sas.png",
         "C:\\logos\\nba\\tor.png","C:\\logos\\nba\\uta.png","C:\\logos\\nba\\was.png")

plan(multiprocess)
teams_players_stats(seasons=2020, types='team',season_types="Regular Season",
                    tables='general',measures='Base',modes='PerGame',
                    is_rank=F,is_plus_minus=F,is_pace_adjusted=F)

df20 <- dataGeneralTeams %>% 
  arrange(nameTeam) %>% 
  mutate(Logo=logo) %>% 
  select(Logo,nameTeam,fg3m,fg3a,pctFG3,pctWins) %>% 
  rename(Team=nameTeam,`3PM 2019-20`=fg3m,`3PA 2019-20`=fg3a,
         `3P% 2019-20`=pctFG3,`Win% 2019-20`=pctWins)

plan(multiprocess)
teams_players_stats(seasons=2021, types='team',season_types="Regular Season",
                    tables='general',measures='Base',modes='PerGame',
                    is_rank=F,is_plus_minus=F,is_pace_adjusted=F)

df21 <- dataGeneralTeams %>% 
  arrange(nameTeam) %>% 
  mutate(Logo=logo) %>% 
  select(Logo,nameTeam,fg3m,fg3a,pctFG3,pctWins) %>% 
  rename(Team=nameTeam,`3PM 2020-21`=fg3m,`3PA 2020-21`=fg3a,
         `3P% 2020-21`=pctFG3,`Win% 2020-21`=pctWins)

rm(dataGeneralTeams)

df <- df20 %>% 
  inner_join(df21, by=c("Team","Logo")) %>% 
  mutate(`3PA Diff`= `3PA 2020-21` - `3PA 2019-20`,
         `3PM Diff`= `3PM 2020-21` - `3PM 2019-20`,
         `3P% Diff (pp)`= round(`3P% 2020-21`*100 - `3P% 2019-20`*100,1),
         `Win% Diff (pp)`= `Win% 2020-21`*100 - `Win% 2019-20`*100)


# 3PA chart
p1 <- ggplot(data=df, aes(x=`3PA 2019-20`,y=`3PA 2020-21`)) +
  geom_image(aes(image=Logo),size=0.08) + scale_size_identity() +
  geom_abline(intercept=0,slope=1, colour='gray55',linetype="dashed") +
  scale_x_continuous(limits=c(25,50),breaks=seq(25,50,5)) +
  scale_y_continuous(limits=c(25,50),breaks=seq(25,50,5)) +
  labs(title="3P Shooting: Attempts in 2019-20 vs 2020-21",
       subtitle="Which teams changed their 3P volume from last year?",
       caption="Source: NBA.com \n Filippos Polyzos | @filippos_pol")
ggsave("p1.png",p1,width=8,height=8,dpi=600)



# Table with changes from year to year

gt1 <- df %>% 
  select(Logo,Team,`3PA Diff`,`3PM Diff`,`3P% Diff (pp)`,`Win% Diff (pp)`) %>% 
  gt() %>% 
  cols_label(Logo="") %>% 
  tab_header(
    title = 
      html("<b>3P Shooting: 2019-20 vs 2020-21</b>"),
    subtitle = html(
      "<em>Last update: 02/06/21</em>"
    )) %>%
  cols_align(
    align = "left",
    columns = vars(Logo)
  ) %>% 
  text_transform(
    locations = cells_body(vars(Logo)),
    fn = function(Logo) {
      lapply(Logo, local_image)
    }
  ) %>% 
  tab_source_note(
    md("**Source:** NBA.com <br> **Table by:** Filippos Polyzos | @filippos_pol")
  )%>%
  tab_style(
    style=cell_text(font="Bahnschrift"),
    locations = list(cells_title(groups=c("title","subtitle")),
                     cells_body(columns=T,rows=T),
                     cells_column_labels(columns=T)
                     
    )) %>% 
  data_color(
    columns = vars(`3PA Diff`),
    colors = scales::col_numeric(
      palette = RColorBrewer::brewer.pal(5,"Blues"),
      domain = NULL
    )) %>% 
  data_color(
    columns = vars(`Win% Diff (pp)`),
    colors = scales::col_numeric(
      palette = RColorBrewer::brewer.pal(5,"Greens"),
      domain = NULL
    )) %>% 
  data_color(
    columns = vars(`3PM Diff`),
    colors = scales::col_numeric(
      palette = RColorBrewer::brewer.pal(5,"RdPu"),
      domain = NULL
    )) %>% 
  data_color(
    columns = vars(`3P% Diff (pp)`),
    colors = scales::col_numeric(
      palette = RColorBrewer::brewer.pal(5,"Reds"),
      domain = NULL
    ))
gtsave(gt1,"p2.png")  

# check correlations
c20<-round(cor(df[,3:6]),2) ; c21<-round(cor(df[,7:10]),2)




  