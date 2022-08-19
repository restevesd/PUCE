library(lubridate)
library(rtweet)
library(tidyverse)
library(udpipe)


## store api keys (these are fake example values; replace with your own keys)
consumer_key = ''
consumer_secret = ''
access_token = ''
access_secret = ''

## authenticate via web browser
token <- create_token(
  app = "TallerSee",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

keyword = c("asalto","robo","sicariato","asesinato","terrorista")

cn <- search_tweets2(
  q = keyword,
  n = 600, 
  include_rts = FALSE, 
  lang = "es",
  geocode_for_free("guayaquil, ec") %>% near_geocode(50))

cn <- cn %>% mutate(created_at = with_tz(created_at, tz = "America/Bogota") )

cn2 <- cn %>% select(created_at,screen_name,text)

write.csv2(cn2,"tweet-seguridad2.csv")

cn %>% group_by(screen_name) %>% summarise(total=n()) %>% arrange(desc(total)) %>% top_n(10) %>%
  ggplot(aes(reorder(screen_name,total),total,label=total,fill=total))+
  geom_bar(stat="identity")+ coord_flip() +
  theme_minimal() + geom_label( colour = "white")+
  theme(plot.title = element_text(face = "bold", size = 13)) +
  theme(axis.text = element_text(size=10))+
  labs(
    x = NULL, y = NULL,
    title = "Cuentas que más postean sobre inseguridad",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## Horas de posteo ###

cn%>% mutate(created_at = hour(created_at)) %>%  
  group_by(created_at) %>% summarise(total=n()) %>% 
  ggplot(aes(created_at,total,label=total,fill=total))+
  geom_bar(stat="identity")+
  theme_minimal() + geom_label( colour = "white")+
  theme(plot.title = element_text(face = "bold", size = 13)) +
  theme(axis.text = element_text(size=10))+
  scale_x_discrete(name ="Horas", 
                   limits=seq(0,23))+
  labs(
    x = NULL, y = NULL,
    title = "Hora de posteo sobre seguridad",
    subtitle = paste0(nrow(cn)," mensajes analizados"),
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

### Cantidad de Posteos por día ###
cn %>% mutate(created_at = floor_date(as.Date(created_at)),"hour")%>% 
  group_by(created_at) %>%
  summarise(total = n()) %>% 
  ggplot(aes(created_at,total,label=total,fill = total))+
  geom_bar(stat="identity")+ 
  geom_text(aes(label=total), vjust=-0.5, colour="black", size=4)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  theme(axis.text = element_text(size=10))+
  labs(
    x = NULL, y = NULL,
    title = "Tweets hablando de seguridad",
    subtitle = "Cantidad de tweets analizados por día",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

### Frecuenciad de mensajes usando TS ###
cn %>% ts_plot("2 hour") +
  geom_point() + theme_minimal()+ 
  theme(plot.title = element_text(face="bold"))+
  labs(x = NULL, y = NULL,
       title = "Frecuencia mensajes",
       subtitle = "Tweets posteados acerca de seguridad",
       capton ="'\nSource: Data vía rtweet - made by: Roberto Esteves")

require(tmaptools)

geocode_for_free <- function (address, ...) 
{
  if (missing(address)) 
    stop("must supply address", call. = FALSE)
  
  stopifnot(is.atomic(address))
  
  res_geocode_osm <- tmaptools::geocode_OSM(address, ...)
  
  convert_osm_to_rtweet(res_geocode_osm)
  
}

convert_osm_to_rtweet <- function(geocode_osm) {
  
  place <- geocode_osm$query
  
  boxp <- geocode_osm$bbox %>%
    set_names(c("sw.lng", "sw.lat", "ne.lng", "ne.lat")) %>%
    .[c("sw.lng", "sw.lat", "ne.lng", "ne.lat")]
  
  point <- geocode_osm$coords %>%
    set_names(c("lng", "lat")) %>%
    .[c("lat", "lng")]
  
  return(as.coords(place, boxp, point))
}

as.coords <- function(place, box, point) {
  coords <- list(place = place, box = box, point =  point)
  class(coords) <- c("coords", "list")
  coords
}

near_geocode <- function(geocode, distance = 1000) {
  c(geocode$point, str_c(distance, "mi")) %>% str_c(collapse = ",") 
}


lda_ <- function(df)
{
  ud_model <- udpipe_download_model(language = "spanish")
  ud_model2 <- udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")
  annot_bio <- udpipe_annotate(ud_model2, x = df$text, doc_id = df$source)
  annot_bio <- as.data.frame(annot_bio)
  
  annot_bio <- annot_bio %>%
    filter(upos %in% c("NOUN"))
  
  annot_bio <- document_term_frequencies(annot_bio, document = "doc_id", term = "lemma")
  annot_bio <- document_term_matrix(x = annot_bio)
  annot_bio <- dtm_remove_lowfreq(annot_bio, minfreq = 5)
  
  k <- 4
  burnin = 1000
  iter = 1000
  keep = 50
  seedNum <- 42
  
  lda <- LDA(annot_bio, k = k, method = "Gibbs", control = list(
    burnin = burnin, iter = iter, keep = keep, seed=seedNum))
  
  as_tibble(terms(lda,5))
  
  candidatos_topics <- broom::tidy(lda, matrix="beta")
  
  candidatos_topterms <- candidatos_topics %>%
    group_by(topic) %>% 
    top_n(5, beta) %>%
    ungroup() %>%  
    arrange(topic, -beta) %>%
    mutate(term = reorder(term, beta))
  
  return(candidatos_topterms)
} 

ggplot(lda_(cn) %>% filter(topic <= 9), aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()+  labs(
    x = NULL, y = NULL,
    title="Temáticas usando LDA",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
