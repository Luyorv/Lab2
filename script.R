#pregunta 1.1
html_base <- httr::GET("https://www.mediawiki.org/wiki/MediaWiki")

library(XML)

xml_base <-  htmlParse(html_base, asText = TRUE)

#pregunta 1.2
title <- xpathSApply(xml_base,'//title')

title[1]

#pregunta 1.3
list_texte <- xpathSApply(xml_base, "//a", xmlAttrs, 'href')

class(list_texte)

#pregunta 1.4

library(dplyr)

agg_tbl <- df_link %>% group_by(url,text) %>%  summarise(total_count=n(), .groups = 'drop')

df_links2 <- agg_tbl %>% as.data.frame()


#pregunta 1.5

getstatuscode <- function(url){
  #Sys.sleep(runif(1, min=1, max=1))
  demo1 <- httr::GET(urlstandar(url))
  res <- head(demo1)
  return(res$status_code)
}


urlstandar <- function(url){
  print(url)
  if (startsWith(url,'//')) {
    updated_url <- gsub(" ", "", paste('https:',url))
    return(updated_url)
  }
  if(startsWith(url,'/')){
    updated_url <- gsub(" ", "", paste('https://www.mediawiki.org',url))
    return(updated_url)
  }
  if(grepl('#', url)){
    return('https://www.mediawiki.org/wiki/MediaWiki')
  }
  return(url)
}


df_links3 <- df_links2

df_links3$estatus <- sapply(df_links3$url,getstatuscode)

df_links3$enlace <- sapply(df_links3$url,urlstandar)

#Pregunta 2.1

rela_norela <- function(url){
  if (startsWith(url,'//')) {
    return('absoluta')
  }
  if (startsWith(url,'http')) {
    return('absoluta')
  }
  if(startsWith(url,'/')){
    return('relativa')
  }
  if(grepl('#', url)){
    return('absoluta')
  }
  return(url)
}

df_links3$url_cat <- sapply(df_links3$url,rela_norela)

library(ggplot2)

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
  plot(df_links3$total_count, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  
})

url_2_tbl <- df_links3 %>% group_by(url_cat)

url_2_tbl <- df_links3 %>% group_by(url_cat,url) %>%  summarise(total_count_cat=n(), .groups = 'drop')

hist(x = df_links3$url_cat, breaks = 10, col = "blue", main = "CVSSv3 Score
distribution")

url_abs <- df_links3[df_links3$url_cat == 'absoluta',]


url_rela <- df_links3[df_links3$url_cat == 'relativa',]


hist(x = df_links3$total_count, labels = df_links3$url, breaks = 10, col = "blue", main = "Frecuencia de aparicion de URL Absolutas")
