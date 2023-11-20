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