library(XML)

#Funcao: se for branco ou nulo = NA
nullToNA <- function(x) {
  if (is.null(x)){
    return(NA)
  } else (x)
}

#notícias falsas - 37 resultados
#url_base <- "http://www.tse.jus.br/jurisprudencia/@@monocraticas-search?url=&q=&as_epq=not%C3%ADcias+falsas&start=lin"

#fake news - 64 resultados
url_base <- "http://www.tse.jus.br/jurisprudencia/@@monocraticas-search?url=&q=&as_epq=fake+news&start=lin"


#Pegando os links das a????es na justi??a eleitoral
linksTSE <- c()
for (i in 1:3){
  print(i)
  i <- (i - 1) * 10 + 0
  url <- gsub("lin", i, url_base)
  
  pagina <- readLines(url)
  pagina <- htmlParse(pagina, encoding = "UTF-8")
  pagina <- xmlRoot(pagina)
  
  link <- nullToNA(xpathSApply(pagina, "//div[1]/span[6]/span/a", xmlGetAttr, name = "href"))
  linksTSE <- c(linksTSE, link)
}


#Raspando os dados dos links dos processos
dados <- data.frame()
for (link in linksTSE){
  print(link)
  
  pagina <- readLines(link)
  pagina <- htmlParse(pagina, encoding = "UTF-8")
  pagina <- xmlRoot(pagina)
  
  processo <- nullToNA(xpathSApply(pagina, "//tr[1]/td[3]/text()[1]", xmlValue))
  muni <- nullToNA(xpathSApply(pagina, "//tr[2]/td[3]/text()", xmlValue))
  
  dados <- rbind(dados, data.frame(processo, muni, link, check.rows = T))
}

#retira espaços em branco
dados$muni <- gsub("\\s+", " ", dados$muni)
dados$processo <- gsub("\\s+", " ", dados$processo)
dados$link <- gsub("\\s+", " ", dados$link)

#Salva dados TSE
write.csv2(dados, file = "dados_2018.csv",  eol = "\r\n", na = "NA", fileEncoding = "macroman")

##########

#Raspando os textos

TextoTSE <- data.frame()
for (i in 1:7){
  print(i)
  i <- (i - 1) * 10 + 0
  url <- gsub("lin", i, url_base)
  
  pagina <- readLines(url)
  pagina <- htmlParse(pagina, encoding = "UTF-8")
  pagina <- xmlRoot(pagina)
  
  Link <- nullToNA(xpathSApply(pagina, "//div[1]/span[6]/span/a", xmlGetAttr, name = "href"))
  Publicaçao <- nullToNA(xpathSApply(pagina, "//div[@class='s box-dados largura-max']/ul/li/text()",xmlValue))
  Cidade <- nullToNA(xpathSApply(pagina, "//div[@class='s box-dados relator']/span/text()",xmlValue))
  ClasseProcessual <- nullToNA(xpathSApply(pagina, "//div[@class='s box-dados largura-max']/span/text()",xmlValue))
  Relator <- nullToNA(xpathSApply(pagina, "//div[@class='s box-dados largura-max relator']/span/text()", xmlValue))
  
  texto_vetor <- nullToNA(xpathSApply(pagina, "//div/div[@class='box-ementa-sf-div']", xmlValue))
  texto1 <- nullToNA(texto_vetor)
  
  TextoTSE <- rbind(TextoTSE, data.frame(Link, Publicacao,Cidade,ClasseProcessual,Relator,texto1, check.rows = T))
}

Relator <- url %>%
  read_html() %>%
  html_nodes(xpath = "//div[@class='s box-dados largura-max relator']/span") %>%
  html_text()


TextoTSE$pub <- gsub("\\s+", " ", TextoTSE$pub)
TextoTSE$texto1 <- gsub("\\s+", " ", TextoTSE$texto1)

#Salva Texto TSE
setwd("~/Google Drive/Artigos/Abuso do poder religioso/Scraping Processos/Modelo TSE Looping/bancos")
write.csv2(TextoTSE, file = "texto_igreja_2018.csv",  eol = "\r", na = "NA", fileEncoding = "macroman")

###############

#Pega dados das ações

DadosTSE <- data.frame()
for (i in 1:9){
  print(i)
  i <- (i - 1) * 10 + 0
  url <- gsub("lin", i, url_base)
  
  pagina <- readLines(url)
  pagina <- htmlParse(pagina,encoding = "UTF-8")
  pagina <- xmlRoot(pagina)
  
  Link <- nullToNA(xpathSApply(pagina, "//div[1]/span[6]/span/a", xmlGetAttr, name = "href"))
  Publicacao <- nullToNA(xpathSApply(pagina, "//div[12]/ul/li/text()|
                                              //div[11]/ul/li/text()|
                                              //div[10]/ul/li/text()",xmlValue))
  Cidade <- nullToNA(xpathSApply(pagina, "//div[@class='s box-dados relator']/span/text()",xmlValue))
  ClasseProcessual <- nullToNA(xpathSApply(pagina, "//div[@class='s box-dados largura-max']/span/text()",xmlValue))
  Relator <- nullToNA(xpathSApply(pagina, "//div[@class='s box-dados largura-max relator']/span/text()", xmlValue))
  
  
  DadosTSE <- rbind(DadosTSE, data.frame(Link,Publicacao,Cidade,ClasseProcessual, Relator, check.rows = T))
}

DadosTSE$pub <- gsub("\\s+", " ", DadosTSE$pub)
DadosTSE$texto1 <- gsub("\\s+", " ", DadosTSE$texto1)

