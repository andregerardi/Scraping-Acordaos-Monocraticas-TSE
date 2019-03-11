library(XML)

#Função: se for branco ou nulo = NA
nullToNA <- function(x) {
  if (is.null(x)){
    return(NA)
  } else (x)
}

#fake news
url_base <- "http://www.tse.jus.br/jurisprudencia/@@monocraticas-search?url=&q=&as_epq=fake+news&start=CONTADORLINK"

#Raspando os textos
TextoTSE <- data.frame()
for (i in 1:7){
  print(i)
  i <- (i - 1) * 10 + 0
  url <- gsub("CONTADORLINK", i, url_base)
  
  pagina <- readLines(url)
  pagina <- htmlParse(pagina, encoding = "UTF-8")
  pagina <- xmlRoot(pagina)
  
  texto_vetor <- nullToNA(xpathSApply(pagina, "//div/div[@class='box-ementa-sf-div']", xmlValue))
  texto1 <- nullToNA(texto_vetor)
  
  TextoTSE <- rbind(TextoTSE, data.frame(texto1, check.rows = T))
}

#Remove espaços em branco
TextoTSE$texto1 <- gsub("\\s+", " ", TextoTSE$texto1)
