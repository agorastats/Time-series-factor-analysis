##read html table from web page calendario. Enter in inspect and copy the css path generator of the table.
require(XML)

u = 'https://www.calendario-365.es/dias-festivos/2017.html'

# install.packages("xml2")
require(xml2)

doc.html<-read_html(u)


taulafest<-read_html(u) %>% html_nodes(xpath="/html/body/div[2]/div[2]/div/div[1]/div/table") %>% 
  html_table(fill=TRUE)

d<-as.data.frame(taulafest)

require(writexl)

save(d,file="festivos_2017.RData")

##agafem i cambiem idioma environment a ESP i pasem les dates a dies o Timestamps.
Sys.setenv(LANGUAGE="es")
as.Date('Abril 26, 2001',format='%B %d, %Y')
as.Date('Abril 26, 2001',format='%B %d, %Y')


festivos<-as.Date(paste0(d$Fecha,", 2017"),format='%d %B, %Y')
fiestat<-as.Date(paste0(d$Fecha[grepl("Santo|Santa",d$Día.festivo)],", 2017"),format='%d %B, %Y')[-8]
fiestat<-c(fiestat,dmy(paste0(rep(20:31),"/12/2017")),dmy(paste0(rep(01:05),"/01/2017")))

news <- superm %>% mutate(Date = dmy(Date), festivo = ifelse(Date %in% festivos,1,0),holw = ifelse(Date %in% fiestat,1,0)) %>% as.data.frame()

save(news,file = "tabla_festivos.RData")
