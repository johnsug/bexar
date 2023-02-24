
library(data.table)
library(ggplot2)
library(lubridate)
library(Cairo)
library(googlesheets4)
library(forecast)

#font_import()
# ## anti-alias (only need to run this once)
# trace(grDevices::png, quote({
#   if (missing(type) && missing(antialias)) {
#     type <- "cairo-png"
#     antialias <- "subpixel"
#   }
# }), print = FALSE)

## read in data
b <- read_sheet("https://docs.google.com/spreadsheets/d/1qtLRIkicROhzvH7PU6V0b8_dejjCQfVJA9gXVEdcYLI/edit#sharing", range="Sheet1!A:C")
b <- data.table(b)
b <- b[!is.na(Date)]
b[, .N, by=Date]
b <- b[, .(Vert=sum(Vert)), by=c("Runner", "Date")]
b[, Date:=as_date(Date)]
## fresh start
b <- rbind(b[, .(Date=as_date("2023-01-31"), Vert=0), by=Runner], b)
## cum sum
b[, T_Vert:=cumsum(Vert), by=Runner]
## add label to last point
b[, Text:=ifelse(T_Vert==max(T_Vert), Runner, ""), by=Runner]

## feet per day
ev <- data.table(
  Runner=rep("Everest", 29),
  Date=as_date("2023-01-31") + 0:28,
  T_Vert=round((0:28)/28*29092),
  Text="")
ev

## I need an empty dates vector
## then I need to join on the b vector
b[, .(Date=max(Date)), by=Runner]

## forecast where runners will end up...
bf <- data.table(Runner="", Date=as_date(Sys.Date()), Vert=0, T_Vert=0, Text="NULL")
for (i in b[, unique(Runner)]){
  f <- forecast::thetaf(b[Runner==i, T_Vert], h=as.numeric(as_date("2023-02-28") - b[Runner==i, max(Date)]))
  bf <- rbind(bf,
              rbind(b[Runner==i & Date==b[Runner==i, max(Date)]],
                    data.table(
                      Runner=i,
                      Date=b[Runner==i, max(Date)] + 1:(length(f$mean)),
                      Vert=0,
                      T_Vert=as.numeric(round(f$mean)),
                      Text="")))
 
 
}
bf <- bf[Text!="NULL"]
bf[, Text:=ifelse(T_Vert==max(T_Vert), Runner, ""), by=Runner]

## convert to day of month
b[, DOM:=as.numeric(substr(as.character(Date),9,10))]
b[, DOM:=ifelse(DOM==31,0,DOM)]

## final plot
ggplot(b, aes(x=Date, y=T_Vert, group=Runner, label=Text)) +
  ## gray
  geom_vline(xintercept=as_date(Sys.Date()), color="gray", linetype="dashed", size=1, alpha=.5) +
  geom_line(data=ev[Date<=as_date(Sys.Date())], aes(x=Date, y=T_Vert, group=Runner), color="gray", size=2, alpha=.5) +
  geom_line(data=ev, aes(x=Date, y=T_Vert, group=Runner), color="gray", size=2, alpha=.25) +
  geom_point(data=ev[Date==as_date(Sys.Date())], aes(x=Date, y=T_Vert, group=Runner), color="gray", size=3, alpha=.5) +
  ## orange
  #geom_line(color="#F84601", size=3.5, alpha=.5) +
  geom_line(color="#F84601", size=1, alpha=.8) +
  #geom_point(data=b[Text!=""], aes(x=Date, y=T_Vert, group=Runner), color="#F84601", size=5, alpha=.5) +
  geom_point(data=b[Text!=""], aes(x=Date, y=T_Vert, group=Runner), color="#F84601", size=2) +
  ## forecast!
  geom_line(data=bf, aes(x=Date, y=T_Vert, group=Runner), color="orange", size=2, alpha=.4) +
  ## labels
  #geom_text(hjust=-0.2) +
  #ggrepel::geom_text_repel() +
  ## forcast label
  ggrepel::geom_text_repel(data=bf, aes(x=Date, y=T_Vert, group=Runner, label=Text)) +
  ## misc formatting
  scale_y_continuous(labels=scales::comma) +
  labs(x="", y="") +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank())

## ig = 1080 x 1920 px
