## ------------------------------------------------------------------------
download.file(url = "http://www.gks.ru/free_doc/2017/demo/t3_3.xls",
              destfile = "data/deaths-in-russia-2017.xls", mode = "wb")

## install.packages("readxl")

library(readxl)
read_excel

df <- read_excel("2018.4.06/data/deaths-in-russia-2017.xls",
                 col_names = F)

df <- df[-c(8, 10:12, 14, 15, 18, 22:24, 30, 39), c(1,2)] 

names(df) <- c("cause","number")
df <- df[-c(1:7),]
df$cause <- gsub("из них от|в том числе от|1)|\\:", "",
                 df$cause)
df$number <- as.numeric(df$number)
order(df$number,decreasing = T)
df <- df[order(df$number,decreasing = T),]
df

library(ggplot2)

p <- ggplot(df, aes(y = number, x = reorder(cause,number))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Статистика смертности россиян в 2017.\nСмерть наступила от...")+
  xlab("") +
  ylim(0,600000)+
  ylab("Число умерших") +
  theme_minimal() +
  geom_text(aes(label=paste(substr(number, 0, nchar(number)-3), "тысяч")), hjust = -0.2)

p

ggsave("2018.4.06/results/deaths-in-russia-2017.png", p,
       width = 30, height = 10, units = "cm", dpi = 900)



library(ggplot2)
p <- ggplot(data=df,
       aes(x=reorder(cause,number),y=number)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Причины смерти россиян в 2017 году.\nСмерть наступила от...") +
  xlab("") +
  ylab("Число умерших") +
  theme_minimal()

p

ggsave("results/deaths-in-russia-2017.png",
       width = 10, height = 5, dpi = 400)

