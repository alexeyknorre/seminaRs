library(ggplot2)
library(dplyr)
library(rvest)
library(stringdist)

read_data <- function(year){
  path <- paste0("2018.3.30/missing-people-in-russia/data/",year,".csv")
  df <- read.csv(path,
                 encoding = "UTF-8", stringsAsFactors = F)
  # Remove first row and second column
  df <- df[-1,c(1,3,4)]
  # Rename columns
  names(df) <- c("region", "variable", "value")
  # Leave only selected observations
  df <- df[df$variable == "Всего разыскивалось лиц, в том числе лиц, пропавших без вести" |
             df$variable == "Установлено лиц из числа находившихся в розыске, в том числе лиц, пропавших без вести",]
  # Convert to numeric
  df$value <- as.numeric(df$value)
  # Long to wide format
  df <- reshape(df, idvar = "region", timevar = "variable", direction = "wide")
  #
  df$lost <- df$`value.Всего разыскивалось лиц, в том числе лиц, пропавших без вести` - df$`value.Установлено лиц из числа находившихся в розыске, в том числе лиц, пропавших без вести`
  df <- df[,c(1,4)]
  df$year <- year
  return(df)
}

# Execute function and bind all dataframes together
df <- data.frame(matrix(nrow = 0, ncol = 3))
df <- data.table::rbindlist(list(read_data("2017"),
                                 read_data("2016"),
                                 read_data("2015"),
                                 read_data("2014")))

# Clear observations
df <- df[!grepl("округ|Всего|ФО|с а/о|с АО|ГУВД", df$region),]

### Trend

df_years <- df %>% 
  group_by(year) %>% 
  summarise(sum = sum(lost))

ggplot(df_years, aes(year, sum)) +
  geom_bar(stat="identity")

### Get population data
wiki_url <- "https://ru.wikipedia.org/wiki/%D0%A1%D1%83%D0%B1%D1%8A%D0%B5%D0%BA%D1%82%D1%8B_%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B9%D1%81%D0%BA%D0%BE%D0%B9_%D0%A4%D0%B5%D0%B4%D0%B5%D1%80%D0%B0%D1%86%D0%B8%D0%B8"
wiki_xpath <- '//*[@id="mw-content-text"]/div/table[4]'
wiki_table <- wiki_url %>% read_html() %>% html_node(xpath = wiki_xpath) %>% html_table()
wiki_table <- wiki_table[,c(2,5,6)]
names(wiki_table) <- c("region", "area", "population")
wiki_table <- wiki_table[complete.cases(wiki_table),]
# Делаем нормальный столбец с населением
wiki_table$population <- as.numeric(gsub("[^0-9\\.]", "", wiki_table$population))
# Удаляем строку с Российской Федерацией
wiki_table <- wiki_table[!grepl("Российская Федерация", wiki_table$region),]
wiki_table$region <- gsub("\\[|\\]|[0-9]", "", wiki_table$region)

# Merge
df$region <- gsub("Республика", "", df$region)
df$wiki_region_match <- NA

for (i in 1:nrow(df)){
  dists <- stringdist(df$region[i], wiki_table$region)
  names(dists) <- wiki_table$region
  df$wiki_region_match[i] <- names(dists[order(dists)][1])
}

df <- merge(df, wiki_table, by.x = "wiki_region_match", by.y = "region")
df <- subset(df, select = -c(wiki_region_match) )
df <- subset(df, year == 2017)

df$lost_rate <- df$lost / df$population * 100000
