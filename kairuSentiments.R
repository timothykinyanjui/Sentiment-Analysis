# In this work, I will analyse the sentiment data left from wootric surveys

# Clear the workspace
rm(list = ls())

# Load the required packages
require("magrittr")
require("tidyverse")
require("tidytext")
require("curl")
require("tm")
require("getPass")
require("jsonlite")

################ Fetch data - Wootric API ################

# Log in to the wootric account using API connection and get log-in credentials

# Request password
userName <- readline(prompt="Enter username:")
passcode <- getPass(msg = "Enter password:")

# Create an empty handle and populate the form
h <- new_handle()
handle_setform(h, .list = list(grant_type = "password",username=userName,password=passcode))

# Remove credentials
rm(passcode)
rm(userName)

# Download the connection details and store in workspace - alternatively in a file
connection <- curl(url = "https://api.wootric.com/oauth/token",handle = h)
connectData <- readLines(connection, warn = FALSE)

# Extract the connection token - Required to download the data
pattern1 <- "access_token"
pattern2 <- "token_type"
pos1 <- regexpr(pattern1, connectData)
pos2 <- regexpr(pattern2, connectData)
keep <- substr(connectData, pos1[1]+nchar(pattern1), pos2[1]-1)

# Remove punctuation to get access token
my_access_token <- removePunctuation(keep)

# Access the data in the three pages 50 records per page - Can do a loop here
connectURL1 = paste("https://api.wootric.com/v1/responses?access_token=",my_access_token,"&page=1&per_page=50",sep = " ")
connectURL2 = paste("https://api.wootric.com/v1/responses?access_token=",my_access_token,"&page=2&per_page=50",sep = " ")
connectURL3 = paste("https://api.wootric.com/v1/responses?access_token=",my_access_token,"&page=3&per_page=50",sep = " ")

# Remove the white space
connectURL1 <- gsub(" ","",connectURL1, fixed = TRUE)
connectURL2 <- gsub(" ","",connectURL2, fixed = TRUE)
connectURL3 <- gsub(" ","",connectURL3, fixed = TRUE)


# Connect and get the data and save in a json file
curl_download(url = connectURL1, destfile = "responsesData1.json")
curl_download(url = connectURL2, destfile = "responsesData2.json")
curl_download(url = connectURL3, destfile = "responsesData3.json")

# Close the connection after reading in the data
close(connection)


################ Send email survey ################
#h2 = new_handle()
#h2 <- handle_setform(h2, .list = list(access_token=my_access_token,emails="beth@kairuhairhub.co.uk",survey_immediately="true",survey_settings="Thank you!",subject="Would you mind telling us how we are performing?",intro="We are constantly looking for ways to improve our service. Your opinion matters to us"))
#curl(url = "https://api.wootric.com/v1/email_survey", handle = h2, open = "r")
#-d "access_token=access_token"
#-d "emails[]=timothykinyanjui@gmail.com"
#-d "survey_immediately=true"
#-d "survey_settings[custom_messages][followup_text]=Thank you!"
#-d "subject=Would you mind telling us how we are performing?"
#-d "intro=We are constantly looking for ways to improve our service. Your opinion matters to us"

################ Read in json file and make dataframe/tibble ################
dataUn1 <- stream_in(file("responsesData1.json"))
dataUn2 <- stream_in(file("responsesData2.json"))
dataUn3 <- stream_in(file("responsesData3.json"))

# Flatten the nested data frames and save as tibble
dataflat <- as_data_frame(rbind(flatten(dataUn1),flatten(dataUn2),flatten(dataUn3)))


################ Analysis starts here ################
data_clean <- dataflat %>%
  dplyr::select(score,text)%>%
  na.omit() %>%
  unique()

# Tokenize so that each row is a word
data_unnest <- data_clean %>% tidytext::unnest_tokens(word,text)

# Load the stop words
data("stop_words")

# Remove the stop words
data_tidy <- data_unnest %>% dplyr::anti_join(stop_words, by = "word")

# Count the words and plot (atleast those used more than 3 times)
data_tidy %>%
  dplyr::count(word,sort = T) %>%
  dplyr::filter(n >= 3) %>%
  dplyr::mutate(word = reorder(word,n)) %>%
  ggplot2::ggplot(ggplot2::aes(word,n)) +
  ggplot2::geom_col() +
  ggplot2::xlab("Words") +
  ggplot2::ylab("Frequency") +
  ggplot2::coord_flip()

# Create a word cloud
qdap::trans_cloud(text.var = data_tidy$word, title.names = c("Trending words"), stem = FALSE, min.freq = 2)

# Tidy up and remove sensitive customer data
file.remove(c("responsesData1.json","responsesData2.json","responsesData3.json"))
