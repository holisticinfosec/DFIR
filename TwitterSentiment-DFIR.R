# Twitter Sentiment example for DFIR Redefined: Deeper Functionality for Investigators with R
# Derived from Michael Levy's Playing With Twitter Data
# http://michaellevy.name/blog/conference-twitter/

lapply(c('twitteR', 'dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm', 'stringr'),
       library, character.only = TRUE)

# You'll need to get your own Twitter API keys
# See http://rtweet.info/index.html and https://apps.twitter.com to do so

setup_twitter_oauth("YourKeyHere", "YourKeyHere",
                    access_token="YourKeyHere", 
                    access_secret="YourKeyHere")

# Results change over time so you'll want to set you own handles, hashtags and date to experiment
# You can opt to read in the example data proved (DDOS.csv) as follows:
# Comment out d = twListToDF(tw)
# Uncomment # d = read.csv('C:/coding/R/data/TwitterSentiment/DDOS.csv') and use the path where you stored it 
tw = searchTwitter('#DDOS AND #Android AND #WireX', n = 1000, since = '2017-08-27')
d = twListToDF(tw)
# d = read.csv('C:/coding/R/data/TwitterSentiment/DDOS.csv')

# Option: If desired, set this path to where you'd like your 
# data written in CSV format, and uncomment next line. Not required, but useful for analysis.
#write.csv(d, 'C:/coding/R/data/TwitterSentiment/DDOS.csv')

d$created = with_tz(d$created, 'America/Los_Angeles')

timeDist = ggplot(d, aes(created)) + 
  geom_density(aes(fill = isRetweet), alpha = .5) +
  scale_fill_discrete(guide = 'none') +
  xlab('All tweets')

# Zoom in on day of month
dayOf = filter(d, mday(created) == 29)
timeDistDayOf = ggplot(dayOf, aes(created)) + 
  geom_density(aes(fill = isRetweet), adjust = .25, alpha = .5) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
  xlab('Day-of tweets')
cowplot::plot_grid(timeDist, timeDistDayOf)

#Number of tweets posted by platform

par(mar = c(3, 3, 3, 2))
d$statusSource = substr(d$statusSource, 
                        regexpr('>', d$statusSource) + 1, 
                        regexpr('</a>', d$statusSource) - 1)
dotchart(sort(table(d$statusSource)))
mtext('Number of tweets posted by platform')

# Restart R session first
#cto debug polarity issue with new session minus the twitteR package
lapply(c('dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm', 'stringr'),
       library, character.only = TRUE)

# Split into retweets and original tweets
sp = split(d, d$isRetweet)
orig = sp[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))
# Fix for UTF Encoding failures
orig$text = iconv(orig$text, "latin1", "ASCII", sub="")
pol = 
  lapply(orig$text, function(txt) {
    # strip sentence enders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
orig$emotionalValence = sapply(pol, function(x) x$all$polarity)
# As reality check, what are the most and least positive tweets
orig$text[which.max(orig$emotionalValence)]
orig$text[which.min(orig$emotionalValence)]

# How does emotionalValence change over the day?
filter(orig, mday(created) == 29) %>%
  ggplot(aes(created, emotionalValence)) +
  geom_point() + 
  geom_smooth(span = .5)

# Do happier tweets get retweeted more?
ggplot(orig, aes(x = emotionalValence, y = retweetCount)) +
  geom_point(position = 'jitter') +
  geom_smooth()

# Emotional Content

polWordTables = 
  sapply(pol, function(p) {
    words = c(positiveWords = paste(p[[1]]$pos.words[[1]], collapse = ' '), 
              negativeWords = paste(p[[1]]$neg.words[[1]], collapse = ' '))
    gsub('-', '', words)  # Get rid of nothing found's "-"
  }) %>%
  apply(1, paste, collapse = ' ') %>% 
  stripWhitespace() %>% 
  strsplit(' ') %>%
  sapply(table)

par(mfrow = c(1, 2))
invisible(
  lapply(1:2, function(i) {
    dotchart(sort(polWordTables[[i]]), cex = .8)
    mtext(names(polWordTables)[i])
  }))

# Emotionally associated non-emotional words
polSplit = split(orig, sign(orig$emotionalValence))
polText = sapply(polSplit, function(df) {
  paste(tolower(df$text), collapse = ' ') %>%
    gsub(' (http|@)[^[:blank:]]+', '', .) %>%
    gsub('[[:punct:]]', '', .)
}) %>%
  structure(names = c('negative', 'neutral', 'positive'))

# remove emotive words
polText['negative'] = removeWords(polText['negative'], names(polWordTables$negativeWords))
polText['positive'] = removeWords(polText['positive'], names(polWordTables$positiveWords))

# Make a corpus by valence and a wordcloud from it
corp = Corpus(VectorSource(polText))
col3 = RColorBrewer::brewer.pal(3, 'Paired') # Define some pretty colors, mostly for later
wordcloud::comparison.cloud(as.matrix(TermDocumentMatrix(corp)), 
                            max.words = 100, min.freq = 2, random.order=FALSE, 
                            rot.per = 0, colors = col3, vfont = c("sans serif", "plain"))

# Who's retweeting whom?
el = as.data.frame(cbind(sender = tolower(rt$sender), 
                         receiver = tolower(rt$screenName)))
el = count(el, sender, receiver) 
rtnet = network(el, matrix.type = 'edgelist', directed = TRUE, 
                ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs = rtnet %v% 'vertex.names'
vlabs[degree(rtnet, cmode = 'outdegree') == 0] = NA

par(mar = c(0, 0, 3, 0))
plot(rtnet, label = vlabs, label.pos = 5, label.cex = .8, 
     vertex.cex = log(degree(rtnet)) + .5, vertex.col = col3[1],
     edge.lwd = 'num', edge.col = 'gray70', main = '#WireX #Android #DDoS Retweet Network')

# Who's mentioning whom?

# Extract who is mentioned in each tweet. 
# Someone has probably written a function to do this, but it's a fun regex problem.
mentioned = 
  lapply(orig$text, function(tx) {
    matches = gregexpr('@[^([:blank:]|[:punct:])]+', tx)[[1]]
    sapply(seq_along(matches), function(i) 
      substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
  })
# Make an edge from the tweeter to the mentioned, for each mention
mentionEL = 
  lapply(seq_along(orig$text), function(i) {
    # If the tweet didn't have a mention, don't make edges
    if(mentioned[[i]] == '')  
      return(NULL)
    # Otherwise, loop over each person mentioned, make an edge, and rbind them
    lapply(mentioned[[i]], function(m)
      c(sender = orig$screenName[i], receiver = m)) %>%
      do.call(rbind, .) %>% as.data.frame()
  }) %>% 
  do.call(rbind, .) %>%
  count(tolower(sender), tolower(receiver))
# Make the network
mentionNet = network(mentionEL, matrix.type = 'edgelist', directed = TRUE, 
                     ignore.eval = FALSE, names.eval = 'num')
# Color mention entities
vCol = rep(col3[3], network.size(mentionNet))
tweeters = c('joe4security', 'HackRead', 'defendmalware', 'AI__TECH', 
             'Robert_Straus_', 'ChiNetworks', 'MobilePotpurri', 'dp2web', 
             'UnequalG', 'ComputerWeekly', 'infosecsw')
vCol[(mentionNet %v% 'vertex.names') %in% tweeters] = col3[1]
vCol[mentionNet %v% 'vertex.names' == 'hackread'] = col3[2]
plot(mentionNet, displaylabels = TRUE, label.pos = 5, label.cex = .8, 
     vertex.cex = degree(mentionNet, cmode = 'indegree'), vertex.col = vCol,
     edge.lwd = 'num', edge.col = 'gray70', main = '#WireX #Android #DDoS Mention Network')
#legend(x = 'bottomleft', legend = c('Speaker', 'Host', 'Other'), 
#       pt.bg = col3, pch = 21, pt.cex = 1.5, bty = 'n')

#GeoLocate

#Lat Long for Milan, IT (@HackRead HQ)
tweets <- searchTwitter("@joe4security OR @HackRead OR @defendmalware OR @AI__TECH OR @Robert_Straus_ OR @ChiNetworks OR @MobilePotpurri OR @dp2web OR @UnequalG OR @ComputerWeekly OR @infosecsw", 
                        n=100, lang="en", geocode='45.4642,9.1900,5mi', since="2017-08-27")
tweets_geolocated.df <- twListToDF(tweets)
write.csv(tweets_geolocated.df, "tweets_geolocated.csv")