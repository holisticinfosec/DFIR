# Have I Been Pwned example for DFIR Redefined: Deeper Functionality for Investigators with R
# HINPwned created by Steph Locke
# https://github.com/stephlocke/HIBPwned
# https://holisticinfosec.blogspot.com/2016/07/toolsmith-release-advisory-steph-lockes.html

library("HIBPwned")

# Has Russ been pwned?
account_breaches(c("rmcree@yahoo.com","holisticinfosec@gmail.com","russ@holisticinfosec.org"))
breached_site("LinkedIn")
pastes("russ@holisticinfosec.org")

# Has Doug been pwned?
account_breaches("doug.burks@gmail.com")
breached_site("OnlinerSpambot")

# Has Mark been pwned?
account_breaches("mbaggett@sans.org")