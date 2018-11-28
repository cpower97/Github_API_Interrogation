#Method sourced from https://gist.github.com/mGalarnyk/533ccfd1bb46680925448774bcb71647#file-r_github_api-r

#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
#install.packages(xml2)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "API_Interrogation",
                   key = "640421140be54901944a",
                   secret = "d0cb00d937e806396903d9c6ac980bacb846a61e")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
req

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 


userData = fromJSON("https://api.github.com/users/cpower97")
userData
userData$login
userData$followers
userData$public_repos
data = toJSON(userData,pretty = T)
data

#Retrieving info about my followers
userFollowers = fromJSON("https://api.github.com/users/cpower97/followers")
userFollowers


#Beginning of visualisations

#Installation and creation of plotly account for data visualisation 
#install.packages("plotly")
library(plotly)
Sys.setenv("plotly_username"="cpower97")
Sys.setenv("plotly_api_key"="cvbGEJbplQFjKFArdobz")

#Extracting information about user's repositories and representing it as a data frame
userRepos <- content(GET("https://api.github.com/users/",mbostock,"/repos?per_page=100;",gtoken))

#Creation of function to see language of repositories for given user

getLanguages() <- function(user)
  {

  repoInfo <- content(reposDF)
  languageData <- data.frame()
  numOfRepos <- length(repoInfo)
  
  for(i in 1:numOfRepos)
  {
    repoLanguage <- repoInfo[i]$language
    repoName <- repoInfo[i]$name
    
    if(repoLanguage == null)
    {
      currentLanguageData <- data.fram(repo = repoName, language = "N/A")
    }
    else
    {
      currentLanguageData <- data.fram(repo = repoName, language = repoLanguage)
    }
    
    languageData <- rbind(languageData, currentLanguageData)
  }
  
}

#Interrogating and visualisng data based on user mbostock


































