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
userFollowers = fromJSON("https://api.github.com/users/cpower97")
userFollowers


#Beginning of visualisations

#Installation and creation of plotly account for data visualisation 
#install.packages("plotly")
library(plotly)
Sys.setenv("plotly_username"="cpower97")
Sys.setenv("plotly_api_key"="cvbGEJbplQFjKFArdobz")


#Creation of function to see language of repositories for given user

  getLanguages <- function(user)
  {

  #Extracting information about user's repositories and representing it as a data frame
  repoInfo  <- content(GET(paste0("https://api.github.com/users/",user,"/repos?per_page=100;"),gtoken))
  languageData <- data.frame()
  numOfRepos <- length(repoInfo)
  
  for(i in 1:numOfRepos)
  {
    repoLanguage <- repoInfo[[i]]$language
    repoName <- repoInfo[[i]]$name
    
    if(is.null(repoLanguage))
    {
      currentLanguageData <- data.frame(repo = repoName, language = "Unknown")
    }
    else
    {
      currentLanguageData <- data.frame(repo = repoName, language = repoLanguage)
    }
    languageData <- rbind(languageData, currentLanguageData)
  }
  
  return(languageData)
  
  }
  
#Function to visualise bar chart of languages used in repos of Github User

barChartOfLanguages <- function(user)
{
z <- getLanguages(user)
x <- data.frame(table(z$language))

p <- plot_ly(data=x, x = ~Var1, y = ~Freq, type = 'bar', name = 'Repo Languages') %>%
  layout(yaxis = list(title = 'Repository Count'), xaxis = list(title='Language'),
         title = paste("Languages Used in Repositories of Github User",user))

return(p)
}

#Visualisation of Languages used in repos of user mbostock
mbostockLanguages <- barChartOfLanguages("mbostock")
chart_link1 = api_create(mbostockLanguages, filename="mbostockLanguages")
#chart_link1

#Visualisation of Languages used in repos of user phadej
phadejLanguages <- barChartOfLanguages("phadej")
chart_link2 = api_create(phadejLanguages, filename="phadejLanguages")
#chart_link2

#Function to get the companies of a Github users followers

getCompanies <- function(user)
{
  followerInfo <- content(GET(paste0("https://api.github.com/users/",user,"/followers?per_page=100;"),gtoken))
  companyData <- data.frame()
  numOfUsers <- length(followerInfo)
  
  for(i in 1:numOfUsers)
  {
    userLogin <- followerInfo[[i]]$login
    userInfo <- content(GET(paste0("https://api.github.com/users/",userLogin),gtoken))
    
    if(is.null(userInfo$company))
    {
      next
    }
    else 
    {
      currentCompanyData <- data.frame(login = userLogin, company = userInfo$company )
      companyData <- rbind(companyData, currentCompanyData)
    }
    
  }
    
  return(companyData)
}
  

#c <- getCompanies("phadej")
#c

#Function to visualize the company's of the followers of a particular user

visualizeCompanies <- function(user)
{
  
  z <- getCompanies(user)
  x <- data.frame(table(z$company))
  
  p <- plot_ly(data =x, labels = ~Var1, values = ~Freq, type = 'pie') %>%
    layout(title = paste("Companies of", user,"'s Followers"),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(p)
}

#These graphs appear to show the companies of the people interested in a particular user's work
#i.e. the companies of the user's followers
mbostockFollowerCompanies <- visualizeCompanies("mbostock")
chart_link3 = api_create(mbostockFollowerCompanies, filename="mbostockFollowerCompanies")
chart_link3

phadejFollowerCompanies <- visualizeCompanies("phadej")
chart_link4 = api_create(phadejFollowerCompanies, filename="phadejFollowerCompanies")
chart_link4

BcRikkoFollowerCompanies <- visualizeCompanies("BcRikko")
chart_link5 = api_create(BcRikkoFollowerCompanies, filename="BcRikkoFollowerCompanies")
chart_link5

#Function to extract the number of followers and number of repositories of the followers of a particular user
getFollowersAndRepos <- function(user)
{
  followerInfo <- content(GET(paste0("https://api.github.com/users/",user,"/followers?per_page=100;"),gtoken))
  userData <- data.frame()
  numOfUsers <- length(followerInfo)
  
  for(i in 1:numOfUsers)
  {
    userLogin <- followerInfo[[i]]$login
    userInfo <- content(GET(paste0("https://api.github.com/users/",userLogin),gtoken))
    
    #If statement to remove outliers in the data or user with no followers or no repositories
    if((is.null(userInfo$followers)) || (is.null(userInfo$public_repos))
                                              || (userInfo$followers > 400 )|| (userInfo$public_repos > 200))
    {
      next
    }
    else 
    {
      currentUserData <- data.frame(followers = userInfo$followers, repos = userInfo$public_repos )
      userData <- rbind(userData, currentUserData)
    }
    
  }
  
  return(userData)
}

#Function to plot number of followers against number of repos
plotFollowersAgainstRepos <- function(user)
{
z <-getFollowersAndRepos(user)

p <- plot_ly(z,x =z$followers,y=z$repos)%>%
  layout(title = paste("No. of Followers vs No. of Repos for", user,"'s Followers"),
         xaxis = list(title="No. of Followers"),
         yaxis = list(title="No. of Repos"))

return(p)
}

#Plot of no. of followers vs. no. of repo for phadej's followers
phadejFollowersAgainstRepos <- plotFollowersAgainstRepos("phadej")
chart_link6 = api_create(phadejFollowersAgainstRepos , filename="phadejFollowersAgainstRepos ")
#chart_link6

#Plot of no. of followers vs. no. of repo for mbostock's followers
mbostockFollowersAgainstRepos <- plotFollowersAgainstRepos("mbostock")
chart_link7 = api_create(mbostockFollowersAgainstRepos , filename="mbostockFollowersAgainstRepos ")
#chart_link7

#Function to get the number of follower and number of people being followed by a user's followers
getFollowersAndFollowing <- function(user)
{
  followerInfo <- content(GET(paste0("https://api.github.com/users/",user,"/followers?per_page=100;"),gtoken))
  userData <- data.frame()
  numOfUsers <- length(followerInfo)
  
  for(i in 1:numOfUsers)
  {
    userLogin <- followerInfo[[i]]$login
    userInfo <- content(GET(paste0("https://api.github.com/users/",userLogin),gtoken))
    
    #If statement to remove outliers in the data
    if((is.null(userInfo$followers)) || (is.null(userInfo$public_repos)) || (userInfo$following > 800) ||
       (userInfo$followers > 800))
    {
      next
    }
    else 
    {
      currentUserData <- data.frame(followers = userInfo$followers, repos = userInfo$following )
      userData <- rbind(userData, currentUserData)
    }
    
  }
  return(userData)
}

#Function to plot followers against following
plotFollowersAgainstFollowing <- function(user)
{
  z <-getFollowersAndFollowing(user)
  
  p <- plot_ly(z,x =z$followers,y=z$following)%>%
    layout(title = paste("Followers vs Following for", user,"'s Followers"),
           xaxis = list(title="Followers"),
           yaxis = list(title="Following"))
  
  return(p)
}

#Plot followers against following for phadej's followers
phadejFollowersAgainstFollowing <- plotFollowersAgainstFollowing("phadej")
chart_link8 = api_create(phadejFollowersAgainstFollowing , filename="phadejFollowersAgainstFollowing")
chart_link8

#Plot followers against following for mbostock's followers
mbostockFollowersAgainstFollowing <- plotFollowersAgainstFollowing("mbostock")
chart_link9 = api_create(mbostockFollowersAgainstFollowing , filename="mbostockFollowersAgainstFollowing")
chart_link9
