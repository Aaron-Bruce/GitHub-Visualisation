{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields     #-}


module GitHub where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

type Username = Text
type UserAgent = Text
type Reponame  = Text

data GitHubUser =
  GitHubUser { login :: Text
             , name  :: Text
             , email :: Maybe Text
             } deriving (Generic, FromJSON, Show)
            
data GitHubRepo =
  GitHubRepo { name :: Text
             , fullname :: Maybe Text
             , language :: Maybe Text
             } deriving (Generic, FromJSON, Show)

data RepoCommit =
  RepoCommit { sha :: Text
             } deriving (Generic, FromJSON, Show)

type GitHubAPI = "users" :> Header "user-agent" UserAgent
                         :> Capture "username" Username  :> Get '[JSON] GitHubUser
            :<|> "users" :> Header "user-agent" UserAgent
                         :> Capture "username" Username  :> "repos" :>  Get '[JSON] [GitHubRepo]
            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> Capture "username" Username
                         :> Capture "repo"     Reponame  :> "commits" :>  Get '[JSON] [RepoCommit]

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

getUser :: Maybe UserAgent -> Username -> ClientM GitHubUser
getUserRepos :: Maybe UserAgent -> Username -> ClientM [GitHubRepo]
getRepoCommits :: Maybe UserAgent -> Username -> Reponame -> ClientM [RepoCommit]

getUser :<|> getUserRepos :<|> getRepoCommits = client gitHubAPI