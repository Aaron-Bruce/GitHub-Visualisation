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
type Ref = Text
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
             , message :: Maybe Text
             , url :: Text
             } deriving (Generic, FromJSON, Show)

data CommitInfo =
  CommitInfo { total :: Integer
             } deriving (Generic, FromJSON, Show)

type GitHubAPI = "users" :> Header "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username  :> Get '[JSON] GitHubUser
            :<|> "users" :> Header "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username  :> "repos" :>  Get '[JSON] [GitHubRepo]
            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username
                         :> Capture "repo"     Reponame  :> "commits" :>  Get '[JSON] [RepoCommit]
            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username
                         :> Capture "repo"     Reponame  :> "commits" :>  Capture "ref" Ref
                         :> Get '[JSON] [CommitInfo]

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

getUser :: Maybe UserAgent -> BasicAuthData -> Username -> ClientM GitHubUser
getUserRepos :: Maybe UserAgent -> BasicAuthData -> Username -> ClientM [GitHubRepo]
getRepoCommits :: Maybe UserAgent -> BasicAuthData -> Username -> Reponame -> ClientM [RepoCommit]
getCommitInfo :: Maybe UserAgent -> BasicAuthData -> Username -> Reponame -> Ref -> ClientM [CommitInfo]

getUser :<|> getUserRepos :<|> getRepoCommits :<|> getCommitInfo = client gitHubAPI