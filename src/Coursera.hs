{-# LANGUAGE
    DeriveGeneric
    , OverloadedStrings
    #-}

module Coursera where

import Control.Lens.Operators
import Data.Aeson
import Data.Aeson.Lens
import Data.Text
import GHC.Generics
import Network.Wreq


data Courses = Courses
    { elements :: [Course]
    } deriving (Eq, Generic, Show)


data Course = Course
    { id  :: Int
    , shortName :: Text
    , name      :: Text
    , links     :: Object -- FIXME!!!
    } deriving (Eq, Generic, Show)

instance FromJSON Courses
instance FromJSON Course


-- | List all courses
courses :: IO (Maybe Courses)
courses = do
    r <- asJSON =<< get "https://api.coursera.org/api/catalog.v1/courses" :: IO (Response Courses)
    return $ r ^? responseBody
