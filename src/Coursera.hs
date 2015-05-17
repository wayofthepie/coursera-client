{-# LANGUAGE
    DeriveGeneric
    , OverloadedStrings
    #-}

module Coursera where

import Control.Lens.Operators
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask, runReaderT, ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import GHC.Generics
import Network.Wreq

type CourseraClient a = ReaderT CourseraClientConfig IO a

data CourseraClientConfig = CourseraClientConfig
    { apiBase :: T.Text
    } deriving (Eq, Show)

data Courses = Courses
    { elements :: [Course]
    } deriving (Eq, Generic, Show)


data Course = Course
    { id  :: Int
    , shortName :: T.Text
    , name      :: T.Text
    , links     :: Object -- FIXME!!!
    } deriving (Eq, Generic, Show)

instance FromJSON Courses
instance FromJSON Course

runCourseraClient ::MonadIO m =>  CourseraClientConfig -> CourseraClient a -> m a
runCourseraClient config action =
    liftIO $ runReaderT action config

-- | List all courses
courses :: CourseraClient (Maybe Courses)
courses = do
    cfg <- ask
    resp<- getCoursesJson $ apiBase cfg
    return $ resp ^? responseBody
  where
    getCoursesJson :: T.Text -> CourseraClient (Response Courses)
    getCoursesJson baseUrl = lift $
        asJSON =<< get (T.unpack baseUrl ++ "/catalog.v1/courses")
