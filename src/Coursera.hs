
module Coursera where

import Network.Wreq

test = get "https://api.coursera.org/api/catalog.v1/courses"
