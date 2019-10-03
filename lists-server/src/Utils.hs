module Utils (
  getEnvironment
) where

getEnvironment [] = "dev"
getEnvironment (h: args) = h
