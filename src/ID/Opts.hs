{-# LANGUAGE OverloadedStrings #-}


module ID.Opts
    ( IDOpts(..)
    , options
    , execParser
    ) where


import           Options.Applicative


data IDOpts = IDOpts
            { optConfig :: FilePath
            } deriving (Show)


idOpts :: Parser IDOpts
idOpts =   IDOpts
       <$> strOption (  short 'c' <> long "config" <> metavar "CONFIG_FILE"
                     <> help "This is the location of the configuration file for ID options.")

options :: ParserInfo IDOpts
options = info (helper <*> idOpts)
               (  fullDesc
               <> progDesc "Evolves web pages."
               <> header "intelligent-design: designing web pages through evolution.")

