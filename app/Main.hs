module Main (main) where

import Server.Main (runServer)
import Options.Applicative
import Data.Semigroup ((<>))

newtype Options = Options
    { port :: Int
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> option auto (long "port" <> short 'p' <> help "Port number" <> value 8000 <> metavar "INT")

main = execParser opts >>= run where
    opts = info (optionsParser <**> helper) (fullDesc <> progDesc "Runs the webserver on the specified port." <> header "Lojban teaching platform")

run :: Options -> IO ()
run options = runServer (port options)
