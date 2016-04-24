{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon
import System.Environment
import Data.Char

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- readFile fp
	case runError $ conf $ parse cnt of
		Right (r, _) -> print r
		Left _ -> putStrLn "parse error"

[papillon|

name :: String
	= s:<isLower>+			{ s }
;
value :: Int
	= ds:<isDigit>+			{ read ds }
;
conf1 :: (String, Int)
	= n:name ':' ' ' v:value '\n'	{ (n, v) }
;
conf :: [(String, Int)]
	= cs:conf1+ !_:[True]		{ cs }
;

|]
