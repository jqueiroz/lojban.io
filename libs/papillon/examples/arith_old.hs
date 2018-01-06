{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Prelude hiding (product, sum)
import Text.Papillon
import Data.Char
import System.Environment

main :: IO ()
main = do
	arg : _ <- getArgs
	case expr $ parse arg of
		Right (r, _) -> print r
		Left _ -> putStrLn "parse error"

[papillon|

value :: Int
	= ds:(d:[isDigit d] { d })+	{ read ds }
	/ '(' e:expr ')'		{ e }
;
product :: Int
	= v0:value ops:(op:('*' { (*) } / '/' { div }) v:value { (`op` v) })*
					{ foldl (flip ($)) v0 ops }
;
expr :: Int
	= p0:product ops:(op:('+' { (+) } / '-' { (-) }) p:product { (`op` p) })*
					{ foldl (flip ($)) p0 ops }
;

|]
