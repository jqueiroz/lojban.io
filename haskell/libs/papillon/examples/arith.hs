{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon
import Data.Char
import System.Environment

main :: IO ()
main = do
	arg : _ <- getArgs
	case runError $ expr $ parse arg of
		Right (r, _) -> print r
		Left _ -> putStrLn "parse error"

[papillon|

op1 :: Int -> Int -> Int
	= '*'			{ (*) }
	/ '/'			{ div }
	/ '%'			{ mod }
;
op2 :: Int -> Int -> Int
	= '+'			{ (+) }
	/ '-'			{ (-) }
;
factor :: Int
	= ds:<isDigit>+		{ read ds }
	/ '(' e:expr ')'	{ e }
;
term :: Int
	= f0:factor fs:(op:op1 f:factor { (`op` f) })*
				{ foldl (flip ($)) f0 fs }
;
expr :: Int
	= t0:term ts:(op:op2 t:term { (`op` t) })*
				{ foldl (flip ($)) t0 ts }
;

|]
