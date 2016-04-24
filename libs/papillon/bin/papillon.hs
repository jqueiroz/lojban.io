import Text.Papillon.Core
import System.Environment
import System.Directory
import System.FilePath
import Data.List
import Language.Haskell.TH

import Class

addModules :: String
addModules =
	"import \"monads-tf\" Control.Monad.State\n" ++
--	"import \"monads-tf\" Control.Monad.Identity\n" ++
	"import \"monads-tf\" Control.Monad.Error\n"

papillonStr :: String -> IO (String, String, String)
papillonStr src = do
	(prgm, mn, ppp, pp, decsQ, atp) <- runQ $ papillonFile src
	let	mName = intercalate "." $ myInit mn ++ ["Papillon"]
		importConst = "\nimport " ++ mName ++ "\n"
		dir = joinPath $ myInit mn
	decs <- runQ decsQ
	return (dir, mName,
		unlines (map showPragma $ addPragmas $ delPragmas prgm) ++
		(if null mn then "" else "module " ++ intercalate "." mn) ++
		showExportList ppp ++ (if null mn then "" else "where\n") ++
		importConst ++ addModules ++
		pp ++ "\n" ++ show (ppr decs) ++ "\n" ++ atp ++ "\n")

showExportList :: Maybe Exports -> String
showExportList (Just el) = " (\n\t" ++ el ++ "\n) "
showExportList Nothing = " "

showPragma :: PPragma -> String
showPragma (LanguagePragma []) = ""
showPragma (LanguagePragma p) = "{-# LANGUAGE " ++ intercalate ", " p ++ " #-}"
showPragma (OtherPragma p) = "{-# " ++ p ++ " #-}"

addPragmas :: [PPragma] -> [PPragma]
addPragmas [] = [LanguagePragma additionalPragmas]
addPragmas (LanguagePragma p : ps) = LanguagePragma (p ++ additionalPragmas) : ps
addPragmas (op : ps) = op : addPragmas ps

delPragmas :: [PPragma] -> [PPragma]
delPragmas [] = []
delPragmas (LanguagePragma p : ps) =
	LanguagePragma (filter (`notElem` ["QuasiQuotes", "TypeFamilies"]) p) : ps
delPragmas (op : ps) = op : delPragmas ps

additionalPragmas :: [String]
additionalPragmas = [
	"PackageImports",
	"TypeFamilies",
	"RankNTypes"
 ]

papillonConstant :: String -> IO String
papillonConstant mName = do
	src <- runQ $ do
		pe <- parseErrorT False
		iepe <- instanceErrorParseError False
		mkpet <- mkParseErrorTHT
		mkpe <- mkParseErrorTH
		pepst <- pePositionST
		pepsd <- pePositionSD
		cls <- classSourceQ False
		ret <- runErrorTHT False
		re <- runErrorTH False
		return $ [pe, iepe, mkpet, mkpe, pepst, pepsd] ++ cls ++ [ret, re]
	return $
		"{-# LANGUAGE RankNTypes, TypeFamilies, PackageImports #-}\n" ++
		"module " ++ mName ++ " (\n\t" ++
		intercalate ",\n\t" exportList ++ ") where\n" ++
		"import Control.Monad.Trans.Error (Error(..))\n" ++
		"import \"monads-tf\" Control.Monad.Error\n" ++
		"import \"monads-tf\" Control.Monad.Identity\n" ++
		show (ppr src) ++ "\n"

main :: IO ()
main = do
	args <- getArgs
	case args of
		[fn, dist] -> do
			(d, mName, src) <- papillonStr =<< readFile fn
			let dir = dist </> d
			createDirectoryIfMissing True dir
			writeFile (dir </> takeBaseName fn <.> "hs") src
			writeFile (dir </> "Papillon" <.> "hs")
				=<< papillonConstant mName
		_ -> error "bad arguments"

exportList :: [String]
exportList = [
	"ParseError",
	"mkParseError",
	"peCode",
	"peMessage",
	"peComment",
	"peDerivs",
	"peReading",
	"pePosition",
	"pePositionS",
	"Pos(..)",
	"Source(..)",
	"SourceList(..)",
	"ListPos(..)",
	"runError"
 ]

myInit :: [a] -> [a]
myInit [] = []
myInit [_] = []
myInit (x : xs) = x : myInit xs
