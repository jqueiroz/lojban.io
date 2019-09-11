{-# LANGUAGE RankNTypes, TypeFamilies, PackageImports #-}
module Text.Papillon.Papillon (
	ParseError,
	mkParseError,
	peCode,
	peMessage,
	peComment,
	peDerivs,
	peReading,
	pePosition,
	pePositionS,
	Pos(..),
	Source(..),
	SourceList(..),
	ListPos(..),
	runError) where
import Control.Monad.Trans.Error (Error(..))
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.Identity

import qualified Data.ByteString.Char8 as BSC

data ParseError pos drv
    = ParseError {peCode :: String,
                  peMessage :: String,
                  peComment :: String,
                  peDerivs :: drv,
                  peReading :: ([String]),
                  pePosition :: pos}
instance Error (ParseError pos drv)
    where strMsg msg = ParseError "" msg "" undefined undefined undefined
mkParseError :: forall pos drv . String ->
                                 String -> String -> drv -> [String] -> pos -> ParseError pos drv
mkParseError = ParseError
pePositionS :: forall drv . ParseError (Pos String) drv ->
                            (Int, Int)
pePositionS (ParseError {pePosition = ListPos (CharPos p)}) = p
class Source sl
    where type Token sl
          data Pos sl
          getToken :: sl -> Maybe ((Token sl, sl))
          initialPos :: Pos sl
          updatePos :: Token sl -> Pos sl -> Pos sl
class SourceList c
    where data ListPos c
          listToken :: [c] -> Maybe ((c, [c]))
          listInitialPos :: ListPos c
          listUpdatePos :: c -> ListPos c -> ListPos c
instance SourceList c => Source ([c])
    where type Token ([c]) = c
          newtype Pos ([c]) = ListPos (ListPos c)
          getToken = listToken
          initialPos = ListPos listInitialPos
          updatePos c (ListPos p) = ListPos (listUpdatePos c p)
instance SourceList Char
    where newtype ListPos Char = CharPos ((Int, Int)) deriving (Show)
          listToken (c : s) = Just (c, s)
          listToken _ = Nothing
          listInitialPos = CharPos (1, 1)
          listUpdatePos '\n' (CharPos (y, _)) = CharPos (y + 1, 1)
          listUpdatePos '\t' (CharPos (y, x)) = CharPos (y,
                                                         ((+ 1) . (* 8) . (+ 1) . (`div` 8) . subtract 1) x)
          listUpdatePos _ (CharPos (y, x)) = CharPos (y, x + 1)
runError :: forall err a . ErrorT err Identity a -> Either err a
runError = runIdentity . runErrorT

instance Source BSC.ByteString where
	type Token BSC.ByteString = Char
	data Pos BSC.ByteString = NoPos
	getToken = BSC.uncons
	initialPos = NoPos
	updatePos _ _ = NoPos
