module Text.Papillon (
	papillon,
	Source(..),
	SourceList(..),
	Pos(..),
	ListPos(..),

	-- * For parse error message
	ParseError,
	mkParseError,
	peDerivs,
	peReading,
	peMessage,
	peCode,
	peComment,
	pePosition,
	pePositionS,
	(<*>),
	(<$>),
	runError
) where

import Text.Papillon.Core
import Language.Haskell.TH.Quote

import qualified Data.ByteString.Char8 as BSC

papillon :: QuasiQuoter
papillon = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = papillonCore
 }
