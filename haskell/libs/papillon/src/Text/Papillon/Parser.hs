{-# LANGUAGE FlexibleContexts, TemplateHaskell, UndecidableInstances, BangPatterns, PackageImports, TypeFamilies, RankNTypes #-}
module Text.Papillon.Parser (
	Lookahead(..),
	Lists(..),

	Peg,
	Definition,
	Selection,
	Expression,
	Check,
	ReadFrom(..),

	hsw,

	pprCheck,

	parse,
	Source(..),
	SourceList(..),
	Derivs(pegFile, peg, char),
	ParseError(..),
	mkParseError,
	pePositionS,
	Pos(..),
	ListPos(..),

	PPragma(..),
	ModuleName,
	Exports,
	Code,

	runError,

	dvCharsN

) where

import Prelude hiding (Word, (<>))
import Text.Papillon.Papillon
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Applicative(Applicative, (<$>), (<*>))

import Language.Haskell.TH {- (
	Name, TypeQ, PatQ, ExpQ, mkName,
	conT, tupleT, listT, appT, arrowT,
	wildP, litP, varP, conP, tupP, listP, uInfixP,
	litE, varE, conE, tupE, listE, sigE, appE, infixE, uInfixE, lamE,
	integerL, charL, stringL) -}
import Data.Char (isDigit, isUpper, isLower, isAlphaNum, isHexDigit, chr)
import Numeric (readHex)

import Language.Haskell.TH.PprLib
	((<>), hsep, colon, brackets, text, braces, Doc, parens, (<+>), quotes)
import qualified Language.Haskell.TH.PprLib as P
import Control.Arrow ((***))
import Data.List
import Data.Maybe

data Lookahead = Here | Ahead | NAhead String deriving (Show, Eq)
data Lists = List | List1 | Optional deriving (Show, Eq)

type PegFile = ([PPragma], ModuleName, Maybe Exports, Code, STPeg, Code)
data PPragma = LanguagePragma [String] | OtherPragma String deriving Show
type ModuleName = [String]
type Exports = String
type Code = String

type STPeg = (Maybe Type, Type, String, Peg)
type Peg = [Definition]
type Definition = (String, Maybe Type, Selection)
type Selection = [Expression]
type Expression = Either ([(Lookahead, Check)], Maybe Exp) Exp
type Check = Either ((Pat, String), ReadFrom, Maybe (Exp, String)) (Char, Lists)
data ReadFrom
	= FromVariable (Maybe String)
	| FromSelection Selection
	| FromL Lists ReadFrom
	deriving Show

pprCheck :: Check -> Doc
pprCheck (Left ((pt, _), rf, tst)) =
	ppr pt <> colon <> ppr rf <> maybe P.empty (brackets . ppr . fst) tst
pprCheck (Right (c, l)) = quotes (P.char c) <> ppr l

instance Ppr ReadFrom where
	ppr (FromVariable (Just v)) = text v
	ppr (FromVariable _) = P.empty
	ppr (FromL l rf) = ppr rf <> ppr l
	ppr (FromSelection sel) = parens $ ps sel
		where
		ps = hsep . intersperse (P.char '/') . map pe
		pe (Left (ex, hs)) = (<+> braces (maybe P.empty ppr hs)) $ hsep $
			map (uncurry ($) . (((<>) . ppr) *** pprCheck)) ex
		pe (Right ex) = P.char '<' <> ppr ex <> P.char '>'


instance Ppr Lookahead where
	ppr Here = P.empty
	ppr Ahead = P.char '&'
	ppr (NAhead _) = P.char '!'

instance Ppr Lists where
	ppr List = P.char '*'
	ppr List1 = P.char '+'
	ppr Optional = P.char '?'

mkPegFile :: [PPragma] -> Maybe ([String], Maybe String) -> String -> String ->
	STPeg -> String -> PegFile
mkPegFile ps (Just md) x y z w = (ps, fst md, snd md, x ++ "\n" ++ y, z, w)
mkPegFile ps Nothing x y z w = (ps, [], Nothing, x ++ "\n" ++ y, z, w)

dvCharsN :: String
dvCharsN = "char"

opChars :: String
opChars = ":!#$%&*+./<=>?@\\^|-~"

data Word
	= WSymbol String
	| WType [String] String
	| WVar [String] String
	| WOpCon String
	| WOp String
	| WChar Char
	| WString String
	| WInteger Integer
	| WOQuasiQuote String
	| WCQuasiQuote

	-- ["module", "where", "import", "if", "then", "else"]
	| WModule
	| WWhere
	| WImport
	| WIf
	| WThen
	| WElse
	| WLet
	| WIn

	-- ["::", "->", "..", "{-#", "#-}"]
	| WTypeDef
	| WRightArrow
	| WDotDot
	| WOComment
	| WCComment

	-- "!&*+?=/:<>"
	| WBang
	| WAmp
	| WAsterisk
	| WPlus
	| WQuestion
	| WEqual
	| WSlash
	| WColon
	| WLT
	| WGT

	-- "[]{}()"
	| WOBracket
	| WCBracket
	| WOBrace
	| WCBrace
	| WOParen
	| WCParen

	-- ",;`"
	| WComma
	| WSemiColon
	| WBackQuote
	deriving Show


data Derivs
    = Derivs {pegFile :: (ErrorT (ParseError (Pos String) Derivs)
                                 (State ((Maybe Int)))
                                 ((PegFile, Derivs))),
              pragma :: (ErrorT (ParseError (Pos String) Derivs)
                                (State ((Maybe Int)))
                                ((PPragma, Derivs))),
              pragmaStr :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   ((String, Derivs))),
              pragmaItems :: (ErrorT (ParseError (Pos String) Derivs)
                                     (State ((Maybe Int)))
                                     (([String], Derivs))),
              moduleDec :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   ((Maybe (([String], Maybe String)), Derivs))),
              moduleName :: (ErrorT (ParseError (Pos String) Derivs)
                                    (State ((Maybe Int)))
                                    ((ModuleName, Derivs))),
              moduleDecStr :: (ErrorT (ParseError (Pos String) Derivs)
                                      (State ((Maybe Int)))
                                      ((String, Derivs))),
              whr :: (ErrorT (ParseError (Pos String) Derivs)
                             (State ((Maybe Int)))
                             (((), Derivs))),
              preImpPap :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   ((String, Derivs))),
              prePeg :: (ErrorT (ParseError (Pos String) Derivs)
                                (State ((Maybe Int)))
                                ((String, Derivs))),
              afterPeg :: (ErrorT (ParseError (Pos String) Derivs)
                                  (State ((Maybe Int)))
                                  ((String, Derivs))),
              importPapillon :: (ErrorT (ParseError (Pos String) Derivs)
                                        (State ((Maybe Int)))
                                        (((), Derivs))),
              peg :: (ErrorT (ParseError (Pos String) Derivs)
                             (State ((Maybe Int)))
                             ((STPeg, Derivs))),
              peg_ :: (ErrorT (ParseError (Pos String) Derivs)
                              (State ((Maybe Int)))
                              ((STPeg, Derivs))),
              monadType :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   ((Type, Derivs))),
              sourceType :: (ErrorT (ParseError (Pos String) Derivs)
                                    (State ((Maybe Int)))
                                    ((Type, Derivs))),
              prefix :: (ErrorT (ParseError (Pos String) Derivs)
                                (State ((Maybe Int)))
                                ((String, Derivs))),
              definition :: (ErrorT (ParseError (Pos String) Derivs)
                                    (State ((Maybe Int)))
                                    ((Definition, Derivs))),
              selection :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   (([Expression], Derivs))),
              normalSelection :: (ErrorT (ParseError (Pos String) Derivs)
                                         (State ((Maybe Int)))
                                         (([Expression], Derivs))),
              expressionHs :: (ErrorT (ParseError (Pos String) Derivs)
                                      (State ((Maybe Int)))
                                      ((Expression, Derivs))),
              expressionHsSugar :: (ErrorT (ParseError (Pos String) Derivs)
                                           (State ((Maybe Int)))
                                           ((Expression, Derivs))),
              expressionHsSugar' :: (ErrorT (ParseError (Pos String) Derivs)
                                            (State ((Maybe Int)))
                                            ((Expression, Derivs))),
              expression :: (ErrorT (ParseError (Pos String) Derivs)
                                    (State ((Maybe Int)))
                                    (([(Lookahead, Check)], Derivs))),
              nameLeaf_ :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   (((Lookahead, Check), Derivs))),
              nameLeaf :: (ErrorT (ParseError (Pos String) Derivs)
                                  (State ((Maybe Int)))
                                  ((Check, Derivs))),
              nameLeafNoCom :: (ErrorT (ParseError (Pos String) Derivs)
                                       (State ((Maybe Int)))
                                       ((Check, Derivs))),
              comForErr :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   ((String, Derivs))),
              leaf :: (ErrorT (ParseError (Pos String) Derivs)
                              (State ((Maybe Int)))
                              (((ReadFrom, Maybe ((Exp, String))), Derivs))),
              patOp :: (ErrorT (ParseError (Pos String) Derivs)
                               (State ((Maybe Int)))
                               ((Pat, Derivs))),
              pat :: (ErrorT (ParseError (Pos String) Derivs)
                             (State ((Maybe Int)))
                             ((Pat, Derivs))),
              pat1 :: (ErrorT (ParseError (Pos String) Derivs)
                              (State ((Maybe Int)))
                              ((Pat, Derivs))),
              patList :: (ErrorT (ParseError (Pos String) Derivs)
                                 (State ((Maybe Int)))
                                 (([Pat], Derivs))),
              pats :: (ErrorT (ParseError (Pos String) Derivs)
                              (State ((Maybe Int)))
                              (([Pat], Derivs))),
              charLitLs :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   ((Check, Derivs))),
              readFromLs :: (ErrorT (ParseError (Pos String) Derivs)
                                    (State ((Maybe Int)))
                                    ((ReadFrom, Derivs))),
              readFrom :: (ErrorT (ParseError (Pos String) Derivs)
                                  (State ((Maybe Int)))
                                  ((ReadFrom, Derivs))),
              test :: (ErrorT (ParseError (Pos String) Derivs)
                              (State ((Maybe Int)))
                              (((Exp, String), Derivs))),
              hsExpLam :: (ErrorT (ParseError (Pos String) Derivs)
                                  (State ((Maybe Int)))
                                  ((Exp, Derivs))),
              hsExpTyp :: (ErrorT (ParseError (Pos String) Derivs)
                                  (State ((Maybe Int)))
                                  ((Exp, Derivs))),
              hsExpOp :: (ErrorT (ParseError (Pos String) Derivs)
                                 (State ((Maybe Int)))
                                 ((Exp, Derivs))),
              hsOp :: (ErrorT (ParseError (Pos String) Derivs)
                              (State ((Maybe Int)))
                              ((Exp, Derivs))),
              hsExpLam' :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   ((Exp, Derivs))),
              hsExpTyp' :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   ((Exp, Derivs))),
              hsExpOp' :: (ErrorT (ParseError (Pos String) Derivs)
                                  (State ((Maybe Int)))
                                  ((Exp, Derivs))),
              hsOp' :: (ErrorT (ParseError (Pos String) Derivs)
                               (State ((Maybe Int)))
                               ((Exp, Derivs))),
              hsExp :: (ErrorT (ParseError (Pos String) Derivs)
                               (State ((Maybe Int)))
                               (((Exp -> Exp) -> Exp, Derivs))),
              hsExp1 :: (ErrorT (ParseError (Pos String) Derivs)
                                (State ((Maybe Int)))
                                ((Exp, Derivs))),
              hsExpTpl :: (ErrorT (ParseError (Pos String) Derivs)
                                  (State ((Maybe Int)))
                                  (([Exp], Derivs))),
              hsTypeArr :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   ((Type, Derivs))),
              hsType :: (ErrorT (ParseError (Pos String) Derivs)
                                (State ((Maybe Int)))
                                (((Type -> Type) -> Type, Derivs))),
              hsType1 :: (ErrorT (ParseError (Pos String) Derivs)
                                 (State ((Maybe Int)))
                                 ((Type, Derivs))),
              hsTypeTpl :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   (([Type], Derivs))),
              wrd :: (ErrorT (ParseError (Pos String) Derivs)
                             (State ((Maybe Int)))
                             ((Word, Derivs))),
              hsw :: (ErrorT (ParseError (Pos String) Derivs)
                             (State ((Maybe Int)))
                             ((Word, Derivs))),
              word :: (ErrorT (ParseError (Pos String) Derivs)
                              (State ((Maybe Int)))
                              ((Word, Derivs))),
              hsWord :: (ErrorT (ParseError (Pos String) Derivs)
                                (State ((Maybe Int)))
                                ((Word, Derivs))),
              bras :: (ErrorT (ParseError (Pos String) Derivs)
                              (State ((Maybe Int)))
                              ((Word, Derivs))),
              typ :: (ErrorT (ParseError (Pos String) Derivs)
                             (State ((Maybe Int)))
                             ((String, Derivs))),
              var :: (ErrorT (ParseError (Pos String) Derivs)
                             (State ((Maybe Int)))
                             ((String, Derivs))),
              strLit :: (ErrorT (ParseError (Pos String) Derivs)
                                (State ((Maybe Int)))
                                ((String, Derivs))),
              escChar :: (ErrorT (ParseError (Pos String) Derivs)
                                 (State ((Maybe Int)))
                                 ((Char, Derivs))),
              space :: (ErrorT (ParseError (Pos String) Derivs)
                               (State ((Maybe Int)))
                               (((), Derivs))),
              space' :: (ErrorT (ParseError (Pos String) Derivs)
                                (State ((Maybe Int)))
                                (((), Derivs))),
              lineComment :: (ErrorT (ParseError (Pos String) Derivs)
                                     (State ((Maybe Int)))
                                     (((), Derivs))),
              comment :: (ErrorT (ParseError (Pos String) Derivs)
                                 (State ((Maybe Int)))
                                 (((), Derivs))),
              comments :: (ErrorT (ParseError (Pos String) Derivs)
                                  (State ((Maybe Int)))
                                  (((), Derivs))),
              comStr :: (ErrorT (ParseError (Pos String) Derivs)
                                (State ((Maybe Int)))
                                (((), Derivs))),
              semiColon :: (ErrorT (ParseError (Pos String) Derivs)
                                   (State ((Maybe Int)))
                                   (((), Derivs))),
              notSemiColon :: (ErrorT (ParseError (Pos String) Derivs)
                                      (State ((Maybe Int)))
                                      (((), Derivs))),
              char :: (ErrorT (ParseError (Pos String) Derivs)
                              (State ((Maybe Int)))
                              ((Token String, Derivs))),
              position :: (ErrorT (ParseError (Pos String) Derivs)
                                  (State ((Maybe Int)))
                                  ((Pos String, Derivs)))}
parse :: String -> Derivs
parse = parse1632_0 initialPos
          where parse1632_0 pos1631_1 s1633_2 = d144_3
                                where d144_3 = Derivs pegFile1_4 pragma2_5 pragmaStr3_6 pragmaItems4_7 moduleDec5_8 moduleName6_9 moduleDecStr7_10 whr8_11 preImpPap9_12 prePeg10_13 afterPeg11_14 importPapillon12_15 peg13_16 peg_14_17 monadType15_18 sourceType16_19 prefix17_20 definition18_21 selection19_22 normalSelection20_23 expressionHs21_24 expressionHsSugar22_25 expressionHsSugar'23_26 expression24_27 nameLeaf_25_28 nameLeaf26_29 nameLeafNoCom27_30 comForErr28_31 leaf29_32 patOp30_33 pat31_34 pat132_35 patList33_36 pats34_37 charLitLs35_38 readFromLs36_39 readFrom37_40 test38_41 hsExpLam39_42 hsExpTyp40_43 hsExpOp41_44 hsOp42_45 hsExpLam'43_46 hsExpTyp'44_47 hsExpOp'45_48 hsOp'46_49 hsExp47_50 hsExp148_51 hsExpTpl49_52 hsTypeArr50_53 hsType51_54 hsType152_55 hsTypeTpl53_56 wrd54_57 hsw55_58 word56_59 hsWord57_60 bras58_61 typ59_62 var60_63 strLit61_64 escChar62_65 space63_66 space'64_67 lineComment65_68 comment66_69 comments67_70 comStr68_71 semiColon69_72 notSemiColon70_73 chars1634_74 (return (pos1631_1,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d144_3))
                                      pegFile1_4 = runStateT pegFile71_75 d144_3
                                      pragma2_5 = runStateT pragma72_76 d144_3
                                      pragmaStr3_6 = runStateT pragmaStr73_77 d144_3
                                      pragmaItems4_7 = runStateT pragmaItems74_78 d144_3
                                      moduleDec5_8 = runStateT moduleDec75_79 d144_3
                                      moduleName6_9 = runStateT moduleName76_80 d144_3
                                      moduleDecStr7_10 = runStateT moduleDecStr77_81 d144_3
                                      whr8_11 = runStateT whr78_82 d144_3
                                      preImpPap9_12 = runStateT preImpPap79_83 d144_3
                                      prePeg10_13 = runStateT prePeg80_84 d144_3
                                      afterPeg11_14 = runStateT afterPeg81_85 d144_3
                                      importPapillon12_15 = runStateT importPapillon82_86 d144_3
                                      peg13_16 = runStateT peg83_87 d144_3
                                      peg_14_17 = runStateT peg_84_88 d144_3
                                      monadType15_18 = runStateT monadType85_89 d144_3
                                      sourceType16_19 = runStateT sourceType86_90 d144_3
                                      prefix17_20 = runStateT prefix87_91 d144_3
                                      definition18_21 = runStateT definition88_92 d144_3
                                      selection19_22 = runStateT selection89_93 d144_3
                                      normalSelection20_23 = runStateT normalSelection90_94 d144_3
                                      expressionHs21_24 = runStateT expressionHs91_95 d144_3
                                      expressionHsSugar22_25 = runStateT expressionHsSugar92_96 d144_3
                                      expressionHsSugar'23_26 = runStateT expressionHsSugar'93_97 d144_3
                                      expression24_27 = runStateT expression94_98 d144_3
                                      nameLeaf_25_28 = runStateT nameLeaf_95_99 d144_3
                                      nameLeaf26_29 = runStateT nameLeaf96_100 d144_3
                                      nameLeafNoCom27_30 = runStateT nameLeafNoCom97_101 d144_3
                                      comForErr28_31 = runStateT comForErr98_102 d144_3
                                      leaf29_32 = runStateT leaf99_103 d144_3
                                      patOp30_33 = runStateT patOp100_104 d144_3
                                      pat31_34 = runStateT pat101_105 d144_3
                                      pat132_35 = runStateT pat1102_106 d144_3
                                      patList33_36 = runStateT patList103_107 d144_3
                                      pats34_37 = runStateT pats104_108 d144_3
                                      charLitLs35_38 = runStateT charLitLs105_109 d144_3
                                      readFromLs36_39 = runStateT readFromLs106_110 d144_3
                                      readFrom37_40 = runStateT readFrom107_111 d144_3
                                      test38_41 = runStateT test108_112 d144_3
                                      hsExpLam39_42 = runStateT hsExpLam109_113 d144_3
                                      hsExpTyp40_43 = runStateT hsExpTyp110_114 d144_3
                                      hsExpOp41_44 = runStateT hsExpOp111_115 d144_3
                                      hsOp42_45 = runStateT hsOp112_116 d144_3
                                      hsExpLam'43_46 = runStateT hsExpLam'113_117 d144_3
                                      hsExpTyp'44_47 = runStateT hsExpTyp'114_118 d144_3
                                      hsExpOp'45_48 = runStateT hsExpOp'115_119 d144_3
                                      hsOp'46_49 = runStateT hsOp'116_120 d144_3
                                      hsExp47_50 = runStateT hsExp117_121 d144_3
                                      hsExp148_51 = runStateT hsExp1118_122 d144_3
                                      hsExpTpl49_52 = runStateT hsExpTpl119_123 d144_3
                                      hsTypeArr50_53 = runStateT hsTypeArr120_124 d144_3
                                      hsType51_54 = runStateT hsType121_125 d144_3
                                      hsType152_55 = runStateT hsType1122_126 d144_3
                                      hsTypeTpl53_56 = runStateT hsTypeTpl123_127 d144_3
                                      wrd54_57 = runStateT wrd124_128 d144_3
                                      hsw55_58 = runStateT hsw125_129 d144_3
                                      word56_59 = runStateT word126_130 d144_3
                                      hsWord57_60 = runStateT hsWord127_131 d144_3
                                      bras58_61 = runStateT bras128_132 d144_3
                                      typ59_62 = runStateT typ129_133 d144_3
                                      var60_63 = runStateT var130_134 d144_3
                                      strLit61_64 = runStateT strLit131_135 d144_3
                                      escChar62_65 = runStateT escChar132_136 d144_3
                                      space63_66 = runStateT space133_137 d144_3
                                      space'64_67 = runStateT space'134_138 d144_3
                                      lineComment65_68 = runStateT lineComment135_139 d144_3
                                      comment66_69 = runStateT comment136_140 d144_3
                                      comments67_70 = runStateT comments137_141 d144_3
                                      comStr68_71 = runStateT comStr138_142 d144_3
                                      semiColon69_72 = runStateT semiColon139_143 d144_3
                                      notSemiColon70_73 = runStateT notSemiColon140_144 d144_3
                                      chars1634_74 = runStateT (case getToken s1633_2 of
                                                                    Just (c1616_145,
                                                                          s'1630_146) -> do put (parse1632_0 (updatePos c1616_145 pos1631_1) s'1630_146)
                                                                                            return c1616_145
                                                                    _ -> StateT position >>= (throwError . mkParseError "" "end of input" "" undefined [])) d144_3
                pegFile71_75 = foldl1 mplus [do pr <- list142_147 (StateT pragma)
                                                md <- StateT moduleDec
                                                pip <- StateT preImpPap
                                                _ <- StateT importPapillon
                                                return ()
                                                pp <- StateT prePeg
                                                d149_148 <- get
                                                t648_149 <- StateT hsw
                                                case t648_149 of
                                                    (WOQuasiQuote "papillon") -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "(WOQuasiQuote \"papillon\")" "not match pattern: " "" d149_148 ["hsw"])
                                                let (WOQuasiQuote "papillon") = t648_149
                                                return ()
                                                p <- StateT peg_
                                                d151_150 <- get
                                                t650_151 <- StateT hsw
                                                case t650_151 of
                                                    WCQuasiQuote -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "WCQuasiQuote" "not match pattern: " "" d151_150 ["hsw"])
                                                let WCQuasiQuote = t650_151
                                                return ()
                                                atp <- StateT afterPeg
                                                lift (lift (return $ mkPegFile pr md pip pp p atp)),
                                             do pr <- list142_147 (StateT pragma)
                                                md <- StateT moduleDec
                                                pp <- StateT prePeg
                                                d156_152 <- get
                                                t655_153 <- StateT hsw
                                                case t655_153 of
                                                    (WOQuasiQuote "papillon") -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "(WOQuasiQuote \"papillon\")" "not match pattern: " "" d156_152 ["hsw"])
                                                let (WOQuasiQuote "papillon") = t655_153
                                                return ()
                                                p <- StateT peg_
                                                d158_154 <- get
                                                t657_155 <- StateT hsw
                                                case t657_155 of
                                                    WCQuasiQuote -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "WCQuasiQuote" "not match pattern: " "" d158_154 ["hsw"])
                                                let WCQuasiQuote = t657_155
                                                return ()
                                                atp <- StateT afterPeg
                                                lift (lift (return $ mkPegFile pr md [] pp p atp))]
                pragma72_76 = foldl1 mplus [do d160_156 <- get
                                               t659_157 <- StateT hsw
                                               case t659_157 of
                                                   WOComment -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WOComment" "not match pattern: " "" d160_156 ["hsw"])
                                               let WOComment = t659_157
                                               return ()
                                               d161_158 <- get
                                               t660_159 <- StateT hsw
                                               case t660_159 of
                                                   (WType [] "LANGUAGE") -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WType [] \"LANGUAGE\")" "not match pattern: " "" d161_158 ["hsw"])
                                               let (WType [] "LANGUAGE") = t660_159
                                               return ()
                                               s <- StateT pragmaItems
                                               d163_160 <- get
                                               t662_161 <- StateT hsw
                                               case t662_161 of
                                                   WCComment -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WCComment" "not match pattern: " "" d163_160 ["hsw"])
                                               let WCComment = t662_161
                                               return ()
                                               lift (lift (return $ LanguagePragma s)),
                                            do d164_162 <- get
                                               t663_163 <- StateT hsw
                                               case t663_163 of
                                                   WOComment -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WOComment" "not match pattern: " "" d164_162 ["hsw"])
                                               let WOComment = t663_163
                                               return ()
                                               s <- StateT pragmaStr
                                               d166_164 <- get
                                               t665_165 <- StateT hsw
                                               case t665_165 of
                                                   WCComment -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WCComment" "not match pattern: " "" d166_164 ["hsw"])
                                               let WCComment = t665_165
                                               return ()
                                               lift (lift (return $ OtherPragma s))]
                pragmaStr73_77 = foldl1 mplus [do d168_166 <- get
                                                  do err1604_167 <- ((do d167_168 <- get
                                                                         t666_169 <- StateT hsw
                                                                         case t666_169 of
                                                                             WCComment -> return ()
                                                                             _ -> StateT position >>= (throwError . mkParseError "WCComment" "not match pattern: " "" d167_168 ["hsw"])
                                                                         let WCComment = t666_169
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1604_167 (StateT position >>= (throwError . mkParseError "!WCComment:hsw" "not match: " "" d168_166 ["hsw"]))
                                                  put d168_166
                                                  c <- StateT char
                                                  s <- StateT pragmaStr
                                                  lift (lift (return $ c : s)),
                                               lift (lift (return ""))]
                pragmaItems74_78 = foldl1 mplus [do d171_170 <- get
                                                    t669_171 <- StateT hsw
                                                    case t669_171 of
                                                        (WType [] _) -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "(WType [] t)" "not match pattern: " "" d171_170 ["hsw"])
                                                    let (WType [] t) = t669_171
                                                    return ()
                                                    d172_172 <- get
                                                    t670_173 <- StateT hsw
                                                    case t670_173 of
                                                        WComma -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "WComma" "not match pattern: " "" d172_172 ["hsw"])
                                                    let WComma = t670_173
                                                    return ()
                                                    i <- StateT pragmaItems
                                                    lift (lift (return $ t : i)),
                                                 do d174_174 <- get
                                                    t672_175 <- StateT hsw
                                                    case t672_175 of
                                                        (WType [] _) -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "(WType [] t)" "not match pattern: " "" d174_174 ["hsw"])
                                                    let (WType [] t) = t672_175
                                                    return ()
                                                    lift (lift (return [t]))]
                moduleDec75_79 = foldl1 mplus [do d175_176 <- get
                                                  t673_177 <- StateT hsw
                                                  case t673_177 of
                                                      WModule -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WModule" "not match pattern: " "" d175_176 ["hsw"])
                                                  let WModule = t673_177
                                                  return ()
                                                  n <- StateT moduleName
                                                  d177_178 <- get
                                                  t675_179 <- StateT hsw
                                                  case t675_179 of
                                                      WOParen -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WOParen" "not match pattern: " "" d177_178 ["hsw"])
                                                  let WOParen = t675_179
                                                  return ()
                                                  s <- StateT moduleDecStr
                                                  _ <- StateT whr
                                                  return ()
                                                  lift (lift (return $ Just (n, Just s))),
                                               do d180_180 <- get
                                                  t678_181 <- StateT hsw
                                                  case t678_181 of
                                                      WModule -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WModule" "not match pattern: " "" d180_180 ["hsw"])
                                                  let WModule = t678_181
                                                  return ()
                                                  n <- StateT moduleName
                                                  d182_182 <- get
                                                  t680_183 <- StateT hsw
                                                  case t680_183 of
                                                      WWhere -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WWhere" "not match pattern: " "" d182_182 ["hsw"])
                                                  let WWhere = t680_183
                                                  return ()
                                                  lift (lift (return $ Just (n, Nothing))),
                                               lift (lift (return Nothing))]
                moduleName76_80 = foldl1 mplus [do d183_184 <- get
                                                   t681_185 <- StateT hsw
                                                   case t681_185 of
                                                       (WType _ _) -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "(WType m t)" "not match pattern: " "" d183_184 ["hsw"])
                                                   let (WType m t) = t681_185
                                                   return ()
                                                   lift (lift (return $ m ++ [t]))]
                moduleDecStr77_81 = foldl1 mplus [do d185_186 <- get
                                                     do err1605_187 <- ((do _ <- StateT whr
                                                                            return ()) >> return False) `catchError` const (return True)
                                                        unless err1605_187 (StateT position >>= (throwError . mkParseError "!_:whr" "not match: " "" d185_186 ["whr"]))
                                                     put d185_186
                                                     c <- StateT char
                                                     s <- StateT moduleDecStr
                                                     lift (lift (return $ c : s)),
                                                  lift (lift (return ""))]
                whr78_82 = foldl1 mplus [do d188_188 <- get
                                            t685_189 <- StateT hsw
                                            case t685_189 of
                                                WCParen -> return ()
                                                _ -> StateT position >>= (throwError . mkParseError "WCParen" "not match pattern: " "" d188_188 ["hsw"])
                                            let WCParen = t685_189
                                            return ()
                                            d189_190 <- get
                                            t686_191 <- StateT hsw
                                            case t686_191 of
                                                WWhere -> return ()
                                                _ -> StateT position >>= (throwError . mkParseError "WWhere" "not match pattern: " "" d189_190 ["hsw"])
                                            let WWhere = t686_191
                                            return ()
                                            return ()]
                preImpPap79_83 = foldl1 mplus [do d191_192 <- get
                                                  do err1606_193 <- ((do _ <- StateT importPapillon
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1606_193 (StateT position >>= (throwError . mkParseError "!_:importPapillon" "not match: " "" d191_192 ["importPapillon"]))
                                                  put d191_192
                                                  d193_194 <- get
                                                  do err1607_195 <- ((do d192_196 <- get
                                                                         t688_197 <- StateT hsw
                                                                         case t688_197 of
                                                                             (WOQuasiQuote "papillon") -> return ()
                                                                             _ -> StateT position >>= (throwError . mkParseError "(WOQuasiQuote \"papillon\")" "not match pattern: " "" d192_196 ["hsw"])
                                                                         let (WOQuasiQuote "papillon") = t688_197
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1607_195 (StateT position >>= (throwError . mkParseError "!(WOQuasiQuote \"papillon\"):hsw" "not match: " "" d193_194 ["hsw"]))
                                                  put d193_194
                                                  c <- StateT char
                                                  pip <- StateT preImpPap
                                                  lift (lift (return $ c : pip)),
                                               lift (lift (return ""))]
                prePeg80_84 = foldl1 mplus [do d197_198 <- get
                                               do err1608_199 <- ((do d196_200 <- get
                                                                      t691_201 <- StateT hsw
                                                                      case t691_201 of
                                                                          (WOQuasiQuote "papillon") -> return ()
                                                                          _ -> StateT position >>= (throwError . mkParseError "(WOQuasiQuote \"papillon\")" "not match pattern: " "" d196_200 ["hsw"])
                                                                      let (WOQuasiQuote "papillon") = t691_201
                                                                      return ()) >> return False) `catchError` const (return True)
                                                  unless err1608_199 (StateT position >>= (throwError . mkParseError "!(WOQuasiQuote \"papillon\"):hsw" "not match: " "" d197_198 ["hsw"]))
                                               put d197_198
                                               c <- StateT char
                                               pp <- StateT prePeg
                                               lift (lift (return $ c : pp)),
                                            lift (lift (return ""))]
                afterPeg81_85 = foldl1 mplus [do c <- StateT char
                                                 atp <- StateT afterPeg
                                                 lift (lift (return $ c : atp)),
                                              lift (lift (return ""))]
                importPapillon82_86 = foldl1 mplus [do d202_202 <- get
                                                       t696_203 <- StateT hsw
                                                       case t696_203 of
                                                           WImport -> return ()
                                                           _ -> StateT position >>= (throwError . mkParseError "WImport" "not match pattern: " "" d202_202 ["hsw"])
                                                       let WImport = t696_203
                                                       return ()
                                                       d203_204 <- get
                                                       t697_205 <- StateT hsw
                                                       case t697_205 of
                                                           (WType ["Text"] "Papillon") -> return ()
                                                           _ -> StateT position >>= (throwError . mkParseError "(WType [\"Text\"] \"Papillon\")" "not match pattern: " "" d203_204 ["hsw"])
                                                       let (WType ["Text"] "Papillon") = t697_205
                                                       return ()
                                                       return ()]
                peg83_87 = foldl1 mplus [do _ <- list142_147 (StateT space)
                                            return ()
                                            p <- StateT peg_
                                            lift (lift (return p))]
                peg_84_88 = foldl1 mplus [do mt <- optional141_206 (StateT monadType)
                                             st <- optional141_206 (StateT sourceType)
                                             prfx <- optional141_206 (StateT prefix)
                                             p <- list142_147 (StateT definition)
                                             lift (lift (return (mt,
                                                                 fromMaybe (ConT $ mkName "String") st,
                                                                 fromMaybe "" prfx,
                                                                 p)))]
                monadType85_89 = foldl1 mplus [do d210_207 <- get
                                                  t704_208 <- StateT position
                                                  let (!(ListPos (CharPos (_, x)))) = t704_208
                                                  b1189_209 <- lift (lift (put (Just x) >> return True))
                                                  unless b1189_209 (StateT position >>= (throwError . mkParseError "put (Just x) >> return True" "not match: " "" d210_207 ["position"]))
                                                  d211_210 <- get
                                                  t705_211 <- StateT wrd
                                                  case t705_211 of
                                                      (WSymbol "monad") -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "(WSymbol \"monad\")" "not match pattern: " "" d211_210 ["wrd"])
                                                  let (WSymbol "monad") = t705_211
                                                  return ()
                                                  d212_212 <- get
                                                  t706_213 <- StateT wrd
                                                  case t706_213 of
                                                      WColon -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WColon" "not match pattern: " "" d212_212 ["wrd"])
                                                  let WColon = t706_213
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  d214_214 <- get
                                                  t708_215 <- StateT wrd
                                                  case t708_215 of
                                                      WSemiColon -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WSemiColon" "not match pattern: " "" d214_214 ["wrd"])
                                                  let WSemiColon = t708_215
                                                  return ()
                                                  lift (lift (put Nothing >> return t))]
                sourceType86_90 = foldl1 mplus [do d215_216 <- get
                                                   t709_217 <- StateT position
                                                   let (!(ListPos (CharPos (_, x)))) = t709_217
                                                   b1194_218 <- lift (lift (put (Just x) >> return True))
                                                   unless b1194_218 (StateT position >>= (throwError . mkParseError "put (Just x) >> return True" "not match: " "" d215_216 ["position"]))
                                                   d216_219 <- get
                                                   t710_220 <- StateT wrd
                                                   case t710_220 of
                                                       (WSymbol "source") -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "(WSymbol \"source\")" "not match pattern: " "" d216_219 ["wrd"])
                                                   let (WSymbol "source") = t710_220
                                                   return ()
                                                   d217_221 <- get
                                                   t711_222 <- StateT wrd
                                                   case t711_222 of
                                                       WColon -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WColon" "not match pattern: " "" d217_221 ["wrd"])
                                                   let WColon = t711_222
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   d219_223 <- get
                                                   t713_224 <- StateT wrd
                                                   case t713_224 of
                                                       WSemiColon -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WSemiColon" "not match pattern: " "" d219_223 ["wrd"])
                                                   let WSemiColon = t713_224
                                                   return ()
                                                   lift (lift (put Nothing >> return t))]
                prefix87_91 = foldl1 mplus [do d220_225 <- get
                                               t714_226 <- StateT position
                                               let (!(ListPos (CharPos (_, x)))) = t714_226
                                               b1199_227 <- lift (lift (put (Just x) >> return True))
                                               unless b1199_227 (StateT position >>= (throwError . mkParseError "put (Just x) >> return True" "not match: " "" d220_225 ["position"]))
                                               d221_228 <- get
                                               t715_229 <- StateT wrd
                                               case t715_229 of
                                                   (WSymbol "prefix") -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WSymbol \"prefix\")" "not match pattern: " "" d221_228 ["wrd"])
                                               let (WSymbol "prefix") = t715_229
                                               return ()
                                               d222_230 <- get
                                               t716_231 <- StateT wrd
                                               case t716_231 of
                                                   WColon -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WColon" "not match pattern: " "" d222_230 ["wrd"])
                                               let WColon = t716_231
                                               return ()
                                               d223_232 <- get
                                               t717_233 <- StateT hsw
                                               case t717_233 of
                                                   (WString _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WString prfx)" "not match pattern: " "" d223_232 ["hsw"])
                                               let (WString prfx) = t717_233
                                               return ()
                                               d224_234 <- get
                                               t718_235 <- StateT wrd
                                               case t718_235 of
                                                   WSemiColon -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WSemiColon" "not match pattern: " "" d224_234 ["wrd"])
                                               let WSemiColon = t718_235
                                               return ()
                                               lift (lift (put Nothing >> return prfx))]
                definition88_92 = foldl1 mplus [do d225_236 <- get
                                                   t719_237 <- StateT position
                                                   let (!(ListPos (CharPos (_, x)))) = t719_237
                                                   b1204_238 <- lift (lift (put (Just x) >> return True))
                                                   unless b1204_238 (StateT position >>= (throwError . mkParseError "put (Just x) >> return True" "not match: " "" d225_236 ["position"]))
                                                   d226_239 <- get
                                                   t720_240 <- StateT wrd
                                                   case t720_240 of
                                                       (WSymbol _) -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "(WSymbol v)" "not match pattern: " "" d226_239 ["wrd"])
                                                   let (WSymbol v) = t720_240
                                                   return ()
                                                   mt <- optional141_206 (foldl1 mplus [do d228_241 <- get
                                                                                           t722_242 <- StateT hsw
                                                                                           case t722_242 of
                                                                                               WTypeDef -> return ()
                                                                                               _ -> StateT position >>= (throwError . mkParseError "WTypeDef" "not match pattern: " "" d228_241 ["hsw"])
                                                                                           let WTypeDef = t722_242
                                                                                           return ()
                                                                                           t <- StateT hsTypeArr
                                                                                           lift (lift (return t))])
                                                   d230_243 <- get
                                                   t724_244 <- StateT wrd
                                                   case t724_244 of
                                                       WEqual -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WEqual" "not match pattern: " "" d230_243 ["wrd"])
                                                   let WEqual = t724_244
                                                   return ()
                                                   sel <- StateT selection
                                                   d232_245 <- get
                                                   t726_246 <- StateT wrd
                                                   case t726_246 of
                                                       WSemiColon -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WSemiColon" "not match pattern: " "" d232_245 ["wrd"])
                                                   let WSemiColon = t726_246
                                                   return ()
                                                   lift (lift (put Nothing >> return (v, mt, sel)))]
                selection89_93 = foldl1 mplus [do ns <- StateT normalSelection
                                                  lift (lift (return ns))]
                normalSelection90_94 = foldl1 mplus [do ex <- StateT expressionHs
                                                        d235_247 <- get
                                                        t729_248 <- StateT wrd
                                                        case t729_248 of
                                                            WSlash -> return ()
                                                            _ -> StateT position >>= (throwError . mkParseError "WSlash" "not match pattern: " "" d235_247 ["wrd"])
                                                        let WSlash = t729_248
                                                        return ()
                                                        sel <- StateT normalSelection
                                                        lift (lift (return $ ex : sel)),
                                                     do ex <- StateT expressionHs
                                                        lift (lift (return [ex]))]
                expressionHs91_95 = foldl1 mplus [do e <- StateT expressionHsSugar
                                                     lift (lift (return e)),
                                                  do e <- StateT expression
                                                     mh <- optional141_206 (foldl1 mplus [do d241_249 <- get
                                                                                             t735_250 <- StateT wrd
                                                                                             case t735_250 of
                                                                                                 WOBrace -> return ()
                                                                                                 _ -> StateT position >>= (throwError . mkParseError "WOBrace" "not match pattern: " "" d241_249 ["wrd"])
                                                                                             let WOBrace = t735_250
                                                                                             return ()
                                                                                             h <- StateT hsExpLam
                                                                                             d243_251 <- get
                                                                                             t737_252 <- StateT wrd
                                                                                             case t737_252 of
                                                                                                 WCBrace -> return ()
                                                                                                 _ -> StateT position >>= (throwError . mkParseError "WCBrace" "not match pattern: " "" d243_251 ["wrd"])
                                                                                             let WCBrace = t737_252
                                                                                             return ()
                                                                                             lift (lift (return h))])
                                                     lift (lift (return $ Left (e, mh)))]
                expressionHsSugar92_96 = foldl1 mplus [do d244_253 <- get
                                                          t738_254 <- StateT wrd
                                                          case t738_254 of
                                                              WLT -> return ()
                                                              _ -> StateT position >>= (throwError . mkParseError "WLT" "not match pattern: " "" d244_253 ["wrd"])
                                                          let WLT = t738_254
                                                          return ()
                                                          h <- StateT hsExpLam
                                                          d246_255 <- get
                                                          t740_256 <- StateT wrd
                                                          case t740_256 of
                                                              WGT -> return ()
                                                              _ -> StateT position >>= (throwError . mkParseError "WGT" "not match pattern: " "" d246_255 ["wrd"])
                                                          let WGT = t740_256
                                                          return ()
                                                          lift (lift (return $ Right h))]
                expressionHsSugar'93_97 = foldl1 mplus [do d247_257 <- get
                                                           t741_258 <- StateT wrd
                                                           case t741_258 of
                                                               WLT -> return ()
                                                               _ -> StateT position >>= (throwError . mkParseError "WLT" "not match pattern: " "" d247_257 ["wrd"])
                                                           let WLT = t741_258
                                                           return ()
                                                           h <- StateT hsExpLam'
                                                           d249_259 <- get
                                                           t743_260 <- StateT wrd
                                                           case t743_260 of
                                                               WGT -> return ()
                                                               _ -> StateT position >>= (throwError . mkParseError "WGT" "not match pattern: " "" d249_259 ["wrd"])
                                                           let WGT = t743_260
                                                           return ()
                                                           lift (lift (return $ Right h))]
                expression94_98 = foldl1 mplus [do l <- StateT nameLeaf_
                                                   e <- StateT expression
                                                   lift (lift (return $ l : e)),
                                                lift (lift (return []))]
                nameLeaf_95_99 = foldl1 mplus [do d252_261 <- get
                                                  t746_262 <- StateT wrd
                                                  case t746_262 of
                                                      WBang -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WBang" "not match pattern: " "" d252_261 ["wrd"])
                                                  let WBang = t746_262
                                                  return ()
                                                  nl <- StateT nameLeafNoCom
                                                  com <- optional141_206 (StateT comForErr)
                                                  lift (lift (return (NAhead $ maybe "" id com,
                                                                      nl))),
                                               do d255_263 <- get
                                                  t749_264 <- StateT wrd
                                                  case t749_264 of
                                                      WAmp -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WAmp" "not match pattern: " "" d255_263 ["wrd"])
                                                  let WAmp = t749_264
                                                  return ()
                                                  nl <- StateT nameLeaf
                                                  lift (lift (return (Ahead, nl))),
                                               do nl <- StateT nameLeaf
                                                  lift (lift (return (Here, nl)))]
                nameLeaf96_100 = foldl1 mplus [do cl <- StateT charLitLs
                                                  lift (lift (return cl)),
                                               do n <- StateT pat1
                                                  com <- optional141_206 (StateT comForErr)
                                                  d261_265 <- get
                                                  t755_266 <- StateT wrd
                                                  case t755_266 of
                                                      WColon -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WColon" "not match pattern: " "" d261_265 ["wrd"])
                                                  let WColon = t755_266
                                                  return ()
                                                  (rf, p) <- StateT leaf
                                                  lift (lift (return $ Left ((n, maybe "" id com),
                                                                             rf,
                                                                             p))),
                                               do n <- StateT pat1
                                                  com <- optional141_206 (StateT comForErr)
                                                  lift (lift (return $ Left ((n, maybe "" id com),
                                                                             FromVariable Nothing,
                                                                             Nothing)))]
                nameLeafNoCom97_101 = foldl1 mplus [do cl <- StateT charLitLs
                                                       lift (lift (return cl)),
                                                    do n <- StateT pat1
                                                       com <- optional141_206 (StateT comForErr)
                                                       d268_267 <- get
                                                       t762_268 <- StateT wrd
                                                       case t762_268 of
                                                           WColon -> return ()
                                                           _ -> StateT position >>= (throwError . mkParseError "WColon" "not match pattern: " "" d268_267 ["wrd"])
                                                       let WColon = t762_268
                                                       return ()
                                                       (rf, p) <- StateT leaf
                                                       lift (lift (return $ Left ((n,
                                                                                   maybe "" id com),
                                                                                  rf,
                                                                                  p))),
                                                    do n <- StateT pat1
                                                       lift (lift (return $ Left ((n, ""),
                                                                                  FromVariable Nothing,
                                                                                  Nothing)))]
                comForErr98_102 = foldl1 mplus [do d271_269 <- get
                                                   t765_270 <- StateT hsw
                                                   case t765_270 of
                                                       WOComment -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WOComment" "not match pattern: " "" d271_269 ["hsw"])
                                                   let WOComment = t765_270
                                                   return ()
                                                   d272_271 <- get
                                                   t766_272 <- StateT hsw
                                                   case t766_272 of
                                                       (WString _) -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "(WString s)" "not match pattern: " "" d272_271 ["hsw"])
                                                   let (WString s) = t766_272
                                                   return ()
                                                   d273_273 <- get
                                                   t767_274 <- StateT hsw
                                                   case t767_274 of
                                                       WCComment -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WCComment" "not match pattern: " "" d273_273 ["hsw"])
                                                   let WCComment = t767_274
                                                   return ()
                                                   lift (lift (return s))]
                leaf99_103 = foldl1 mplus [do rf <- StateT readFromLs
                                              t <- StateT test
                                              lift (lift (return (rf, Just t))),
                                           do rf <- StateT readFromLs
                                              lift (lift (return (rf, Nothing))),
                                           do t <- StateT test
                                              lift (lift (return (FromVariable Nothing, Just t)))]
                patOp100_104 = foldl1 mplus [do p <- StateT pat
                                                d279_275 <- get
                                                t773_276 <- StateT hsw
                                                case t773_276 of
                                                    (WOpCon _) -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "(WOpCon o)" "not match pattern: " "" d279_275 ["hsw"])
                                                let (WOpCon o) = t773_276
                                                return ()
                                                po <- StateT patOp
                                                lift (lift (return $ UInfixP p (mkName o) po)),
                                             do p <- StateT pat
                                                d282_277 <- get
                                                t776_278 <- StateT hsw
                                                case t776_278 of
                                                    WBackQuote -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d282_277 ["hsw"])
                                                let WBackQuote = t776_278
                                                return ()
                                                d283_279 <- get
                                                t777_280 <- StateT hsw
                                                case t777_280 of
                                                    (WType [] _) -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "(WType [] t)" "not match pattern: " "" d283_279 ["hsw"])
                                                let (WType [] t) = t777_280
                                                return ()
                                                d284_281 <- get
                                                t778_282 <- StateT hsw
                                                case t778_282 of
                                                    WBackQuote -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d284_281 ["hsw"])
                                                let WBackQuote = t778_282
                                                return ()
                                                po <- StateT patOp
                                                lift (lift (return $ UInfixP p (mkName t) po)),
                                             do p <- StateT pat
                                                lift (lift (return p))]
                pat101_105 = foldl1 mplus [do d287_283 <- get
                                              t781_284 <- StateT hsw
                                              case t781_284 of
                                                  (WType [] _) -> return ()
                                                  _ -> StateT position >>= (throwError . mkParseError "(WType [] t)" "not match pattern: " "" d287_283 ["hsw"])
                                              let (WType [] t) = t781_284
                                              return ()
                                              ps <- StateT pats
                                              lift (lift (return $ ConP (mkName t) ps)),
                                           do d289_285 <- get
                                              t783_286 <- StateT hsw
                                              case t783_286 of
                                                  WOParen -> return ()
                                                  _ -> StateT position >>= (throwError . mkParseError "WOParen" "not match pattern: " "" d289_285 ["hsw"])
                                              let WOParen = t783_286
                                              return ()
                                              d290_287 <- get
                                              t784_288 <- StateT hsw
                                              case t784_288 of
                                                  (WOpCon _) -> return ()
                                                  _ -> StateT position >>= (throwError . mkParseError "(WOpCon o)" "not match pattern: " "" d290_287 ["hsw"])
                                              let (WOpCon o) = t784_288
                                              return ()
                                              d291_289 <- get
                                              t785_290 <- StateT hsw
                                              case t785_290 of
                                                  WCParen -> return ()
                                                  _ -> StateT position >>= (throwError . mkParseError "WCParen" "not match pattern: " "" d291_289 ["hsw"])
                                              let WCParen = t785_290
                                              return ()
                                              ps <- StateT pats
                                              lift (lift (return $ ConP (mkName o) ps)),
                                           do p <- StateT pat1
                                              lift (lift (return p))]
                pat1102_106 = foldl1 mplus [do d294_291 <- get
                                               t788_292 <- StateT hsw
                                               case t788_292 of
                                                   (WVar [] _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WVar [] n)" "not match pattern: " "" d294_291 ["hsw"])
                                               let (WVar [] n) = t788_292
                                               return ()
                                               d295_293 <- get
                                               t789_294 <- StateT hsw
                                               case t789_294 of
                                                   (WOp "@") -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WOp \"@\")" "not match pattern: " "" d295_293 ["hsw"])
                                               let (WOp "@") = t789_294
                                               return ()
                                               p <- StateT pat1
                                               lift (lift (return $ AsP (mkName n) p)),
                                            do d297_295 <- get
                                               t791_296 <- StateT hsw
                                               case t791_296 of
                                                   (WType [] _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WType [] t)" "not match pattern: " "" d297_295 ["hsw"])
                                               let (WType [] t) = t791_296
                                               return ()
                                               lift (lift (return $ ConP (mkName t) [])),
                                            do d298_297 <- get
                                               t792_298 <- StateT hsw
                                               case t792_298 of
                                                   (WVar [] "_") -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WVar [] \"_\")" "not match pattern: " "" d298_297 ["hsw"])
                                               let (WVar [] "_") = t792_298
                                               return ()
                                               lift (lift (return WildP)),
                                            do d299_299 <- get
                                               t793_300 <- StateT hsw
                                               case t793_300 of
                                                   (WVar [] _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WVar [] n)" "not match pattern: " "" d299_299 ["hsw"])
                                               let (WVar [] n) = t793_300
                                               return ()
                                               lift (lift (return $ VarP $ mkName n)),
                                            do d300_301 <- get
                                               t794_302 <- StateT hsw
                                               case t794_302 of
                                                   (WInteger _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WInteger i)" "not match pattern: " "" d300_301 ["hsw"])
                                               let (WInteger i) = t794_302
                                               return ()
                                               lift (lift (return $ LitP (IntegerL i))),
                                            do d301_303 <- get
                                               t795_304 <- StateT hsw
                                               case t795_304 of
                                                   (WOp "-") -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WOp \"-\")" "not match pattern: " "" d301_303 ["hsw"])
                                               let (WOp "-") = t795_304
                                               return ()
                                               d302_305 <- get
                                               t796_306 <- StateT hsw
                                               case t796_306 of
                                                   (WInteger _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WInteger i)" "not match pattern: " "" d302_305 ["hsw"])
                                               let (WInteger i) = t796_306
                                               return ()
                                               lift (lift (return $ LitP (IntegerL $ negate i))),
                                            do d303_307 <- get
                                               t797_308 <- StateT hsw
                                               case t797_308 of
                                                   (WChar _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WChar c)" "not match pattern: " "" d303_307 ["hsw"])
                                               let (WChar c) = t797_308
                                               return ()
                                               lift (lift (return $ LitP $ CharL c)),
                                            do d304_309 <- get
                                               t798_310 <- StateT hsw
                                               case t798_310 of
                                                   (WString _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WString s)" "not match pattern: " "" d304_309 ["hsw"])
                                               let (WString s) = t798_310
                                               return ()
                                               lift (lift (return $ LitP $ StringL s)),
                                            do d305_311 <- get
                                               t799_312 <- StateT hsw
                                               case t799_312 of
                                                   WOParen -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WOParen" "not match pattern: " "" d305_311 ["hsw"])
                                               let WOParen = t799_312
                                               return ()
                                               p <- StateT patList
                                               d307_313 <- get
                                               t801_314 <- StateT hsw
                                               case t801_314 of
                                                   WCParen -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WCParen" "not match pattern: " "" d307_313 ["hsw"])
                                               let WCParen = t801_314
                                               return ()
                                               lift (lift (return $ TupP p)),
                                            do d308_315 <- get
                                               t802_316 <- StateT hsw
                                               case t802_316 of
                                                   WOBracket -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WOBracket" "not match pattern: " "" d308_315 ["hsw"])
                                               let WOBracket = t802_316
                                               return ()
                                               p <- StateT patList
                                               d310_317 <- get
                                               t804_318 <- StateT hsw
                                               case t804_318 of
                                                   WCBracket -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WCBracket" "not match pattern: " "" d310_317 ["hsw"])
                                               let WCBracket = t804_318
                                               return ()
                                               lift (lift (return $ ListP p)),
                                            do d311_319 <- get
                                               t805_320 <- StateT hsw
                                               case t805_320 of
                                                   WOParen -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WOParen" "not match pattern: " "" d311_319 ["hsw"])
                                               let WOParen = t805_320
                                               return ()
                                               p <- StateT pat
                                               d313_321 <- get
                                               t807_322 <- StateT hsw
                                               case t807_322 of
                                                   WCParen -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WCParen" "not match pattern: " "" d313_321 ["hsw"])
                                               let WCParen = t807_322
                                               return ()
                                               lift (lift (return p)),
                                            do d314_323 <- get
                                               t808_324 <- StateT hsw
                                               case t808_324 of
                                                   (WOp "!") -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WOp \"!\")" "not match pattern: " "" d314_323 ["hsw"])
                                               let (WOp "!") = t808_324
                                               return ()
                                               p <- StateT pat
                                               lift (lift (return $ BangP p))]
                patList103_107 = foldl1 mplus [do p <- StateT patOp
                                                  d317_325 <- get
                                                  t811_326 <- StateT hsw
                                                  case t811_326 of
                                                      WComma -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WComma" "not match pattern: " "" d317_325 ["hsw"])
                                                  let WComma = t811_326
                                                  return ()
                                                  ps <- StateT patList
                                                  lift (lift (return $ p : ps)),
                                               do p <- StateT patOp
                                                  lift (lift (return [p])),
                                               lift (lift (return []))]
                pats104_108 = foldl1 mplus [do p <- StateT pat
                                               ps <- StateT pats
                                               lift (lift (return $ p : ps)),
                                            lift (lift (return []))]
                charLitLs105_109 = foldl1 mplus [do d322_327 <- get
                                                    t816_328 <- StateT hsw
                                                    case t816_328 of
                                                        (WChar _) -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "(WChar c)" "not match pattern: " "" d322_327 ["hsw"])
                                                    let (WChar c) = t816_328
                                                    return ()
                                                    d323_329 <- get
                                                    t817_330 <- StateT wrd
                                                    case t817_330 of
                                                        WAsterisk -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "WAsterisk" "not match pattern: " "" d323_329 ["wrd"])
                                                    let WAsterisk = t817_330
                                                    return ()
                                                    lift (lift (return $ Right (c, List))),
                                                 do d324_331 <- get
                                                    t818_332 <- StateT hsw
                                                    case t818_332 of
                                                        (WChar _) -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "(WChar c)" "not match pattern: " "" d324_331 ["hsw"])
                                                    let (WChar c) = t818_332
                                                    return ()
                                                    d325_333 <- get
                                                    t819_334 <- StateT wrd
                                                    case t819_334 of
                                                        WPlus -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "WPlus" "not match pattern: " "" d325_333 ["wrd"])
                                                    let WPlus = t819_334
                                                    return ()
                                                    lift (lift (return $ Right (c, List1))),
                                                 do d326_335 <- get
                                                    t820_336 <- StateT hsw
                                                    case t820_336 of
                                                        (WChar _) -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "(WChar c)" "not match pattern: " "" d326_335 ["hsw"])
                                                    let (WChar c) = t820_336
                                                    return ()
                                                    d327_337 <- get
                                                    t821_338 <- StateT wrd
                                                    case t821_338 of
                                                        WQuestion -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "WQuestion" "not match pattern: " "" d327_337 ["wrd"])
                                                    let WQuestion = t821_338
                                                    return ()
                                                    lift (lift (return $ Right (c, Optional)))]
                readFromLs106_110 = foldl1 mplus [do rf <- StateT readFrom
                                                     d329_339 <- get
                                                     t823_340 <- StateT wrd
                                                     case t823_340 of
                                                         WAsterisk -> return ()
                                                         _ -> StateT position >>= (throwError . mkParseError "WAsterisk" "not match pattern: " "" d329_339 ["wrd"])
                                                     let WAsterisk = t823_340
                                                     return ()
                                                     lift (lift (return $ FromL List rf)),
                                                  do rf <- StateT readFrom
                                                     d331_341 <- get
                                                     t825_342 <- StateT wrd
                                                     case t825_342 of
                                                         WPlus -> return ()
                                                         _ -> StateT position >>= (throwError . mkParseError "WPlus" "not match pattern: " "" d331_341 ["wrd"])
                                                     let WPlus = t825_342
                                                     return ()
                                                     lift (lift (return $ FromL List1 rf)),
                                                  do rf <- StateT readFrom
                                                     d333_343 <- get
                                                     t827_344 <- StateT wrd
                                                     case t827_344 of
                                                         WQuestion -> return ()
                                                         _ -> StateT position >>= (throwError . mkParseError "WQuestion" "not match pattern: " "" d333_343 ["wrd"])
                                                     let WQuestion = t827_344
                                                     return ()
                                                     lift (lift (return $ FromL Optional rf)),
                                                  do rf <- StateT readFrom
                                                     lift (lift (return rf))]
                readFrom107_111 = foldl1 mplus [do d335_345 <- get
                                                   t829_346 <- StateT wrd
                                                   case t829_346 of
                                                       (WSymbol _) -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "(WSymbol v)" "not match pattern: " "" d335_345 ["wrd"])
                                                   let (WSymbol v) = t829_346
                                                   return ()
                                                   lift (lift (return $ FromVariable $ Just v)),
                                                do d336_347 <- get
                                                   t830_348 <- StateT wrd
                                                   case t830_348 of
                                                       WOParen -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WOParen" "not match pattern: " "" d336_347 ["wrd"])
                                                   let WOParen = t830_348
                                                   return ()
                                                   s <- StateT selection
                                                   d338_349 <- get
                                                   t832_350 <- StateT wrd
                                                   case t832_350 of
                                                       WCParen -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WCParen" "not match pattern: " "" d338_349 ["wrd"])
                                                   let WCParen = t832_350
                                                   return ()
                                                   lift (lift (return $ FromSelection s)),
                                                do e <- StateT expressionHsSugar'
                                                   lift (lift (return $ FromSelection [e]))]
                test108_112 = foldl1 mplus [do d340_351 <- get
                                               t834_352 <- StateT wrd
                                               case t834_352 of
                                                   WOBracket -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WOBracket" "not match pattern: " "" d340_351 ["wrd"])
                                               let WOBracket = t834_352
                                               return ()
                                               h <- StateT hsExpLam
                                               com <- optional141_206 (StateT comForErr)
                                               d343_353 <- get
                                               t837_354 <- StateT wrd
                                               case t837_354 of
                                                   WCBracket -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WCBracket" "not match pattern: " "" d343_353 ["wrd"])
                                               let WCBracket = t837_354
                                               return ()
                                               lift (lift (return (h, maybe "" id com)))]
                hsExpLam109_113 = foldl1 mplus [do d344_355 <- get
                                                   t838_356 <- StateT hsw
                                                   case t838_356 of
                                                       (WOp "\\") -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "(WOp \"\\\\\")" "not match pattern: " "" d344_355 ["hsw"])
                                                   let (WOp "\\") = t838_356
                                                   return ()
                                                   ps <- StateT pats
                                                   d346_357 <- get
                                                   t840_358 <- StateT hsw
                                                   case t840_358 of
                                                       WRightArrow -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WRightArrow" "not match pattern: " "" d346_357 ["hsw"])
                                                   let WRightArrow = t840_358
                                                   return ()
                                                   e <- StateT hsExpTyp
                                                   lift (lift (return $ LamE ps e)),
                                                do e <- StateT hsExpTyp
                                                   lift (lift (return e))]
                hsExpTyp110_114 = foldl1 mplus [do eo <- StateT hsExpOp
                                                   d350_359 <- get
                                                   t844_360 <- StateT hsw
                                                   case t844_360 of
                                                       WTypeDef -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WTypeDef" "not match pattern: " "" d350_359 ["hsw"])
                                                   let WTypeDef = t844_360
                                                   return ()
                                                   t <- StateT hsTypeArr
                                                   lift (lift (return $ SigE eo t)),
                                                do eo <- StateT hsExpOp
                                                   lift (lift (return eo))]
                hsExpOp111_115 = foldl1 mplus [do l <- StateT hsExp
                                                  o <- StateT hsOp
                                                  r <- StateT hsExpOp
                                                  lift (lift (return $ UInfixE (l id) o r)),
                                               do e <- StateT hsExp
                                                  lift (lift (return $ e id))]
                hsOp112_116 = foldl1 mplus [do d357_361 <- get
                                               t851_362 <- StateT hsw
                                               case t851_362 of
                                                   (WOp _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WOp op)" "not match pattern: " "" d357_361 ["hsw"])
                                               let (WOp op) = t851_362
                                               return ()
                                               lift (lift (return $ VarE $ mkName op)),
                                            do d358_363 <- get
                                               t852_364 <- StateT hsw
                                               case t852_364 of
                                                   (WOpCon _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WOpCon op)" "not match pattern: " "" d358_363 ["hsw"])
                                               let (WOpCon op) = t852_364
                                               return ()
                                               lift (lift (return $ ConE $ mkName op)),
                                            do d359_365 <- get
                                               t853_366 <- StateT hsw
                                               case t853_366 of
                                                   WBackQuote -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d359_365 ["hsw"])
                                               let WBackQuote = t853_366
                                               return ()
                                               d360_367 <- get
                                               t854_368 <- StateT hsw
                                               case t854_368 of
                                                   (WVar [] _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WVar [] v)" "not match pattern: " "" d360_367 ["hsw"])
                                               let (WVar [] v) = t854_368
                                               return ()
                                               d361_369 <- get
                                               t855_370 <- StateT hsw
                                               case t855_370 of
                                                   WBackQuote -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d361_369 ["hsw"])
                                               let WBackQuote = t855_370
                                               return ()
                                               lift (lift (return $ VarE $ mkName v)),
                                            do d362_371 <- get
                                               t856_372 <- StateT hsw
                                               case t856_372 of
                                                   WBackQuote -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d362_371 ["hsw"])
                                               let WBackQuote = t856_372
                                               return ()
                                               d363_373 <- get
                                               t857_374 <- StateT hsw
                                               case t857_374 of
                                                   (WType [] _) -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "(WType [] t)" "not match pattern: " "" d363_373 ["hsw"])
                                               let (WType [] t) = t857_374
                                               return ()
                                               d364_375 <- get
                                               t858_376 <- StateT hsw
                                               case t858_376 of
                                                   WBackQuote -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d364_375 ["hsw"])
                                               let WBackQuote = t858_376
                                               return ()
                                               lift (lift (return $ ConE $ mkName t))]
                hsExpLam'113_117 = foldl1 mplus [do d365_377 <- get
                                                    t859_378 <- StateT hsw
                                                    case t859_378 of
                                                        (WOp "\\") -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "(WOp \"\\\\\")" "not match pattern: " "" d365_377 ["hsw"])
                                                    let (WOp "\\") = t859_378
                                                    return ()
                                                    ps <- StateT pats
                                                    d367_379 <- get
                                                    t861_380 <- StateT hsw
                                                    case t861_380 of
                                                        WRightArrow -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "WRightArrow" "not match pattern: " "" d367_379 ["hsw"])
                                                    let WRightArrow = t861_380
                                                    return ()
                                                    e <- StateT hsExpTyp'
                                                    lift (lift (return $ LamE ps e)),
                                                 do e <- StateT hsExpTyp'
                                                    lift (lift (return e))]
                hsExpTyp'114_118 = foldl1 mplus [do eo <- StateT hsExpOp'
                                                    d371_381 <- get
                                                    t865_382 <- StateT hsw
                                                    case t865_382 of
                                                        WTypeDef -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "WTypeDef" "not match pattern: " "" d371_381 ["hsw"])
                                                    let WTypeDef = t865_382
                                                    return ()
                                                    t <- StateT hsTypeArr
                                                    lift (lift (return $ SigE eo t)),
                                                 do eo <- StateT hsExpOp'
                                                    lift (lift (return eo))]
                hsExpOp'115_119 = foldl1 mplus [do l <- StateT hsExp
                                                   o <- StateT hsOp'
                                                   r <- StateT hsExpOp'
                                                   lift (lift (return $ UInfixE (l id) o r)),
                                                do e <- StateT hsExp
                                                   lift (lift (return $ e id))]
                hsOp'116_120 = foldl1 mplus [do d378_383 <- get
                                                t872_384 <- StateT hsw
                                                case t872_384 of
                                                    (WOp _) -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "(WOp op)" "not match pattern: " "" d378_383 ["hsw"])
                                                let (WOp op) = t872_384
                                                return ()
                                                b1357_385 <- lift (lift (return $ head op /= '>'))
                                                unless b1357_385 (StateT position >>= (throwError . mkParseError "return $ head op /= '>'" "not match: " "" d378_383 ["hsw"]))
                                                lift (lift (return $ VarE $ mkName op)),
                                             do d379_386 <- get
                                                t873_387 <- StateT hsw
                                                case t873_387 of
                                                    (WOpCon _) -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "(WOpCon op)" "not match pattern: " "" d379_386 ["hsw"])
                                                let (WOpCon op) = t873_387
                                                return ()
                                                lift (lift (return $ ConE $ mkName op)),
                                             do d380_388 <- get
                                                t874_389 <- StateT hsw
                                                case t874_389 of
                                                    WBackQuote -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d380_388 ["hsw"])
                                                let WBackQuote = t874_389
                                                return ()
                                                v <- StateT var
                                                d382_390 <- get
                                                t876_391 <- StateT hsw
                                                case t876_391 of
                                                    WBackQuote -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d382_390 ["hsw"])
                                                let WBackQuote = t876_391
                                                return ()
                                                lift (lift (return $ VarE $ mkName v)),
                                             do d383_392 <- get
                                                t877_393 <- StateT hsw
                                                case t877_393 of
                                                    WBackQuote -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d383_392 ["hsw"])
                                                let WBackQuote = t877_393
                                                return ()
                                                t <- StateT typ
                                                d385_394 <- get
                                                t879_395 <- StateT hsw
                                                case t879_395 of
                                                    WBackQuote -> return ()
                                                    _ -> StateT position >>= (throwError . mkParseError "WBackQuote" "not match pattern: " "" d385_394 ["hsw"])
                                                let WBackQuote = t879_395
                                                return ()
                                                lift (lift (return $ ConE $ mkName t))]
                hsExp117_121 = foldl1 mplus [do e <- StateT hsExp1
                                                h <- StateT hsExp
                                                lift (lift (return (\f -> h (f e `AppE`)))),
                                             do e <- StateT hsExp1
                                                lift (lift (return (\f -> f e)))]
                hsExp1118_122 = foldl1 mplus [do d389_396 <- get
                                                 t883_397 <- StateT hsw
                                                 case t883_397 of
                                                     WOParen -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WOParen" "not match pattern: " "" d389_396 ["hsw"])
                                                 let WOParen = t883_397
                                                 return ()
                                                 l <- optional141_206 (StateT hsExpOp)
                                                 o <- StateT hsOp
                                                 r <- optional141_206 (StateT hsExpOp)
                                                 d393_398 <- get
                                                 t887_399 <- StateT hsw
                                                 case t887_399 of
                                                     WCParen -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WCParen" "not match pattern: " "" d393_398 ["hsw"])
                                                 let WCParen = t887_399
                                                 return ()
                                                 lift (lift (return $ InfixE l o r)),
                                              do d394_400 <- get
                                                 t888_401 <- StateT hsw
                                                 case t888_401 of
                                                     WOParen -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WOParen" "not match pattern: " "" d394_400 ["hsw"])
                                                 let WOParen = t888_401
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d396_402 <- get
                                                 t890_403 <- StateT hsw
                                                 case t890_403 of
                                                     WCParen -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WCParen" "not match pattern: " "" d396_402 ["hsw"])
                                                 let WCParen = t890_403
                                                 return ()
                                                 lift (lift (return $ case et of {[x] -> x; _ -> TupE $ fmap Just et})),
                                              do d397_404 <- get
                                                 t891_405 <- StateT hsw
                                                 case t891_405 of
                                                     WOBracket -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WOBracket" "not match pattern: " "" d397_404 ["hsw"])
                                                 let WOBracket = t891_405
                                                 return ()
                                                 et <- StateT hsExpTpl
                                                 d399_406 <- get
                                                 t893_407 <- StateT hsw
                                                 case t893_407 of
                                                     WCBracket -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WCBracket" "not match pattern: " "" d399_406 ["hsw"])
                                                 let WCBracket = t893_407
                                                 return ()
                                                 lift (lift (return $ ListE et)),
                                              do d400_408 <- get
                                                 t894_409 <- StateT hsw
                                                 case t894_409 of
                                                     WOBracket -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WOBracket" "not match pattern: " "" d400_408 ["hsw"])
                                                 let WOBracket = t894_409
                                                 return ()
                                                 b <- StateT hsExpOp
                                                 d402_410 <- get
                                                 t896_411 <- StateT hsw
                                                 case t896_411 of
                                                     WDotDot -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WDotDot" "not match pattern: " "" d402_410 ["hsw"])
                                                 let WDotDot = t896_411
                                                 return ()
                                                 e <- StateT hsExpOp
                                                 d404_412 <- get
                                                 t898_413 <- StateT hsw
                                                 case t898_413 of
                                                     WCBracket -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WCBracket" "not match pattern: " "" d404_412 ["hsw"])
                                                 let WCBracket = t898_413
                                                 return ()
                                                 lift (lift (return $ ArithSeqE $ FromToR b e)),
                                              do d405_414 <- get
                                                 t899_415 <- StateT hsw
                                                 case t899_415 of
                                                     (WVar [] _) -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "(WVar [] v)" "not match pattern: " "" d405_414 ["hsw"])
                                                 let (WVar [] v) = t899_415
                                                 return ()
                                                 lift (lift (return $ VarE $ mkName v)),
                                              do d406_416 <- get
                                                 t900_417 <- StateT hsw
                                                 case t900_417 of
                                                     (WType [] _) -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "(WType [] t)" "not match pattern: " "" d406_416 ["hsw"])
                                                 let (WType [] t) = t900_417
                                                 return ()
                                                 lift (lift (return $ ConE $ mkName t)),
                                              do d407_418 <- get
                                                 t901_419 <- StateT hsw
                                                 case t901_419 of
                                                     (WInteger _) -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "(WInteger i)" "not match pattern: " "" d407_418 ["hsw"])
                                                 let (WInteger i) = t901_419
                                                 return ()
                                                 lift (lift (return $ LitE $ integerL i)),
                                              do d408_420 <- get
                                                 t902_421 <- StateT hsw
                                                 case t902_421 of
                                                     (WChar _) -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "(WChar c)" "not match pattern: " "" d408_420 ["hsw"])
                                                 let (WChar c) = t902_421
                                                 return ()
                                                 lift (lift (return $ LitE $ charL c)),
                                              do d409_422 <- get
                                                 t903_423 <- StateT hsw
                                                 case t903_423 of
                                                     (WString _) -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "(WString s)" "not match pattern: " "" d409_422 ["hsw"])
                                                 let (WString s) = t903_423
                                                 return ()
                                                 lift (lift (return $ LitE $ stringL s)),
                                              do d410_424 <- get
                                                 t904_425 <- StateT hsw
                                                 case t904_425 of
                                                     (WOp "-") -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "(WOp \"-\")" "not match pattern: " "" d410_424 ["hsw"])
                                                 let (WOp "-") = t904_425
                                                 return ()
                                                 e <- StateT hsExp1
                                                 lift (lift (return $ AppE (VarE $ mkName "negate") e)),
                                              do d412_426 <- get
                                                 t906_427 <- StateT hsw
                                                 case t906_427 of
                                                     WIf -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WIf" "not match pattern: " "" d412_426 ["hsw"])
                                                 let WIf = t906_427
                                                 return ()
                                                 p <- StateT hsExpLam
                                                 d414_428 <- get
                                                 t908_429 <- StateT hsw
                                                 case t908_429 of
                                                     WThen -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WThen" "not match pattern: " "" d414_428 ["hsw"])
                                                 let WThen = t908_429
                                                 return ()
                                                 t <- StateT hsExpLam
                                                 d416_430 <- get
                                                 t910_431 <- StateT hsw
                                                 case t910_431 of
                                                     WElse -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WElse" "not match pattern: " "" d416_430 ["hsw"])
                                                 let WElse = t910_431
                                                 return ()
                                                 e <- StateT hsExpLam
                                                 lift (lift (return $ CondE p t e)),
                                              do d418_432 <- get
                                                 t912_433 <- StateT hsw
                                                 case t912_433 of
                                                     WLet -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WLet" "not match pattern: " "" d418_432 ["hsw"])
                                                 let WLet = t912_433
                                                 return ()
                                                 d419_434 <- get
                                                 t913_435 <- StateT hsw
                                                 case t913_435 of
                                                     (WVar [] _) -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "(WVar [] v)" "not match pattern: " "" d419_434 ["hsw"])
                                                 let (WVar [] v) = t913_435
                                                 return ()
                                                 d420_436 <- get
                                                 t914_437 <- StateT hsw
                                                 case t914_437 of
                                                     (WOp "=") -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "(WOp \"=\")" "not match pattern: " "" d420_436 ["hsw"])
                                                 let (WOp "=") = t914_437
                                                 return ()
                                                 e <- StateT hsExpLam
                                                 d422_438 <- get
                                                 t916_439 <- StateT hsw
                                                 case t916_439 of
                                                     WIn -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "WIn" "not match pattern: " "" d422_438 ["hsw"])
                                                 let WIn = t916_439
                                                 return ()
                                                 b <- StateT hsExpLam
                                                 lift (lift (return $ LetE [ValD (VarP $ mkName v) (NormalB e) []] b))]
                hsExpTpl119_123 = foldl1 mplus [do e <- StateT hsExpLam
                                                   d425_440 <- get
                                                   t919_441 <- StateT hsw
                                                   case t919_441 of
                                                       WComma -> return ()
                                                       _ -> StateT position >>= (throwError . mkParseError "WComma" "not match pattern: " "" d425_440 ["hsw"])
                                                   let WComma = t919_441
                                                   return ()
                                                   et <- StateT hsExpTpl
                                                   lift (lift (return $ e : et)),
                                                do e <- StateT hsExpLam
                                                   lift (lift (return [e])),
                                                lift (lift (return []))]
                hsTypeArr120_124 = foldl1 mplus [do l <- StateT hsType
                                                    d429_442 <- get
                                                    t923_443 <- StateT hsw
                                                    case t923_443 of
                                                        WRightArrow -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "WRightArrow" "not match pattern: " "" d429_442 ["hsw"])
                                                    let WRightArrow = t923_443
                                                    return ()
                                                    r <- StateT hsTypeArr
                                                    lift (lift (return $ AppT (AppT ArrowT $ l id) r)),
                                                 do t <- StateT hsType
                                                    lift (lift (return $ t id))]
                hsType121_125 = foldl1 mplus [do t <- StateT hsType1
                                                 ts <- StateT hsType
                                                 lift (lift (return $ (\f -> ts (f t `AppT`)))),
                                              do t <- StateT hsType1
                                                 lift (lift (return ($ t)))]
                hsType1122_126 = foldl1 mplus [do d435_444 <- get
                                                  t929_445 <- StateT hsw
                                                  case t929_445 of
                                                      WOBracket -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WOBracket" "not match pattern: " "" d435_444 ["hsw"])
                                                  let WOBracket = t929_445
                                                  return ()
                                                  d436_446 <- get
                                                  t930_447 <- StateT hsw
                                                  case t930_447 of
                                                      WCBracket -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WCBracket" "not match pattern: " "" d436_446 ["hsw"])
                                                  let WCBracket = t930_447
                                                  return ()
                                                  lift (lift (return ListT)),
                                               do d437_448 <- get
                                                  t931_449 <- StateT hsw
                                                  case t931_449 of
                                                      WOBracket -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WOBracket" "not match pattern: " "" d437_448 ["hsw"])
                                                  let WOBracket = t931_449
                                                  return ()
                                                  t <- StateT hsTypeArr
                                                  d439_450 <- get
                                                  t933_451 <- StateT hsw
                                                  case t933_451 of
                                                      WCBracket -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WCBracket" "not match pattern: " "" d439_450 ["hsw"])
                                                  let WCBracket = t933_451
                                                  return ()
                                                  lift (lift (return $ ListT `AppT` t)),
                                               do d440_452 <- get
                                                  t934_453 <- StateT hsw
                                                  case t934_453 of
                                                      WOParen -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WOParen" "not match pattern: " "" d440_452 ["hsw"])
                                                  let WOParen = t934_453
                                                  return ()
                                                  tt <- StateT hsTypeTpl
                                                  d442_454 <- get
                                                  t936_455 <- StateT hsw
                                                  case t936_455 of
                                                      WCParen -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WCParen" "not match pattern: " "" d442_454 ["hsw"])
                                                  let WCParen = t936_455
                                                  return ()
                                                  lift (lift (return $ foldl AppT (TupleT $ length tt) tt)),
                                               do d443_456 <- get
                                                  t937_457 <- StateT hsw
                                                  case t937_457 of
                                                      (WType [] _) -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "(WType [] t)" "not match pattern: " "" d443_456 ["hsw"])
                                                  let (WType [] t) = t937_457
                                                  return ()
                                                  lift (lift (return $ ConT $ mkName t)),
                                               do d444_458 <- get
                                                  t938_459 <- StateT hsw
                                                  case t938_459 of
                                                      WOParen -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WOParen" "not match pattern: " "" d444_458 ["hsw"])
                                                  let WOParen = t938_459
                                                  return ()
                                                  d445_460 <- get
                                                  t939_461 <- StateT hsw
                                                  case t939_461 of
                                                      WRightArrow -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WRightArrow" "not match pattern: " "" d445_460 ["hsw"])
                                                  let WRightArrow = t939_461
                                                  return ()
                                                  d446_462 <- get
                                                  t940_463 <- StateT hsw
                                                  case t940_463 of
                                                      WCParen -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "WCParen" "not match pattern: " "" d446_462 ["hsw"])
                                                  let WCParen = t940_463
                                                  return ()
                                                  lift (lift (return ArrowT))]
                hsTypeTpl123_127 = foldl1 mplus [do t <- StateT hsTypeArr
                                                    d448_464 <- get
                                                    t942_465 <- StateT hsw
                                                    case t942_465 of
                                                        WComma -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "WComma" "not match pattern: " "" d448_464 ["hsw"])
                                                    let WComma = t942_465
                                                    return ()
                                                    tt <- StateT hsTypeTpl
                                                    lift (lift (return $ t : tt)),
                                                 do t <- StateT hsTypeArr
                                                    lift (lift (return [t])),
                                                 lift (lift (return []))]
                wrd124_128 = foldl1 mplus [do w <- StateT word
                                              _ <- list142_147 (StateT space)
                                              return ()
                                              lift (lift (return w)),
                                           do _ <- StateT semiColon
                                              return ()
                                              _ <- list142_147 (StateT space')
                                              return ()
                                              lift (lift (return WSemiColon))]
                hsw125_129 = foldl1 mplus [do w <- StateT hsWord
                                              _ <- list142_147 (StateT space)
                                              return ()
                                              lift (lift (return w))]
                word126_130 = foldl1 mplus [do v <- list1143_466 (foldl1 mplus [do d458_467 <- get
                                                                                   t952_468 <- StateT char
                                                                                   let c1616_145 = t952_468
                                                                                   b1437_469 <- return (((||) <$> isAlphaNum <*> (`elem` "_'")) c1616_145)
                                                                                   unless b1437_469 (StateT position >>= (throwError . mkParseError "((||) <$> isAlphaNum <*> (`elem` \"_'\")) c1616_0" "not match: " "" d458_467 ["char"]))
                                                                                   return c1616_145])
                                               lift (lift (return $ WSymbol v)),
                                            do d459_470 <- get
                                               t953_471 <- StateT char
                                               case t953_471 of
                                                   '!' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'!'" "not match pattern: " "" d459_470 ["char"])
                                               let '!' = t953_471
                                               return ()
                                               lift (lift (return WBang)),
                                            do d460_472 <- get
                                               t954_473 <- StateT char
                                               case t954_473 of
                                                   '&' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'&'" "not match pattern: " "" d460_472 ["char"])
                                               let '&' = t954_473
                                               return ()
                                               lift (lift (return WAmp)),
                                            do d461_474 <- get
                                               t955_475 <- StateT char
                                               case t955_475 of
                                                   '*' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'*'" "not match pattern: " "" d461_474 ["char"])
                                               let '*' = t955_475
                                               return ()
                                               lift (lift (return WAsterisk)),
                                            do d462_476 <- get
                                               t956_477 <- StateT char
                                               case t956_477 of
                                                   '+' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'+'" "not match pattern: " "" d462_476 ["char"])
                                               let '+' = t956_477
                                               return ()
                                               lift (lift (return WPlus)),
                                            do d463_478 <- get
                                               t957_479 <- StateT char
                                               case t957_479 of
                                                   '?' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'?'" "not match pattern: " "" d463_478 ["char"])
                                               let '?' = t957_479
                                               return ()
                                               lift (lift (return WQuestion)),
                                            do d464_480 <- get
                                               t958_481 <- StateT char
                                               case t958_481 of
                                                   '=' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'='" "not match pattern: " "" d464_480 ["char"])
                                               let '=' = t958_481
                                               return ()
                                               lift (lift (return WEqual)),
                                            do d465_482 <- get
                                               t959_483 <- StateT char
                                               case t959_483 of
                                                   '/' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'/'" "not match pattern: " "" d465_482 ["char"])
                                               let '/' = t959_483
                                               return ()
                                               lift (lift (return WSlash)),
                                            do d466_484 <- get
                                               t960_485 <- StateT char
                                               case t960_485 of
                                                   ':' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "':'" "not match pattern: " "" d466_484 ["char"])
                                               let ':' = t960_485
                                               return ()
                                               lift (lift (return WColon)),
                                            do d467_486 <- get
                                               t961_487 <- StateT char
                                               case t961_487 of
                                                   '<' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'<'" "not match pattern: " "" d467_486 ["char"])
                                               let '<' = t961_487
                                               return ()
                                               lift (lift (return WLT)),
                                            do d468_488 <- get
                                               t962_489 <- StateT char
                                               case t962_489 of
                                                   '>' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d468_488 ["char"])
                                               let '>' = t962_489
                                               return ()
                                               lift (lift (return WGT)),
                                            do b <- StateT bras
                                               lift (lift (return b))]
                hsWord127_131 = foldl1 mplus [do d470_490 <- get
                                                 t964_491 <- StateT char
                                                 case t964_491 of
                                                     'm' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d470_490 ["char"])
                                                 let 'm' = t964_491
                                                 return ()
                                                 d471_492 <- get
                                                 t965_493 <- StateT char
                                                 case t965_493 of
                                                     'o' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d471_492 ["char"])
                                                 let 'o' = t965_493
                                                 return ()
                                                 d472_494 <- get
                                                 t966_495 <- StateT char
                                                 case t966_495 of
                                                     'd' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'d'" "not match pattern: " "" d472_494 ["char"])
                                                 let 'd' = t966_495
                                                 return ()
                                                 d473_496 <- get
                                                 t967_497 <- StateT char
                                                 case t967_497 of
                                                     'u' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'u'" "not match pattern: " "" d473_496 ["char"])
                                                 let 'u' = t967_497
                                                 return ()
                                                 d474_498 <- get
                                                 t968_499 <- StateT char
                                                 case t968_499 of
                                                     'l' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d474_498 ["char"])
                                                 let 'l' = t968_499
                                                 return ()
                                                 d475_500 <- get
                                                 t969_501 <- StateT char
                                                 case t969_501 of
                                                     'e' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d475_500 ["char"])
                                                 let 'e' = t969_501
                                                 return ()
                                                 lift (lift (return $ WModule)),
                                              do d476_502 <- get
                                                 t970_503 <- StateT char
                                                 case t970_503 of
                                                     'w' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'w'" "not match pattern: " "" d476_502 ["char"])
                                                 let 'w' = t970_503
                                                 return ()
                                                 d477_504 <- get
                                                 t971_505 <- StateT char
                                                 case t971_505 of
                                                     'h' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d477_504 ["char"])
                                                 let 'h' = t971_505
                                                 return ()
                                                 d478_506 <- get
                                                 t972_507 <- StateT char
                                                 case t972_507 of
                                                     'e' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d478_506 ["char"])
                                                 let 'e' = t972_507
                                                 return ()
                                                 d479_508 <- get
                                                 t973_509 <- StateT char
                                                 case t973_509 of
                                                     'r' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d479_508 ["char"])
                                                 let 'r' = t973_509
                                                 return ()
                                                 d480_510 <- get
                                                 t974_511 <- StateT char
                                                 case t974_511 of
                                                     'e' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d480_510 ["char"])
                                                 let 'e' = t974_511
                                                 return ()
                                                 lift (lift (return $ WWhere)),
                                              do d481_512 <- get
                                                 t975_513 <- StateT char
                                                 case t975_513 of
                                                     'i' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d481_512 ["char"])
                                                 let 'i' = t975_513
                                                 return ()
                                                 d482_514 <- get
                                                 t976_515 <- StateT char
                                                 case t976_515 of
                                                     'm' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'m'" "not match pattern: " "" d482_514 ["char"])
                                                 let 'm' = t976_515
                                                 return ()
                                                 d483_516 <- get
                                                 t977_517 <- StateT char
                                                 case t977_517 of
                                                     'p' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'p'" "not match pattern: " "" d483_516 ["char"])
                                                 let 'p' = t977_517
                                                 return ()
                                                 d484_518 <- get
                                                 t978_519 <- StateT char
                                                 case t978_519 of
                                                     'o' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'o'" "not match pattern: " "" d484_518 ["char"])
                                                 let 'o' = t978_519
                                                 return ()
                                                 d485_520 <- get
                                                 t979_521 <- StateT char
                                                 case t979_521 of
                                                     'r' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d485_520 ["char"])
                                                 let 'r' = t979_521
                                                 return ()
                                                 d486_522 <- get
                                                 t980_523 <- StateT char
                                                 case t980_523 of
                                                     't' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d486_522 ["char"])
                                                 let 't' = t980_523
                                                 return ()
                                                 lift (lift (return $ WImport)),
                                              do d487_524 <- get
                                                 t981_525 <- StateT char
                                                 case t981_525 of
                                                     'i' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d487_524 ["char"])
                                                 let 'i' = t981_525
                                                 return ()
                                                 d488_526 <- get
                                                 t982_527 <- StateT char
                                                 case t982_527 of
                                                     'f' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'f'" "not match pattern: " "" d488_526 ["char"])
                                                 let 'f' = t982_527
                                                 return ()
                                                 lift (lift (return $ WIf)),
                                              do d489_528 <- get
                                                 t983_529 <- StateT char
                                                 case t983_529 of
                                                     't' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d489_528 ["char"])
                                                 let 't' = t983_529
                                                 return ()
                                                 d490_530 <- get
                                                 t984_531 <- StateT char
                                                 case t984_531 of
                                                     'h' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'h'" "not match pattern: " "" d490_530 ["char"])
                                                 let 'h' = t984_531
                                                 return ()
                                                 d491_532 <- get
                                                 t985_533 <- StateT char
                                                 case t985_533 of
                                                     'e' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d491_532 ["char"])
                                                 let 'e' = t985_533
                                                 return ()
                                                 d492_534 <- get
                                                 t986_535 <- StateT char
                                                 case t986_535 of
                                                     'n' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d492_534 ["char"])
                                                 let 'n' = t986_535
                                                 return ()
                                                 lift (lift (return $ WThen)),
                                              do d493_536 <- get
                                                 t987_537 <- StateT char
                                                 case t987_537 of
                                                     'e' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d493_536 ["char"])
                                                 let 'e' = t987_537
                                                 return ()
                                                 d494_538 <- get
                                                 t988_539 <- StateT char
                                                 case t988_539 of
                                                     'l' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d494_538 ["char"])
                                                 let 'l' = t988_539
                                                 return ()
                                                 d495_540 <- get
                                                 t989_541 <- StateT char
                                                 case t989_541 of
                                                     's' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'s'" "not match pattern: " "" d495_540 ["char"])
                                                 let 's' = t989_541
                                                 return ()
                                                 d496_542 <- get
                                                 t990_543 <- StateT char
                                                 case t990_543 of
                                                     'e' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d496_542 ["char"])
                                                 let 'e' = t990_543
                                                 return ()
                                                 lift (lift (return $ WElse)),
                                              do d497_544 <- get
                                                 t991_545 <- StateT char
                                                 case t991_545 of
                                                     'l' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'l'" "not match pattern: " "" d497_544 ["char"])
                                                 let 'l' = t991_545
                                                 return ()
                                                 d498_546 <- get
                                                 t992_547 <- StateT char
                                                 case t992_547 of
                                                     'e' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'e'" "not match pattern: " "" d498_546 ["char"])
                                                 let 'e' = t992_547
                                                 return ()
                                                 d499_548 <- get
                                                 t993_549 <- StateT char
                                                 case t993_549 of
                                                     't' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d499_548 ["char"])
                                                 let 't' = t993_549
                                                 return ()
                                                 lift (lift (return $ WLet)),
                                              do d500_550 <- get
                                                 t994_551 <- StateT char
                                                 case t994_551 of
                                                     'i' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'i'" "not match pattern: " "" d500_550 ["char"])
                                                 let 'i' = t994_551
                                                 return ()
                                                 d501_552 <- get
                                                 t995_553 <- StateT char
                                                 case t995_553 of
                                                     'n' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d501_552 ["char"])
                                                 let 'n' = t995_553
                                                 return ()
                                                 lift (lift (return $ WIn)),
                                              do d502_554 <- get
                                                 t996_555 <- StateT char
                                                 case t996_555 of
                                                     '[' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'['" "not match pattern: " "" d502_554 ["char"])
                                                 let '[' = t996_555
                                                 return ()
                                                 v <- StateT var
                                                 d504_556 <- get
                                                 t998_557 <- StateT char
                                                 case t998_557 of
                                                     '|' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d504_556 ["char"])
                                                 let '|' = t998_557
                                                 return ()
                                                 lift (lift (return $ WOQuasiQuote v)),
                                              do d505_558 <- get
                                                 t999_559 <- StateT char
                                                 case t999_559 of
                                                     '|' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'|'" "not match pattern: " "" d505_558 ["char"])
                                                 let '|' = t999_559
                                                 return ()
                                                 d506_560 <- get
                                                 t1000_561 <- StateT char
                                                 case t1000_561 of
                                                     ']' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "']'" "not match pattern: " "" d506_560 ["char"])
                                                 let ']' = t1000_561
                                                 return ()
                                                 lift (lift (return WCQuasiQuote)),
                                              do d507_562 <- get
                                                 t1001_563 <- StateT char
                                                 case t1001_563 of
                                                     ':' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "':'" "not match pattern: " "" d507_562 ["char"])
                                                 let ':' = t1001_563
                                                 return ()
                                                 d508_564 <- get
                                                 t1002_565 <- StateT char
                                                 case t1002_565 of
                                                     ':' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "':'" "not match pattern: " "" d508_564 ["char"])
                                                 let ':' = t1002_565
                                                 return ()
                                                 lift (lift (return WTypeDef)),
                                              do d509_566 <- get
                                                 t1003_567 <- StateT char
                                                 case t1003_567 of
                                                     '-' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d509_566 ["char"])
                                                 let '-' = t1003_567
                                                 return ()
                                                 d510_568 <- get
                                                 t1004_569 <- StateT char
                                                 case t1004_569 of
                                                     '>' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'>'" "not match pattern: " "" d510_568 ["char"])
                                                 let '>' = t1004_569
                                                 return ()
                                                 lift (lift (return WRightArrow)),
                                              do d511_570 <- get
                                                 t1005_571 <- StateT char
                                                 case t1005_571 of
                                                     '.' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d511_570 ["char"])
                                                 let '.' = t1005_571
                                                 return ()
                                                 d512_572 <- get
                                                 t1006_573 <- StateT char
                                                 case t1006_573 of
                                                     '.' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d512_572 ["char"])
                                                 let '.' = t1006_573
                                                 return ()
                                                 lift (lift (return WDotDot)),
                                              do d513_574 <- get
                                                 t1007_575 <- StateT char
                                                 case t1007_575 of
                                                     '{' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d513_574 ["char"])
                                                 let '{' = t1007_575
                                                 return ()
                                                 d514_576 <- get
                                                 t1008_577 <- StateT char
                                                 case t1008_577 of
                                                     '-' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d514_576 ["char"])
                                                 let '-' = t1008_577
                                                 return ()
                                                 d515_578 <- get
                                                 t1009_579 <- StateT char
                                                 case t1009_579 of
                                                     '#' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d515_578 ["char"])
                                                 let '#' = t1009_579
                                                 return ()
                                                 lift (lift (return WOComment)),
                                              do d516_580 <- get
                                                 t1010_581 <- StateT char
                                                 case t1010_581 of
                                                     '#' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d516_580 ["char"])
                                                 let '#' = t1010_581
                                                 return ()
                                                 d517_582 <- get
                                                 t1011_583 <- StateT char
                                                 case t1011_583 of
                                                     '-' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d517_582 ["char"])
                                                 let '-' = t1011_583
                                                 return ()
                                                 d518_584 <- get
                                                 t1012_585 <- StateT char
                                                 case t1012_585 of
                                                     '}' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d518_584 ["char"])
                                                 let '}' = t1012_585
                                                 return ()
                                                 lift (lift (return WCComment)),
                                              do m <- list142_147 (foldl1 mplus [do t <- StateT typ
                                                                                    d521_586 <- get
                                                                                    t1015_587 <- StateT char
                                                                                    case t1015_587 of
                                                                                        '.' -> return ()
                                                                                        _ -> StateT position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d521_586 ["char"])
                                                                                    let '.' = t1015_587
                                                                                    return ()
                                                                                    lift (lift (return t))])
                                                 t1 <- StateT typ
                                                 lift (lift (return $ WType m t1)),
                                              do m <- list142_147 (foldl1 mplus [do t <- StateT typ
                                                                                    d525_588 <- get
                                                                                    t1019_589 <- StateT char
                                                                                    case t1019_589 of
                                                                                        '.' -> return ()
                                                                                        _ -> StateT position >>= (throwError . mkParseError "'.'" "not match pattern: " "" d525_588 ["char"])
                                                                                    let '.' = t1019_589
                                                                                    return ()
                                                                                    lift (lift (return t))])
                                                 v1 <- StateT var
                                                 lift (lift (return $ WVar m v1)),
                                              do d527_590 <- get
                                                 t1021_591 <- StateT char
                                                 case t1021_591 of
                                                     ':' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "':'" "not match pattern: " "" d527_590 ["char"])
                                                 let ':' = t1021_591
                                                 return ()
                                                 w <- list142_147 (foldl1 mplus [do d529_592 <- get
                                                                                    t1023_593 <- StateT char
                                                                                    let c1617_594 = t1023_593
                                                                                    b1508_595 <- return ((`elem` opChars) c1617_594)
                                                                                    unless b1508_595 (StateT position >>= (throwError . mkParseError "(`elem` opChars) c1617_0" "not match: " "" d529_592 ["char"]))
                                                                                    return c1617_594])
                                                 lift (lift (return $ WOpCon $ ':' : w)),
                                              do w <- list1143_466 (foldl1 mplus [do d531_596 <- get
                                                                                     t1025_597 <- StateT char
                                                                                     let c1618_598 = t1025_597
                                                                                     b1510_599 <- return ((`elem` opChars) c1618_598)
                                                                                     unless b1510_599 (StateT position >>= (throwError . mkParseError "(`elem` opChars) c1618_0" "not match: " "" d531_596 ["char"]))
                                                                                     return c1618_598])
                                                 lift (lift (return $ WOp w)),
                                              do d532_600 <- get
                                                 t1026_601 <- StateT char
                                                 case t1026_601 of
                                                     '\'' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d532_600 ["char"])
                                                 let '\'' = t1026_601
                                                 return ()
                                                 c <- foldl1 mplus [do d534_602 <- get
                                                                       t1028_603 <- StateT char
                                                                       let c1619_604 = t1028_603
                                                                       b1513_605 <- return ((`notElem` "\\'") c1619_604)
                                                                       unless b1513_605 (StateT position >>= (throwError . mkParseError "(`notElem` \"\\\\'\") c1619_0" "not match: " "" d534_602 ["char"]))
                                                                       return c1619_604]
                                                 d535_606 <- get
                                                 t1029_607 <- StateT char
                                                 case t1029_607 of
                                                     '\'' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d535_606 ["char"])
                                                 let '\'' = t1029_607
                                                 return ()
                                                 lift (lift (return $ WChar c)),
                                              do d536_608 <- get
                                                 t1030_609 <- StateT char
                                                 case t1030_609 of
                                                     '\'' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d536_608 ["char"])
                                                 let '\'' = t1030_609
                                                 return ()
                                                 d537_610 <- get
                                                 t1031_611 <- StateT char
                                                 case t1031_611 of
                                                     '\\' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d537_610 ["char"])
                                                 let '\\' = t1031_611
                                                 return ()
                                                 c <- StateT escChar
                                                 d539_612 <- get
                                                 t1033_613 <- StateT char
                                                 case t1033_613 of
                                                     '\'' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d539_612 ["char"])
                                                 let '\'' = t1033_613
                                                 return ()
                                                 lift (lift (return $ WChar c)),
                                              do d540_614 <- get
                                                 t1034_615 <- StateT char
                                                 case t1034_615 of
                                                     '"' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d540_614 ["char"])
                                                 let '"' = t1034_615
                                                 return ()
                                                 s <- StateT strLit
                                                 d542_616 <- get
                                                 t1036_617 <- StateT char
                                                 case t1036_617 of
                                                     '"' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d542_616 ["char"])
                                                 let '"' = t1036_617
                                                 return ()
                                                 lift (lift (return $ WString s)),
                                              do d <- list1143_466 (foldl1 mplus [do d544_618 <- get
                                                                                     t1038_619 <- StateT char
                                                                                     let c1620_620 = t1038_619
                                                                                     b1523_621 <- return (isDigit c1620_620)
                                                                                     unless b1523_621 (StateT position >>= (throwError . mkParseError "isDigit c1620_0" "not match: " "" d544_618 ["char"]))
                                                                                     return c1620_620])
                                                 lift (lift (return $ WInteger $ read d)),
                                              do b <- StateT bras
                                                 lift (lift (return b)),
                                              do d546_622 <- get
                                                 t1040_623 <- StateT char
                                                 case t1040_623 of
                                                     ',' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "','" "not match pattern: " "" d546_622 ["char"])
                                                 let ',' = t1040_623
                                                 return ()
                                                 lift (lift (return WComma)),
                                              do d547_624 <- get
                                                 t1041_625 <- StateT char
                                                 case t1041_625 of
                                                     '`' -> return ()
                                                     _ -> StateT position >>= (throwError . mkParseError "'`'" "not match pattern: " "" d547_624 ["char"])
                                                 let '`' = t1041_625
                                                 return ()
                                                 lift (lift (return WBackQuote))]
                bras128_132 = foldl1 mplus [do d548_626 <- get
                                               t1042_627 <- StateT char
                                               case t1042_627 of
                                                   '[' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'['" "not match pattern: " "" d548_626 ["char"])
                                               let '[' = t1042_627
                                               return ()
                                               lift (lift (return WOBracket)),
                                            do d549_628 <- get
                                               t1043_629 <- StateT char
                                               case t1043_629 of
                                                   ']' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "']'" "not match pattern: " "" d549_628 ["char"])
                                               let ']' = t1043_629
                                               return ()
                                               lift (lift (return WCBracket)),
                                            do d550_630 <- get
                                               t1044_631 <- StateT char
                                               case t1044_631 of
                                                   '{' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d550_630 ["char"])
                                               let '{' = t1044_631
                                               return ()
                                               lift (lift (return WOBrace)),
                                            do d551_632 <- get
                                               t1045_633 <- StateT char
                                               case t1045_633 of
                                                   '}' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d551_632 ["char"])
                                               let '}' = t1045_633
                                               return ()
                                               lift (lift (return WCBrace)),
                                            do d552_634 <- get
                                               t1046_635 <- StateT char
                                               case t1046_635 of
                                                   '(' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "'('" "not match pattern: " "" d552_634 ["char"])
                                               let '(' = t1046_635
                                               return ()
                                               lift (lift (return WOParen)),
                                            do d553_636 <- get
                                               t1047_637 <- StateT char
                                               case t1047_637 of
                                                   ')' -> return ()
                                                   _ -> StateT position >>= (throwError . mkParseError "')'" "not match pattern: " "" d553_636 ["char"])
                                               let ')' = t1047_637
                                               return ()
                                               lift (lift (return WCParen))]
                typ129_133 = foldl1 mplus [do u <- foldl1 mplus [do d555_638 <- get
                                                                    t1049_639 <- StateT char
                                                                    let c1621_640 = t1049_639
                                                                    b1534_641 <- return (isUpper c1621_640)
                                                                    unless b1534_641 (StateT position >>= (throwError . mkParseError "isUpper c1621_0" "not match: " "" d555_638 ["char"]))
                                                                    return c1621_640]
                                              cs <- list142_147 (foldl1 mplus [do d557_642 <- get
                                                                                  t1051_643 <- StateT char
                                                                                  let c1622_644 = t1051_643
                                                                                  b1536_645 <- return (((||) <$> isAlphaNum <*> (`elem` "_'")) c1622_644)
                                                                                  unless b1536_645 (StateT position >>= (throwError . mkParseError "((||) <$> isAlphaNum <*> (`elem` \"_'\")) c1622_0" "not match: " "" d557_642 ["char"]))
                                                                                  return c1622_644])
                                              lift (lift (return $ u : cs))]
                var130_134 = foldl1 mplus [do l <- foldl1 mplus [do d559_646 <- get
                                                                    t1053_647 <- StateT char
                                                                    let c1623_648 = t1053_647
                                                                    b1538_649 <- return (((||) <$> isLower <*> (== '_')) c1623_648)
                                                                    unless b1538_649 (StateT position >>= (throwError . mkParseError "((||) <$> isLower <*> (== '_')) c1623_0" "not match: " "" d559_646 ["char"]))
                                                                    return c1623_648]
                                              cs <- list142_147 (foldl1 mplus [do d561_650 <- get
                                                                                  t1055_651 <- StateT char
                                                                                  let c1624_652 = t1055_651
                                                                                  b1540_653 <- return (((||) <$> isAlphaNum <*> (`elem` "_'")) c1624_652)
                                                                                  unless b1540_653 (StateT position >>= (throwError . mkParseError "((||) <$> isAlphaNum <*> (`elem` \"_'\")) c1624_0" "not match: " "" d561_650 ["char"]))
                                                                                  return c1624_652])
                                              lift (lift (return $ l : cs))]
                strLit131_135 = foldl1 mplus [do c0 <- foldl1 mplus [do d563_654 <- get
                                                                        t1057_655 <- StateT char
                                                                        case t1057_655 of
                                                                            '\\' -> return ()
                                                                            _ -> StateT position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d563_654 ["char"])
                                                                        let '\\' = t1057_655
                                                                        return ()
                                                                        c <- StateT escChar
                                                                        lift (lift (return c)),
                                                                     do d565_656 <- get
                                                                        t1059_657 <- StateT char
                                                                        let c1625_658 = t1059_657
                                                                        b1544_659 <- return ((`notElem` "\"\\") c1625_658)
                                                                        unless b1544_659 (StateT position >>= (throwError . mkParseError "(`notElem` \"\\\"\\\\\") c1625_0" "not match: " "" d565_656 ["char"]))
                                                                        return c1625_658]
                                                 s <- StateT strLit
                                                 lift (lift (return $ c0 : s)),
                                              lift (lift (return ""))]
                escChar132_136 = foldl1 mplus [do d567_660 <- get
                                                  t1061_661 <- StateT char
                                                  case t1061_661 of
                                                      '\\' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'\\\\'" "not match pattern: " "" d567_660 ["char"])
                                                  let '\\' = t1061_661
                                                  return ()
                                                  lift (lift (return '\\')),
                                               do d568_662 <- get
                                                  t1062_663 <- StateT char
                                                  case t1062_663 of
                                                      '\'' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'\\''" "not match pattern: " "" d568_662 ["char"])
                                                  let '\'' = t1062_663
                                                  return ()
                                                  lift (lift (return '\'')),
                                               do d569_664 <- get
                                                  t1063_665 <- StateT char
                                                  case t1063_665 of
                                                      '"' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'\"'" "not match pattern: " "" d569_664 ["char"])
                                                  let '"' = t1063_665
                                                  return ()
                                                  lift (lift (return '"')),
                                               do d570_666 <- get
                                                  t1064_667 <- StateT char
                                                  case t1064_667 of
                                                      'n' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'n'" "not match pattern: " "" d570_666 ["char"])
                                                  let 'n' = t1064_667
                                                  return ()
                                                  lift (lift (return '\n')),
                                               do d571_668 <- get
                                                  t1065_669 <- StateT char
                                                  case t1065_669 of
                                                      'r' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'r'" "not match pattern: " "" d571_668 ["char"])
                                                  let 'r' = t1065_669
                                                  return ()
                                                  lift (lift (return '\r')),
                                               do d572_670 <- get
                                                  t1066_671 <- StateT char
                                                  case t1066_671 of
                                                      't' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'t'" "not match pattern: " "" d572_670 ["char"])
                                                  let 't' = t1066_671
                                                  return ()
                                                  lift (lift (return '\t')),
                                               do d573_672 <- get
                                                  t1067_673 <- StateT char
                                                  case t1067_673 of
                                                      'x' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'x'" "not match pattern: " "" d573_672 ["char"])
                                                  let 'x' = t1067_673
                                                  return ()
                                                  ds <- list1143_466 (foldl1 mplus [do d575_674 <- get
                                                                                       t1069_675 <- StateT char
                                                                                       let c1626_676 = t1069_675
                                                                                       b1554_677 <- return (isHexDigit c1626_676)
                                                                                       unless b1554_677 (StateT position >>= (throwError . mkParseError "isHexDigit c1626_0" "not match: " "" d575_674 ["char"]))
                                                                                       return c1626_676])
                                                  lift (lift (return $ chr $ fst $ head $ readHex ds))]
                space133_137 = foldl1 mplus [do _ <- foldl1 mplus [do d577_678 <- get
                                                                      t1071_679 <- StateT char
                                                                      let c1627_680 = t1071_679
                                                                      b1556_681 <- return ((`elem` " \t") c1627_680)
                                                                      unless b1556_681 (StateT position >>= (throwError . mkParseError "(`elem` \" \\t\") c1627_0" "not match: " "" d577_678 ["char"]))
                                                                      return c1627_680]
                                                return ()
                                                return (),
                                             do _ <- StateT notSemiColon
                                                return ()
                                                return (),
                                             do _ <- StateT comment
                                                return ()
                                                return (),
                                             do _ <- StateT lineComment
                                                return ()
                                                return ()]
                space'134_138 = foldl1 mplus [do _ <- foldl1 mplus [do d582_682 <- get
                                                                       t1076_683 <- StateT char
                                                                       let c1628_684 = t1076_683
                                                                       b1561_685 <- return ((`elem` " \t") c1628_684)
                                                                       unless b1561_685 (StateT position >>= (throwError . mkParseError "(`elem` \" \\t\") c1628_0" "not match: " "" d582_682 ["char"]))
                                                                       return c1628_684]
                                                 return ()
                                                 return (),
                                              do _ <- list1143_466 (foldl1 mplus [do d584_686 <- get
                                                                                     t1078_687 <- StateT char
                                                                                     case t1078_687 of
                                                                                         '\n' -> return ()
                                                                                         _ -> StateT position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d584_686 ["char"])
                                                                                     let '\n' = t1078_687
                                                                                     return ()
                                                                                     lift (lift (return ()))])
                                                 return ()
                                                 return (),
                                              do _ <- StateT comment
                                                 return ()
                                                 return (),
                                              do _ <- StateT lineComment
                                                 return ()
                                                 return ()]
                lineComment135_139 = foldl1 mplus [do d587_688 <- get
                                                      t1081_689 <- StateT char
                                                      case t1081_689 of
                                                          '-' -> return ()
                                                          _ -> StateT position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d587_688 ["char"])
                                                      let '-' = t1081_689
                                                      return ()
                                                      d588_690 <- get
                                                      t1082_691 <- StateT char
                                                      case t1082_691 of
                                                          '-' -> return ()
                                                          _ -> StateT position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d588_690 ["char"])
                                                      let '-' = t1082_691
                                                      return ()
                                                      _ <- list142_147 (foldl1 mplus [do d590_692 <- get
                                                                                         t1084_693 <- StateT char
                                                                                         let c1629_694 = t1084_693
                                                                                         b1569_695 <- return ((/= '\n') c1629_694)
                                                                                         unless b1569_695 (StateT position >>= (throwError . mkParseError "(/= '\\n') c1629_0" "not match: " "" d590_692 ["char"]))
                                                                                         return c1629_694])
                                                      return ()
                                                      d592_696 <- get
                                                      _ <- do d591_697 <- get
                                                              t1085_698 <- StateT char
                                                              case t1085_698 of
                                                                  '\n' -> return ()
                                                                  _ -> StateT position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d591_697 ["char"])
                                                              let '\n' = t1085_698
                                                              return ()
                                                      put d592_696
                                                      return ()]
                comment136_140 = foldl1 mplus [do d593_699 <- get
                                                  t1086_700 <- StateT char
                                                  case t1086_700 of
                                                      '{' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'{'" "not match pattern: " "" d593_699 ["char"])
                                                  let '{' = t1086_700
                                                  return ()
                                                  d594_701 <- get
                                                  t1087_702 <- StateT char
                                                  case t1087_702 of
                                                      '-' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d594_701 ["char"])
                                                  let '-' = t1087_702
                                                  return ()
                                                  d596_703 <- get
                                                  do err1609_704 <- ((do d595_705 <- get
                                                                         t1088_706 <- StateT char
                                                                         case t1088_706 of
                                                                             '#' -> return ()
                                                                             _ -> StateT position >>= (throwError . mkParseError "'#'" "not match pattern: " "" d595_705 ["char"])
                                                                         let '#' = t1088_706
                                                                         return ()) >> return False) `catchError` const (return True)
                                                     unless err1609_704 (StateT position >>= (throwError . mkParseError "!'#':" "not match: " "" d596_703 ["char"]))
                                                  put d596_703
                                                  _ <- StateT comments
                                                  return ()
                                                  d598_707 <- get
                                                  t1090_708 <- StateT char
                                                  case t1090_708 of
                                                      '-' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d598_707 ["char"])
                                                  let '-' = t1090_708
                                                  return ()
                                                  d599_709 <- get
                                                  t1091_710 <- StateT char
                                                  case t1091_710 of
                                                      '}' -> return ()
                                                      _ -> StateT position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d599_709 ["char"])
                                                  let '}' = t1091_710
                                                  return ()
                                                  return ()]
                comments137_141 = foldl1 mplus [do _ <- StateT comStr
                                                   return ()
                                                   _ <- StateT comment
                                                   return ()
                                                   _ <- StateT comments
                                                   return ()
                                                   return (),
                                                do _ <- StateT comStr
                                                   return ()
                                                   return ()]
                comStr138_142 = foldl1 mplus [do d605_711 <- get
                                                 do err1610_712 <- ((do _ <- StateT comment
                                                                        return ()) >> return False) `catchError` const (return True)
                                                    unless err1610_712 (StateT position >>= (throwError . mkParseError "!_:comment" "not match: " "" d605_711 ["comment"]))
                                                 put d605_711
                                                 d609_713 <- get
                                                 do err1611_714 <- ((do _ <- foldl1 mplus [do d607_715 <- get
                                                                                              t1098_716 <- StateT char
                                                                                              case t1098_716 of
                                                                                                  '-' -> return ()
                                                                                                  _ -> StateT position >>= (throwError . mkParseError "'-'" "not match pattern: " "" d607_715 ["char"])
                                                                                              let '-' = t1098_716
                                                                                              return ()
                                                                                              d608_717 <- get
                                                                                              t1099_718 <- StateT char
                                                                                              case t1099_718 of
                                                                                                  '}' -> return ()
                                                                                                  _ -> StateT position >>= (throwError . mkParseError "'}'" "not match pattern: " "" d608_717 ["char"])
                                                                                              let '}' = t1099_718
                                                                                              return ()
                                                                                              return ()]
                                                                        return ()) >> return False) `catchError` const (return True)
                                                    unless err1611_714 (StateT position >>= (throwError . mkParseError "!_:('-': '}': {})" "not match: " "" d609_713 ["char"]))
                                                 put d609_713
                                                 _ <- StateT char
                                                 return ()
                                                 _ <- StateT comStr
                                                 return ()
                                                 return (),
                                              return ()]
                semiColon139_143 = foldl1 mplus [do d612_719 <- get
                                                    t1102_720 <- StateT char
                                                    case t1102_720 of
                                                        ';' -> return ()
                                                        _ -> StateT position >>= (throwError . mkParseError "';'" "not match pattern: " "" d612_719 ["char"])
                                                    let ';' = t1102_720
                                                    return ()
                                                    return (),
                                                 do _ <- list1143_466 (foldl1 mplus [do d614_721 <- get
                                                                                        t1104_722 <- StateT char
                                                                                        case t1104_722 of
                                                                                            '\n' -> return ()
                                                                                            _ -> StateT position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d614_721 ["char"])
                                                                                        let '\n' = t1104_722
                                                                                        return ()
                                                                                        lift (lift (return ()))])
                                                    return ()
                                                    _ <- list142_147 (foldl1 mplus [do d616_723 <- get
                                                                                       t1106_724 <- StateT char
                                                                                       case t1106_724 of
                                                                                           '\t' -> return ()
                                                                                           _ -> StateT position >>= (throwError . mkParseError "'\\t'" "not match pattern: " "" d616_723 ["char"])
                                                                                       let '\t' = t1106_724
                                                                                       return ()
                                                                                       return (),
                                                                                    do d617_725 <- get
                                                                                       t1107_726 <- StateT char
                                                                                       case t1107_726 of
                                                                                           ' ' -> return ()
                                                                                           _ -> StateT position >>= (throwError . mkParseError "' '" "not match pattern: " "" d617_725 ["char"])
                                                                                       let ' ' = t1107_726
                                                                                       return ()
                                                                                       return ()])
                                                    return ()
                                                    d618_727 <- get
                                                    t1108_728 <- StateT position
                                                    let (!(ListPos (CharPos (_, x)))) = t1108_728
                                                    b1593_729 <- lift (lift (maybe False (x <=) <$> get))
                                                    unless b1593_729 (StateT position >>= (throwError . mkParseError "maybe False (x <=) <$> get" "not match: " "" d618_727 ["position"]))
                                                    d620_730 <- get
                                                    do err1612_731 <- ((do _ <- StateT lineComment
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1612_731 (StateT position >>= (throwError . mkParseError "!_:lineComment" "not match: " "" d620_730 ["lineComment"]))
                                                    put d620_730
                                                    d622_732 <- get
                                                    do err1613_733 <- ((do _ <- StateT comment
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1613_733 (StateT position >>= (throwError . mkParseError "!_:comment" "not match: " "" d622_732 ["comment"]))
                                                    put d622_732
                                                    d624_734 <- get
                                                    do err1614_735 <- ((do d623_736 <- get
                                                                           t1111_737 <- StateT char
                                                                           case t1111_737 of
                                                                               ';' -> return ()
                                                                               _ -> StateT position >>= (throwError . mkParseError "';'" "not match pattern: " "" d623_736 ["char"])
                                                                           let ';' = t1111_737
                                                                           return ()) >> return False) `catchError` const (return True)
                                                       unless err1614_735 (StateT position >>= (throwError . mkParseError "!';':" "not match: " "" d624_734 ["char"]))
                                                    put d624_734
                                                    return (),
                                                 do d626_738 <- get
                                                    do err1615_739 <- ((do d625_740 <- get
                                                                           _ <- StateT char
                                                                           b1597_741 <- lift (lift (return True))
                                                                           unless b1597_741 (StateT position >>= (throwError . mkParseError "return True" "not match: " "" d625_740 ["char"]))) >> return False) `catchError` const (return True)
                                                       unless err1615_739 (StateT position >>= (throwError . mkParseError "!_:[return True]" "not match: " "" d626_738 ["char"]))
                                                    put d626_738
                                                    return ()]
                notSemiColon140_144 = foldl1 mplus [do _ <- list1143_466 (foldl1 mplus [do d628_742 <- get
                                                                                           t1114_743 <- StateT char
                                                                                           case t1114_743 of
                                                                                               '\n' -> return ()
                                                                                               _ -> StateT position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d628_742 ["char"])
                                                                                           let '\n' = t1114_743
                                                                                           return ()
                                                                                           lift (lift (return ()))])
                                                       return ()
                                                       _ <- list142_147 (foldl1 mplus [do d630_744 <- get
                                                                                          t1116_745 <- StateT char
                                                                                          case t1116_745 of
                                                                                              '\t' -> return ()
                                                                                              _ -> StateT position >>= (throwError . mkParseError "'\\t'" "not match pattern: " "" d630_744 ["char"])
                                                                                          let '\t' = t1116_745
                                                                                          return ()
                                                                                          return (),
                                                                                       do d631_746 <- get
                                                                                          t1117_747 <- StateT char
                                                                                          case t1117_747 of
                                                                                              ' ' -> return ()
                                                                                              _ -> StateT position >>= (throwError . mkParseError "' '" "not match pattern: " "" d631_746 ["char"])
                                                                                          let ' ' = t1117_747
                                                                                          return ()
                                                                                          return ()])
                                                       return ()
                                                       d632_748 <- get
                                                       t1118_749 <- StateT position
                                                       let (!(ListPos (CharPos (_, x)))) = t1118_749
                                                       b1603_750 <- lift (lift (maybe True (x >) <$> get))
                                                       unless b1603_750 (StateT position >>= (throwError . mkParseError "maybe True (x >) <$> get" "not match: " "" d632_748 ["position"]))
                                                       return (),
                                                    do _ <- list1143_466 (foldl1 mplus [do d634_751 <- get
                                                                                           t1120_752 <- StateT char
                                                                                           case t1120_752 of
                                                                                               '\n' -> return ()
                                                                                               _ -> StateT position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d634_751 ["char"])
                                                                                           let '\n' = t1120_752
                                                                                           return ()
                                                                                           lift (lift (return ()))])
                                                       return ()
                                                       _ <- StateT lineComment
                                                       return ()
                                                       return (),
                                                    do _ <- list1143_466 (foldl1 mplus [do d637_753 <- get
                                                                                           t1123_754 <- StateT char
                                                                                           case t1123_754 of
                                                                                               '\n' -> return ()
                                                                                               _ -> StateT position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d637_753 ["char"])
                                                                                           let '\n' = t1123_754
                                                                                           return ()
                                                                                           lift (lift (return ()))])
                                                       return ()
                                                       _ <- StateT comment
                                                       return ()
                                                       return (),
                                                    do _ <- list1143_466 (foldl1 mplus [do d640_755 <- get
                                                                                           t1126_756 <- StateT char
                                                                                           case t1126_756 of
                                                                                               '\n' -> return ()
                                                                                               _ -> StateT position >>= (throwError . mkParseError "'\\n'" "not match pattern: " "" d640_755 ["char"])
                                                                                           let '\n' = t1126_756
                                                                                           return ()
                                                                                           lift (lift (return ()))])
                                                       return ()
                                                       d642_757 <- get
                                                       _ <- do d641_758 <- get
                                                               t1127_759 <- StateT char
                                                               case t1127_759 of
                                                                   ';' -> return ()
                                                                   _ -> StateT position >>= (throwError . mkParseError "';'" "not match pattern: " "" d641_758 ["char"])
                                                               let ';' = t1127_759
                                                               return ()
                                                       put d642_757
                                                       return ()]
                list142_147 :: forall m a . (MonadPlus m, Applicative m) =>
                                            m a -> m ([a])
                list1143_466 :: forall m a . (MonadPlus m, Applicative m) =>
                                             m a -> m ([a])
                list142_147 p = list1143_466 p `mplus` return []
                list1143_466 p = ((:) <$> p) <*> list142_147 p
                optional141_206 :: forall m a . (MonadPlus m, Applicative m) =>
                                                m a -> m (Maybe a)
                optional141_206 p = (Just <$> p) `mplus` return Nothing


