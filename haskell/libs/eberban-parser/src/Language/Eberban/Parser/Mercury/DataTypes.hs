module Language.Eberban.Parser.Mercury.DataTypes where

import Prelude hiding (Word)

data Preamble = Preamble [PreambleUnit]
    deriving (Show)

data PreambleUnit
    = PreambleUnitInterjection DE PredicateWithTransformations
    | PreambleUnitParenthetical DO EberbanText DOI
    deriving (Show)

data EberbanText = EberbanText Preamble (Maybe Paragraphs)
    deriving (Show)

data Paragraphs = Paragraphs [Paragraph]
    deriving (Show)

data Paragraph = Paragraph (Maybe PU) [ParagraphUnit]
    deriving (Show)

data ParagraphUnitWithErasureBoolean
    = ParagraphUnitErased ParagraphUnit CU
    | ParagraphUnitNotErased ParagraphUnit
    deriving (Show)

data ParagraphUnit
    = ParagraphUnitDefinition Definition
    | ParagraphUnitSentence Sentence
    deriving (Show)

data Definition = Definition PO Defined Scope
    deriving (Show)

data Defined
    = DefinedGi GI
    | DefinedFreeformVariable String
    | DefinedCompound Compound
    | DefinedRoot String
    deriving (Show)

data Sentence = Sentence (Maybe PA) Scope
    deriving (Show)

data Scope = Scope (Maybe ArgumentList) [(Chaining, Maybe BU)]
    deriving (Show)

data Chaining = Chaining [ChainingItem]
    deriving (Show)

data ChainingItem
    = ChainingNegation BI Chaining
    | ChainingPredicate PredicateWithTransformationsAndFree [VeScope]
    deriving (Show)

data VeScope = VeScope VeScopeFirst [VeScopeNext] (Maybe VEI)
    deriving (Show)

data VeScopeFirst = VeScopeFirst (Maybe BI) VE Scope
    deriving (Show)

data VeScopeNext = VeScopeNext (Maybe BI) FE Scope
    deriving (Show)

data PredicateWithTransformationsAndFree = PredicateWithTransformationsAndFree PredicateWithTransformations [Free]
    deriving (Show)

data PredicateWithTransformations = PredicateWithTransformations [PredicateTransformation] Predicate
    deriving (Show)

data PredicateTransformation
    = PredicateTransformationSe SE
    | PredicateTransformationZi ZI
    deriving (Show)

-- maybe rename to SimplePredicate?
data Predicate
    = PredicateRoot String
    | PredicateCompound Compound
    | PredicateBorrowing [String] (Maybe BE)
    | PredicateQuote Quote
    | PredicateNumber Number
    | PredicateBa BA
    | PredicateMi MI
    | PredicateVariable (Maybe BO) Variable
    | PredicateScope PE Scope (Maybe PEI)
    deriving (Show)

data Number = Number [TI] (Maybe BE)
    deriving (Show)

newtype ForeignQuoteDelimiter = ForeignQuoteDelimiter NativeWord
    deriving (Show)

newtype ForeignQuoteContent = ForeignQuoteContent String
    deriving (Show)

data Quote
    = GrammaticalQuote CA EberbanText CAI
    | OneWordQuote CE Word
    | OneCompoundQuote CE Compound
    | ForeignQuote CO ForeignQuoteDelimiter ForeignQuoteContent
    deriving (Show)

data Variable
    = VariableKi KI
    | VariableGi GI
    | VariableFreeform String
    deriving (Show)

data Argument
    = ArgumentKi KI
    | ArgumentGi GI
    deriving (Show)

data ArgumentList = ArgumentList [Argument] PI
    deriving (Show)

data Override = Override DU PredicateWithTransformations
    deriving (Show)

data Free
    = FreeMetadata DA
    | FreeSubscript DI Number
    | FreeParenthetical DO EberbanText DOI
    | FreeInterjection DE PredicateWithTransformations
    deriving (Show)

data Compound = Compound [Word]
    deriving (Show)

data Word
    = Borrowing String
    | Native NativeWord
    deriving (Show)

data NativeWord
    = Root String
    | Particle String
    deriving (Show)

data BA = BA String
    deriving (Show)
data BE = BE String
    deriving (Show)
data BI = BI String [Free]
    deriving (Show)
data BO = BO String
    deriving (Show)
data BU = BU String
    deriving (Show)

data DA = DA String
    deriving (Show)
data DE = DE String
    deriving (Show)
data DI = DI String
    deriving (Show)
data DO = DO String
    deriving (Show)
data DOI = DOI String
    deriving (Show)
data DU = DU String
    deriving (Show)

data SE = SE String (Maybe Override)
    deriving (Show)
data ZI = ZI String (Maybe Override)
    deriving (Show)
data VE = VE String (Maybe Override) [Free]
    deriving (Show)
data FE = FE String (Maybe Override) [Free]
    deriving (Show)
data VEI = VEI String
    deriving (Show)

data GI = GI String
    deriving (Show)
data KI = KI String
    deriving (Show)
data MI = MI String
    deriving (Show)

data PA = PA String [Free]
    deriving (Show)
data PE = PE String [Free]
    deriving (Show)
data PEI = PEI String
    deriving (Show)
data PI = PI String
    deriving (Show)
data PO = PO String [Free]
    deriving (Show)
data PU = PU String [Free]
    deriving (Show)

data TI = TI String
    deriving (Show)

data CA = CA String
    deriving (Show)
data CAI = CAI String
    deriving (Show)
data CE = CE String
    deriving (Show)
data CO = CO String
    deriving (Show)
data CU = CU String
    deriving (Show)
