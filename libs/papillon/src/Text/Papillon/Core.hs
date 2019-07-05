{-# LANGUAGE TemplateHaskell, PackageImports, TypeFamilies, FlexibleContexts,
	FlexibleInstances, TupleSections #-}

module Text.Papillon.Core (
	-- * For Text.Papillon library
	papillonCore,

	Source(..),
	SourceList(..),

	-- ** For parse error message
	ParseError,
	mkParseError,
	peDerivs,
	peReading,
	peMessage,
	peCode,
	peComment,
	pePosition,
	pePositionS,
	Pos(..),
	ListPos(..),

	-- * For papillon command
	papillonFile,
	PPragma(..),
	ModuleName,
	Exports,
	Code,
	(<*>),
	(<$>),
	runError
) where

import Language.Haskell.TH hiding (infixApp, doE)
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Identity
import "monads-tf" Control.Monad.Error

import Control.Applicative

import Text.Papillon.Parser
import Data.Maybe
import Data.IORef
import Data.Char

import Text.Papillon.List

import System.IO.Unsafe

dvPosN :: String -> Name
dvPosN prefix = mkName $ prefix ++ "position"

mkPrName :: String -> String -> Name
mkPrName prefix = mkName . (prefix ++)

mkPrUName :: String -> String -> Name
mkPrUName prefix = mkName . (capitalize prefix ++)
	where
	capitalize "" = ""
	capitalize (h : t) = toUpper h : t

papillonCore :: String -> DecsQ
papillonCore str = case flip evalState Nothing $ runErrorT $ peg $ parse str of
	Right (stpegq, _) -> do
		let (monad, src, prefix, parsed) = stpegq
		decParsed True monad src prefix parsed
	Left err -> error $ "parse error: " ++ showParseError err

papillonFile :: String ->
	Q ([PPragma], ModuleName, Maybe Exports, Code, DecsQ, Code)
papillonFile str = case flip evalState Nothing $ runErrorT $ pegFile $ parse str of
	Right (pegfileq, _) -> do
		let	(prgm, mn, ppp, pp, (monad, src, prefix, parsed), atp) = pegfileq
			lu = listUsed parsed
			ou = optionalUsed parsed
			addApplicative = if lu || ou
				then "import Control.Applicative" ++
					"(Applicative, (<$>), (<*>))\n" else ""
			addIdentity = if isJust monad then ""
				else "import \"monads-tf\" Control.Monad.Identity\n"
		return (prgm, mn, ppp,
			addApplicative ++ addIdentity ++ pp,
			decs monad src prefix parsed, atp)
		where
		decs = decParsed False
	Left err -> error $ "parse error: " ++ showParseError err

decParsed :: Bool -> Maybe Type -> Type -> String -> Peg -> DecsQ
decParsed th monad src prefix parsed = do
	let	d = derivs th (fromMaybe (ConT $ identityN th) monad) src prefix parsed
		pt = SigD (mkPrName prefix "parse") $
			src `arrT` ConT (mkPrUName prefix "Derivs")
	p <- funD (mkPrName prefix "parse") [mkParseBody th (isJust monad) prefix parsed]
	d' <- d
	return [d', pt, p]

derivs :: Bool -> Type -> Type -> String -> Peg -> DecQ
derivs th monad src prefix pg = do
	ns <- notStrict
	return $ DataD [] (mkPrUName prefix "Derivs") [] Nothing [
		RecC (mkPrUName prefix "Derivs") $ map (derivs1 ns) pg ++ [
			(mkPrName prefix dvCharsN, ns, resultT tkn),
			(dvPosN prefix, ns, resultT $ ConT (mkName "Pos") `AppT` src)
		]] []
	where
	tkn = ConT (mkName "Token") `AppT` src
	derivs1 ns (name, Just t, _) = (mkPrName prefix name, ns, resultT t)
	derivs1 ns (name, Nothing, _) =
		(mkPrName prefix name, ns, resultT $ TupleT 0)
	resultT typ = ConT (errorTTN th)
		`AppT` (ConT (mkName "ParseError")
			`AppT` (ConT (mkName "Pos") `AppT` src)
			`AppT` ConT (mkPrUName prefix "Derivs"))
		`AppT` monad
		`AppT` (TupleT 2 `AppT` typ `AppT` ConT (mkPrUName prefix "Derivs"))

type Variables = [(String, [Name])]

newVariable :: IORef Int -> Variables -> String -> Q Variables
newVariable g vs n = (: vs) . (n ,) <$> vars
	where vars = runIO $ unsafeInterleaveIO $ (:)
		<$> runQ (newNewName g n) <*> runQ vars

getVariable :: String -> Variables -> Name
getVariable = ((head . fromJust) .) . lookup

nextVariable :: String -> Variables -> Variables
nextVariable n vs = (n, tail $ fromJust $ lookup n vs) : vs

mkParseBody :: Bool -> Bool -> String -> Peg -> ClauseQ
mkParseBody th monadic prefix pg = do
	glb <- runIO $ newIORef 1
	vars <- foldM (newVariable glb) [] [
		"parse", "chars", "pos", "d", "c", "s", "s'", "x", "t", "err", "b",
		"list", "list1", "optional"]
	let	pgn = getVariable "parse" vars
	rets <- mapM (newNewName glb . \(n, _, _) -> prefix ++ n) pg
	rules <- mapM (newNewName glb . \(n, _, _) -> prefix ++ n) pg
	let	decs = flip evalState vars $ (:)
			<$> (FunD pgn <$> (: []) <$> mkParseCore th prefix rets rules)
			<*> zipWithM mkr rules pg
		list = if not $ listUsed pg then return [] else listDec
			(getVariable "list" vars) (getVariable "list1" vars) th
		opt = if not $ optionalUsed pg then return [] else optionalDec
			(getVariable "optional" vars) th
	lst <- list
	op <- opt
	return $ flip (Clause []) (decs ++ lst ++ op) $ NormalB $
		VarE pgn `AppE` VarE (mkName "initialPos")
	where
	mkr rule (_, _, sel) =
		flip (ValD $ VarP rule) [] . NormalB <$> mkRule th monadic prefix sel

mkParseCore :: Bool -> String -> [Name] -> [Name] -> State Variables Clause
mkParseCore th prefix rets rules = do
	[ch, p, s, d] <- mapM (gets . getVariable) ["chars", "pos", "s", "d"]
	let def ret rule = flip (ValD $ VarP ret) [] $
		NormalB $ VarE (runStateTN th) `AppE` VarE rule `AppE` VarE d
	pc <- parseChar th prefix
	return $ Clause [VarP p, VarP s] (NormalB $ VarE d) $ [
		flip (ValD $ VarP d) [] $ NormalB $ foldl1 AppE $
			ConE (mkPrUName prefix "Derivs") : map VarE rets ++ [VarE ch,
				VarE (mkName "return") `AppE` TupE [VarE p, VarE d]]
	 ] ++ zipWith def rets rules ++ [pc]

parseChar :: Bool -> String -> State Variables Dec
parseChar th prefix = do
	[prs, ch, p, c, s, s', d] <- mapM (gets . getVariable)
		["parse", "chars", "pos", "c", "s", "s'", "d"]
	let	emsg = "end of input"
		np = VarE (mkName "updatePos") `AppE` VarE c `AppE` VarE p
	return $ flip (ValD $ VarP ch) [] $ NormalB $ VarE (runStateTN th) `AppE`
		CaseE (VarE (mkName "getToken") `AppE` VarE s) [
			Match (mkName "Just" `ConP` [TupP [VarP c, VarP s']])
				(NormalB $ DoE $ map NoBindS [
					VarE (putN th) `AppE`
						(VarE prs `AppE` np
							`AppE` VarE s'),
					VarE (mkName "return") `AppE` VarE c])
				[],
			flip (Match WildP) [] $ NormalB $
				throwErrorTH th prefix (mkName "undefined") [] emsg "" ""
		 ] `AppE` VarE d

mkRule :: Bool -> Bool -> String -> Selection -> State Variables Exp
mkRule t m prefix s = (VarE (mkName "foldl1") `AppE` VarE (mplusN t) `AppE`) . ListE <$>
	case s of
		exs -> expression t m prefix `mapM` exs

expression :: Bool -> Bool -> String -> Expression -> State Variables Exp
expression th m prefix (Left (e, r)) =
--	(doE . (++ [NoBindS $ retLift `AppE` r]) . concat <$>) $
	(doE . (++ [NoBindS $ mkRetLift r]) . concat <$>) $
		forM e $ \(la, ck) ->
			lookahead th prefix la (show $ pprCheck ck) (getReadings ck) =<<
				check th m prefix ck
	where
	mkRetLift (Just rr) = retLift rr
	mkRetLift Nothing = VarE (mkName "return") `AppE` TupE []
	retLift x = if m
		then VarE (liftN th) `AppE` (VarE (liftN th) `AppE` x)
		else VarE (mkName "return") `AppE` x
	getReadings (Left (_, rf, _)) = readings rf
	getReadings (Right _) = [dvCharsN]
expression th _ prefix (Right e) = do
	c <- gets $ getVariable "c"
	modify $ nextVariable "c"
	let	e' = [(Here, Left ((VarP c, ""), FromVariable Nothing,
			Just (e `AppE` VarE c, "")))]
		r = VarE c
	expression th False prefix (Left (e', Just r))

check :: Bool -> Bool -> String -> Check -> State Variables [Stmt]
check th monadic prefix (Left ((n, nc), rf, test)) = do
	t <- gets $ getVariable "t"
	d <- gets $ getVariable "d"
	b <- gets $ getVariable "b"
	modify $ nextVariable "t"
	modify $ nextVariable "d"
	modify $ nextVariable "b"
	case (n, test) of
		(WildP, Just p) -> ((BindS (VarP d) (VarE $ getN th) :) .
				(: afterCheck th monadic prefix b p d (readings rf))) .
			BindS WildP <$> transReadFrom th monadic prefix rf
		(_, Just p)
			| notHaveOthers n -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = LetS [ValD n (NormalB $ VarE t) []]
					c = afterCheck th monadic prefix b p d (readings rf)
				s <- BindS (VarP t) <$> transReadFrom th monadic prefix rf
				return $ [bd, s, m] ++ c
			| otherwise -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = beforeMatch th prefix t n d (readings rf) nc
					c = afterCheck th monadic prefix b p d (readings rf)
				s <- BindS (VarP t) <$> transReadFrom th monadic prefix rf
				return $ [bd, s]  ++ m ++ c
		(WildP, _) -> sequence [
			BindS WildP <$> transReadFrom th monadic prefix rf,
			return $ NoBindS $ VarE (mkName "return") `AppE` TupE []
		 ]
		_	| notHaveOthers n ->
				(: []) . BindS n <$> transReadFrom th monadic prefix rf
			| otherwise -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = beforeMatch th prefix t n d (readings rf) nc
				s <- BindS (VarP t) <$> transReadFrom th monadic prefix rf
				return $ bd : s : m
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers (BangP _) = True
	notHaveOthers _ = False
check th monadic prefix (Right (c, l)) =
	check th monadic prefix
		(Left ((WildP, ""), FromL l $ FromSelection sel, Nothing))
	where
	sel = [Left
		([(Here, Left ((LitP $ CharL c, ""), FromVariable Nothing, Nothing))],
		Just $ if monadic then VarE (mkName "return") `AppE` TupE [] else TupE [])]

lookahead :: Bool -> String -> Lookahead -> String -> [String] -> [Stmt] ->
	State Variables [Stmt]
lookahead _ _ Here _ _ ret = return ret
lookahead th _ Ahead _ _ ret = do
	d <- gets $ getVariable "d"
	modify $ nextVariable "d"
	return [BindS (VarP d) $ VarE (getN th),
		BindS WildP $ doE ret,
		NoBindS $ VarE (putN th) `AppE` VarE d]
lookahead th prefix (NAhead com) ck ns ret = do
	d <- gets $ getVariable "d"
	modify $ nextVariable "d"
	n <- negative th prefix ('!' : ck) com
		d ns (doE ret)
	return [BindS (VarP d) $ VarE (getN th),
		NoBindS n,
		NoBindS $ VarE (putN th) `AppE` VarE d]

negative :: Bool -> String -> String -> String -> Name -> [String] -> Exp ->
	State Variables Exp
negative th prefix code com d ns act = do
	err <- gets $ getVariable "err"
	modify $ nextVariable "err"
	return $ DoE [
		BindS (VarP err) $ infixApp
			(infixApp act (VarE $ mkName ">>")
				(VarE (mkName "return") `AppE`
					ConE (mkName "False")))
			(VarE $ catchErrorN th)
			(VarE (mkName "const") `AppE`
				(VarE (mkName "return") `AppE`
					ConE (mkName "True"))),
		NoBindS $ VarE (unlessN th) `AppE` VarE err `AppE`
			throwErrorTH th prefix d ns "not match: " code com]

transReadFrom :: Bool -> Bool -> String -> ReadFrom -> State Variables Exp
transReadFrom th _ prefix (FromVariable Nothing) = return $
	ConE (stateTN th) `AppE` VarE (mkPrName prefix dvCharsN)
transReadFrom th _ prefix (FromVariable (Just var)) = return $
	ConE (stateTN th) `AppE` VarE (mkPrName prefix var)
transReadFrom th m prefix (FromSelection sel) = mkRule th m prefix sel
transReadFrom th m prefix (FromL List rf) = do
	list <- gets $ getVariable "list"
	(VarE list `AppE`) <$> transReadFrom th m prefix rf
transReadFrom th m prefix (FromL List1 rf) = do
	list1 <- gets $ getVariable "list1"
	(VarE list1 `AppE`) <$> transReadFrom th m prefix rf
transReadFrom th m prefix (FromL Optional rf) = do
	opt <- gets $ getVariable "optional"
	(VarE opt `AppE`) <$> transReadFrom th m prefix rf

beforeMatch :: Bool -> String -> Name -> Pat -> Name -> [String] -> String -> [Stmt]
beforeMatch th prefix t nn d ns nc = [
	NoBindS $ CaseE (VarE t) [
		flip (Match $ vpw nn) [] $ NormalB $
			VarE (mkName "return") `AppE` TupE [],
		flip (Match WildP) [] $ NormalB $
			throwErrorTH th prefix d ns "not match pattern: " (show $ ppr nn) nc],
	LetS [ValD nn (NormalB $ VarE t) []],
	NoBindS $ VarE (mkName "return") `AppE` TupE []]
	where
	vpw (VarP _) = WildP
	vpw (ConP n ps) = ConP n $ map vpw ps
	vpw (InfixP p1 n p2) = InfixP (vpw p1) n (vpw p2)
	vpw (UInfixP p1 n p2) = InfixP (vpw p1) n (vpw p2)
	vpw (ListP ps) = ListP $ vpw `map` ps
	vpw (TupP ps) = TupP $ vpw `map` ps
	vpw o = o

afterCheck :: Bool -> Bool -> String -> Name -> (Exp, String) -> Name -> [String] -> [Stmt]
afterCheck th monadic prefix b (pp, pc) d ns = [
	BindS (VarP b) $ retLift pp,
	NoBindS $ VarE (unlessN th) `AppE` VarE b `AppE`
		throwErrorTH th prefix d ns "not match: " (show $ ppr pp) pc]
	where
	retLift x = if monadic
		then VarE (liftN th) `AppE` (VarE (liftN th) `AppE` x)
		else VarE (mkName "return") `AppE` x

listUsed, optionalUsed :: Peg -> Bool
listUsed = any $ sel . \(_, _, s) -> s
	where
	sel = any ex
	ex (Left e) = any (either (rf . \(_, r, _) -> r) chr . snd) $ fst e
	ex (Right _) = False
	chr (_, List) = True
	chr (_, List1) = True
	chr (_, _) = False
	rf (FromL List _) = True
	rf (FromL List1 _) = True
	rf (FromSelection s) = sel s
	rf _ = False
optionalUsed = any $ sel . \(_, _, s) -> s
	where
	sel = any ex
	ex (Left e) = any (either (rf . \(_, r, _) -> r) chr . snd) $ fst e
	ex (Right _) = False
	chr (_, Optional) = True
	chr (_, _) = False
	rf (FromL Optional _) = True
	rf (FromSelection s) = sel s
	rf _ = False

throwErrorTH :: Bool -> String -> Name -> [String] -> String -> String -> String -> Exp
throwErrorTH th prefix d ns msg code com = InfixE
	(Just $ ConE (stateTN th) `AppE` VarE (dvPosN prefix))
	(VarE $ mkName ">>=")
	(Just $ InfixE
		(Just $ VarE $ throwErrorN th)
		(VarE $ mkName ".")
		(Just $ VarE (mkName "mkParseError")
			`AppE` LitE (StringL code)
			`AppE` LitE (StringL msg)
			`AppE` LitE (StringL com)
			`AppE` VarE d
			`AppE` ListE (map (LitE . StringL) ns)))

newNewName :: IORef Int -> String -> Q Name
newNewName g base = do
	n <- runIO $ readIORef g
	runIO $ modifyIORef g succ
	newName $ base ++ show n

showParseError :: ParseError (Pos String) Derivs -> String
showParseError pe =
	unwords (map (showReading d) ns) ++ (if null ns then "" else " ") ++
	m ++ c ++ " at position: " ++ show p
	where
	[c, m, _] = ($ pe) `map` [peCode, peMessage, peComment]
	ns = peReading pe
	d = peDerivs pe
	p = pePositionS pe

showReading :: Derivs -> String -> String
showReading d n
	| n == dvCharsN = case flip evalState Nothing $ runErrorT $ char d of
		Right (c, _) -> show c
		Left _ -> error "bad"
showReading d "hsw" = case flip evalState Nothing $ runErrorT $ hsw d of
	Right (c, _) -> show c
	Left _ -> error "bad"
showReading _ n = "yet: " ++ n

doE :: [Stmt] -> Exp
doE [NoBindS ex] = ex
doE stmts = DoE stmts

arrT :: Type -> Type -> Type
arrT a r = ArrowT `AppT` a `AppT` r

infixApp :: Exp -> Exp -> Exp -> Exp
infixApp e1 op e2 = InfixE (Just e1) op (Just e2)

stateTN, runStateTN, putN, getN :: Bool -> Name
stateTN True = 'StateT
stateTN False = mkName "StateT"
runStateTN True = 'runStateT
runStateTN False = mkName "runStateT"
putN True = 'put
putN False = mkName "put"
getN True = 'get
getN False = mkName "get"

unlessN, mplusN :: Bool -> Name
unlessN True = 'unless
unlessN False = mkName "unless"
mplusN True = 'mplus
mplusN False = mkName "mplus"

throwErrorN, catchErrorN :: Bool -> Name
throwErrorN True = 'throwError
throwErrorN False = mkName "throwError"
catchErrorN True = 'catchError
catchErrorN False = mkName "catchError"

errorTTN, identityN :: Bool -> Name
errorTTN True = ''ErrorT
errorTTN False = mkName "ErrorT"
identityN True = ''Identity
identityN False = mkName "Identity"

liftN :: Bool -> Name
liftN True = 'lift
liftN False = mkName "lift"

readings :: ReadFrom -> [String]
readings (FromVariable (Just s)) = [s]
readings (FromVariable _) = [dvCharsN]
readings (FromL _ rf) = readings rf
readings (FromSelection s) = concat $
	(mapM $ either
		(either (readings . \(_, rf, _) -> rf) (const [dvCharsN]) . snd .
			head . fst)
		(const [dvCharsN])) s
