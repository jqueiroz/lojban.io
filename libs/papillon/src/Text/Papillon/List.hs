{-# LANGUAGE TemplateHaskell, PackageImports #-}

module Text.Papillon.List (
	listDec,
	optionalDec
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Control.Applicative
import Control.Monad

{-

list, list1 :: (MonadPlus m, Applicative m) => m a -> m [a]
list p = list1 p `mplus` return []
list1 p = (:) <$> p <*> list p

-}

monadPlusN, mplusN, applicativeN, applyN, applyContN :: Bool -> Name
monadPlusN True = ''MonadPlus
monadPlusN False = mkName "MonadPlus"
applicativeN True = ''Applicative
applicativeN False = mkName "Applicative"
mplusN True = 'mplus
mplusN False = mkName "mplus"
applyN True = '(<$>)
applyN False = mkName "<$>"
applyContN True = '(<*>)
applyContN False = mkName "<*>"

m, a, p :: Name
m = mkName "m"
a = mkName "a"
p = mkName "p"

listDec :: Name -> Name -> Bool -> [Dec]
listDec list list1 th = [
	SigD list $ ForallT [PlainTV m, PlainTV a]
		([classP' (monadPlusN th) [vm],
			classP' (applicativeN th) [vm]]) $
		ArrowT	`AppT` (VarT m `AppT` VarT a)
			`AppT` (VarT m `AppT` (ListT `AppT` VarT a)),
	SigD list1 $ ForallT [PlainTV m, PlainTV a]
		([classP' (monadPlusN th) [vm],
			classP' (applicativeN th) [vm]]) $
		ArrowT	`AppT` (VarT m `AppT` VarT a)
			`AppT` (VarT m `AppT` (ListT `AppT` VarT a)),
	FunD list $ (: []) $ flip (Clause [VarP p]) [] $ NormalB $
		InfixE (Just $ VarE list1 `AppE` VarE p)
			(VarE $ mplusN th)
			(Just returnEmpty),
	FunD list1 $ (: []) $ flip (Clause [VarP p]) [] $ NormalB $
		InfixE (Just $ InfixE (Just cons) app (Just $ VarE p))
			next
			(Just $ VarE list `AppE` VarE p)
 ] where
	vm = VarT m
	returnEmpty = VarE (mkName "return") `AppE` ListE []
	cons = ConE $ mkName ":"
	app = VarE $ applyN th
	next = VarE $ applyContN th

{-

optional :: (MonadPlus m, Applicative m) => m a -> m (Maybe a)
optional p = (Just <$> p) `mplus` return Nothing

-}

optionalDec :: Name -> Bool -> [Dec]
optionalDec optionalN th = [
	SigD optionalN $ mplusAndApp $ (VarT m `AppT` VarT a) `arrT`
		(VarT m `AppT` (ConT (mkName "Maybe") `AppT` VarT a)),
	FunD optionalN $ (: []) $ flip (Clause [VarP p]) [] $ NormalB $
		ConE (mkName "Just") `app` VarE p `mplusE` returnNothing
 ] where
	mplusAndApp = ForallT [PlainTV m, PlainTV a] [
		classP' (monadPlusN th) [VarT m],
		classP' (applicativeN th) [VarT m]
	 ]
	arrT f x = ArrowT `AppT` f `AppT` x
	mplusE x = InfixE (Just x) (VarE $ mplusN th) . Just
	returnNothing = VarE (mkName "return") `AppE` ConE (mkName "Nothing")
	app x = InfixE (Just x) (VarE $ applyN th) . Just
	
classP' :: Name -> [Type] -> Pred
classP' name ts = foldl AppT (ConT name) ts
