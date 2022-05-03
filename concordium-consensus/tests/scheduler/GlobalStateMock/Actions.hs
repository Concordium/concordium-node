{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GlobalStateMock.Actions where

import Concordium.Utils
import Data.Type.Equality
import Language.Haskell.TH

-- |Typeclass of mocked action types.  An action type is parametrised by the result type.
-- Action types are implemented as GADTs.
class (forall a. Show (f a)) => Act f where
    -- |Returns @Just Refl@ if the two actions are equal, and @Nothing@ otherwise.
    eqAct :: f a -> f b -> Maybe (a :~: b)

    -- |Show a value of the result type of the given action.
    showRes :: f r -> r -> String

-- |Generate an instance of 'Act' for a type.
generateAct :: Name -> Q [Dec]
generateAct typName = do
    (TyConI (DataD _ _ bindrs _ constructors _)) <- reify typName
    let bndName (PlainTV nm) = nm
        bndName (KindedTV nm _) = nm
    let vars = bndName <$> init bindrs
    let accumConstrs (NormalC n l) = ((n, length l) :)
        accumConstrs (RecC n l) = ((n, length l) :)
        accumConstrs (InfixC _ n _) = ((n, 2) :)
        accumConstrs (ForallC _ _ c) = accumConstrs c
        accumConstrs (GadtC ns l _) = ([(n, length l) | n <- ns] ++)
        accumConstrs (RecGadtC ns l _) = ([(n, length l) | n <- ns] ++)
        constrs = foldr accumConstrs [] constructors
        xs l = [mkName ('x' : show n) | n <- [1 .. l]]
        ys l = [mkName ('y' : show n) | n <- [1 .. l]]
        eqGuard l = PatG [NoBindS (InfixE (Just $ VarE x) (VarE $ mkName "==") (Just $ VarE y)) | (x, y) <- zip (xs l) (ys l)]
    let eqActDec =
            FunD (mkName "eqAct") $
                [ Clause
                    [ConP n (VarP <$> xs l), ConP n (VarP <$> ys l)]
                    (GuardedB [(eqGuard l, ConE (mkName "Just") `AppE` ConE (mkName "Refl"))])
                    []
                  | (n, l) <- constrs
                ]
                    ++ [Clause [WildP, WildP] (NormalB (ConE (mkName "Nothing"))) []]
    let showResDec =
            FunD
                (mkName "showRes")
                [ Clause [ConP n (replicate l WildP)] (NormalB (VarE (mkName "show"))) []
                  | (n, l) <- constrs
                ]
    return
        [ InstanceD
            Nothing
            []
            (AppT (ConT ''Act) (foldr (flip AppT . VarT) (ConT typName) vars))
            [eqActDec, showResDec]
        ]

-- |Generate an instance of a typeclass from a mocking datatype.
-- This takes a partial instance as the first argument.
-- The second argument is the name of the datatype.
-- The constructed instance takes each constructor of the datatype.
-- If there is not already a function in the instance corresponding to the constructor (but with
-- the first character lower-cased), then a function is added by applying the third argument
-- (a quoted expression) to the fully-applied constructor.
mockOperations :: Q [Dec] -> Name -> Q Exp -> Q [Dec]
mockOperations qinst typName expr = do
    [InstanceD olp ctx inst decs0] <- qinst
    (TyConI (DataD _ _ _ _ constructors _)) <- reify typName
    e <- expr
    let accumConstrs (NormalC n l) = ((n, length l) :)
        accumConstrs (RecC n l) = ((n, length l) :)
        accumConstrs (InfixC _ n _) = ((n, 2) :)
        accumConstrs (ForallC _ _ c) = accumConstrs c
        accumConstrs (GadtC ns l _) = ([(n, length l) | n <- ns] ++)
        accumConstrs (RecGadtC ns l _) = ([(n, length l) | n <- ns] ++)
        constrs = foldr accumConstrs [] constructors
        xs l = [mkName ('x' : show n) | n <- [1 .. l]]
        excls = [nameBase n | (FunD n _) <- decs0]
    let decs =
            [ FunD
                (mkName (firstLower (nameBase n)))
                [ Clause
                    (VarP <$> xs l)
                    (NormalB (AppE e (foldl AppE (ConE n) (VarE <$> xs l))))
                    []
                ]
              | (n, l) <- constrs,
                firstLower (nameBase n) `notElem` excls
            ]
    return
        [ InstanceD
            olp
            ctx
            inst
            (decs0 ++ decs)
        ]
