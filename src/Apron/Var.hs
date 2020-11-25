module Apron.Var where

data Var = Top | Bottom | Const Int
  deriving Show

isBottom :: Var -> Bool
isBottom Bottom = True
isBottom _      = False

isTop :: Var -> Bool
isTop Top = True
isTop _   = False

isLeq :: Var -> Var -> Bool
isLeq Bottom _ = True
isLeq _ Top    = True
-- Case (Top Top) and (Bottom Bottom) has been excluded
isLeq _ Bottom = False
isLeq Top _    = False
isLeq _ _      = True

isEq :: Var -> Var -> Bool
isEq Bottom Bottom       = True
isEq Top Top             = True
isEq (Const _) (Const _) = True
isEq _ _                 = False

meet :: Var -> Var -> Var
meet Bottom _            = Bottom
meet _ Bottom            = Bottom
meet Top a               = a
meet a Top               = a
meet (Const a) (Const b) =
  case (a == b) of
    True  -> Const a
    False -> Bottom

join :: Var -> Var -> Var
join Bottom a            = a
join a Bottom            = a
join Top _               = Top
join _ Top               = Top
join (Const a) (Const b) =
  case (a == b) of
    True  -> Const a
    False -> Top

-- No difference between widen and join in this domain
widen :: Var -> Var -> Var
widen a b = join a b
