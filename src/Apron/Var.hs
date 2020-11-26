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

add :: Var -> Var -> Var
add Bottom _            = Bottom
add _ Bottom            = Bottom
add Top _               = Top
add _ Top               = Top
add (Const a) (Const b) = Const (a + b)

sub :: Var -> Var -> Var
sub Bottom _            = Bottom
sub _ Bottom            = Bottom
sub Top _               = Top
sub _ Top               = Top
sub (Const a) (Const b) = Const (a - b)

mul :: Var -> Var -> Var
mul Bottom _            = Bottom
mul _ Bottom            = Bottom
mul Top _               = Top
mul _ Top               = Top
mul (Const a) (Const b) = Const (a * b)

div :: Var -> Var -> Var
div Bottom _            = Bottom
div _ Bottom            = Bottom
div Top _               = Top
div _ Top               = Top
div (Const a) (Const b) = Const (a / b)

mod :: Var -> Var -> Var
mod Bottom _            = Bottom
mod _ Bottom            = Bottom
mod Top _               = Top
mod _ Top               = Top
mod (Const a) (Const b) = Const (a `mod` b)
