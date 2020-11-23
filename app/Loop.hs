-- A module that deals with threshold narrowing

module Loop where

import Init
import Types
import Operation
import Abstract1
import AbstractMonad
import Texpr1
import Tcons1
import GHC.Int
import Apron.Scalar
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.Syntax.Constants
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict (liftIO)

-- We want to have a Texpr for every variable
type LastVar = Map String (Maybe Texpr1)

-- A simple AST to record the entire constraint on the last iteration
data LoopAST = LTcons Tcons1
             | LNull
             | LAnd LoopAST LoopAST
             | LOr LoopAST LoopAST
             | LNot LoopAST

-- Notice that a variable in the constraint may never appear in the loop
-- body, so that it is never in LastVar
lastCond :: Abstract1 -> String -> LastVar -> CExpression a -> Abstract LoopAST
lastCond a f lv (CBinary bop expr1 expr2 _)
  | isBOpLogic bop = do
    lAst <- lastCond a f lv expr1
    rAst <- lastCond a f lv expr2
    case bop of
      CLndOp -> return (LAnd lAst rAst)
      CLorOp -> return (LOr lAst rAst)
  | isBOpCons bop = do
    lAst <- lastCondHelper a f lv expr1
    rAst <- lastCondHelper a f lv expr2
    lastCondBOpHelper lAst rAst bop
  | otherwise = error "Invalid Comparison"

lastCondBOpHelper :: Maybe Texpr1 -> Maybe Texpr1 -> CBinaryOp -> Abstract LoopAST
lastCondBOpHelper Nothing _ _ = return LNull
lastCondBOpHelper _ Nothing _ = return LNull
lastCondBOpHelper (Just l) (Just r) bop = do
  nl <- case bop of
          CLeOp  -> texprMakeBinOp SUB_OP r l ROUND_INT ROUND_DOWN
          CLeqOp -> texprMakeBinOp SUB_OP r l ROUND_INT ROUND_DOWN
          _      -> texprMakeBinOp SUB_OP l r ROUND_INT ROUND_DOWN
  nr <- liftIO $ constrScalar 0
  tcons <- tconsMake (evalConsBOp bop) nl nr
  return (LTcons tcons)

lastCondHelper :: Abstract1 -> String -> LastVar -> CExpression a -> Abstract (Maybe Texpr1)
lastCondHelper a f lv (CVar (Ident v _ _) _) = do
  var   <- findScope v f
  let texpr = Map.lookup var lv
  case texpr of
    Nothing -> (texprMakeLeafVar var) >>= (\a -> return (Just a))
    Just a  -> return a
lastCondHelper a f lv (CConst (CIntConst n _)) = do
  texpr <- texprMakeConstant $ ScalarVal $ IntValue $ (fromInteger (getCInteger n) :: Int32)
  return (Just texpr)
lastCondHelper a f lv _ = error "Statement not supported in loop"

-- The goal is to obtain an AST of texpr1
-- In this case, we don't need to worry about init
getLoopCons :: Abstract1 -> String -> CExpression a -> Maybe (CExpression a) -> CStatement a -> Abstract LoopAST
getLoopCons a f cond step stmt = do
  -- Initialization: every variable is mapped to itself
  -- We delay this step until a variable is actually mentioned
  let varSt = Just (Map.empty)
  -- Backward-analyze the loop
  (initVS, _) <- case step of
                 Nothing     -> return (varSt, Nothing)
                 Just (expr) -> backwardExprCheck a f varSt expr
  finalVS <- backwardCheck a f initVS stmt
  case finalVS of
    Nothing -> return LNull
    Just vs -> lastCond a f vs cond

backwardCheck :: Abstract1 -> String -> Maybe LastVar -> CStatement a -> Abstract (Maybe LastVar)
backwardCheck _ _ Nothing _ = return Nothing
backwardCheck _ _ initSt (CExpr Nothing _) = return initSt
backwardCheck a f initSt (CExpr (Just expr) _) = do
  (nSt, _) <- backwardExprCheck a f initSt expr
  return nSt
-- For If Statement, as long as the variable is changed the same way for
-- both cases, we are goodd
backwardCheck a f initSt (CIf _ _ _ _) = return Nothing
{-
backwardCheck a f initSt (CIf cons tstmt Nothing _) = do
  tSt <- backwardCheck a f initSt tstmt
  case (initSt == tSt) of
    True  -> return tSt
    False -> return Nothing
backwardCheck a f initSt (CIf cons tstmt (Just fstmt) _) = do
  tSt <- backwardCheck a f initSt tstmt
  fSt <- backwardCheck a f initSt fstmt
  case (tSt == fSt) of
    True  -> return tSt
    False -> return Nothing
-}
-- Ignore nested loops for now
backwardCheck _ _ _ (CFor _ _ _ _ _) = return Nothing
backwardCheck _ _ _ (CWhile _ _ _ _) = return Nothing
backwardCheck a f initSt (CCompound _ cbis _) = foldr (backwardCBICheck a f) (return initSt) cbis
backwardCheck _ _ _ _ = error "Unsupported statements inside a loop"

backwardCBICheck :: Abstract1 -> String -> CCompoundBlockItem a -> Abstract (Maybe LastVar) -> Abstract (Maybe LastVar)
backwardCBICheck a f cbi lv = do
  l <- lv
  case (l, cbi) of
    (Nothing, _) -> return Nothing
    (iSt, CBlockStmt stmt) -> backwardCheck a f iSt stmt
    (iSt, CBlockDecl decl) -> backwardDeclCheck a f iSt decl

backwardDeclCheck :: Abstract1 -> String -> Maybe LastVar -> CDeclaration a -> Abstract (Maybe LastVar)
backwardDeclCheck _ _ Nothing _ = return Nothing
backwardDeclCheck a f initSt (CDecl _ d _) = foldr (backwardDeclHelper a f) (return initSt) d

backwardDeclHelper :: Abstract1 -> String -> (Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a)) -> Abstract (Maybe LastVar) -> Abstract (Maybe LastVar)
-- We might be able to find out what the value of the variable is before it
-- is assigned, but right now let's ignore it
backwardDeclHelper a f (Just (CDeclr (Just (Ident v _ _)) _ _ _ _), Nothing, Nothing) initSt = do
  var <- findScope v f
  Just iSt <- initSt
  let nSt = Map.insert var Nothing iSt
  return (Just nSt)
backwardDeclHelper a f (Just (CDeclr (Just (Ident v _ _)) _ _ _ _), (Just (CInitExpr expr _)), Nothing) initSt = do
  var <- findScope v f
  Just iSt <- initSt
  (nSt, nVal) <- backwardExprCheck a f (Just iSt) expr
  case (nSt, nVal) of
    (Nothing, _) -> return Nothing
    (Just n, _)  -> return (Just (Map.insert var Nothing n))
backwardDeclHelper _ _ _ _ = error "Declaration case not supported"

-- We also need to keep track of the value of the current expression
-- Nothing if the current expression does not give us an integer value
backwardExprCheck :: Abstract1 -> String -> Maybe LastVar -> CExpression a -> Abstract (Maybe LastVar, Maybe Texpr1)
backwardExprCheck _ _ Nothing _ = return (Nothing, Nothing)
backwardExprCheck a f (Just iSt) (CVar (Ident v _ _) _) = do
  var <- findScope v f
  let varExpr = Map.lookup var iSt
  vtexpr <- texprMakeLeafVar var
  let nSt = case varExpr of
              Nothing -> Just (Map.insert var (Just vtexpr) iSt)
              _       -> Just iSt
  case varExpr of
    Nothing -> return (nSt, Just vtexpr)
    Just ve -> return (nSt, ve)
backwardExprCheck _ _ initSt (CConst (CIntConst cint _)) = do
  i <- texprMakeConstant $ ScalarVal $ IntValue $ (fromInteger (getCInteger cint) :: Int32)
  return (initSt, Just i)
-- We need to deal with ++ or --
backwardExprCheck a f initSt@(Just iSt) (CUnary uop (CVar (Ident v _ _) _) _) = do
  var <- findScope v f
  let varExpr = Map.lookup var iSt
  nExpr <- case varExpr of
             Nothing -> (texprMakeLeafVar var) >>= (\a -> return (Just a))
             Just Nothing -> return Nothing
             Just (Just ve) -> return (Just ve)
  l <- texprMakeConstant $ ScalarVal $ IntValue (-1)
  let tmbo op te = (texprMakeBinOp op te l ROUND_INT ROUND_DOWN) >>= (\a -> return (Just a))
  fExpr <- case (uop, nExpr) of
             (_, Nothing) -> return Nothing
             (CPreIncOp, Just n)  -> tmbo ADD_OP n 
             (CPostIncOp, Just n) -> tmbo ADD_OP n
             (CPreDecOp, Just n)  -> tmbo SUB_OP n
             (CPostDecOp, Just n) -> tmbo SUB_OP n
             (_, Just n)          -> return (Just n)
  let nSt = Just (Map.insert var fExpr iSt)
  minSt <- case nExpr of
             Nothing -> return Nothing
             Just ve -> texprMakeBinOp MUL_OP ve l ROUND_INT ROUND_DOWN >>= (\a -> return (Just a))
  case uop of
    CPreIncOp  -> return (nSt, nExpr)
    CPostIncOp -> return (nSt, fExpr)
    CPreDecOp  -> return (nSt, nExpr)
    CPostDecOp -> return (nSt, fExpr)
    CPlusOp    -> return (initSt, nExpr)
    CMinOp     -> return (initSt, minSt)
    _          -> return (initSt, Nothing)
{-
backwardExprCheck a f initSt (CBinary bop expr1 expr2 _)
  (lSt, ltexpr) <- backwardExprCheck a f initSt expr1
  (rSt, rtexpr) <- backwardExprCheck a f initSt expr2
  | (lSt == Nothing) || (rSt == Nothing)   = (Nothing, Nothing)
  -- Let's hope that there are no nested assignment
  | (lSt /= initSt) || (rSt /= initSt)     = (Nothing, Nothing)
  | (lInt == Nothing) || (rInt == Nothing) = (initSt, Nothing)
  | otherwise =
    let (Just l) = lInt
        (Just r) = rInt
    in case bop of
         CMulOp -> (initSt, Just (l * r))
         CDivOp -> (initSt, Just (l `div` r))
         CRmdOp -> (initSt, Just (l `mod` r))
         CAddOp -> (initSt, Just (l + r))
         CSubOp -> (initSt, Just (l - r))
         _      -> (initSt, Nothing)
-}
backwardExprCheck a f initSt@(Just iSt) (CAssign aop (CVar (Ident v _ _) _) expr _) = do
  (nSt, nInt) <- backwardExprCheck a f initSt expr
  var <- findScope v f
  let varExpr = Map.lookup var iSt
  backwardAOpHelper nSt nInt var aop varExpr
backwardExprCheck _ _ _ _ = error "expression case not implemented in loop"

backwardAOpHelper :: Maybe LastVar -> Maybe Texpr1 -> String -> CAssignOp -> Maybe (Maybe Texpr1) -> Abstract (Maybe LastVar, Maybe Texpr1)
-- There are so many different cases
-- Case 1: the variable state becomes false
backwardAOpHelper Nothing _ _ _ _ = return (Nothing, Nothing)
-- Case 2: the evaluated value is undetermined
backwardAOpHelper (Just iSt) Nothing var _ _ = do
  let nSt = Just (Map.insert var Nothing iSt)
  return (nSt, Nothing)
-- Case 3: the variable is reassigned
backwardAOpHelper (Just iSt) _ var CAssignOp _ = do
  let nSt = Just (Map.insert var Nothing iSt)
  return (nSt, Nothing)
-- Case 4: the normal case
backwardAOpHelper (Just iSt) (Just ntexpr) var aop texpr0 = do
  -- If the variable is never in the LastVar before, create one and make the
  -- AST same as the old one
  texpr1 <- case texpr0 of
              Nothing -> (texprMakeLeafVar var) >>= (\a -> return (Just a))
              Just t0 -> return t0
  let tmbo op t1 t2 = (texprMakeBinOp op t1 t2 ROUND_INT ROUND_DOWN) >>= (\a -> return (Just a))
  texpr2 <- case (texpr1, aop) of
              (Nothing, _) -> return Nothing
              (Just t1, CAddAssOp) -> tmbo SUB_OP t1 ntexpr
              (Just t1, CSubAssOp) -> tmbo ADD_OP t1 ntexpr
              (Just t1, CMulAssOp) -> tmbo DIV_OP t1 ntexpr
              -- For other assop like div or mod, we cannot obtain the inverse
              (_, _)               -> return Nothing
  let nSt = Just (Map.insert var texpr2 iSt)
  -- Unlike the case of ++ / --, the resultant expression is always returned
  return (nSt, texpr1)
