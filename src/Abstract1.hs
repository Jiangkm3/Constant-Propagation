module Abstract1 ( Abstract1
                 , abstractBottom
                 , abstractTop
                 , abstractOfLinconsArray
                 , abstractToLinconsArray
                 , abstractOfTconsArray
                 , abstractToTconsArray
                 , abstractPrint
                 -- * Environment
                 , abstractGetEnvironment
                 , abstractChangeEnvironment
                 , abstractMinimizeEnvironment
                 , abstractUpdateEnvironment
                 -- * Tests
                 , abstractIsTop
                 , abstractIsBottom
                 , abstractIsLeq
                 , abstractIsEq
                 , abstractSatLincons
                 , abstractSatTcons
                 , abstractVarIsUnconstrained
                 -- * Extract properties
                 , abstractBoundLinexpr
                 , abstractBoundTexpr
                 , abstractBoundVar
                 -- * Operations
                 , abstractMeet
                 , abstractJoin
                 , abstractArrayMeet
                 , abstractArrayJoin
                 , abstractLinconsArrayMeet
                 , abstractTconsArrayMeet
                 , abstractUnify
                 , abstractCanonicalize
                 , abstractMinimize
                 -- * Expand, fold, widen, closure
                 , abstractExpand
                 , abstractFold
                 , abstractWiden
                 , abstractClosure
                 -- * Assign, substitute
                 , abstractAssignTexprArray
                 ) where
import           AbstractMonad
import           Apron.Abstract1
import           Apron.Environment
import           Apron.Interval
import           Apron.Lincons1
import           Apron.Linexpr1
import           Apron.Tcons1
import           Apron.Texpr1
import           Apron.Var
import           Control.Monad.State.Strict
import           Foreign                    hiding (addForeignPtrFinalizer,
                                             void)
import           Foreign.Concurrent
import           Foreign.ForeignPtr.Unsafe

-- Internal infrastructure

getAbstractVar :: Abstract1
               -> VarName
               -> Abstract Var
getAbstractVar a v = do
  env <- abstractGetEnvironment a
  var <- varMake v
  exists <- liftIO $ apEnvironmentMemVarWrapper env var
  unless exists $
    error $ unwords ["Variable", v, "does not exist in the abstract environment"]
  return var

getAbstractVars :: Abstract1
                -> [VarName]
                -> Abstract Var
getAbstractVars a vns = do
  -- replace this with something that doesn't allocate someday
  forM_ vns $ getAbstractVar a
  varsMake vns

_newAbstractVar :: VarName -> Abstract Var
_newAbstractVar = varMake

newAbstractVars :: [VarName] -> Abstract Var
newAbstractVars = varsMake

makeAbstractArray :: [Abstract1] -> Abstract Abstract1
makeAbstractArray as = liftIO $ do
  cptr <- doMakeArray
  fptr <- castForeignPtr `liftM` newForeignPtr_ cptr
  addForeignPtrFinalizer fptr $ free cptr
  return $ Abstract1 $ fptr
  where doMakeArray = do
          newArray =<< traverse toPtr as
        toPtr (Abstract1 fptr) = do
          -- This is gross
          touchForeignPtr fptr
          return $ unsafeForeignPtrToPtr fptr

-- Constructors

abstractBottom :: Abstract Abstract1
abstractBottom = do
  man <- getManager
  env <- getEnvironment
  liftIO $ apAbstract1BottomWrapper man env

abstractTop :: Abstract Abstract1
abstractTop = do
  man <- getManager
  env <- getEnvironment
  liftIO $ apAbstract1TopWrapper man env

abstractOfLinconsArray :: Lincons1Array -> Abstract Abstract1
abstractOfLinconsArray arr = do
  man <- getManager
  env <- getEnvironment
  liftIO $ apAbstract1OfLinconsArrayWrapper man env arr

abstractToLinconsArray :: Abstract1 -> Abstract Lincons1Array
abstractToLinconsArray a = do
  man <- getManager
  liftIO $ apAbstract1ToLinconsArrayWrapper man a

abstractToTconsArray :: Abstract1 -> Abstract Tcons1Array
abstractToTconsArray a = do
  man <- getManager
  liftIO $ apAbstract1ToTconsArrayWrapper man a

abstractOfTconsArray :: Tcons1Array -> Abstract Abstract1
abstractOfTconsArray arr = do
  man <- getManager
  env <- getEnvironment
  liftIO $ apAbstract1OfTconsArrayWrapper man env arr

-- Printing

abstractPrint :: Abstract1 -> Abstract ()
abstractPrint a = do
  man <- getManager
  liftIO $ printAbstract1 man a

-- Environment

abstractGetEnvironment :: Abstract1 -> Abstract Environment
abstractGetEnvironment a = do
  man <- getManager
  liftIO $ apAbstract1Environment man a

-- | Change the abstract environment
abstractChangeEnvironment :: Abstract1
                          -> Environment
                          -> Bool
                          -> Abstract Abstract1
abstractChangeEnvironment a env proj = do
  man <- getManager
  liftIO $ apAbstract1ChangeEnvironmentWrapper man False a env proj

-- | Remove from the environment of the abstract value
-- variables that are unconstrained in it.
abstractMinimizeEnvironment :: Abstract1 -> Abstract Abstract1
abstractMinimizeEnvironment a = do
  man <- getManager
  liftIO $ apAbstract1MinimizeEnvironmentWrapper man False a

-- | Parallel renaming. The new variables should not interfere with
-- the variables that are not renamed.
abstractUpdateEnvironment :: Abstract1
                          -> [VarName]
                          -> [VarName]
                          -> Abstract Abstract1
abstractUpdateEnvironment a vns nvns = do
  man <- getManager
  vars <- getAbstractVars a vns
  newVars <- newAbstractVars nvns
  liftIO $ apAbstract1RenameArrayWrapper man False a vars newVars $ fromIntegral len
  where len = length vns

-- Tests

abstractIsBottom :: Abstract1 -> Abstract Bool
abstractIsBottom a = do
  man <- getManager
  liftIO $ apAbstract1IsBottom man a

abstractIsTop :: Abstract1 -> Abstract Bool
abstractIsTop a = do
  man <- getManager
  liftIO $ apAbstract1IsTop man a

abstractIsLeq :: Abstract1 -> Abstract1 -> Abstract Bool
abstractIsLeq a1 a2 = do
  man <- getManager
  liftIO $ apAbstract1IsLeq man a1 a2

abstractIsEq :: Abstract1 -> Abstract1 -> Abstract Bool
abstractIsEq a1 a2 = do
  man <- getManager
  liftIO $ apAbstract1IsEq man a1 a2

abstractSatLincons :: Abstract1 -> Lincons1 -> Abstract Bool
abstractSatLincons a c = do
  man <- getManager
  liftIO $ apAbstract1SatLincons man a c

abstractSatTcons :: Abstract1 -> Tcons1 -> Abstract Bool
abstractSatTcons a c = do
  man <- getManager
  liftIO $ apAbstract1SatTcons man a c

abstractVarIsUnconstrained :: Abstract1 -> VarName -> Abstract Bool
abstractVarIsUnconstrained a v = do
  man <- getManager
  var <- getAbstractVar a v
  liftIO $ apAbstract1IsVariableUnconstrained man a var

-- Extracting properties

abstractBoundLinexpr :: Abstract1 -> Linexpr1 -> Abstract Interval
abstractBoundLinexpr a e = do
  man <- getManager
  liftIO $ apAbstract1BoundLinexpr man a e

abstractBoundTexpr :: Abstract1 -> Texpr1 -> Abstract Interval
abstractBoundTexpr a t = do
  man <- getManager
  liftIO $ apAbstract1BoundTexpr man a t

abstractBoundVar :: Abstract1 -> VarName -> Abstract Interval
abstractBoundVar a v = do
  man <- getManager
  var <- getAbstractVar a v
  liftIO $ apAbstract1BoundVariable man a var

-- Operations

-- | Meet of two abstract values.
abstractMeet :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractMeet a1 a2 = do
  man <- getManager
  liftIO $ apAbstract1MeetWrapper man False a1 a2

-- | Join of two abstract values.
abstractJoin :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractJoin a1 a2 = do
  man <- getManager
  liftIO $ apAbstract1JoinWrapper man False a1 a2

abstractArrayMeet :: [Abstract1] -> Word32 -> Abstract Abstract1
abstractArrayMeet as size = do
  a <- makeAbstractArray as
  man <- getManager
  liftIO $ apAbstract1MeetArrayWrapper man a $ fromIntegral size

abstractArrayJoin :: [Abstract1] -> Word32 -> Abstract Abstract1
abstractArrayJoin as size = do
  a <- makeAbstractArray as
  man <- getManager
  liftIO $ apAbstract1JoinArrayWrapper man a $ fromIntegral size

abstractLinconsArrayMeet :: Abstract1 -> Lincons1Array -> Abstract Abstract1
abstractLinconsArrayMeet a arr = do
  man <- getManager
  liftIO $ apAbstract1MeetLinconsArrayWrapper man False a arr

abstractTconsArrayMeet :: Abstract1 -> Tcons1Array -> Abstract Abstract1
abstractTconsArrayMeet a arr = do
  man <- getManager
  liftIO $ apAbstract1MeetTconsArrayWrapper man False a arr

abstractUnify :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractUnify a1 a2 = do
  man <- getManager
  liftIO $ apAbstract1UnifyWrapper man False a1 a2

-- | Put the abstract value in canonical form. (not yet clear definition).
abstractCanonicalize :: Abstract1 -> Abstract ()
abstractCanonicalize a = do
  man <- getManager
  liftIO $ apAbstract1Canonicalize man a

-- | Minimize the size of the representation of the abstract input.
-- his may result in a later recomputation of internal information.
abstractMinimize :: Abstract1 -> Abstract ()
abstractMinimize a = do
  man <- getManager
  liftIO $ apAbstract1Minimize man a

-- Expanding, folding, widening, closure

abstractExpand :: Abstract1
               -> VarName
               -> [VarName]
               -> Abstract Abstract1
abstractExpand a v vns = do
  man <- getManager
  var <- getAbstractVar a v
  vars <- newAbstractVars vns
  liftIO $ apAbstract1ExpandWrapper man False a var vars $ fromIntegral len
  where len = length vns

abstractFold :: Abstract1 -> [VarName] -> Abstract Abstract1
abstractFold a vns = do
  man <- getManager
  vars <- getAbstractVars a vns
  liftIO $ apAbstract1FoldWrapper man False a vars (fromIntegral $ length vns)

abstractWiden :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractWiden a1 a2 = do
  man <- getManager
  liftIO $ apAbstract1WideningWrapper man a1 a2

abstractClosure :: Abstract1 -> Abstract Abstract1
abstractClosure a = do
  man <- getManager
  liftIO $ apAbstract1ClosureWrapper man False a

-- | Assign a list of variables in the abstract domain to the evaluation
-- a tree expression
abstractAssignTexprArray :: Abstract1 -> [VarName] -> Texpr1 -> Word32 -> Abstract1 -> Abstract Abstract1
abstractAssignTexprArray a1 vs texpr size a2 = do
  man <- getManager
  var <- getVar (vs !! 0)
  liftIO $ apAbstract1AssignTexprArrayWrapper man False a1 var texpr (fromIntegral size) a2
