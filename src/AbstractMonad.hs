{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AbstractMonad where
import           Apron.Environment
import           Apron.Manager
import           Apron.Var
import qualified Control.Monad.Fail         as Fail
import           Control.Monad.State.Strict
import           Data.List                  (nub)
import qualified Data.Map                   as M
import           Foreign                    hiding (addForeignPtrFinalizer,
                                             void)
import           Foreign.C.String
import           Foreign.Concurrent


data Domain = Intervals
            | Polyhedra
            | Octagons
            | Zonotopes
            deriving (Eq, Ord, Show)

type VarName = String

data AbstractState = AbstractState { unDomain      :: Domain
                                   , unManager     :: Manager
                                   , unEnvironment :: Environment
                                   , unVars        :: M.Map VarName Var
                                   }

-- | Monad for a given analysis
newtype Abstract a = Abstract { unAbstractState :: StateT AbstractState IO a }
    deriving (Functor, Applicative, Monad, MonadState AbstractState, MonadIO, Fail.MonadFail)

-- | Evaluate an abstract action with a given configurations.
evalAbstract :: AbstractState -> Abstract a -> IO a
evalAbstract aState act = evalStateT (unAbstractState act) aState

defaultState :: AbstractState
defaultState = AbstractState Intervals undefined undefined M.empty

getDomain :: Abstract Domain
getDomain = gets unDomain

getManager :: Abstract Manager
getManager = gets unManager

getEnvironment :: Abstract Environment
getEnvironment = gets unEnvironment

initVar :: VarName -> Abstract ()
initVar v = do
  var <- varMake v
  s0 <- get
  put $ s0 { unVars = M.insert v var $ unVars s0 }

findVar :: VarName -> Abstract Bool
findVar v = do
  vs <- gets unVars
  case M.lookup v vs of
    Nothing -> return False
    _       -> return True

getVar :: VarName -> Abstract Var
getVar v = do
  vs <- gets unVars
  case M.lookup v vs of
    Nothing -> error $ unwords [ "Variable", v, "does not exist in environment" ]
    Just var -> return var

getVars :: [VarName] -> Abstract Var
getVars vns = do
  -- To fail if they're not in the map
  forM_ vns getVar
  varsMake vns

-- More complicated setup and var creation

-- | Given a variable name, make a Var representing that variable
varMake :: VarName -> Abstract Var
varMake v = liftIO $ do
  str <- newCString v
  makeVar str

-- | Given a list of variable names, make a pointer to a C list of variable representations
-- This is a really gross function.  We could unsafe cast the Var
-- foreign pointer to pointer, but that would be unsafe. Let's
-- instead just check the map for the var names and just create the
-- string array again.
varsMake :: [VarName] -> Abstract Var
varsMake vns = liftIO $ do
  cptr <- doMakeVarArray
  fptr <- castForeignPtr `liftM` newForeignPtr_ cptr
  addForeignPtrFinalizer fptr $ doFreeVarArray cptr
  return $ Var $ fptr
  where len = length vns
        doMakeVarArray = do
          newArray =<< traverse newCString vns
        doFreeVarArray ptr = do
             strs <- peekArray len ptr
             void $ traverse free strs
             free ptr

initAbstractState :: Domain
                 -> [VarName]
                 -> [VarName]
                 -> Abstract ()
initAbstractState domain intVars realVars = do
  unless (intVars == nub intVars)   $ error $ unwords [ "Dupe int vars",  show intVars  ]
  unless (realVars == nub realVars) $ error $ unwords [ "Dupe real vars", show realVars ]
  env <- liftIO $ apEnvironmentAlloc intVars realVars
  manager <- case domain of
               Intervals -> liftIO boxManagerAlloc
               _ -> error $ unwords [ "Unsupported domain:"
                                    , show domain
                                    ]
  forM_ (intVars ++ realVars) initVar
  s0 <- get
  put $ s0 { unDomain      = domain
           , unManager     = manager
           , unEnvironment = env
           }

liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO

-- | Lift an 'IO' operation with 2 arguments into another monad
liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 = ((.).(.)) liftIO

-- | Lift an 'IO' operation with 3 arguments into another monad
liftIO3 :: MonadIO m => (a -> b -> c -> IO d) -> a -> b -> c -> m d
liftIO3 = ((.).((.).(.))) liftIO

-- | Lift an 'IO' operation with 4 arguments into another monad
liftIO4 :: MonadIO m => (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> m e
liftIO4 = ((.).((.).((.).(.)))) liftIO

-- | Lift an 'IO' operation with 5 arguments into another monad
liftIO5 :: MonadIO m => (a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> m f
liftIO5 = ((.).((.).((.).((.).(.))))) liftIO

