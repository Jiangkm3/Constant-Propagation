module Init where
import           Language.C.Data.Ident
import           Language.C.Data.Node
import           Language.C.Syntax.AST
import           Parser
import           Symbol
import           Abstract1
import           AbstractMonad

-- | A simple user-defined AST annotation type
-- You can extend this if it would be helpful
-- For now it perserves source code location info
data State a = State { stInfo  :: a
                     , locInfo :: NodeInfo
                     }
  deriving (Eq, Ord)

type AbsState = State (Abstract Abstract1)

instance Show (State a) where
  show _ = ""

toState :: a -> NodeInfo -> State a
toState = State

-- Uses language-c's built-in (nice) annotation support
initTo :: a -> CTranslUnit -> CTranslationUnit (State a)
initTo abs1 t = toState abs1 <$> t

-- Initialize everything to a start state and then print the AST
analyzeAST :: String -> IO (CTranslationUnit AbsState)
analyzeAST name = do
  symT <- getSymT name
  let abs1 = astHelper symT
  tu <- parseC name
  let initS = initTo abs1 tu
  return initS

astHelper :: [String] -> Abstract Abstract1
astHelper symT = do
  initAbstractState Intervals symT []
  abs <- abstractBottom
  return abs

-- Find the actual name of a variable
-- Assume that the variable must exist
-- We only need to find if it is local or global
findScope :: String -> String -> Abstract String
findScope varName funcName
  | funcName == "" = return varName
  | otherwise = do
    let localName = funcName ++ "@" ++ varName
    l <- findVar localName
    case l of
      True  -> return localName
      False -> return varName
