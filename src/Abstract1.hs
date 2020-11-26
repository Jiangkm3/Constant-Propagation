module Abstract1  where
import           AbstractMonad
import           Apron.Var
import           Apron.Interval
import           Apron.Lincons1
import           Apron.Linexpr1
import           Apron.Tcons1
import           Apron.Texpr1
import           Control.Monad.State.Strict

type VarMap = M.Map VarName Var

-- Bool is used to determine if the state is unreachable
-- If it is unreachable, the values no longer matter
-- Once an abstract state becomes unreachable, there is really no way to
-- make it reachable again, except reinitialize and join
data Abstract1 = Abs1 VarMap Bool

-- Constructors

abstractBottom :: Abstract Abstract1
abstractBottom = do
  (Env vs) <- gets unEnvironment
  -- No need to initialize the mapping since Bottom is a placeholder anyways
  let abs = M.Empty
  return (Abs1 abs True)

abstractTop :: Abstract Abstract1
abstractTop = do
  (Enc vs) <- gets unEnvironment
  let abs = fromList (map (\a -> (a, Top) vs ))
  return (Abs1 abs False)

-- Printing

abstractPrint :: Abstract1 -> Abstract ()
abstractPrint a = do
  man <- getManager
  liftIO $ printAbstract1 man a

-- Tests

abstractIsBottom :: Abstract1 -> Abstract Bool
abstractIsBottom (Abs1 _ bot) = do
  return bot

-- Every Variable is Top
abstractIsTop :: Abstract1 -> Abstract Bool
abstractIsTop (Abs1 vs bot) = do
  it = foldl (and) True (map (\a -> isTop a) vs)
  case (it, bot) of
    (_    , True)  -> return False
    (True , False) -> return True
    (False, False) -> return False

-- We have to assume that the variables are the same. Otherwise abstractIsLeq
-- won't make sense
abstractIsLeq :: Abstract1 -> Abstract1 -> Abstract Bool
abstractIsLeq (Abs1 _ True) _ = return True
abstractIsLeq _ (Abs1 _ True) = return False
abstractIsLeq (Abs1 vs1 _) (Abs1 vs2 _) = do
  nvs = intersectionWith isLeq vs1 vs2
  return (foldl and True nvs)

-- Again, assume that the variables are the same.
abstractIsEq :: Abstract1 -> Abstract1 -> Abstract Bool
abstractIsEq (Abs1 _ True) (Abs1 _ True) = return True
abstractIsEq (Abs1 vs1 False) (Abs vs2 False) = do
  nvs = intersectionWith isEq vs1 vs2
  return (foldl and True nvs)
abstractIsEq _ _ = return False

-- Operations

-- | Meet of two abstract values.
abstractMeet :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractMeet (Abs1 _ True) _           = return (Abs1 M.Empty True)
abstractMeet _ (Abs1 _ True)           = return (Abs1 M.Empty True)
abstractMeet (Abs1 vs1 _) (Abs1 vs2 _) =
  nvs = unionWith meet vs1 vs2
  return (Abs1 nvs False)

-- | Join of two abstract values.
abstractJoin :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractJoin (Abs1 _ True) a           = return a
abstractJoin a (Abs1 _ True)           = return a
abstractJoin (Abs1 vs1 _) (Abs1 vs2 _) =
  nvs = unionWith join vs1 vs2
  return (Abs1 nvs False)

abstractTconsArrayMeet :: Abstract1 -> Tcons1Array -> Abstract Abstract1
abstractTconsArrayMeet a arr = do
  man <- getManager
  liftIO $ apAbstract1MeetTconsArrayWrapper man False a arr

abstractWiden :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractWiden (Abs1 _ True) a           = return a
abstractWiden a (Abs1 _ True)           = return a
abstractWiden (Abs1 vs1 _) (Abs1 vs2 _) =
  nvs = unionWith widen vs1 vs2
  return (Abs1 nvs False)

-- | Assign a list of variables in the abstract domain to the evaluation
-- a tree expression
abstractAssignTexprArray :: Abstract1 -> [VarName] -> Texpr1 -> Word32 -> Abstract1 -> Abstract Abstract1
abstractAssignTexprArray a1 vs texpr size a2 = do
  man <- getManager
  var <- getVar (vs !! 0)
  liftIO $ apAbstract1AssignTexprArrayWrapper man False a1 var texpr (fromIntegral size) a2
