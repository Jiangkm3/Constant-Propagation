module Tcons1 where
import           AbstractMonad
import           Apron.Lincons1
import           Apron.Scalar
import           Control.Monad              (void)
import           Control.Monad.State.Strict (liftIO)

data Tcons1 = Tcons1 Constyp Texpr1 Scalar

type Scalar = Int

tconsMake :: Constyp
          -> Texpr1
          -> Scalar
          -> Abstract Tcons1
tconsMake c t s = do
  return (Tcons1 c t s)
