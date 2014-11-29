module Text.XML.Light.Extractors.Result 
 ( Result(..)
 , toEither
 , escalate

 , ResultT
 , runResultT
 , throwError
 , throwFatal
 , mapResult

 , module Control.Monad.Trans.Error
 , Control.Monad.Trans.Class.lift
 )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Error (Error, noMsg, strMsg)
import Control.Monad.Trans.Class


-- | 'Result' is like 'Either' but with two error states, 'Fail' and 'Fatal'.
--
-- 'Fail' is precisely analogous to 'Left' while 'Fatal' has short cut
-- semantics for 'Alternative'.
--
-- The idea is that 'Fatal' errors cannot be circumvented by '<|>' etc.
data Result e a = Fatal e
                | Fail e
                | Ok a
  deriving Show


instance Functor (Result e) where
  fmap f (Ok a)    = Ok (f a)
  fmap _ (Fail e)  = Fail e
  fmap _ (Fatal e) = Fatal e


instance Applicative (Result e) where
  pure = Ok
  
  Ok f    <*> a = fmap f a
  Fatal e <*> _ = Fatal e
  Fail e  <*> _ = Fail e


instance Error e => Alternative (Result e) where
  empty = Fail noMsg

  Fatal e <|> _ = Fatal e
  Fail _  <|> x = x
  m       <|> _ = m


instance Monad (Result e) where
  return = pure

  Fatal e >>= _ = Fatal e
  Fail e  >>= _ = Fail e
  Ok a    >>= k = k a


-- | Maps 'Fail' to 'Fatal'.
escalate :: Result e a -> Result e a
escalate (Fail e) = Fatal e
escalate x        = x


-- | Maps 'Fail' and 'Fatal' to 'Left'.
toEither (Fatal e) = Left e
toEither (Fail e)  = Left e
toEither (Ok a)    = Right a

--------------------------------------------------------------------------------

newtype ResultT e m a = ResultT { runResultT :: m (Result e a) }


instance Functor m => Functor (ResultT e m) where
  fmap f = ResultT . fmap (fmap f) . runResultT


instance (Functor m, Monad m) => Applicative (ResultT e m) where
  pure a = ResultT $ return (Ok a)

  f <*> v = ResultT $ do
              mf <- runResultT f
              case mf of
                Fatal e -> return (Fatal e)
                Fail  e -> return (Fail e)
                Ok   f' -> do
                   mv <- runResultT v
                   return (fmap f' mv)


instance (Error e, Monad m) => MonadPlus (ResultT e m) where
  mzero = ResultT $ return (Fail noMsg)
  
  mplus x y = ResultT $ do
                l <- runResultT x
                case l of
                  Fatal e -> return (Fatal e)
                  Fail  _ -> runResultT y
                  Ok    a -> return (Ok a)


instance (Monad m, Error e) => Monad (ResultT e m) where
  return = ResultT . return . Ok

  m >>= k = ResultT $ do
              r <- runResultT m
              case r of
                Fatal e -> return (Fatal e)
                Fail  e -> return (Fail e)
                Ok    a -> runResultT (k a)

  fail msg = ResultT $ return $ Fail (strMsg msg)


instance (Functor m, Monad m, Error e) => Alternative (ResultT e m) where
  empty = mzero
  (<|>) = mplus


instance (Error e) => MonadTrans (ResultT e) where
  lift m = ResultT $ do
             a <- m
             return (Ok a)


throwError :: (Error e, Monad m) => e -> ResultT e m a
throwError = ResultT . return . Fail


throwFatal :: (Error e, Monad m) => e -> ResultT e m a
throwFatal = ResultT . return . Fatal


mapResult
  :: (Functor m, Monad m) =>
     (Result e1 a1 -> Result e a) -> ResultT e1 m a1 -> ResultT e m a
mapResult f = ResultT . fmap f . runResultT

--------------------------------------------------------------------------------

testX :: ResultT String IO Int
testX = lift (print "x") >> return 1

testY :: ResultT String IO Int
testY = lift (print "error") >> throwError "error"

testZ :: ResultT String IO Int
testZ = lift (print "fatal") >> throwFatal "fatal"

--------------------------------------------------------------------------------
