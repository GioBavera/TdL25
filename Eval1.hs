module Eval1 (eval, evalIntExp) where

import AST
import Control.Monad (liftM, ap)
import Control.Monad.IO.Class

type Env = [(Variable, Integer)]

initState :: Env
initState = []

newtype StateErrorIO a = StateErrorIO { runStateErrorIO :: Env -> IO (Maybe (a, Env)) }

instance Functor StateErrorIO where
  fmap = liftM

instance Applicative StateErrorIO where
    pure = return
    (<*>) = ap

instance Monad StateErrorIO where
    return a = StateErrorIO $ \s -> return $ Just (a, s)
    m >>= k = StateErrorIO $ \s -> do
        a <- runStateErrorIO m s
        case a of
            Nothing -> return Nothing
            Just (a', s') -> runStateErrorIO (k a') s'

class Monad m => MonadState m where
    lookfor :: Variable -> m Integer
    update :: Variable -> Integer -> m ()

instance MonadState StateErrorIO where
    lookfor var = StateErrorIO $ \s -> return $ maybe Nothing (\v -> Just (v, s)) $ lookfor' var s
        where lookfor' var [] = Nothing
              lookfor' var ((var', val):ss) | var == var' = Just val
                                            | otherwise = lookfor' var ss
    update var val = StateErrorIO $ \s -> return $ Just ((), update' var val s)
        where update' var val [] = [(var, val)]
              update' var val ((var', val'):ss) | var == var' = (var, val):ss
                                                | otherwise = (var', val'):update' var val ss

class Monad m => MonadError m where
    throw :: m a

instance MonadError StateErrorIO where
    throw = StateErrorIO $ \_ -> return Nothing

instance MonadIO StateErrorIO where
    liftIO io = StateErrorIO $ \s -> do
                                        a <- io
                                        return $ Just (a, s)

eval :: Comm -> IO ()
eval p = do
            p' <- runStateErrorIO (evalComm p) initState
            case p' of
                Just (v, s) -> return v
                Nothing     -> putStrLn "ERROR!"

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadIO m) => Comm -> m ()
evalComm Skip           = return ()
evalComm (Let v e)      = do val <- evalIntExp e
                             update v val
evalComm (Seq l r)      = do evalComm l
                             evalComm r
evalComm (Cond b tc fc) = do bval <- evalBoolExp b
                             if bval then evalComm tc
                             else evalComm fc
evalComm (Repeat c b)   = do bval <- evalBoolExp b
                             if bval then evalComm (Seq c (Repeat c b))
                             else return ()
evalComm (Print xs)     = do 
                            mapM_ printArg xs
                            liftIO $ putChar '\n'
    where 
        printArg (Str s) = liftIO $ putStr s 
        printArg (Booleano b) = do 
                                  b' <- evalBoolExp b 
                                  liftIO $ putStr $ show b' 
        printArg (Entero e) = do 
                                  e' <- evalIntExp e 
                                  liftIO $ putStr $ show e' 
evalComm (Input s v)    = do  
                            liftIO (putStr s)               -- Imprime un mensaje previo o no
                            x <- liftIO getLine             -- Obtiene la entrada
                            let i = ((read x) :: Integer)   -- Convierte el texto a un entero
                            update v i                      -- Con el valor reconvertido, actualiza

evalComm (Switch v c)   = do                                -- Switch x [cases]
                            v' <- lookfor v                                                     --
                            let c' = dropWhile (\x -> x /= Default && x /= (Case v')) c
                            let c'' = takeWhile (\x -> x /= BreakCase) c'
                            mapM_ evalComm c''

evalComm (Case v)       = return ()
evalComm (Default)      = return ()
evalComm (BreakCase)    = return ()

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m, MonadIO m) => IntExp -> m Integer
evalIntExp (Const n)   = return n
evalIntExp (Var v)     = do val <- lookfor v
                            return val
evalIntExp (UMinus e)  = do val <- evalIntExp e
                            return (negate val)
evalIntExp (Plus l r)  = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            
                            return (lval + rval)
evalIntExp (Minus l r) = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            
                            return (lval - rval)
evalIntExp (Times l r) = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            
                            return (lval * rval)
evalIntExp (Div l r)   = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            if rval == 0 then throw
                            else do 
                                    return (div lval rval)

--- Operadores añadidos
evalIntExp (Mod l r) = do   lval <- evalIntExp l
                            rval <- evalIntExp r
                            if rval == 0 then throw             -- En caso de division por 0, arroja error
                            else do     
                                    return (mod lval rval)      -- Caso contrario, realiza la operacion  
evalIntExp (Max l r) = do   lval <- evalIntExp l
                            rval <- evalIntExp r
                            return (max lval rval)              
evalIntExp (Min l r) = do   lval <- evalIntExp l
                            rval <- evalIntExp r
                            return (min lval rval)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m, MonadIO m) => BoolExp -> m Bool
evalBoolExp BTrue     = return True
evalBoolExp BFalse    = return False
evalBoolExp (Eq l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval == rval)
evalBoolExp (Lt l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval < rval)
evalBoolExp (Gt l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval > rval)
evalBoolExp (And l r) = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval && rval)
evalBoolExp (Or l r)  = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval || rval)
evalBoolExp (Not b)   = do bval <- evalBoolExp b
                           return (not bval)


