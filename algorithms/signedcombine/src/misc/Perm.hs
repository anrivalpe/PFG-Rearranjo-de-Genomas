{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, ConstraintKinds, FlexibleContexts, OverloadedStrings #-}

module Perm (Perm, toPerm, permInerList, PermData(..), findDistancePerm, initPerm, finishPerm, MonadDist, MonadRandomDist, renumberToSort) where

------------------------------------------------------------
import Utils
import Types
------------------------------------------------------------
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Random (MonadRandom)
import Foreign (Ptr, mallocArray, pokeArray, free)
import Foreign.C (CInt(CInt))
import System.IO (Handle, hFlush, hGetLine)
import System.Process (createProcess, cleanupProcess, ProcessHandle, proc, StdStream(CreatePipe), CreateProcess(..))
------------------------------------------------------------
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Lazy.Char8 as LBS
------------------------------------------------------------

foreign import ccall "perm/perm_rearrange.h dist" p_dist :: Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

type MonadRandomDist mon = (MonadDist mon, MonadRandom mon)

type CId = Int
type Process = (Handle, Handle, ProcessHandle)
data PermData = CCode Int RearrangeModel | CustomCode Process

data Perm = Perm Bool [Int] deriving (Eq, Show)
toPerm :: Bool -> [Int] -> Perm
toPerm = Perm

permInerList :: Perm -> [Int]
permInerList (Perm _ l) = l

initPerm :: (MonadIO mon) => CId -> RearrangeModel -> String -> mon PermData
initPerm cid model prog = if cid == 0
  then do
    process <- liftIO $ createProcess (proc prog []){std_out=CreatePipe, std_in=CreatePipe}
    case process of
      (Just fin, Just fout, Nothing, procH) -> return $ CustomCode (fin, fout, procH)
      _ -> error patternError
  else return $ CCode cid model

finishPerm :: (MonadIO mon) => PermData -> mon ()
finishPerm (CustomCode (fin,fout,procH)) = do
    liftIO $ LBS.hPutStrLn fin "%"
    liftIO $ hFlush fin
    liftIO $ cleanupProcess (Just fin, Just fout, Nothing, procH)
finishPerm _ = return ()

class Monad mon => MonadDist mon where
  findDistancePerm :: PermData -> Size -> Perm -> Perm -> mon Dist

instance MonadDist Mon where
  findDistancePerm  pd size (Perm withSign_ perm1) (Perm _ perm2) =
    case pd of
      (CustomCode process) -> liftIO $ customFindDistancePerm process
      (CCode cid model) -> liftIO $ cFindDistancePerm cid model
    where
      withSign = if withSign_ then 1 else 0

      customFindDistancePerm (fin, fout, _) = do
        let perm = renumberToSort perm1 perm2
        LBS.hPutStrLn fin (showIntList perm)
        hFlush fin
        read <$> hGetLine fout

      cFindDistancePerm cid model = do
        v_g1 <- mallocArray . fromIntegral $ size
        v_g2 <- mallocArray . fromIntegral $ size
        pokeArray v_g1 . map fromIntegral $ perm1
        pokeArray v_g2 . map fromIntegral $ perm2

        dist <- p_dist v_g1 v_g2 (fromIntegral size) withSign (enumToNum model) (fromIntegral cid) 

        free v_g1
        free v_g2
        return $ fromIntegral dist

renumberToSort :: (Orientable a, Integral a) => [a] -> [a] -> [a]
renumberToSort p1 p2 = (\a ->
    let l = labels IntMap.! (fromIntegral $ canonicOri a) in combineOrientation (getOri a) l) <$> p1
  where
    labels = IntMap.fromList . map (\(a,i) ->
        (fromIntegral . canonicOri $ a, orient (getOri a) i)) $ zip p2 [1..]
