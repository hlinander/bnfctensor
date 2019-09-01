{-# LANGUAGE ForeignFunctionInterface #-}
module RustPerm (
    canonicalizeFree
)
where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Foreign.Marshal

import Data.Int
import System.IO.Unsafe

import Debug.Trace

-- foreign import ccall "canonicalize" winPerm :: Ptr PermT -> Ptr Int64 -> IO ()
foreign import ccall "canonicalize_free" callCFRust :: CLong -> Ptr CLong -> CLong -> Ptr CLong -> Ptr CLong -> IO ()

niehoff :: PermT -> [Int]
niehoff = undefined

data PermT = PermT {
    perm :: [Int],
    generatingSet :: [[Int]],
    frees :: [Int],
    dummies :: [Int]
}

toCLong = map fromIntegral

canonicalizeFree :: [Int] -> [[Int]] -> [Int]
canonicalizeFree p [] = trace ("WARNING: GeneratingSet empty when running canonicalize") p
canonicalizeFree p gs = map ((+) 1) $ map fromIntegral $ unsafePerformIO $ canonicalizeFreeRust (toCLong p) (map toCLong gs)
    
canonicalizeFreeRust :: [CLong] -> [[CLong]] -> IO [CLong]
canonicalizeFreeRust p gs = 
    allocaArray (length p) $ \outPtr ->
    withArrayLen (map (flip (-) 1) p) $ \permLen permPtr ->
    withArrayLen (map (flip (-) 1) $ concat gs) $ \gsLen gsPtr -> do
        callCFRust (fromIntegral permLen) permPtr (fromIntegral (length gs)) gsPtr outPtr 
        peekArray permLen outPtr 

-- length: u64,
-- dummies_length: u64,
-- frees_length: u64,
-- window_size: u64,

-- length: u64,
-- dummies_length: u64,
-- frees_length: u64,
-- window_size: u64,
-- perm: *const u64,
-- generating_set: *const u64,
-- frees: *const u64,
-- dummies: *const u64,

instance Storable PermT where
    -- peek :: Ptr PermT -> IO PermT
    peek fooPtr = do
        let ptr = castPtr fooPtr :: Ptr Int
        len <- peek (advancePtr ptr 0)
        dLen <- peek (advancePtr ptr 1)
        fLen <- peek (advancePtr ptr 2)
        window <- peek (advancePtr ptr 3)
        p <- peekArray len (advancePtr ptr 4)
        gs <- undefined
        fs <- peekArray len (advancePtr ptr 6)
        ds <- peekArray len (advancePtr ptr 7)
        return $ PermT p gs fs ds


    -- poke :: Ptr PermT -> PermT -> IO ()
    poke fooPtr p = do
        let ptr = castPtr fooPtr :: Ptr Int
        poke (advancePtr ptr 0) (length $ perm p)
        poke (advancePtr ptr 1) (length $ dummies p)
        poke (advancePtr ptr 2) (length $ frees p)
        poke (advancePtr ptr 3) (length $ head $ generatingSet p)

        -- poke (advancePtr ptr 4) (length $ perm p)
        -- poke (advancePtr ptr 5) (length $ dummies p)
        -- poke (advancePtr ptr 6) (length $ frees p)
        -- poke (advancePtr ptr 7) (length $ head $ generatingSet p)
        -- p <- pokeArray undefined [undefined]
        -- gs <- undefined
        -- fs <- pokeArray undefined [undefined]
        -- ds <- pokeArray undefined [undefined]
        -- permPtr <- newArray $ perm p
        -- poke (advancePtr ptr 4) =<< newArray (perm p)
        pokeArray ptr $ concat $ generatingSet p
        pokeArray ptr $ frees p
        pokeArray ptr $ dummies p
        where nperms = length $ perm p

    -- sizeOf :: PermT -> Int
    sizeOf p = sizeOf (undefined::Int) * 8

    -- alignment :: PermT -> Int
    alignment _ = 8