{-# LANGUAGE ForeignFunctionInterface #-}
module RustPerm where

import Foreign.C.Types
import Foreign.Ptr

import Data.Int

foreign import ccall "canonicalize" winPerm :: Ptr PermT -> Ptr Int64 -> IO ()

niehoff :: PermT -> [Int]
niehoff = undefined

data PermT = PermT {
    perm :: [Int],
    generatingSet :: [[Int]],
    frees :: [Int],
    dummies :: [Int]
} deriving (Eq, Show)

-- length: u64,
-- dummies_length: u64,
-- frees_length: u64,
-- window_size: u64,
-- perm: *const u64,
-- generating_set: *const u64,
-- frees: *const u64,
-- dummies: *const u64,

instance PermT Storable where
    peek :: Ptr PermT -> IO PermT
    peek fooPtr = do
        ptr <- castPtr fooPtr :: Ptr Int64
        len <- peek (advancePtr ptr 0)
        dLen <- peek (advancePtr ptr 1)
        fLen <- peek (advancePtr ptr 2)
        window <- peek (advancePtr ptr 3)
        p <- peekArray len (advancePtr ptr 4)
        gs <- undefined
        fs <- peekArray len (advancePtr ptr 6)
        ds <- peekArray len (advancePtr ptr 7)


    poke :: Ptr PermT -> PermT -> IO ()
    poke fooPtr p = do
        ptr <- castPtr fooPtr :: Ptr Int64
        poke (advancePtr ptr 0) (length $ perms p)
        poke (advancePtr ptr 1) (length $ dummies p)
        poke (advancePtr ptr 2) (length $ frees p)
        peek (advancePtr ptr 3) (length $ head $ generatingSet p)
        p <- pokeArray undefined [undefined]
        gs <- undefined
        fs <- pokeArray undefined [undefined]
        ds <- pokeArray undefined [undefined]
        where nperms = length $ perm p
    sizeOf p = sizeOf (undefined::Int) * 8
    alignment _ = 8