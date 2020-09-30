{-# language GADTs #-}
{-# language BangPatterns #-}
{-# language PatternGuards #-}
{-# language ScopedTypeVariables #-}

module Unison.Runtime.Foreign
  ( Foreign(..)
  , unwrapForeign
  , maybeUnwrapForeign
  , wrapBuiltin
  , maybeUnwrapBuiltin
  , unwrapBuiltin
  , BuiltinForeign(..)
  ) where

import Control.Concurrent (ThreadId)
import Data.Text (Text)
import Data.Tagged (Tagged(..))
import Network.Socket (Socket)
import System.IO (Handle)
import Unison.Util.Bytes (Bytes)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Type as Ty
import qualified Crypto.Hash as Hash

import Unsafe.Coerce

data Foreign where
  Wrap :: Reference -> e -> Foreign

promote :: (a -> a -> r) -> b -> c -> r
promote (~~) x y = unsafeCoerce x ~~ unsafeCoerce y

ref2eq :: Reference -> Maybe (a -> b -> Bool)
ref2eq r
  | r == Ty.textRef = Just $ promote ((==) @Text)
  | r == Ty.termLinkRef = Just $ promote ((==) @Referent)
  | r == Ty.typeLinkRef = Just $ promote ((==) @Reference)
  | otherwise = Nothing

ref2cmp :: Reference -> Maybe (a -> b -> Ordering)
ref2cmp r
  | r == Ty.textRef = Just $ promote (compare @Text)
  | r == Ty.termLinkRef = Just $ promote (compare @Referent)
  | r == Ty.typeLinkRef = Just $ promote (compare @Reference)
  | otherwise = Nothing

instance Eq Foreign where
  Wrap rl t == Wrap rr u
    | rl == rr , Just (~~) <- ref2eq rl = t ~~ u
  _ == _ = error "Eq Foreign"

instance Ord Foreign where
  Wrap rl t `compare` Wrap rr u
    | rl == rr, Just cmp <- ref2cmp rl = cmp t u
  compare _ _ = error "Ord Foreign"

instance Show Foreign where
  showsPrec p !(Wrap r _)
    = showParen (p>9)
    $ showString "Wrap " . showsPrec 10 r . showString " _"

unwrapForeign :: Foreign -> a
unwrapForeign (Wrap _ e) = unsafeCoerce e

maybeUnwrapForeign :: Reference -> Foreign -> Maybe a
maybeUnwrapForeign rt (Wrap r e)
  | rt == r = Just (unsafeCoerce e)
  | otherwise = Nothing

class BuiltinForeign f where
  foreignRef :: Tagged f Reference

instance BuiltinForeign Text where foreignRef = Tagged Ty.textRef
instance BuiltinForeign Bytes where foreignRef = Tagged Ty.bytesRef
instance BuiltinForeign Handle where foreignRef = Tagged Ty.fileHandleRef
instance BuiltinForeign Socket where foreignRef = Tagged Ty.socketRef
instance BuiltinForeign ThreadId where foreignRef = Tagged Ty.threadIdRef
instance BuiltinForeign (Hash.Context Hash.SHA3_512) where foreignRef = Tagged Ty.sha3_512Ref
instance BuiltinForeign (Hash.Context Hash.SHA3_256) where foreignRef = Tagged Ty.sha3_256Ref
instance BuiltinForeign (Hash.Context Hash.SHA512) where foreignRef = Tagged Ty.sha2_512Ref
instance BuiltinForeign (Hash.Context Hash.SHA256) where foreignRef = Tagged Ty.sha2_256Ref
instance BuiltinForeign (Hash.Context Hash.Blake2s_256) where foreignRef = Tagged Ty.blake2s_256Ref
instance BuiltinForeign (Hash.Context Hash.Blake2b_512) where foreignRef = Tagged Ty.blake2b_512Ref
instance BuiltinForeign (Hash.Context Hash.Blake2b_256) where foreignRef = Tagged Ty.blake2b_256Ref

wrapBuiltin :: forall f. BuiltinForeign f => f -> Foreign
wrapBuiltin x = Wrap r x
  where
  Tagged r = foreignRef :: Tagged f Reference

unwrapBuiltin :: BuiltinForeign f => Foreign -> f
unwrapBuiltin (Wrap _ x) = unsafeCoerce x

maybeUnwrapBuiltin :: forall f. BuiltinForeign f => Foreign -> Maybe f
maybeUnwrapBuiltin (Wrap r x)
  | r == r0 = Just (unsafeCoerce x)
  | otherwise = Nothing
  where
  Tagged r0 = foreignRef :: Tagged f Reference
