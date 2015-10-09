{-| This module exports most of the functions needed to write inline Julia code.
-}
module Language.Julia.Inline (
  -- * Re-exports
    module Language.Julia.Inline.Quote
  , module Language.Julia.Inline.Marshal
  ) where

import Language.Julia.Inline.Quote
import Language.Julia.Inline.Marshal
