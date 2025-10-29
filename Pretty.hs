-- | Pretty printer
-- Based on A Prettier Printer, Philip Wadler

{-# LANGUAGE OverloadedStrings #-}

module Pretty
  ( Doc
  , SDoc(..)
  , group
  , line
  , nest
  , prettyPrint
  , prettyPrintWithInitialLineOffset
  , text
  , withParens
  ) where

import Data.Text (Text)
import qualified Data.Text as T

data Doc
  = Nil
  | Text Text Doc
  | Line Int Doc
  | Union Doc Doc
  -- ^ Note that there are two invariants on Union:
  --   1. Both arguments must flatten to the same layout and,
  --   2. No first line of a document in the first argument is
  --      shorter than the first line of a document in the second
  --      argument.

instance Semigroup Doc where
  Nil <> d = d
  Text s r <> d = Text s (r <> d)
  Line i r <> d = Line i (r <> d)
  Union a b <> d = Union (a <> d) (b <> d)

instance Monoid Doc where
  mempty = Nil

-- | A string of text.
text :: Text -> Doc
text = (`Text` mempty)

-- | A newline.
line :: Doc
line = Line 0 mempty

-- | Set the indentation level of lines in the document.
nest :: Int -> Doc -> Doc
nest _ Nil = Nil
nest i (Text s r) = Text s (nest i r)
nest i (Line j r) = Line (i + j) (nest i r)
nest i (Union a b) = Union (nest i a) (nest i b)

flatten :: Doc -> Doc
flatten Nil = Nil
flatten (Text s r) = Text s (flatten r)
flatten (Line _ r) = Text " " (flatten r)
-- The following case follows from the fact that both arguments of a
-- Union flatten to the same string.
flatten (Union a _) = flatten a

-- | Introduces an alternative into the document, turning all newlines
-- into spaces, if space allows.
group :: Doc -> Doc
group Nil = Nil
group (Text s r) = Text s (group r)
group (Line i r) = Union (text " " <> flatten r) (Line i r)
-- The following case follows from the fact that both arguments of a
-- Union flatten to the same string.
group (Union a b) = Union (group a) b

-- | `best` takes a document with `Union`s and returns a document with
-- none. The first argument is the available width and the second
-- argument is the number of characters already placed on the current
-- line.
best :: Int -> Int -> Doc -> Doc
best _ _ Nil = Nil
best w k (Text s r) = Text s (best w (k + T.length s) r)
best w _ (Line i r) = Line i (best w i r)
best w k (Union a b)
  | fits (w - k) a' = a'
  | otherwise = b'
  where
    a' = best w k a
    b' = best w k b

-- | Test if a document will fit on a line with `w` characters.
fits :: Int -> Doc -> Bool
fits w _ | w < 0 = False
fits _ Nil = True
fits w (Text s r) = fits (w - T.length s) r
fits _ (Line _ _) = True
-- Note that the below is safe since `fits` is always called on
-- documents which have already been called on `best`.
fits _ (Union _ _) = error "Union in fits"

-- | Flatten a `Doc` into a `Text` string.
layout :: Doc -> Text
layout Nil = ""
layout (Text s r) = s <> layout r
layout (Line i r) = "\n" <> T.replicate i " " <> layout r
-- Note that the below is safe because `layout` should only be called
-- after the document has been called on `best`.
layout (Union _ _) = error "Union in layout"

-- | Pretty print a value with the given width, in characters.
prettyPrint :: SDoc a => Int -> a -> Text
prettyPrint w = prettyPrintWithInitialLineOffset w 0

-- | Pretty print a value with the given width, and the given
-- offset. The first line will be @w - o@ characters long, and every
-- following line will be @w@ characters long.
--
-- This is useful if our goal is to pretty print a (potentially multi
-- line) value, but where the value is preceded by some text on the
-- same line.
prettyPrintWithInitialLineOffset :: SDoc a => Int -> Int -> a -> Text
prettyPrintWithInitialLineOffset w o a = layout (best w o (showDoc a))

class SDoc a where
  showDoc :: a -> Doc

instance SDoc Doc where
  showDoc = id

instance SDoc Text where
  showDoc = text

instance SDoc String where
  showDoc = text . T.pack

-- | Wrap an `SDoc` in parenthesis.
withParens :: Doc -> Doc
withParens a = text "(" <> a <> text ")"
