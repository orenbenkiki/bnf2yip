module CharSet (
 CharSet,
 addSets,
 codeSet,
 fakeSet,
 containsCode,
 containsSet,
 distinctSets,
 emptySet,
 maxLowCode,
 rangeSet,
 subtractSets
) where


import Data.List
import Data.Char


-- | A character class consists of a sorted list of "low" (ASCII and possibly
-- NEL) characters, and a list of sorted "high" (non-ASCII) character ranges.
type CharSet = ([ Int ], [ (Int, Int) ])


-- | Maximal Unicode point for sorted list of characters (NEL).
maxLowCode :: Int

maxLowCode = 159


-- | @containsCode code chars@ tests whether the specified character @code@ is
-- included in the @chars@ set.
containsCode :: Int -> CharSet -> Bool

containsCode code (chars, ranges) =
  if code <= maxLowCode
     then containsChars code chars
     else containsRanges (code, code) ranges


-- | @containsChars code chars@ tests whether the specified character @code@ is
-- included in the @chars@ list.
containsChars code chars =
  case find (\x -> x == code) chars of
       Nothing -> False
       _       -> True


-- | @containsRanges range ranges@ tests whether the specified @range@ is
-- contained in any of the @ranges@.
containsRanges (low, high) ranges =
  case find containsRange ranges of
       Nothing -> False
       _       -> True
  where containsRange (low', high') = low' <= low && high <= high'


-- | @containsSet container contained@ tests whether the @container@ set
-- contains the @contained@ set.
containsSet container contained = subtractSets contained container == emptySet


-- | Empty character clas.
emptySet :: CharSet

emptySet = ([], [])


-- | @codeSet code@ creates a character class for a single "low" code.
codeSet :: Char -> CharSet

codeSet code | ord code <= maxLowCode = ([ ord code ], [])


-- | @fakeSet code@ creates a character class for a fake code.
fakeSet :: Int -> CharSet

fakeSet code = ([ code ], [])


-- | @rangeSet low high@ creates a character class for a single range.
rangeSet :: Char -> Char -> CharSet

rangeSet low high
  | ord low <= maxLowCode && maxLowCode < ord high = (rangeChars (ord low) maxLowCode, [ (1 + maxLowCode, ord high) ])
  | ord high <= maxLowCode                         = (rangeChars (ord low) (ord high), [])
  | otherwise =                                      ([],                              [ (ord low, ord high) ])


-- | @rangeChars low high@ converts a range to a list of characters.
rangeChars :: Int -> Int -> [ Int ]

rangeChars low high
  | low <= high = low : (rangeChars (1 + low) high)
  | otherwise   = []


-- | @addSets class1 class2@ creates a character class covering the characters
-- in both @class1@ and @class2@.
addSets :: CharSet -> CharSet -> CharSet

addSets (chars1, ranges1) (chars2, ranges2) =
  ((addChars chars1 chars2), (addRanges ranges1 ranges2))


-- | @addChars chars1 chars2@ returns the sorted list of "low" characters that
-- appear in either @chars1@ or @chars2@.
addChars :: [ Int ] -> [ Int ] -> [ Int ]

addChars [] chars = chars

addChars chars [] = chars

addChars chars1@(first1 : rest1) chars2@(first2 : rest2)
  | first1 == first2 = first1 : addChars rest1 rest2
  | first1 < first2  = first1 : addChars rest1 chars2
  | first1 > first2  = first2 : addChars chars1 rest2

addChars chars1 chars2 =
  error $ "ADD_CHARS: " ++ show chars1 ++ " + " ++ show chars2


-- | @addRanges ranges1 ranges2@ returns the sorted list of "high" ranges that
-- appear in eaither @ranges1@ or @ranges2@. No fancy range merging is done
-- since in our cases all ranges are identical or distinct.
addRanges :: [ (Int, Int) ] -> [ (Int, Int) ] -> [ (Int, Int) ]

addRanges [] ranges = ranges

addRanges ranges [] = ranges

addRanges ranges1@((low1, high1) : rest1) ranges2@((low2, high2) : rest2)
  | low1 == low2 && high1 == high2 = (low1, high1) : addRanges rest1 rest2
  | high1 == low2 - 1              = (low1, high2) : addRanges rest1 rest2
  | high1 < low2 - 1               = (low1, high1) : addRanges rest1 ranges2
  | high2 == low1 - 1              = (low2, high1) : addRanges rest1 rest2
  | high2 < low1 - 1               = (low2, high2) : addRanges ranges1 rest2

addRanges ranges1 ranges2 =
  error $ "ADD_RANGES: " ++ show ranges1 ++ " + " ++ show ranges2

-- | @subtractSets class1 class2@ computes the character class of characters
-- that appear in @class1@ but not in @class2@.
subtractSets :: CharSet -> CharSet -> CharSet

subtractSets (chars1, ranges1) (chars2, ranges2) =
  ((subtractChars chars1 chars2), (subtractRanges ranges1 ranges2))


-- | @subtractChars chars1 chars2@ computes the sorted list of "low"
-- characters that appear in @chars1@ and not in @chars2@.
subtractChars :: [ Int ] -> [ Int ] -> [ Int ]

subtractChars chars [] = chars

subtractChars [] chars = []

subtractChars chars1@(first1 : rest1) chars2@(first2 : rest2)
  | first1 == first2 = subtractChars rest1 rest2
  | first1 < first2  = first1 : subtractChars rest1 chars2
  | first1 > first2  = subtractChars chars1 rest2

subtractChars chars1 chars2 = error $ "SUB_CHARS: " ++ show chars1 ++ " - " ++ show chars2


-- | @subtractRanges ranges1 ranges2@ computes the sorted list of ranges that
-- appear in @ranges1@ and not in @ranges2@. No fancy range splitting is done
-- since in our case all ranges are identical or distinct.
subtractRanges :: [ (Int, Int) ] -> [ (Int, Int) ] -> [ (Int, Int) ]

subtractRanges ranges [] = ranges

subtractRanges [] ranges = []

subtractRanges ranges1@((low1, high1) : rest1) ranges2@((low2, high2) : rest2)
  | high1 < low2                                  = (low1, high1) : subtractRanges rest1 ranges2
  | high2 < low1                                  = subtractRanges ranges1 rest2
  | low2 <= low1 && high1 <= high2                = subtractRanges rest1 ranges2
  | low1 < low2 && high2 < high1                  = (low1, low2 - 1) : subtractRanges ((high2 + 1, high1) : rest1) rest2
  | low1 == low2 && high2 < high1                 = subtractRanges ((high2 + 1, high1) : rest1) rest2
  | low1 < low2 && low2 < high1 && high1 <= high2 = (low1, low2 - 1) : subtractRanges rest1 ranges2
  | low2 < low1 && low1 < high2 && high2 < high1  = subtractRanges ((high2 + 1, high1) : rest1) rest2

subtractRanges ranges1 ranges2 = error $ "SUB_RANGES: " ++ show ranges1 ++ " - " ++ show ranges2


-- | @distinctSets sets@ converts a list of arbitrary sets to a list of
-- distinct sets. Each character contained in any of the original sets is
-- contained in exactly one of the distinct sets.
distinctSets sets = sortBy compare' $ snd $ foldr addDistinct (emptySet, []) sets
  where compare' left right = compare (spoil left) (spoil right)
        spoil ([], ranges) = ([ 999999 ], ranges)
        spoil charSet = charSet


-- | @addDistinct set (union, distinct)@ computes a new @union@ and @distinct@
-- sets to cover any additional characters contained in a @set@.
addDistinct :: CharSet -> (CharSet, [ CharSet ]) -> (CharSet, [ CharSet ])

addDistinct chars (union, distinct) =
  let new = subtractSets chars union
      old = subtractSets chars new
      union' = addSets new union
  in if new == emptySet
        then (union', splitSets old distinct)
        else (union', new : splitSets old distinct)


-- | @splitSets set distinct@ computes a new list of @distinct@ sets that
-- includes all the characters in the given @set@ in a separate member.
splitSets :: CharSet -> [ CharSet ] -> [ CharSet ]

splitSets ([],[]) distinct = distinct

splitSets split distinct@(head : tail)
  | split == head        = distinct
  | splitOnly == split   = head : splitSets split tail
  | headOnly == emptySet = both : splitSets splitOnly tail
  | otherwise            = headOnly : both : splitSets splitOnly tail
  where splitOnly = subtractSets split head
        headOnly  = subtractSets head  split
        both      = subtractSets head  headOnly
