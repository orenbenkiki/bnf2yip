module CharSet (
 CharSet,
 startOfLine,
 fakeCodes,
 empty,
 singleton,
 fake,
 range,
 member,
 contains,
 union,
 subtract,
 distinct,
 directives
) where


import Data.Char
import Data.List hiding (union)
import Data.Maybe
import Prelude hiding (subtract)

-- * Types.


-- | A character set consists of a sorted list of "low" (ASCII and possibly NEL) characters, and a list of sorted "high" (non-ASCII) character ranges.
type CharSet = ([ Int ], [ (Int, Int) ])


-- * Special codes.


-- | Maximal Unicode point for sorted list of characters (NEL).
maxLowCode :: Int

maxLowCode = 159


-- | Fake character code for testing for the start of a line.
startOfLine :: Int

startOfLine = -1


-- | A list of all the fake codes (but not the special ones).
fakeCodes :: [ Int ]

fakeCodes = [ startOfLine ]


-- * Create new sets.


-- | Empty character clas.
empty :: CharSet

empty = ([], [])


-- | @singleton code@ creates a character set for a single "low" code.
singleton :: Char -> CharSet

singleton code | ord code <= maxLowCode = ([ ord code ], [])


-- | @fake code@ creates a character set for a fake code.
fake :: Int -> CharSet

fake code = ([ code ], [])


-- | @range low high@ creates a character set for a single range.
range :: Char -> Char -> CharSet

range low high
  | ord low <= maxLowCode && maxLowCode < ord high = (rangeChars (ord low) maxLowCode, [ (1 + maxLowCode, ord high) ])
  | ord high <= maxLowCode                         = (rangeChars (ord low) (ord high), [])
  | otherwise =                                      ([],                              [ (ord low, ord high) ])


-- | @rangeChars low high@ converts a range to a list of characters.
rangeChars :: Int -> Int -> [ Int ]

rangeChars low high
  | low <= high = low : (rangeChars (1 + low) high)
  | otherwise   = []


-- * Test for inclusion.


-- | @member code chars@ tests whether the specified character /code/ is included in the /chars/ set.
member :: Int -> CharSet -> Bool

member code (chars, ranges) = if code <= maxLowCode
                                 then containsChars code chars
                                 else containsRanges (code, code) ranges


-- | @containsChars code chars@ tests whether the specified character /code/ is included in the /chars/ list.
containsChars code chars = case find (\x -> x == code) chars of
                                Nothing -> False
                                _       -> True


-- | @containsRanges (low, high) ranges@ tests whether the specified /(low, high)/ range is contained in any of the /ranges/.
containsRanges (low, high) ranges = case find containsRange ranges of
                                         Nothing -> False
                                         _       -> True
  where containsRange (low', high') = low' <= low && high <= high'


-- | @contains container contained@ tests whether the /container/ set contains the /contained/ set.
contains container contained = subtract contained container == empty


-- * Set operations.


-- | @union set1 set2@ creates a character set covering the characters in both /set1/ and /set2/.
union :: CharSet -> CharSet -> CharSet

union (chars1, ranges1) (chars2, ranges2) = ((addChars chars1 chars2), (addRanges ranges1 ranges2))


-- | @addChars chars1 chars2@ returns the sorted list of "low" characters that appear in either /chars1/ or /chars2/.
addChars :: [ Int ] -> [ Int ] -> [ Int ]

addChars [] chars = chars

addChars chars [] = chars

addChars chars1@(first1 : rest1) chars2@(first2 : rest2)
  | first1 == first2 = first1 : addChars rest1 rest2
  | first1 < first2  = first1 : addChars rest1 chars2
  | first1 > first2  = first2 : addChars chars1 rest2

addChars chars1 chars2 = error $ "ADD_CHARS: " ++ show chars1 ++ " + " ++ show chars2


-- | @addRanges ranges1 ranges2@ returns the sorted list of "high" ranges that appear in eaither /ranges1/ or /ranges2/.
-- No fancy range merging is done since in our cases all ranges are identical or distinct.
addRanges :: [ (Int, Int) ] -> [ (Int, Int) ] -> [ (Int, Int) ]

addRanges [] ranges = ranges

addRanges ranges [] = ranges

addRanges ranges1@((low1, high1) : rest1) ranges2@((low2, high2) : rest2)
  | low1 == low2 && high1 == high2 = (low1, high1) : addRanges rest1 rest2
  | high1 == low2 - 1              = (low1, high2) : addRanges rest1 rest2
  | high1 < low2 - 1               = (low1, high1) : addRanges rest1 ranges2
  | high2 == low1 - 1              = (low2, high1) : addRanges rest1 rest2
  | high2 < low1 - 1               = (low2, high2) : addRanges ranges1 rest2

addRanges ranges1 ranges2 = error $ "ADD_RANGES: " ++ show ranges1 ++ " + " ++ show ranges2

-- | @subtract set1 set2@ computes the character set of characters that appear in /set1/ but not in /set2/.
subtract :: CharSet -> CharSet -> CharSet

subtract (chars1, ranges1) (chars2, ranges2) = ((subtractChars chars1 chars2), (subtractRanges ranges1 ranges2))


-- | @subtractChars chars1 chars2@ computes the sorted list of "low" characters that appear in /chars1/ and not in /chars2/.
subtractChars :: [ Int ] -> [ Int ] -> [ Int ]

subtractChars chars [] = chars

subtractChars [] chars = []

subtractChars chars1@(first1 : rest1) chars2@(first2 : rest2)
  | first1 == first2 = subtractChars rest1 rest2
  | first1 < first2  = first1 : subtractChars rest1 chars2
  | first1 > first2  = subtractChars chars1 rest2

subtractChars chars1 chars2 = error $ "SUB_CHARS: " ++ show chars1 ++ " - " ++ show chars2


-- | @subtractRanges ranges1 ranges2@ computes the sorted list of ranges that appear in /ranges1/ and not in /ranges2/.
-- No fancy range splitting is done since in our case all ranges are identical or distinct.
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


-- | @distinct sets@ converts a list of arbitrary sets to a list of distinct sets.
-- Each character contained in any of the original sets is contained in exactly one of the distinct sets.
distinct :: [ CharSet ] -> [ CharSet ]

distinct sets = sortBy compare' $ snd $ foldr addDistinct (empty, []) sets
  where compare' left right = compare (spoil left) (spoil right)
        spoil ([], ranges) = ([ 999999 ], ranges)
        spoil charSet = charSet


-- | @addDistinct set (combined, distinct)@ computes a new /combined/ and /distinct/ sets to cover any additional characters contained in a /set/.
addDistinct :: CharSet -> (CharSet, [ CharSet ]) -> (CharSet, [ CharSet ])

addDistinct chars (combined, distinct) = let new = subtract chars combined
                                             old = subtract chars new
                                             combined' = union new combined
                                         in if new == empty
                                               then (combined', distinctSets old distinct)
                                               else (combined', new : distinctSets old distinct)


-- | @distinctSets set distinct@ computes a new list of distinct /distinct/ sets that includes all the characters in the given /set/ in a separate member.
distinctSets :: CharSet -> [ CharSet ] -> [ CharSet ]

distinctSets ([],[]) distinct = distinct

distinctSets chars distinct@(head : tail)
  | chars == head      = distinct
  | splitOnly == chars = head : distinctSets chars tail
  | headOnly == empty  = both : distinctSets splitOnly tail
  | otherwise          = headOnly : both : distinctSets splitOnly tail
  where splitOnly = subtract chars head
        headOnly  = subtract head  chars
        both      = subtract head  headOnly


-- * Convert to directives.


-- | @directives sets@ computes a list of directives for classifying characters using the distinct /sets/.
directives :: [ CharSet ] -> String

directives distinct = "BEGIN_CLASSIFICATION_TABLE(" ++ (show $ 1 + maxLowCode) ++ ")\n"
                   ++ classificationTable distinct
                   ++ "END_CLASSIFICATION_TABLE(" ++ (show $ 1 + maxLowCode) ++ ")\n"
                   ++ "BEGIN_CLASSIFICATION_RANGES\n"
                   ++ classificationRanges distinct
                   ++ "END_CLASSIFICATION_RANGES\n"


-- | @classificationTable distinct@ computes a table assigning a class index to each low character code.
classificationTable :: [ CharSet ] -> String

classificationTable distinct = concatMap codeClass [0 .. maxLowCode]
  where codeClass code = "LOW_CHAR_CLASS(" ++ show code
                      ++ ", " ++ show (classIndex code)
                      ++ ")\n"
        classIndex code = (fromMaybe 0 $ findIndex (member code) distinct) - length fakeCodes


-- | @classificationTable distinct@ computes a set of tests for classifying high character codes.
classificationRanges :: [ CharSet ] -> String

classificationRanges distinct = concatMap rangesTests $ zip [- length fakeCodes ..] distinct
  where rangesTests (index, (_, ranges)) = concatMap (rangeTest index) ranges
        rangeTest index (low, high) = "HIGH_CHAR_RANGE(" ++ (show $ index)
                                   ++ ", " ++ (show low)
                                   ++ ", " ++ (show $ fixMaxUnicode high)
                                   ++ ")\n"
        fixMaxUnicode 0x1FFFF = 0x10FFFF
        fixMaxUnicode code = code
