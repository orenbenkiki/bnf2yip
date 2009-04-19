-------------------------------------------------------------------------------
-- |
-- Module      :  yaml2directives
-- Copyright   :  (c) Oren Ben-Kiki 2008
-- License     :  LGPL
-- 
-- Maintainer  :  yaml-oren@ben-kiki.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Convert the reference BNF file to a fat list of directives for further
-- processing, e.g. as a basis for generating a parser.

module Main(main)
where

import           CharSet
import           Data.Char
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Machine as Machine
import           ParseSyntax
import           Syntax
import           System.Console.GetOpt
import           System.Environment
import           Text.Regex

import           Debug.Trace


infixl 9 |>
-- | @record |> field@ is the same as @field record@,  but is more readable.
(|>) :: record -> (record -> value) -> value
record |> field = field record


-- | Command line flag.
data Flag = Help                   -- ^ Request printing usage.
          | Output String          -- ^ Specify output file name.
          | Input String           -- ^ Specify input file name.
    deriving Show


-- | Command line options.
optionDescriptions :: [OptDescr Flag]

optionDescriptions = [
      Option ['h', '?'] ["help"]   (NoArg Help)           "print usage and exit",
      Option ['o']      ["output"] (ReqArg Output "file") "output file"
  ]


-- | @usage@ returns the usage string for the program.
usage :: String

usage = usageInfo "yaml2yaspar: [options] [file]" optionDescriptions


-- | @collectFlags args@ converts the command line /args/ to list of 'Flag'.
collectFlags :: [String] -> IO [Flag]

collectFlags args =
  case getOpt Permute optionDescriptions args of
       (_,     _,     [ errors ]) -> ioError $ userError $ errors ++ usage
       (flags, [],    [])         -> return flags
       (flags, [arg], [])         -> return $ Input arg : flags
       (flags, _,     [])         -> ioError $ userError "more than one input file"


-- | Options controlling program behavior
data Options = Options {
      oToHelp :: Bool,   -- ^ Whether to just print the usage.
      oInput  :: String, -- ^ Name of input file ("-": @stdin@).
      oOutput :: String  -- ^ Name of output file ("-": @stdout@).
  }


-- | Default options if no flags are specified.
defaultOptions = Options { oToHelp = False, oInput  = "-", oOutput = "-" }


-- | @applyFlags flags options@ applies the specified /flags/ to the /options/.
applyFlags :: [Flag] -> Options -> IO Options

applyFlags [] options = return options

applyFlags (flag : flags) options =
    case flag of
         Help        -> applyFlags flags options { oToHelp = True }
         Output name -> applyFlags flags options { oOutput = name }
         Input name  -> applyFlags flags options { oInput = name }


-- | @fromFile name@ returns the contents of the specified input file called
-- /name/ (may be "-" for @stdin@).
fromFile :: String -> IO String

fromFile name = case name of
                      "-"  -> getContents
                      path -> readFile path


-- | @intoFile name text@ writes the /text/ into the specified output file
-- called /name/ (may be "-" for @stdout@).
intoFile :: String -> String -> IO ()

intoFile name text = case name of
                           "-"  -> putStr text
                           path -> writeFile path $ text


-- | @runWith options@ runs the program with the specified /options/.
runWith :: Options -> IO ()

runWith options = do text <- fromFile (oInput options)
                     let (syntax, index) = parseSyntax $ spoilMaxUnicode text
                     let compiled = compile syntax index
                     intoFile (oOutput options) $ compiled
  where spoilMaxUnicode text = subRegex (mkRegex "10FFFF") text "1FFFF"


-- | @main@ converts an input BNF file to YIP directives. Note that since this
-- is a one-time operation, efficiency is not a consideration in this code.
main :: IO ()

main = do args <- getArgs
          flags <- collectFlags args
          options <- applyFlags flags defaultOptions
          if oToHelp options
             then putStrLn usage
             else runWith options

-- | @compile syntax@ converts the parses syntax to a list of directives that
-- can be implemented in some target programming language.
compile :: Syntax -> [ String ] -> String

compile syntax index =
  let decontexted = Map.fromList $ concatMap decontext $ Map.toList syntax
      inlined = mapSyntax (inlineCalls decontexted Set.empty) decontexted
      goodContext = removeBadContext inlined
      purgedPeeks = mapSyntax purgeSubPeeks goodContext
      simplified = mapSyntax simplify purgedPeeks
      combinedSets = fixPoint singleStep simplified
      sets = mapMaybe classNode $ listSyntax combinedSets
      distinct = distinctSets $ sets ++ map fakeSet Machine.fakeClasses
      classification = classificationDirectives distinct
      classified = mapSyntax (classifySets distinct) combinedSets
      selectified = mapSyntax selectify classified
      machines = concatMap (productionMachines selectified) $ zip [1..] index
      matching = concatMap Machine.directives machines
  in "BEGIN_SYNTAX\n" ++
     "BEGIN_CLASSIFICATION\n" ++
     classification ++
     "END_CLASSIFICATION\n" ++
     "BEGIN_MACHINES(" ++ (show $ length index) ++ ")\n" ++
     matching ++
     "END_MACHINES(" ++ (show $ length index) ++ ")\n" ++
     "END_SYNTAX\n"
  where combineSets' syntax = mapSyntax combineSets syntax
        flattenTrees' syntax = mapSyntax flattenTrees syntax
        reorderLists' syntax = mapSyntax reorderLists syntax
        singleStep syntax = reorderLists' $ combineSets' $ flattenTrees' syntax
        classNode (Chars chars) = Just chars
        classNode node = Nothing
        parameterName (Variable name) = name
        productionMachines syntax (index, name) = catMaybes [ productionMachine syntax index name ]
                                               ++ (map subIndex $ zip [1..] $ catMaybes $ map (productionMachine syntax index) $ map (contextify name) contexts)
        productionMachine syntax index name = case Map.lookup name syntax of
                                                   Just (parameters, pattern) -> Just (show index, name, map parameterName parameters, patternMachineT pattern)
                                                   Nothing -> Nothing
        subIndex (sub, (index, name, parameters, machine)) = (index ++ "." ++ show sub, name, parameters, machine)


patternMachineT pattern =
  let machine = patternMachine {- $ trace ("pattern: " ++ show pattern) -} pattern
  in trace ("Pattern: " ++ show pattern ++ "\nMachine: " ++ show machine ++ "\n") machine


-- | @fixPoint function input@ repeatedly applies @function@ to the @input@
-- until the result remains the same.
fixPoint :: Eq a => (a -> a) -> a -> a

fixPoint function input =
  if output == input
     then input
     else fixPoint function output
  where output = function input


-- | @decontext (name, production)@ gets rid of the @c@ parameter by splitting
-- each @production@ to separate ones.
decontext :: (String, Production) -> [ (String, Production) ]

decontext (name, production@(parameters, pattern))
  | elem (Variable "c") parameters = map productize $ decontextAlternatives pattern
  | otherwise = [ (name, production) ]
      where productize (context, pattern) = (contextify name context, (parameters', pattern))
            parameters' = filter (/= Variable "c") parameters

-- | @contextify name context@ converts a @Production@ @name@ to a name that
-- also contains the @context@.
contextify :: String -> String -> String

contextify name "BlockOut" = name ++ " block-out"
contextify name "BlockIn"  = name ++ " block-in"
contextify name "BlockKey" = name ++ " block-key"
contextify name "FlowOut"  = name ++ " flow-out"
contextify name "FlowIn"   = name ++ " flow-in"
contextify name "FlowKey"  = name ++ " flow-key"

-- | The list of all the possible contexts.
contexts = [ "BlockOut", "BlockIn", "BlockKey", "FlowOut", "FlowIn", "FlowKey" ]

-- | @decontextAlternatives node@ converts a @node@ to a list of alternatives,
-- one for each possible context.
decontextAlternatives :: Node -> [ (String, Node) ]

decontextAlternatives (Case (Variable "c") alternatives)  = map contextAlternative contexts
  where contextAlternative context = case find (isContextAlternative context) alternatives of
                                          Just (_, pattern) -> (context, pattern)
                                          Nothing           -> (context, BadContext)
        isContextAlternative context (Symbol value, _) = context == value

decontextAlternatives node = map decontextNode contexts
  where decontextNode context = (context, mapNode (applyContext context) node)
        applyContext context node@(Call name parameters)
          | elem (Variable "c") parameters = Call (contextify name context) (filter (/= Variable "c") parameters)
          | otherwise = node
        applyContext context node = node


-- | @inlineCalls syntax callers node@ converts each call @node@s to the
-- invoked procedure body, except for recursive calls.
inlineCalls :: Syntax -> Set.Set String -> Node -> Node

inlineCalls syntax callers node@(Call name parameters)
  | foldr (&&) True $ map passthrough parameters =
      if Set.member name callers
         then node
         else let callers' = Set.insert name callers
                  (_, pattern) = fromJust $ Map.lookup name syntax
                  pattern' = mapNode (inlineCalls syntax callers') pattern
              in case find recursiveCall $ listNode pattern' of
                      Nothing -> pattern'
                      Just _  -> Be name pattern'
              where recursiveCall (Call name' _) = name' == name
                    recursiveCall _ = False
                    passthrough (Variable _) = True
                    passthrough _ = False

inlineCalls syntax callers node = node


-- | @removeBadContext syntax@ removes all the @syntax@ @Production@s that have
-- a nonsencical context.
removeBadContext :: Syntax -> Syntax

removeBadContext syntax = Map.filter (notElem BadContext . listNode . snd) syntax


-- | @purgeSubPeeks node@ purges any actions from any lookahead
-- sub-nodes of the @node@.
purgeSubPeeks :: Node -> Node

purgeSubPeeks (AndNot base subtract) = AndNot base $ mapNode purgePeek subtract

purgeSubPeeks (Forbidding forbidden pattern) = Forbidding (mapNode purgePeek forbidden) pattern

purgeSubPeeks (Peek pattern) = Peek $ mapNode purgePeek pattern

purgeSubPeeks (Prev pattern) = Prev $ mapNode purgePeek pattern

purgeSubPeeks (Reject pattern) = Reject $ mapNode purgePeek pattern

purgeSubPeeks node = node


-- | @purgePeek node@ purges any actions from the lookahead @node@.
purgePeek :: Node -> Node

purgePeek (EmptyToken _) = Empty

purgePeek (Token _ node) = node

purgePeek node@(Call _ _) = error $ "PP: " ++ show node

purgePeek node = node


-- | @simplify node@ converts complex operations to atomics ones.
simplify :: Node -> Node

simplify (Token name pattern) = And [ BeginToken name, pattern, EndToken name ]

simplify (RepeatN (Value times) pattern) = And $ replicate times pattern

simplify chars@(Chars _) = And [ chars, NextChar ]

simplify (Optional pattern) = Or [ pattern, Empty ]

simplify (OneOrMore pattern) = And [ pattern, ZeroOrMore pattern ]

simplify StartOfLine = Chars $ fakeSet Machine.startOfLine

simplify (Case _ _) = error "Never happens"

simplify pattern = pattern


flattenTreesT node =
  let node' = flattenTrees node
  in trace ("BEFORE: " ++ show node ++ "\nAFTER: " ++ show node' ++ "\n") node'

-- | @flattenTrees node@ converts deep binary trees of @And@ and @Or@
-- operations to flat lists. It also pushes @AndNot@ operations as far down the
-- tree as possible so later we'll be able to replace them all by simple
-- @CharSet@s.
flattenTrees :: Node -> Node

flattenTrees (AndNot base (And [ chars@(Chars _), NextChar ])) =
  AndNot base chars

flattenTrees (AndNot (And (empty@(EmptyToken _) : head : tail)) subtract) =
  And $ empty : AndNot head subtract : tail

flattenTrees (AndNot (And (prev@(Prev _) : head : tail)) subtract) =
  And $ prev : AndNot head subtract : tail

flattenTrees (AndNot (And (head : tail)) subtract) =
  And $ (AndNot head subtract) : tail

flattenTrees (AndNot (Choice name pattern) subtract) =
  Choice name $ AndNot pattern subtract

flattenTrees (AndNot (Or nodes) subtract) =
  Or $ map andNot nodes
  where andNot node = AndNot node subtract

flattenTrees (AndNot (Token name pattern) subtract) =
  Token name $ AndNot pattern subtract

flattenTrees andNot@(AndNot _ _) = error ("AN: " ++ show andNot) andNot

flattenTrees (And nodes) =
  case nodes' of
       [ single ] -> single
       _          -> And nodes'
  where nodes' = flattenList nodes
        flattenList [] = []
        flattenList (And nodes : tail) = flattenList $ nodes ++ tail
        flattenList (Chars _ : Chars _ : _) = error "Never happens"
        flattenList (BadContext : _) = error "Never happens"
        flattenList (head : tail) = head : flattenList tail

flattenTrees (Or nodes) =
  case nodes' of
       [ single ] -> single
       _          -> Or nodes'
  where nodes' = deepenList $ flattenList nodes
        flattenList [] = []
        flattenList (Or nodes : rest) =
          flattenList $ nodes ++ rest
        flattenList (BadContext : _) = error "Never happens"
        flattenList (first : rest) = first : flattenList rest
        deepenList [] = []
        deepenList (RepeatN (Variable "n") repeatPattern : UptoN (Variable "n") uptoPattern : rest)
          | repeatPattern == uptoPattern =
              deepenList $ UptoN (Plus (Variable "n") (Value 1)) repeatPattern : rest
        deepenList (UptoN (Variable "n") uptoPattern : RepeatN (Variable "n") repeatPattern : rest)
          | repeatPattern == uptoPattern =
              deepenList $ UptoN (Plus (Variable "n") (Value 1)) repeatPattern : rest
        deepenList (And (RepeatN (Variable "n") repeatPattern : repeatTail)
                  : And (UptoN   (Variable "n") uptoPattern   : uptoTail)
                  : rest)
          | repeatPattern == uptoPattern = deepenList $ LoopN (Variable "n") (flattenTrees repeatPattern) (flattenTrees $ And uptoTail) (flattenTrees $ And repeatTail) : rest
        deepenList (And (firstHead : firstTail) : second : rest)
          | firstHead == second =
              let first = flattenTrees $ And firstTail
                  deep = flattenTrees $ And [ firstHead, Or [ first, Empty ] ]
              in deepenList $ deep : rest
        deepenList (first : And (secondHead : secondTail) : rest)
          | first == secondHead = error "Must never happen in a valid syntax"
        deepenList (And (firstHead : firstTail) : And (secondHead : secondTail) : rest)
          | firstHead == secondHead =
              let first = flattenTrees $ And firstTail
                  second = flattenTrees $ And secondTail
                  deep = flattenTrees $ And [ firstHead, Or [ first, second ] ]
              in deepenList $ deep : rest
        deepenList (first@(And (Chars firstChars : firstTail))
                  : second@(And (Chars secondChars : secondTail))
                  : rest)
          | firstTail == secondTail =
             let chars = Chars $ addSets firstChars secondChars
                 head = flattenTrees $ And $ chars : firstTail
             in deepenList $ head : rest
          | otherwise =
             let firstOnly = subtractSets firstChars secondChars
                 secondOnly = subtractSets secondChars firstChars
                 bothChars = subtractSets firstChars firstOnly
                 bothTail = [ Or [ flattenTrees $ And firstTail, flattenTrees $ And secondTail ] ]
                 (head : tail) = catMaybes [ maybeChars firstOnly firstTail, maybeChars secondOnly secondTail, maybeChars bothChars bothTail ] ++ rest
             in head : deepenList tail
             where maybeChars chars tail
                     | chars == emptySet = Nothing
                     | otherwise = Just $ flattenTrees $ And $ (Chars chars) : tail
        deepenList [ Chars _, Empty ] = [ Empty ]
        deepenList (first : rest) = first : deepenList rest

flattenTrees node@(Call _ _) = error $ "FT: " ++ show node

flattenTrees node = node


-- | @reorderLists node@ reorders sequences of sub-nodes to optimize the
-- resulting syntax.
reorderLists :: Node -> Node

reorderLists (And nodes) = And $ reorderNodes nodes

reorderLists node = node

reorderNodes [] = []

reorderNodes [node] = [node]

reorderNodes (first : tail) =
  let (second : tail') = reorderNodes tail
  in if shouldFlip first second
        then reorderNodes (second : first : tail')
        else (first : second : tail')

-- | @shouldFlip left right@ returns whether or flipping the order of the nodes
-- would result in an equivalent, more optimized syntax.
shouldFlip :: Node -> Node -> Bool

shouldFlip (Choice _ (Or nodes)) node = foldr (&&) True $ map ((flip shouldFlip) node) nodes

shouldFlip node (Choice _ (Or nodes)) = foldr (&&) True $ map (shouldFlip node) nodes

shouldFlip (Or nodes) node = foldr (&&) True $ map ((flip shouldFlip) node) nodes

shouldFlip node (Or nodes) = foldr (&&) True $ map (shouldFlip node) nodes

shouldFlip (And nodes) node = foldr (&&) True $ map ((flip shouldFlip) node) nodes

shouldFlip node (And nodes) = foldr (&&) True $ map (shouldFlip node) nodes

shouldFlip (RepeatN times pattern) node = shouldFlip pattern node

shouldFlip node (RepeatN times pattern) = shouldFlip node pattern

shouldFlip (UptoN times pattern) node = shouldFlip pattern node

shouldFlip node (UptoN times pattern) = shouldFlip node pattern

shouldFlip (LoopN times pattern less equal) node = shouldFlip (And [ pattern, less, equal ]) node

shouldFlip node (LoopN times pattern less equal) = shouldFlip node (And [ pattern, less, equal ])

shouldFlip (ZeroOrMore pattern) node = shouldFlip pattern node

shouldFlip node (ZeroOrMore pattern) = shouldFlip node pattern

shouldFlip left right =
  if keepOrder left right || keepOrder right left
     then False
     else case switchOrder left right of
               Just result -> result
               Nothing     -> case switchOrder right left of
                                   Just result -> not result
                                   Nothing     -> error $ "SF: " ++ show left ++ " <=> " ++ show right

-- | @keepOrder left right@ returns whether the @left@ and @right@ nodes must
-- never be flipped.
keepOrder :: Node -> Node -> Bool

keepOrder (AndNot _ _) _ = True

keepOrder (BeginToken _) (EmptyToken _) = True
keepOrder (BeginToken _) (EndToken _) = True
keepOrder (BeginToken _) NextChar = True
keepOrder (Chars _) (Commit _) = True
keepOrder (Chars _) NextChar = True
keepOrder (Chars _) (Chars _) = True
keepOrder (EndToken _) (EmptyToken _) = True
keepOrder (EndToken _) NextChar = True

keepOrder _ _ = False


-- | @switchOrder left right@ returns whether the @left@ and @right@ nodes
-- should be flipped. If it returns @Nothing@, try reversing the order.
switchOrder :: Node -> Node -> Maybe Bool

switchOrder (Chars _) (BeginToken _) = Just False
switchOrder (Chars _) (EmptyToken _) = Just False
switchOrder (Chars _) (EndToken _) = Just False
switchOrder (Chars _) NextLine = Just False
switchOrder (Commit _) (BeginToken _) = Just False
switchOrder (Commit _) (EmptyToken _) = Just False
switchOrder (Commit _) (EndToken _) = Just False
switchOrder (Commit _) NextChar = Just False
switchOrder (EndToken _) NextLine = Just False
switchOrder NextLine (BeginToken _) = Just False

switchOrder _ _ = Nothing


-- | @combineSets syntax node@ replaces nodes by the equivalent @CharSet@
-- where possible.
combineSets :: Node -> Node

combineSets (AndNot (Chars base) (Chars subtract)) =
  Chars $ subtractSets base subtract

combineSets (Or nodes) =
  case nodes' of
       [ single ] -> single
       _          -> Or nodes'
  where nodes' = combineList nodes
        combineList [] = []
        combineList (head@(Chars first) : tail) =
          case tail' of
               ((Chars second) : tail'') -> (Chars $ addSets first second) : tail''
               _                       -> head : tail'
          where tail' = combineList tail
        combineList (head : tail) = head : combineList tail

combineSets node@(Call _ _) = error $ "CS: " ++ show node

combineSets node = node


-- | @selectify syntax node@ replaces (parts of) @Or@ nodes by an equivalent
-- @Select@ node where possible.
selectify :: Node -> Node

selectify (Or nodes) =
  case nodes' of
       [ single ] -> single
       _          -> Or nodes'
  where nodes' = combineList $ map charify nodes
        combineList [] = []
        combineList (And (Classes classes1 : tail1) : And (Classes classes2 : tail2) : rest) = combineList $ (Select [(Just classes1, tailAnd tail1), (Just classes2, tailAnd tail2)]) : rest
        combineList (And (Classes classes1 : tail1) : Classes classes2 : rest) = combineList $ (Select [(Just classes1, tailAnd tail1), (Just classes2, Empty)]) : rest
        combineList [ And (Classes classes : tail), Empty ] = [ Select [ (Just classes, tailAnd tail), (Nothing, Empty) ] ]
        combineList (Select alternatives : And (Classes classes2 : tail2) : rest) = combineList $ (Select $ alternatives ++ [ (Just classes2, tailAnd tail2) ]) : rest
        combineList (Select alternatives : Classes classes2 : rest) = combineList $ (Select $ alternatives ++ [ (Just classes2, Empty) ]) : rest
        combineList [ Select alternatives, pattern ] = [ Select $ alternatives ++ [ (Nothing, pattern) ] ]
        combineList (Classes classes1 : And (Classes classes2 : tail2) : rest) = combineList $ (Select [(Just classes1, Empty), (Just classes2, tailAnd tail2)]) : rest
        combineList (Classes classes : Select alternatives : rest) = combineList $ (Select $ (Just classes, Empty) : alternatives ) : rest
        combineList (Classes classes1 : Classes classes2 : rest) = combineList $ (Select $ [ (Just classes1, Empty), (Just classes2, Empty) ]) : rest
        combineList (head : tail) = head : combineList tail
        tailAnd [ pattern ] = pattern
        tailAnd patterns = And patterns

selectify node@(Choice name pattern)
  | (find isOr $ listNode pattern) == Nothing = mapNode emptyCommit pattern
      where isOr (Or _) = True
            isOr _ = False
            emptyCommit (Commit choice) | choice == name = Empty
            emptyCommit node = node

selectify node = node


-- | @charify node@ converts @node@ to the form @And [ Classes _ , ... ]@, if
-- possible, so that @selectify@ can merge it into a @Select@.
charify :: Node -> Node

charify node =
  case classify node of
       Nothing -> node
       Just classes -> And [ Classes classes, node ]
  where classify (And (Classes _ : _)) = Nothing
        classify (And nodes) = foldr locateClasses Nothing nodes
        classify node = Nothing
        locateClasses (Classes classes) _ = Just classes
        locateClasses NextLine classes = classes
        locateClasses (BeginToken _) classes = classes
        locateClasses (Select alternatives) _ = foldl unionClasses (Just Set.empty) $ map fst alternatives
          where unionClasses (Just classes1) (Just classes2) = Just $ Set.union classes1 classes2
                unionClasses _ _ = Nothing
        locateClasses (UptoN _ pattern) _ = Nothing
        locateClasses (RepeatN _ pattern) _ = Nothing
        -- locateClasses nodes _ = Nothing
        locateClasses nodes _ = error $ "LC: " ++ show nodes

-- | @classificationDirectives@ computes a list of directives for classifyin
-- characters.
classificationDirectives :: [ CharSet ] -> String

classificationDirectives distinct =
  "BEGIN_CLASSIFICATION_TABLE(" ++ (show $ 1 + maxLowCode) ++ ")\n" ++
  classificationTable distinct ++
  "END_CLASSIFICATION_TABLE(" ++ (show $ 1 + maxLowCode) ++ ")\n" ++
  "BEGIN_CLASSIFICATION_RANGES\n" ++
  classificationRanges distinct ++
  "END_CLASSIFICATION_RANGES\n"


-- | @classificationTable distinct@ computes a table assigning a class index
-- to each low character code.
classificationTable :: [ CharSet ] -> String

classificationTable distinct =
  concatMap codeClass [0 .. maxLowCode]
  where codeClass code = "LOW_CHAR_CLASS(" ++ show code
                      ++ ", " ++ show (classIndex code)
                      ++ ")\n"
        classIndex code = (fromMaybe 0 $ findIndex (containsCode code) distinct) - length Machine.fakeClasses


-- | @classificationTable distinct@ computes a set of tests for classifying
-- high character codes.
classificationRanges :: [ CharSet ] -> String

classificationRanges distinct = concatMap rangesTests $ zip [- length Machine.fakeClasses ..] distinct
  where rangesTests (index, (_, ranges)) = concatMap (rangeTest index) ranges
        rangeTest index (low, high) = "HIGH_CHAR_RANGE(" ++ (show $ index)
                                   ++ ", " ++ (show low)
                                   ++ ", " ++ (show $ fixMaxUnicode high)
                                   ++ ")\n"
        fixMaxUnicode 0x1FFFF = 0x10FFFF
        fixMaxUnicode code = code


-- | @classifySets distinct node@ converts @Chars@ nodes to @Classes@ nodes
-- based on a set of @distinct@ @CharSet@s.
classifySets :: [ CharSet ] -> Node -> Node

classifySets distinct (Chars chars) = Classes (Set.fromList classes)
  where classes = mapMaybe containsChars $ zip [0..] distinct
        containsChars (index, distinct_set) =
          if containsSet chars distinct_set
             then Just index
             else Nothing

classifySets distinct node = node


-- | @patternMachine node@ Converts the @node@ to a @Machine@.
patternMachine :: Node -> Machine.Machine

patternMachine (And nodes) = Machine.sequential $ map patternMachineT nodes

patternMachine (Or nodes) = Machine.alternatives $ map patternMachineT nodes

patternMachine (Select alternatives) = Machine.select $ map patternAlternative alternatives
  where patternAlternative (selector, pattern) = (selector, patternMachineT pattern)

patternMachine (Classes classes) = Machine.matchClasses classes

patternMachine NextChar = Machine.nextChar

patternMachine NextLine = Machine.nextLine

patternMachine Empty = Machine.empty

patternMachine (BeginToken name) = Machine.beginToken name

patternMachine (EndToken name) = Machine.endToken name

patternMachine (EmptyToken name) = Machine.emptyToken name

patternMachine (Choice name pattern) = Machine.sequential [ Machine.beginChoice name, patternMachineT pattern, Machine.endChoice name ]

patternMachine (Commit name) = Machine.commit name

patternMachine (RepeatN (Variable "n") pattern) = Machine.repeatN $ patternMachine pattern

patternMachine (UptoN (Variable "n") pattern) = Machine.uptoExcludingN $ patternMachine pattern

patternMachine (UptoN (Plus (Variable "n") (Value 1)) pattern) = Machine.uptoIncludingN $ patternMachine pattern

patternMachine (LoopN (Variable "n") pattern less equal) = Machine.loopN (patternMachine pattern) (patternMachine less) (patternMachine equal)

patternMachine (ZeroOrMore (And [Classes classes, NextChar])) = Machine.zeroOrMoreClasses classes

patternMachine BadContext = Machine.badContext

patternMachine pattern = error $ "PM: " ++ show pattern
