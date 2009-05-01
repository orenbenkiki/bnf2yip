-------------------------------------------------------------------------------
-- |
-- Module      :  bnf2yip
-- Copyright   :  (c) Oren Ben-Kiki 2008
-- License     :  LGPL
-- 
-- Maintainer  :  yaml-oren@ben-kiki.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Convert the reference BNF file to a list of directives for further processing, e.g. as a basis for generating a parser.
-- Since this is a one-time operation, efficiency is most emphatically not a consideration in this code.

-- TODO: 29: Unparsed

module Main(main)
where


import qualified CharSet as CharSet
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
import           Utilities


-- * Command line.


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

collectFlags args = case getOpt Permute optionDescriptions args of
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

applyFlags (flag : flags) options = case flag of
                                         Help        -> applyFlags flags options { oToHelp = True }
                                         Output name -> applyFlags flags options { oOutput = name }
                                         Input name  -> applyFlags flags options { oInput = name }


-- | @fromFile name@ returns the contents of the specified input file called /name/ (may be "-" for @stdin@).
fromFile :: String -> IO String

fromFile name = case name of
                      "-"  -> getContents
                      path -> readFile path


-- | @intoFile name text@ writes the /text/ into the specified output file called /name/ (may be "-" for @stdout@).
intoFile :: String -> String -> IO ()

intoFile name text = case name of
                           "-"  -> putStr text
                           path -> writeFile path $ text


-- | @runWith options@ runs the program with the specified /options/.
runWith :: Options -> IO ()

runWith options = do text <- fromFile (oInput options)
                     let (syntax, index) = parseSyntax $ spoilMaxUnicode text
                     intoFile (oOutput options) $ directives $ finalize index $ optimize $ prepare syntax
  where spoilMaxUnicode text = subRegex (mkRegex "10FFFF") text "1FFFF"


-- | @main@ converts an input BNF file to directives.
main :: IO ()

main = do args <- getArgs
          flags <- collectFlags args
          options <- applyFlags flags defaultOptions
          if oToHelp options
             then putStrLn usage
             else runWith options


-- * Prepare.


-- | @prepare syntax@ performs all the one-time preparations of the parsed /syntax/ before optimization.
prepare :: Syntax -> Syntax

prepare syntax = (mapSyntax $ traced "purgePeeks" purgePeeks)
               $ (traced "filter" $ Map.filter (notElem BadContext . listNode . snd))
               $ (\syntax -> (mapSyntax $ traced "inline" (inline syntax Set.empty)) syntax)
               $ (traced "decontext" $ Map.fromList . concatMap decontext . Map.toList)
               $ (mapSyntax $ traced "simplify" simplify) syntax


-- ** Simplify.


-- | @simplify node@ converts complex operations to atomics ones.
simplify :: Node -> Node

simplify node@(And _) = node

simplify node@(AndNot _ _) = node

simplify node@(Call _ _) = node

simplify node@(Case _ _) = node

simplify node@(Chars _) = node

simplify node@(Choice _ _) = node

simplify node@(Commit _) = node

simplify node@NextChar = node

simplify node@NextLine = node

simplify (OneOrMore pattern) = And [ pattern, ZeroOrMore pattern ]

simplify (Optional pattern) = Or [ pattern, Empty ]

simplify node@(Or _) = node

simplify node@(Plus _ _) = node

simplify (RepeatN (Value times) pattern) = And $ replicate times pattern

simplify node@(RepeatN _ _) = node

simplify node@StartOfLine = Chars $ CharSet.fake $ CharSet.startOfLine

simplify node@(Symbol _) = node

simplify (Token name pattern) = And [ BeginToken name, pattern, EndToken name, PrefixError $ EndToken "Unparsed" ]

simplify node@(UptoN _ _) = node

simplify node@(Value _) = node

simplify node@(Variable _) = node

simplify (WrapTokens beginToken endToken pattern) = And [ EmptyToken beginToken, pattern, EmptyToken endToken, PrefixError $ EmptyToken endToken ]

simplify node = error $ "simplify: " ++ show node


-- ** Decontext.


-- | @decontext (name, production)@ gets rid of the /c/ parameter by splitting each /production/ to separate ones.
-- The name of each one is the original /name/ combined with the specific context.
decontext :: (String, Production) -> [ (String, Production) ]

decontext (name, production@(parameters, pattern))
  | elem (Variable "c") parameters = map productize $ catMaybes $ decontextAlternatives pattern
  | otherwise = [ (name, production) ]
      where productize (context, node) = (contextify name context, (parameters', node))
            parameters' = filter (/= Variable "c") parameters


-- | The list of all the possible contexts.
contexts = [ "block-out", "block-in", "block-key", "flow-out", "flow-in", "flow-key" ]


-- | @decontextAlternatives node@ converts a /node/ to a list of alternatives, one for each possible context.
decontextAlternatives :: Node -> [ Maybe (String, Node) ]

decontextAlternatives (Case (Variable "c") alternatives)  = map contextAlternative contexts
  where contextAlternative context = case find (isContextAlternative context) alternatives of
                                          Just (_, node) -> Just (context, node)
                                          Nothing        -> Nothing
        isContextAlternative context (Symbol value, _) = context == value

decontextAlternatives node = map decontextNode contexts
  where decontextNode context = Just (context, mapNode (applyContext context) node)
        applyContext context node@(Call name parameters)
          | elem (Variable "c") parameters = Call (contextify name context) (filter (/= Variable "c") parameters)
          | otherwise = node
        applyContext context node = node


-- | @contextify name context@ converts a @Production@ @name@ to a name that also contains the @context@.
contextify :: String -> String -> String

contextify name context = name ++ " " ++ context

-- ** Inline.


-- | @inline syntax callers node@ converts each call @node@s to the invoked procedure body, except for recursive calls.
inline :: Syntax -> Set.Set String -> Node -> Node

inline syntax callers node@(Call name parameters)
  | foldr (||) False $ map isComplex parameters = error $ "inline: " ++ show node
  | Set.member name callers = node
  | Map.lookup name syntax == Nothing = BadContext
  | otherwise = let callers' = Set.insert name callers
                    Just (_, pattern) = Map.lookup name syntax
                    pattern' = mapNode (inline syntax callers') pattern
                in case find isRecursiveCall $ listNode pattern' of
                        Just _  -> Be name pattern'
                        Nothing -> pattern'
  where isComplex (Variable _) = False
        isComplex _ = True
        isRecursiveCall (Call name' _) = name' == name
        isRecursiveCall _ = False

inline syntax callers node = node


-- ** Purge peeks.


-- | @purgePeeks node@ purges any side-effect actions from inside a peek /node/.
purgePeeks :: Node -> Node

purgePeeks (AndNot base subtract) = AndNot base $ purgePeekActions subtract

purgePeeks node@(And _) = node

purgePeeks node@(BeginToken _) = node

purgePeeks node@(Chars _) = node

purgePeeks node@(Choice _ _) = node

purgePeeks node@(Commit _) = node

purgePeeks node@Empty = node

purgePeeks node@(EmptyToken _) = node

purgePeeks node@(EndToken _) = node

purgePeeks node@NextChar = node

purgePeeks node@NextLine = node

purgePeeks node@(Or _) = node

purgePeeks node@(Plus _ _) = node

purgePeeks node@(PrefixError _) = node

purgePeeks node@(RepeatN _ _) = node

purgePeeks node@(UptoN _ _) = node

purgePeeks node@(Value _) = node

purgePeeks node@(Variable _) = node

purgePeeks node@(ZeroOrMore _) = node

purgePeeks node = error $ "purgePeeks: " ++ show node


-- | @purgePeekActions node@ purges any side-effect actions from the peek /node/.
purgePeekActions :: Node -> Node

purgePeekActions node@(And _) = node

purgePeekActions node@NextChar = node

purgePeekActions node@(Or _) = node

purgePeekActions node = error $ "purgePeekActions: " ++ show node


-- * Optimize.


-- | @optimize syntax@ repeatedly performs optimizations on the /syntax/ until it reaches a (hopefully) optimal form.
optimize :: Syntax -> Syntax

optimize syntax = fixpoint (mapSyntax (traced "collapseLists" collapseLists)
                          . mapSyntax (traced "computeAndNots" computeAndNots)
                          . mapSyntax (traced "distinctOrChars" distinctOrChars)
                          . mapSyntax (traced "flattenAnds" flattenAnds)
                          . mapSyntax (traced "flattenOrs" flattenOrs)
                          . mapSyntax (traced "mergeOrChars" mergeOrChars)
                          . mapSyntax (traced "mergeOrHeads" mergeOrHeads)
                          . mapSyntax (traced "mergeOrLoops" mergeOrLoops)
                          . mapSyntax (traced "removeAndEmpties" removeAndEmpties)
                          . mapSyntax (traced "removeSimpleChoices" removeSimpleChoices)
                          . mapSyntax (traced "reorderAndLists" reorderAndLists)
                          . mapSyntax (traced "simplifyAndNots" simplifyAndNots)) syntax


-- | @collapseLists node@ collapses sub-node lists of an @Or@ or an @And@ /node/.
collapseLists :: Node -> Node

collapseLists (And [ node ]) = node

collapseLists (Or [ node ]) = node

collapseLists node = node


-- | @distinctOrChars node@ splits consequtive @Chars@ in an @Or@ node to distinct sets.
distinctOrChars :: Node -> Node

distinctOrChars (Or nodes) = Or $ foldr merge [] nodes
  where merge first@(And (Chars firstChars : firstTail)) (second@(And (Chars secondChars : secondTail)) : rest) =
          let firstOnlyChars = CharSet.subtract firstChars secondChars
              firstOnly = And $ Chars firstOnlyChars : firstTail
              secondOnlyChars = CharSet.subtract secondChars firstChars
              secondOnly = And $ Chars secondOnlyChars : secondTail
              bothChars = CharSet.subtract firstChars firstOnlyChars
              both = And [ Chars bothChars, Or [ And firstTail, And secondTail ] ]
          in catMaybes [ maybe firstOnlyChars firstOnly, maybe secondOnlyChars secondOnly, maybe bothChars both ] ++ rest
        merge first@(And (Chars firstChars : firstTail)) (second@(Chars secondChars) : rest) =
          let firstOnlyChars = CharSet.subtract firstChars secondChars
              firstOnly = And $ Chars firstOnlyChars : firstTail
              secondOnlyChars = CharSet.subtract secondChars firstChars
              secondOnly = Chars secondOnlyChars
              bothChars = CharSet.subtract firstChars firstOnlyChars
              both = And [ Chars bothChars, Or [ And firstTail, Empty ] ]
          in catMaybes [ maybe firstOnlyChars firstOnly, maybe secondOnlyChars secondOnly, maybe bothChars both ] ++ rest
        merge node nodes = node : nodes
        maybe chars node
          | chars == CharSet.empty = Nothing
          | otherwise              = Just node

distinctOrChars node = node


-- | @computeAndNots node@ computes character sets in and @AndNot@ /node/.
computeAndNots :: Node -> Node

computeAndNots (AndNot (And (Chars baseChars : baseTail)) (And (Chars subtract : _))) = And $ (Chars $ CharSet.subtract baseChars subtract) : baseTail

computeAndNots node = node


-- | @flattenAnds node@ flattens and @And@ of @And@ /node/ to a single @Node@.
flattenAnds :: Node -> Node

flattenAnds (And nodes) = And $ foldr merge [] nodes
  where merge (And nested) nodes = nested ++ nodes
        merge node nodes = node : nodes

flattenAnds node = node


-- | @flattenOrs node@ flattens and @Or@ of @Or@ /node/ to a single @Node@.
flattenOrs :: Node -> Node

flattenOrs (Or nodes) = Or $ foldr merge [] nodes
  where merge (Or nested) nodes = nested ++ nodes
        merge node nodes = node : nodes

flattenOrs node = node


-- | @mergeOrChars node@ merges consequtive @Chars@ sub-nodes of an @Or@ /node/.
mergeOrChars :: Node -> Node

mergeOrChars (Or nodes) = Or $ foldr merge [] nodes
  where merge node [] = [ node ]
        -- Q merge (Chars first) (Chars second : nodes) = (Chars $ CharSet.union first second) : nodes
        merge (And (Chars firstChars : firstTail)) (And (Chars secondChars : secondTail) : rest)
          | firstTail == secondTail = And (Chars (CharSet.union firstChars secondChars) : firstTail) : rest
        merge node nodes = node : nodes

mergeOrChars node = node


-- | @mergeOrHeads node@ merges common head elements of an @Or@ /node/ sub-nodes lists.
mergeOrHeads :: Node -> Node

mergeOrHeads (Or nodes) = Or $ foldr merge [] nodes
  where merge node [] = [ node ]
        merge (And (firstHead : firstTail)) (And (secondHead : secondTail) : rest)
          | firstHead == secondHead = And [ firstHead, Or [ And firstTail, And secondTail ] ] : rest
        merge (And (firstHead : firstTail)) (second : rest)
          | firstHead == second = And [ firstHead, Or [ And firstTail, Empty ] ] : rest
        merge node nodes = node : nodes

mergeOrHeads node = node


-- | @mergeOrLoops node@ merges loop head elements of an @Or@ /node/ sub-nodes lists.
mergeOrLoops :: Node -> Node

mergeOrLoops (Or nodes) = Or $ foldr merge [] nodes
  where merge node [] = [ node ]
        merge (And (RepeatN repeatTimes repeatPattern : repeatTail)) (And (UptoN uptoTimes uptoPattern : uptoTail) : rest)
          | uptoTimes == repeatTimes && uptoPattern == repeatPattern = (LoopN repeatTimes repeatPattern (And uptoTail) (And repeatTail)) : rest
        merge node nodes = node : nodes

mergeOrLoops node = node


-- | @removeAndEmpties node@ removes @Empty@ nodes from and @And@ /node/ sub-nodes list.
removeAndEmpties :: Node -> Node

removeAndEmpties (And nodes) = And $ filter (/= Empty) nodes

removeAndEmpties node = node


-- | @removeSimpleChoices node@ converts simple @Choice@ /node/s to @Or@ nodes.
removeSimpleChoices :: Node -> Node

removeSimpleChoices (Choice name pattern)
  | Nothing == find isComplexChoice (listNode pattern) = mapNode clearChoices pattern
  where clearChoices (Choice choice pattern) | choice == name = pattern
        clearChoices (Commit choice) | choice == name = Empty
        clearChoices node = node
        isComplexChoice (Or nodes) = foldr isComplexAlternative False nodes
        isComplexChoice node = False
        isComplexAlternative (And (Chars _ : _)) result = result
        isComplexAlternative (Chars _) result = result
        isComplexAlternative Empty result = result
        isComplexAlternative _ _ = True

removeSimpleChoices node = node


-- | @reorderAndLists node@ reorders @And@ /node/ sub-nodes lists.
reorderAndLists :: Node -> Node

reorderAndLists (And nodes) = And $ foldr merge [] nodes
  where merge node [] = [ node ]
        merge first (second : rest) = if shouldFlip first second
                                         then second : first : rest
                                         else first : second : rest

reorderAndLists node = node


-- | @simplifyAndNots node@ simplifies complex @AndNode@ /node/s.
simplifyAndNots :: Node -> Node

simplifyAndNots (AndNot (Choice name pattern) subtract) = Choice name $ AndNot pattern subtract

simplifyAndNots (AndNot (Or alternatives) subtract) = Or $ map andNot alternatives
  where andNot pattern = AndNot pattern subtract

simplifyAndNots node = node


-- | @shouldFlip first second@ returns whether flipping the order of the /first/ and /second/ nodes would improve the syntax.
shouldFlip :: Node -> Node -> Bool

shouldFlip (And nodes) node = foldr (&&) True $ map ((flip shouldFlip) node) nodes

shouldFlip node (And nodes) = foldr (&&) True $ map (shouldFlip node) nodes

shouldFlip (Choice _ pattern) node = shouldFlip pattern node

shouldFlip node (Choice _ pattern) = shouldFlip node pattern

shouldFlip (LoopN times pattern less equal) node = shouldFlip pattern node && shouldFlip less node && shouldFlip equal node

shouldFlip node (LoopN times pattern less equal) = shouldFlip node pattern && shouldFlip node less && shouldFlip node equal

shouldFlip (Or nodes) node = foldr (&&) True $ map ((flip shouldFlip) node) nodes

shouldFlip node (Or nodes) = foldr (&&) True $ map (shouldFlip node) nodes

shouldFlip (PrefixError pattern) node = shouldFlip pattern node

shouldFlip node (PrefixError pattern) = shouldFlip node pattern

shouldFlip (RepeatN _ pattern) node = shouldFlip pattern node

shouldFlip node (RepeatN _ pattern) = shouldFlip node pattern

shouldFlip (UptoN _ pattern) node = shouldFlip pattern node

shouldFlip node (UptoN _ pattern) = shouldFlip node pattern

shouldFlip (ZeroOrMore pattern) node = shouldFlip pattern node

shouldFlip node (ZeroOrMore pattern) = shouldFlip node pattern

shouldFlip first second =
  if keepOrder first second || keepOrder second first
     then False
     else case switchOrder first second of
               Just result -> result
               Nothing     -> case switchOrder second first of
                                   Just result -> not result
                                   Nothing     -> error $ "shouldFlip: " ++ show first ++ " <=> " ++ show second


-- | @keepOrder first second@ returns whether the /first/ and /second/ nodes should not be flipped, regardless of the order they are in.
keepOrder :: Node -> Node -> Bool

keepOrder (BeginToken _) (BeginToken _) = True

keepOrder (BeginToken _) (EndToken _) = True

keepOrder (BeginToken _) (EmptyToken _) = True

keepOrder (Chars _) (Chars _) = True

keepOrder (Chars _) (Commit _) = True

keepOrder (EmptyToken _) (EmptyToken _) = True

keepOrder (EmptyToken _) (EndToken _) = True

keepOrder (EndToken _) (EndToken _) = True

keepOrder NextChar (BeginToken _) = True

keepOrder NextChar (Chars _) = True

keepOrder NextChar (EndToken _) = True

keepOrder NextChar NextLine = True

keepOrder _ _ = False


-- | @switchOrder first second@ returns whether the @first@ and @second@ nodes should be flipped.
-- If it returns @Nothing@, try reversing the order (and the result).
switchOrder :: Node -> Node -> Maybe Bool

switchOrder (Chars _) (BeginToken _) = Just False

switchOrder (Chars _) (EmptyToken _) = Just False

switchOrder (Chars _) (EndToken _) = Just False

switchOrder (Chars _) NextLine = Just False

switchOrder (Commit _) (BeginToken _) = Just False

switchOrder (Commit _) (EmptyToken _) = Just False

switchOrder (Commit _) (EndToken _) = Just False

switchOrder (Commit _) NextChar = Just False

switchOrder NextLine (EndToken _) = Just False

switchOrder NextLine (BeginToken _) = Just False

switchOrder _ _ = Nothing


-- * Finalize.


-- | @finalize index syntax@ performs final one-time processing on the /syntax/ using its /index/ before converting it to directives.
finalize :: [ String ] -> Syntax -> ([ CharSet.CharSet ], [ Machine.Named ])

finalize index syntax = let distinct = distinctSets syntax
                        in (distinct, machinize index $ selectify $ classify distinct syntax)


-- ** Classify.


-- | @distinctSets syntax@ computes the distinct sets of characters that are equivalent as far as the /syntax/ is concerned.
distinctSets :: Syntax -> [ CharSet.CharSet ]

distinctSets syntax = CharSet.distinct $ map CharSet.fake CharSet.fakeCodes ++ (mapMaybe nodeChars $ listSyntax syntax)
                      where nodeChars (Chars chars) = Just chars
                            nodeChars _ = Nothing


-- | @classify distinct syntax@ converts all @Chars@ in the /syntax/ to @Classes@ according to the /distinct/ sets.
classify :: [ CharSet.CharSet ] -> Syntax -> Syntax

classify distinct syntax = mapSyntax (classifyNode distinct) syntax


-- | @classifyNode distinct node@ converts each @Chars@ /node/ to @Classes@ based on a set of /distinct/ @CharSet@s.
classifyNode :: [ CharSet.CharSet ] -> Node -> Node

classifyNode distinct (Chars chars) = Classes $ Set.fromList $ mapMaybe usedClassIndex $ zip [0..] distinct
  where usedClassIndex (index, set) = if CharSet.contains chars set
                                         then Just index
                                         else Nothing

classifyNode distinct node = node


-- ** Selectify


-- | @selectify syntax@ converts suitable @Or@s in the /syntax/ to @Select@s.
selectify :: Syntax -> Syntax

selectify syntax = mapSyntax (traced "selectifyNode" selectifyNode) syntax


-- | @selectifyNode node@ converts as much as possible of an @Or@ /node/ to a @Select@.
selectifyNode :: Node -> Node

selectifyNode (Or nodes) = collapseLists $ Or $ foldr merge [] nodes
  where merge (Classes classes) [] = [ Select [ (Just classes, Empty) ] ]
        merge Empty [] = [ Select [ (Nothing, Empty) ] ]
        merge node [] = [ node ]
        merge (Classes classes) nodes = nodes
        merge (And (Classes firstClasses : firstTail)) (And (Classes secondClasses : secondTail) : rest) =
          Select [ (Just firstClasses, collapseLists $ And firstTail), (Just secondClasses, collapseLists $ And secondTail) ] : rest
        merge (And (Classes classes : tail)) (Select alternatives : rest) = (Select $ (Just classes, collapseLists $ And tail) : alternatives) : rest
        merge (And (Classes classes : tail)) nodes = [ Select [ (Just classes, collapseLists $ And tail), (Nothing, collapseLists $ Or nodes) ] ]
        merge node nodes = node : nodes

selectifyNode node = node


-- ** Machinize.


-- | @machinize index syntax@ converts each /syntax/ @Production@ to a single @Named@ machine using the /index/.
machinize :: [ String ] -> Syntax -> [ Machine.Named ]

machinize index syntax = concatMap (productionMachines syntax) $ zip [1..] index
  where productionMachines syntax (index, name) = catMaybes [ productionMachine syntax index name ]
                                              ++ (map subMachine $ zip [1..] $ mapMaybe (productionMachine syntax index) $ map (contextify name) contexts)
        subMachine (index, named) = named { Machine.nIndex = named|>Machine.nIndex ++ "." ++ show index }
        parameterName (Variable name) = name
        productionMachine syntax index name = case Map.lookup name syntax of
                                                   Nothing                 -> Nothing
                                                   Just (parameters, node) -> Just Machine.Named { Machine.nIndex = show index,
                                                                                                   Machine.nName = name,
                                                                                                   Machine.nParameters = map parameterName parameters,
                                                                                                   Machine.nMachine = traced "Optimize" Machine.optimize $ traced "nodeMachine" nodeMachine $ node }


-- | @nodeMachine node@ Converts the /node/ to a @Machine@.
nodeMachine :: Node -> Machine.Machine

nodeMachine (And nodes) = foldl merge Machine.empty nodes
  where merge machine (PrefixError pattern) = Machine.prefixFailure machine $ traced "nodeMachine" nodeMachine pattern
        merge machine node = Machine.sequential [ machine, traced "nodeMachine" nodeMachine node ]

nodeMachine (BeginToken name) = Machine.beginToken name

nodeMachine (Choice name pattern) = Machine.sequential [ Machine.beginChoice name, traced "nodeMachine" nodeMachine $ pattern, Machine.endChoice name ]

nodeMachine (Classes classes) = Machine.matchClasses classes

nodeMachine (Commit name) = Machine.commit name

nodeMachine Empty = Machine.empty

nodeMachine (EmptyToken name) = Machine.emptyToken name

nodeMachine (EndToken name) = Machine.endToken name

nodeMachine (LoopN (Variable "n") pattern less equal) = Machine.loopN (traced "nodeMachine" nodeMachine pattern) (traced "nodeMachine" nodeMachine less) (traced "nodeMachine" nodeMachine equal)

nodeMachine NextChar = Machine.nextChar

nodeMachine NextLine = Machine.nextLine

nodeMachine (RepeatN (Variable "n") pattern) = Machine.repeatN $ traced "nodeMachine" nodeMachine pattern

nodeMachine (Select alternatives) = Machine.select $ map alternativeMachine alternatives
  where alternativeMachine (selector, node) = (selector, traced "nodeMachine" nodeMachine $ node)

nodeMachine (UptoN (Variable "n") pattern) = Machine.uptoExcludingN $ traced "nodeMachine" nodeMachine pattern

nodeMachine (UptoN (Plus (Variable "n") (Value 1)) pattern) = Machine.uptoIncludingN $ traced "nodeMachine" nodeMachine pattern

nodeMachine (ZeroOrMore (And [ Classes classes, NextChar ])) = Machine.zeroOrMoreClasses classes

nodeMachine (ZeroOrMore pattern) = Machine.zeroOrMore $ traced "nodeMachine" nodeMachine pattern

nodeMachine node = error $ "nodeMachine: " ++ show node


-- * Directives.


-- | @directives distinct machines@ converts the /distinct/ character sets and the @Named@ /machines/ to directives.
directives :: ([ CharSet.CharSet ], [ Machine.Named ]) -> String

directives (distinct, machines) = "BEGIN_SYNTAX\n"
                               ++ "BEGIN_CLASSIFICATION\n"
                               ++ CharSet.directives distinct
                               ++ "END_CLASSIFICATION\n"
                               ++ "BEGIN_MACHINES(" ++ (show $ length machines) ++ ")\n"
                               ++ Machine.directives machines
                               ++ "END_MACHINES(" ++ (show $ length machines) ++ ")\n"
                               ++ "END_SYNTAX\n"
