module Machine (
    Named(..),
    Machine,
    empty,
    badContext,
    matchClasses,
    zeroOrMoreClasses,
    nextChar,
    nextLine,
    beginToken,
    endToken,
    emptyToken,
    beginChoice,
    endChoice,
    commit,
    prefixFailure,
    finally,
    zeroOrMore,
    sequential,
    alternatives,
    select,
    repeatN,
    uptoExcludingN,
    uptoIncludingN,
    loopN,
    optimize,
    directives
) where


import qualified CharSet as CharSet
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Text.Regex
import           Utilities

import Debug.Trace

-- * Types.


-- | A named state machine.
data Named = Named { nIndex      :: String,
                     nName       :: String,
                     nParameters :: [ String ],
                     nMachine    :: Machine }


-- | A state machine. The start @State@ is always at index 0.
-- The failure @State@ is always at @Map.size machine - 2@ and the success @State@ is always at @Map.size machine - 1@.
type Machine = Map.Map Int State


-- | A state in the @Machine@.
data State = State {
    sAction      :: Maybe Action,
    sTransitions :: [ Transition ]
  }
  deriving (Eq, Ord, Show)


-- | Action performed when entering a @State@.
data Action = BeginToken String
            | BadContext
            | BeginChoice String
            | Commit String
            | EmptyToken String
            | EndChoice String
            | EndToken String
            | Failure
            | IncrementCounter
            | NextChar
            | NextLine
            | NonPositiveN
            | PopState
            | PrevChar
            | PushState
            | ResetCounter
            | ResetState
            | SetState
            | Success
  deriving (Eq, Ord, Show)


-- | Transition between states.
data Transition = Transition {
    tClasses    :: Maybe (Set.Set Int),
    tStateIndex :: Int
  }
  deriving (Eq, Ord, Show)


-- * Special classes.


-- | Fake character class for testing the loop counter against the /n/ variable.
counterLessThanN = -1000


-- | Fake character class for testing the loop counter against the /n/ variable.
counterLessEqualN = -2000


-- | The number of special classes. This includes all the fake codes as well.
specialClasses :: [ Int ]

specialClasses = counterLessThanN : counterLessEqualN : map (+ length CharSet.fakeCodes) CharSet.fakeCodes


-- * Create transitions.


-- | @gotoTransition index@ is a @Transition@ that always goes to the given state /index/.
gotoTransition :: Int -> Transition

gotoTransition index = Transition { tClasses = Nothing, tStateIndex = index }


-- | @classesTransition classes index@ is a @Transition@ that always goes to the given state /index/ if the current character is any of the given /classes/.
classesTransition :: Set.Set Int -> Int -> Transition

classesTransition classes index = Transition { tClasses = Just classes, tStateIndex = index }


-- * Create states.


-- | @successState@ is one of the the two final @State@s of every machine.
successState = State { sAction = Just Success, sTransitions = [] }


-- | @failureState@ is one of the the two final @State@s of every machine.
failureState = State { sAction = Just Failure, sTransitions = [] }


-- | @gotoState index@ is a @State@ that unconditionally moves to another state /index/.
gotoState :: Int -> State

gotoState index = State { sAction = Nothing, sTransitions = [ gotoTransition index ] }


-- | @classesState classes onSuccess onFailure@ is a @State@ that moves to one of the /onSuccess/ or /onFailure/ state indices,
-- depending on whether the current character belongs to any of the /classes/.
classesState :: Set.Set Int -> Int -> Int -> State

classesState classes onSuccess onFailure = State { sAction = Nothing, sTransitions = [ classesTransition classes onSuccess, gotoTransition onFailure ] }


-- | @actionState action index@ is a @State@ that performs the /action/ and unconditionally moves to the state /index/.
actionState :: Action -> Int -> State

actionState action index = State { sAction = Just action, sTransitions = [ gotoTransition index ] }

-- * Create simple machines.


--- | A @Machine@ that always succeeds.
empty :: Machine

empty = Map.fromList [ (0, gotoState 2),
                       (1, failureState),
                       (2, successState) ]


-- | A @Machine@ that always fails due to an unexpected context parameter.
-- This "never happens", except when creating machines for testing productions.
badContext :: Machine

badContext = Map.fromList [ (0, actionState BadContext 1),
                            (1, failureState),
                            (2, successState) ]


-- | @matchClasses classes@ creates a @Machine@ that succeeds if the current character belongs to one of the /classes/.
matchClasses :: Set.Set Int -> Machine

matchClasses classes = Map.fromList [ (0, classesState classes 2 1),
                                      (1, failureState),
                                      (2, successState) ]


-- | @zeroOrMoreClasses classes@ creates a @Machine@ that consumes any number of characters of the specified /classes/.
-- It never fails.
zeroOrMoreClasses :: Set.Set Int -> Machine

zeroOrMoreClasses classes = Map.fromList [ (0, classesState classes 1 3),
                                           (1, actionState NextChar 0),
                                           (2, failureState),
                                           (3, successState) ]


-- | @actionMachine action@ creates a @Machine@ that performs a single /action/.
actionMachine :: Action -> Machine

actionMachine action = Map.fromList [ (0, actionState action 2),
                                      (1, failureState),
                                      (2, successState) ]


-- | @nextChar@ creates a @Machine@ that consumes the next character.
nextChar :: Machine

nextChar = actionMachine NextChar


-- | @nextLine@ creates a @Machine@ that increments the line counter (and whatever else needs to be updated when starting a new line).
nextLine :: Machine

nextLine = actionMachine NextLine


-- | @beginToken name@ creates a @Machine@ that begins collecting characters for a token with the given /name/.
beginToken :: String -> Machine

beginToken name = actionMachine $ BeginToken name


-- | @endToken name@ creates a @Machine@ that ends collecting characters for a token with the given /name/.
endToken :: String -> Machine

endToken name = actionMachine $ EndToken name


-- | @emptyToken name@ creates a @Machine@ that emits an empty token with the given /name/.
emptyToken :: String -> Machine

emptyToken name = actionMachine $ EmptyToken name


-- | @beginChoice name@ creates a @Machine@ that marks the beginning of a named choice point with the given /name/.
beginChoice :: String -> Machine

beginChoice name = actionMachine $ BeginChoice name


-- | @endChoice name@ creates a @Machine@ that marks the end of a named choice point with the given /name/.
endChoice :: String -> Machine

endChoice name = actionMachine $ EndChoice name


-- | @commit name@ creates a @Machine@ that commits to the choice point of the given /name/.
commit :: String -> Machine

commit name = actionMachine $ Commit name


-- * Complex machines.


-- | @shiftIndices offset machine@ will shift all the indices of the /machine/ @State@s by the /offset/.
shiftIndices :: Int -> Machine -> Machine

shiftIndices offset machine = Map.fromList $ map applyToPair $ Map.toList machine
  where applyToPair (index, state) = (index + offset , applyToState state)
        applyToState state = state { sTransitions = map applyToTransition $ state |>sTransitions }
        applyToTransition transition = transition { tStateIndex = transition|>tStateIndex + offset }


-- | @appendMachine first second@ appends the @State@s of the /second/ @Machine@ immediatly after the /first/ one's.
appendMachine :: Machine -> Machine -> Machine

appendMachine first second = Map.union first $ shiftIndices (Map.size first) second


-- | @prefixFailure machine prefix@ creates a @Machine@ that executes the first /machine/ and if it fails, executes the /prefix/ machine before failing.
prefixFailure :: Machine -> Machine -> Machine

prefixFailure machine prefix = insertList [ (Map.size machine - 2,                   gotoState $ Map.size machine + 0),
                                            (Map.size machine - 1,                   gotoState $ Map.size machine + Map.size prefix + 1),
                                            (Map.size machine + Map.size prefix - 2, gotoState $ Map.size machine + Map.size prefix + 0),
                                            (Map.size machine + Map.size prefix - 1, gotoState $ Map.size machine + Map.size prefix + 0),
                                            (Map.size machine + Map.size prefix + 0, failureState),
                                            (Map.size machine + Map.size prefix + 1, successState) ]
                             $ appendMachine machine prefix


-- | @finally machine suffix@ creates a @Machine@ that executes the first /machine/, and then always executes /suffix/.
-- This succeeds only if the first /machine/ succeeds.
finally :: Machine -> Machine -> Machine

finally machine suffix = sequential [ prefixFailure machine suffix, suffix ]


-- | @zeroOrMoreClasses machine@ creates a @Machine@ that consumes any number of atomic (complete) repetitions of the @machine@.
-- It never fails.
zeroOrMore :: Machine -> Machine

zeroOrMore machine = insertList [ (0,                        actionState PushState 1),
                                  (1 + Map.size machine - 2, gotoState $ 1 + Map.size machine + 1),
                                  (1 + Map.size machine - 1, gotoState $ 1 + Map.size machine + 0),
                                  (1 + Map.size machine + 0, actionState SetState 1),
                                  (1 + Map.size machine + 1, actionState PopState $ 1 + Map.size machine + 3),
                                  (1 + Map.size machine + 2, failureState),
                                  (1 + Map.size machine + 3, successState) ]
                   $ shiftIndices 1 machine


-- | @sequential machines@ returns a new @Machine@ that executes all the specified /machines/ in order.
sequential :: [ Machine ] -> Machine

sequential machines = foldr1 merge machines
  where merge first second = insertList [ (Map.size first - 2, gotoState $ Map.size first + Map.size second - 2),
                                          (Map.size first - 1, gotoState $ Map.size first) ]
                           $ appendMachine first second


-- | @alternatives machines@ returns a new @Machine@ that tries each of the alternative /machines/ in turn.
alternatives :: [ Machine ] -> Machine

alternatives [ machine ] = machine

alternatives (first : rest) = let first' = alternative (actionMachine PushState) first
                                  rest' = map (alternative $ actionMachine ResetState) rest
                                  merged = foldr1 merge (first' : rest')
                              in merged `finally` actionMachine PopState
  where alternative prefix machine
          | machine == empty = empty
          | otherwise = sequential [ prefix, machine ]
        merge first second = insertList [ (Map.size first - 2, gotoState $ Map.size first),
                                          (Map.size first - 1, gotoState $ Map.size first + Map.size second - 1) ]
                           $ appendMachine first second


-- | @select [ (Maybe classes, machine) ]@ returns a new @Machine@ that merges several /machine/s,
-- given each of them is only applicable if the current character belongs to some /classes/.
-- The final entry may omit the /classes/ to serve as a default. Otherwise the default is to fail.
select :: [ (Maybe (Set.Set Int), Machine) ] -> Machine

select alternatives = let (successIndices, failureIndices, transitions, machine) = foldl addAlternative ([], [], [], Map.empty) alternatives
                          transitions' = if (last transitions)|>tClasses == Nothing
                                            then transitions
                                            else transitions ++ [ gotoTransition $ 1 + Map.size machine - 2 ]
                          states = (0, State { sAction = Nothing, sTransitions = transitions' })
                                : ( catMaybes $ map (finalState $ 1 + Map.size machine - 1) successIndices
                                              ++ map (finalState $ 1 + Map.size machine - 2) failureIndices )
                      in insertList states $ shiftIndices 1 machine
  where addAlternative (successIndices, failureIndices, transitions, combinedMachine) (classes, machine) =
          let combinedMachine' = appendMachine combinedMachine machine
              transitions' = transitions ++ [ Transition { tClasses = classes, tStateIndex = 1 + Map.size combinedMachine } ]
              successIndices' = (1 + Map.size combinedMachine' - 1) : successIndices
              failureIndices' = (1 + Map.size combinedMachine' - 2) : failureIndices
          in (successIndices', failureIndices', transitions', combinedMachine')
        finalState finalIndex stateIndex
          | finalIndex == stateIndex = Nothing
          | otherwise                = Just (stateIndex, gotoState finalIndex)


-- | @repeatN machine@ returns a new @Machine@ that repeats the given /machine/ exactly the number of times specified in the /n/ variable.
-- If /n/ is negative ("never happens") it is taken to mean zero.
repeatN :: Machine -> Machine

repeatN machine = insertList [ (0,                        actionState ResetCounter 1),
                               (1,                        classesState (Set.fromList [ counterLessThanN ]) 2 (3 + Map.size machine + 1)),
                               (2,                        actionState IncrementCounter 3),
                               (3 + Map.size machine - 2, gotoState $ 3 + Map.size machine + 0),
                               (3 + Map.size machine - 1, gotoState 1),
                               (3 + Map.size machine + 0, failureState),
                               (3 + Map.size machine + 1, successState) ]
                $ shiftIndices 3 machine


-- | @uptoN machine@ returns a new @Machine@ that repeats the given atomic /machine/ as long as some /test/ (fake class) is not satisfied.
-- This never fails (unless /n/ is negative, which "never happens").
uptoTestN :: Int -> Machine -> Machine

uptoTestN test machine = insertList [ (0,                        actionState ResetCounter 1),
                                      (1,                        classesState (Set.fromList [ test ]) 2 (3 + Map.size machine + 0)),
                                      (2,                        actionState IncrementCounter 3),
                                      (3 + Map.size machine - 2, gotoState $ 3 + Map.size machine + 3),
                                      (3 + Map.size machine - 1, classesState (Set.fromList [ test ]) 2 (3 + Map.size machine + 1)),
                                      (3 + Map.size machine + 0, actionState NonPositiveN $ 3 + Map.size machine + 2),
                                      (3 + Map.size machine + 1, actionState PrevChar $ 3 + Map.size machine + 2),
                                      (3 + Map.size machine + 2, failureState),
                                      (3 + Map.size machine + 3, successState) ]
                         $ shiftIndices 3 machine


-- | @uptoN machine@ returns a new @Machine@ that repeats the given @machine@ upto (but not including) a specific number of times.
uptoExcludingN :: Machine -> Machine

uptoExcludingN machine = uptoTestN counterLessThanN machine


-- | @uptoIncludingN machine@ returns a new @Machine@ that repeats the given @machine@ upto and including a specific number of times.
uptoIncludingN :: Machine -> Machine

uptoIncludingN machine = uptoTestN counterLessEqualN machine


-- @loopN machine less equal@ returns a new @Machine@ that repeats /machine/ up to and including a specific number of times.
-- If it reached that number, it then executes the /equal/ machine, otherwise it executes the /less/ machine.
loopN :: Machine -> Machine-> Machine -> Machine

loopN machine less equal = insertList [ (0, actionState ResetCounter 1),
                                        (1, classesState (Set.fromList [ counterLessThanN ]) 2 (3 + Map.size machine + Map.size less)),
                                        (2, actionState IncrementCounter 3),
                                        (3 + Map.size machine - 2, gotoState $ 3 + Map.size machine),
                                        (3 + Map.size machine - 1, gotoState 1),
                                        (3 + Map.size machine + Map.size less - 2, gotoState $ 3 + Map.size machine + Map.size less + Map.size equal - 2),
                                        (3 + Map.size machine + Map.size less - 1, gotoState $ 3 + Map.size machine + Map.size less + Map.size equal - 1) ]
                         $ Map.unions [ shiftIndices 3 machine, shiftIndices (3 + Map.size machine) less, shiftIndices (3 + Map.size machine + Map.size less) equal ]


-- * Optimize.


-- | @optimize machine@ returns a smaller equivalent /machine/.
-- It does not maintain the two final states at the end, but that's OK as this is the last step before emitting the directives.
optimize :: Machine -> Machine

optimize machine = fixpoint (traced "removeUnusedStates" removeUnusedStates
                           . traced "skipUnnecessaryEndTokens" skipUnnecessaryEndTokens
                           . traced "skipUnnecessaryUnparsed" skipUnnecessaryUnparsed
                           . traced "mergeIdenticalStates" mergeIdenticalStates
                           . traced " mergeConsequtiveStates" mergeConsequtiveStates) machine


-- | If one state always leads to another which has no action, then @mergeConsequtiveStates machine@ will merge them.
mergeConsequtiveStates :: Machine -> Machine

mergeConsequtiveStates machine = Map.map merge machine
  where merge state0@(State { sTransitions = [ Transition { tClasses = Nothing, tStateIndex = index1 } ] }) = let Just state1 = Map.lookup index1 machine
                                                                                                              in case (state0|>sAction, state1|>sAction) of
                                                                                                                      (Nothing,         action ) -> state1
                                                                                                                      (action,          Nothing) -> state1 { sAction = action }
                                                                                                                      (_,               _      ) -> state0
        merge state = state


-- | @mergeIdenticalStates machine@ identify identical @State@s, and modify all transitions to point to only one of them.
mergeIdenticalStates :: Machine -> Machine

mergeIdenticalStates machine = let statesMap = Map.foldWithKey collect Map.empty machine
                                   newIndices = Map.mapWithKey (lookup statesMap) machine
                               in Map.fromList $ map (applyToPair newIndices) $ Map.toList machine
  where collect index state statesMap = Map.insertWith max state index statesMap
        lookup statesMap _ state = fromJust $ Map.lookup state statesMap
        applyToPair newIndices (index, state) = (index, applyToState newIndices state)
        applyToState newIndices state = state { sTransitions = map (applyToTransition newIndices) $ state |>sTransitions }
        applyToTransition newIndices transition = transition { tStateIndex = fromJust $ Map.lookup (transition|>tStateIndex) newIndices }


-- | @removeUnusedStates machine@ removes all unused @State@s and reindexes all remaining states so there are no gaps in the indices.
removeUnusedStates :: Machine -> Machine

removeUnusedStates machine = let usedIndices = collectReachable machine Set.empty 0
                                 newIndices = foldl (nextIndex usedIndices) Map.empty [0..((Map.size machine) - 1)]
                             in Map.fromList $ catMaybes $ map (applyToPair usedIndices newIndices) $ Map.toList machine
  where collectReachable machine reached index = if Set.member index reached
                                                    then reached
                                                    else let reached' = Set.insert index reached
                                                             Just state = Map.lookup index machine
                                                          in foldl (collectReachable machine) reached' $ map tStateIndex $ state|>sTransitions
        nextIndex usedIndices newIndices index = if Set.member index usedIndices
                                                    then Map.insert index (Map.size newIndices) newIndices
                                                    else newIndices
        applyToPair usedIndices newIndices (index, state) = if Set.member index usedIndices
                                                               then Just (fromJust $ Map.lookup index newIndices, applyToState newIndices state)
                                                               else Nothing
        applyToState newIndices state = state { sTransitions = map (applyToTransition newIndices) $ state |>sTransitions }
        applyToTransition newIndices transition = transition { tStateIndex = fromJust $ Map.lookup (transition|>tStateIndex) newIndices }


-- * Dynamic Programming


data Location = Entry | Exit
  deriving (Ord, Eq)


-- | @dynamic actionFunction mergeFunction initValue machine@ performs dynamic programming to compute values for each @State@.
-- The first @State@ is entered with the /initValue/. This value is modified by the /actionFunction/, yielding the exit value.
-- When the exit value is changed (or set for the first time), it is used as the entry value for updating all @State@s reached through @Transition@s.
-- Since the same @State@ may be reached through multiple paths, the /mergeFunction/ merges a new entry value with an old one.
-- The process repeats until no @State@s change their exit values.
-- The final result is the entry and exit values for each @State@.
dynamic :: (Eq a) => (a -> Maybe Action -> a) -> (a -> a -> a) -> a -> Machine -> Map.Map (Location, Int) a

dynamic actionFunction mergeFunction initValue machine = dynamicUpdate Map.empty [ (0, initValue) ]
  where dynamicUpdate stateData [] = stateData
        dynamicUpdate stateData ((index, entryValue) : updates) = let newEntryValue = case Map.lookup (Entry, index) stateData of
                                                                                           Just oldEntryValue -> mergeFunction entryValue oldEntryValue
                                                                                           Nothing            -> entryValue
                                                                      Just state = Map.lookup index machine
                                                                      newExitValue = actionFunction newEntryValue $ state|>sAction
                                                                      stateData' = insertList [ ((Entry, index), newEntryValue), ((Exit, index), newExitValue) ] stateData
                                                                  in if Just newExitValue == Map.lookup (Exit, index) stateData
                                                                        then dynamicUpdate stateData' updates
                                                                        else dynamicUpdate stateData' $ updates ++ map (updateTransition newExitValue) (state|>sTransitions)
        updateTransition value transition = (transition|>tStateIndex, value)


-- | @unendedTokens machine@ computes the set of unended tokens for each state of the /machine/.
unendedTokens :: Machine -> Map.Map (Location, Int) (Set.Set String)

unendedTokens machine = dynamic actionFunction mergeFunction Set.empty machine
  where actionFunction tokens (Just (EmptyToken name))
          | "Begin" `isPrefixOf` name = Set.insert (drop 5 name) tokens
          | "End" `isPrefixOf` name = Set.delete (drop 3 name) tokens
        actionFunction tokens _ = tokens
        mergeFunction newTokens oldTokens = Set.union newTokens oldTokens


-- | @skipUnnecessaryEndTokens machine@ skips all the unnecessary @EndToken@ states in /machine/.
skipUnnecessaryEndTokens :: Machine -> Machine

skipUnnecessaryEndTokens machine = let stateTokens = unendedTokens machine
                                   in Map.mapWithKey (fixState stateTokens) machine
  where fixState stateTokens index state = state { sTransitions = map (fixTransition $ Map.findWithDefault Set.empty (Exit, index) stateTokens) $ state|>sTransitions }
        fixTransition tokens transition = let Just state = Map.lookup (transition|>tStateIndex) machine
                                          in case state of
                                                  State { sAction = Just (EmptyToken name), sTransitions = [ Transition { tStateIndex = index, tClasses = Nothing } ] } ->
                                                    if "End" `isPrefixOf` name && not (Set.member (drop 3 name) tokens)
                                                       then transition { tStateIndex = index }
                                                       else transition
                                                  State { sAction = Just (EmptyToken _) } -> error $ "fixTransition: " ++ show state
                                                  _ -> transition


-- | @unterminatedTokens machine@ computes for each state of the /machine/ the depth of the unterminated tokens stack when entering the state.
unterminatedTokens :: Machine -> Map.Map (Location, Int) Int

unterminatedTokens machine = dynamic actionFunction mergeFunction 0 machine
  where actionFunction depth (Just (BeginToken _)) = depth + 1
        actionFunction depth (Just (EndToken _)) = depth - 1
        actionFunction depth _ = depth
        mergeFunction newDepth oldDepth = max newDepth oldDepth


-- | @skipUnnecessaryUnparsed machine@ skips all the unnecessary @Unparsed@ states in /machine/.
skipUnnecessaryUnparsed :: Machine -> Machine

skipUnnecessaryUnparsed machine = let stateDepth = unterminatedTokens machine
                                  in Map.mapWithKey (fixState stateDepth) machine
  where fixState stateDepth index state = case Map.lookup (Exit, index) stateDepth of
                                               Nothing -> state
                                               Just 0  -> state { sTransitions = map fixTransition $ state|>sTransitions }
                                               _       -> state
        fixTransition transition = let Just state = Map.lookup (transition|>tStateIndex) machine
                                   in case state of
                                           State { sAction = Just (EndToken "Unparsed"), sTransitions = [ Transition { tStateIndex = index, tClasses = Nothing } ] } -> transition { tStateIndex = index }
                                           State { sAction = Just (EndToken _) } -> error $ "fixTransition: " ++ show state
                                           _ -> transition


-- * Directives.


-- | @directives nameds@ converts the /nameds/ machines to directives.
directives :: [ Named ] -> String

directives nameds = concatMap namedDirectives nameds


-- | @namedDirectives named@ converts a /named/ machine to directives.
namedDirectives :: Named -> String

namedDirectives named = "BEGIN_MACHINE(" ++ (named|>nIndex) ++ ", " ++ spoilName (named|>nName) ++ ", `" ++ (named|>nName) ++ "')\n"
                     ++ listDirectives "PARAMETERS" (map parameterDirective $ named|>nParameters)
                     ++ "BEGIN_STATES(" ++ (show $ Map.size $ named|>nMachine) ++ ")\n"
                     ++ (concatMap stateDirectives $ Map.toAscList $ named|>nMachine)
                     ++ "END_STATES(" ++ (show $ Map.size $ named|>nMachine) ++ ")\n"
                     ++ "END_MACHINE(" ++ (named|>nIndex) ++ ", " ++ spoilName (named|>nName) ++ ",  `" ++ (named|>nName) ++ "')\n"


-- | @parameterDirective name@ converts the parameter /name/ to a directive.
parameterDirective :: String -> String

parameterDirective name = "PARAMETER(" ++ name ++ ")\n"


-- | @spoilName name@ converts the \"-\" and \"+\" characters in a production name into \"_\" and \"__\" to make them valid identifiers.
spoilName name   = name''
  where name'  = subRegex (mkRegex "\\+") name "__"
        name'' = subRegex (mkRegex "[ -]") name' "_"


-- | @stateDirectives (index, state)@ converts a /state/ with an /index/ to a list of directives.
stateDirectives :: (Int, State) -> String

stateDirectives (index, state) = "BEGIN_STATE(" ++ show index ++ ")\n"
                              ++ (actionDirective $ state|>sAction)
                              ++ "BEGIN_TRANSITIONS(" ++ (show $ length $ state|>sTransitions) ++ ")\n"
                              ++ (concatMap transitionDirectives $ zip [0..] $ state|>sTransitions)
                              ++ "END_TRANSITIONS(" ++ (show $ length $ state|>sTransitions) ++ ")\n"
                              ++ "END_STATE(" ++ show index ++ ")\n"


-- | @actionDirective action@ converts an /action/ to a directive.
actionDirective :: Maybe Action -> String

actionDirective Nothing = "NO_ACTION\n"

actionDirective (Just NextChar) = "NEXT_CHAR\n"

actionDirective (Just PrevChar) = "PREV_CHAR\n"

actionDirective (Just NextLine) = "NEXT_LINE\n"

actionDirective (Just (BeginToken name)) = "BEGIN_TOKEN(" ++ name ++ ")\n"

actionDirective (Just (EndToken name)) = "END_TOKEN(" ++ name ++ ")\n"

actionDirective (Just (EmptyToken name)) = "EMPTY_TOKEN(" ++ name ++ ")\n"

actionDirective (Just (BeginChoice name)) = "BEGIN_CHOICE(" ++ name ++ ")\n"

actionDirective (Just (EndChoice name)) = "END_CHOICE(" ++ name ++ ")\n"

actionDirective (Just (Commit name)) = "COMMIT(" ++ name ++ ")\n"

actionDirective (Just PushState) = "PUSH_STATE\n"

actionDirective (Just SetState) = "SET_STATE\n"

actionDirective (Just ResetState) = "RESET_STATE\n"

actionDirective (Just PopState) = "POP_STATE\n"

actionDirective (Just ResetCounter) = "RESET_COUNTER\n"

actionDirective (Just IncrementCounter) = "INCREMENT_COUNTER\n"

actionDirective (Just NonPositiveN) = "NON_POSITIVE_N\n"

actionDirective (Just BadContext) = "BAD_CONTEXT\n"

actionDirective (Just Success) = "SUCCESS\n"

actionDirective (Just Failure) = "FAILURE\n"


-- | @transitionDirectives (index, transition)@ converts a /transition/ with an /index/ to a list of directives.
transitionDirectives :: (Int, Transition) -> String

transitionDirectives (index, transition) = "BEGIN_TRANSITION(" ++ show index ++ ", " ++ show state_index ++ ")\n"
                                        ++ fake_directives
                                        ++ real_directives
                                        ++ "END_TRANSITION(" ++ show index ++ ", " ++ show state_index ++ ")\n"
  where state_index = transition|>tStateIndex
        fake_classes = Set.intersection (fromMaybe Set.empty $ transition|>tClasses) $ Set.fromList specialClasses
        real_classes = Set.difference (fromMaybe Set.empty $ transition|>tClasses) $ Set.fromList specialClasses
        fake_directives = listDirectives "SPECIAL" $ map fakeDirectives $ sort $ Set.toList fake_classes
        real_directives = listDirectives "CLASSES" $ map classDirectives $ sort $ Set.toList real_classes


-- | @classDirectives index@ converts a class with an /index/ to a directive.
classDirectives :: Int -> String

classDirectives index = "CLASS(" ++ show (index - length CharSet.fakeCodes) ++ ")\n"


-- | @fakeDirectives index@ converts a fake class with an /index/ to a directive.
fakeDirectives :: Int -> String

fakeDirectives index
  | index == length CharSet.fakeCodes + CharSet.startOfLine = "START_OF_LINE\n"
  | index == counterLessThanN  = "COUNTER_LESS_THAN_N\n"
  | index == counterLessEqualN = "COUNTER_LESS_EQUAL_N\n"
  | otherwise = error ("FD: " ++ show index)


-- | @listDirectives name directives@ emits @NO_@/name/ if the list of /directives/ is empty,
-- or the concatenation of @BEGIN_@/name/, the /directives/, and @END_@/name/ otherwise.
listDirectives :: String -> [ String ] -> String

listDirectives name directives = if directives == []
                                    then "NO_" ++ name ++ "\n"
                                    else "BEGIN_" ++ name ++ "\n"
                                       ++ concat directives
                                       ++ "END_" ++ name ++ "\n"
