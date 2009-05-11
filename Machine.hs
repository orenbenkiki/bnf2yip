module Machine (
    Named(..),
    Machine,
    empty,
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
    unexpected,
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
            | BeginChoice String
            | Commit String
            | DoneToken
            | EmptyToken String
            | EndChoice String
            | EndToken String
            | EndUnparsed Int
            | Failure
            | IncrementCounter
            | NextChar
            | NextLine
            | PopState
            | PrevChar
            | PushState
            | ResetCounter
            | ResetState
            | SetState
            | Success
            | Unexpected
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


-- | Special classes.
specialClasses :: [ Int ]

specialClasses = [ counterLessThanN, counterLessEqualN ]


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


-- | @tokenActionMachine action@ creates a @Machine@ that performs a single /action/ that may cause a token to be returned.
-- The @DoneToken@ is just a no-op, ensuring that the @State@ always unconditionally leads to a single following @State@.
-- This following @State@ is where the @Machine@ continues from after the caller consumes the token.
tokenActionMachine :: Action -> Machine

tokenActionMachine action = Machine.sequential [ actionMachine action, actionMachine DoneToken ]


-- | @nextChar@ creates a @Machine@ that consumes the next character.
nextChar :: Machine

nextChar = actionMachine NextChar


-- | @nextLine@ creates a @Machine@ that increments the line counter (and whatever else needs to be updated when starting a new line).
nextLine :: Machine

nextLine = actionMachine NextLine


-- | @beginToken name@ creates a @Machine@ that begins collecting characters for a token with the given /name/.
beginToken :: String -> Machine

beginToken name = tokenActionMachine $ BeginToken name


-- | @endToken name@ creates a @Machine@ that ends collecting characters for a token with the given /name/.
endToken :: String -> Machine

endToken name = tokenActionMachine $ EndToken name


-- | @emptyToken name@ creates a @Machine@ that emits an empty token with the given /name/.
emptyToken :: String -> Machine

emptyToken name = tokenActionMachine $ EmptyToken name


-- | @beginChoice name@ creates a @Machine@ that marks the beginning of a named choice point with the given /name/.
beginChoice :: String -> Machine

beginChoice name = actionMachine $ BeginChoice name


-- | @endChoice name@ creates a @Machine@ that marks the end of a named choice point with the given /name/.
endChoice :: String -> Machine

endChoice name = tokenActionMachine $ EndChoice name


-- | @commit name@ creates a @Machine@ that commits to the choice point of the given /name/.
commit :: String -> Machine

commit name = tokenActionMachine $ Commit name


-- | @unexpected@ creates a @Machine@ that emits an "unexpected character" error token.
unexpected :: Machine

unexpected = tokenActionMachine $ Unexpected


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
                                  (1 + Map.size machine - 2, gotoState $ 1 + Map.size machine + 2),
                                  (1 + Map.size machine - 1, gotoState $ 1 + Map.size machine + 0),
                                  (1 + Map.size machine + 0, actionState SetState $ 1 + Map.size machine + 1),
                                  (1 + Map.size machine + 1, actionState DoneToken 1),
                                  (1 + Map.size machine + 2, actionState ResetState $ 1 + Map.size machine + 3),
                                  (1 + Map.size machine + 3, actionState PopState $ 1 + Map.size machine + 5),
                                  (1 + Map.size machine + 4, failureState),
                                  (1 + Map.size machine + 5, successState) ]
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


-- | @repeatN classes@ returns a new @Machine@ that consumes the given /classes/ exactly the number of times specified in the /n/ variable.
-- If /n/ is negative ("never happens") it is taken to mean zero.
repeatN :: Set.Set Int -> Machine

repeatN classes = Map.fromList [ (0, actionState ResetCounter 1),
                                 (1, classesState (Set.fromList [ counterLessThanN ]) 2 6),
                                 (2, actionState IncrementCounter 3),
                                 (3, classesState classes 4 5),
                                 (4, actionState NextChar 1),
                                 (5, failureState),
                                 (6, successState) ]


-- | @uptoN classes test@ returns a new @Machine@ that consumes the given /classes/ as long as some /test/ (special class) is not satisfied.
-- If the following character belongs to the /classes/, the @Machine@ fails.
uptoTestN :: Set.Set Int -> Int -> Machine

uptoTestN classes test = Map.fromList [ (0, actionState ResetCounter 1),
                                        (1, actionState IncrementCounter 2),
                                        (2, classesState (Set.fromList [ test ]) 3 5),
                                        (3, classesState classes 4 7),
                                        (4, actionState NextChar 1),
                                        (5, classesState classes 6 7),
                                        (6, failureState),
                                        (7, successState) ]

-- | @uptoN classes@ returns a new @Machine@ that consumes /classes/ upto (but not including) a specific number of times.
-- If the following character belongs to the /classes/, the @Machine@ fails.
uptoExcludingN :: Set.Set Int -> Machine

uptoExcludingN classes = uptoTestN classes counterLessThanN


-- | @uptoIncludingN classes@ returns a new @Machine@ that consumes /classes/ upto and not including a specific number of times.
-- If the following character belongs to the /classes/, the @Machine@ fails.
uptoIncludingN :: Set.Set Int -> Machine

uptoIncludingN classes = uptoTestN classes counterLessEqualN


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

optimize machine = traced "removeUnusedStates" removeUnusedStates
                 $ traced "loopDone" loopDone
                 $ fixpoint (traced "removeUnusedStates" removeUnusedStates
                           {- Not necessary since the machine is kept clean (with blood, sweat and tears).
                           . traced "skipUnnecessaryEndTokens" skipUnnecessaryEndTokens
                           . traced "skipUnnecessaryUnparsed" skipUnnecessaryUnparsed
                           -}
                           . traced "mergeIdenticalStates" mergeIdenticalStates
                           . traced "mergeConsequtiveStates" mergeConsequtiveStates
                           . traced "mergeSuccessFailure" mergeSuccessFailure) machine


-- | @loopDone machine@ converts @EmptyToken "Done"@ to an endless "Done" token stream in the /machine/.
-- This makes the end states unreachable.
loopDone :: Machine -> Machine

loopDone machine = Map.mapWithKey loop machine
  where loop index state@(State { sAction = Just (EmptyToken "Done") }) = state { sTransitions = [ gotoTransition index ] }
        loop _ state = state

-- | @mergeSuccessFailure machine@ converts all @Failure@ states in the /machine/ to @Success@.
-- This is only done on the final optimized machine which is assumed to include the end-of-production error handling logic.
mergeSuccessFailure :: Machine -> Machine

mergeSuccessFailure machine = Map.map merge machine
  where merge state@(State { sAction = Just Failure }) = state { sAction = Just Success }
        merge state = state


-- | If one state always leads to another which has no action, then @mergeConsequtiveStates machine@ will merge them.
-- A no-op will be preserved if it is the only way to ensure a single unconditional following @State@ after a token-generating action.
mergeConsequtiveStates :: Machine -> Machine

mergeConsequtiveStates machine = Map.map merge machine
  where merge state0@(State { sTransitions = [ Transition { tClasses = Nothing, tStateIndex = index1 } ] }) = let Just state1 = Map.lookup index1 machine
                                                                                                              in case (state0|>sAction, state1|>sAction) of
                                                                                                                      (Nothing,              action ) -> state1
                                                                                                                      (Just (BeginToken _),  _      ) -> state0
                                                                                                                      (Just (Commit _),      _      ) -> state0
                                                                                                                      (Just DoneToken,       action ) -> state1
                                                                                                                      (Just (EmptyToken _),  _      ) -> state0
                                                                                                                      (Just (EndChoice _),   _      ) -> state0
                                                                                                                      (Just (EndToken _),    _      ) -> state0
                                                                                                                      (Just (EndUnparsed _), _      ) -> state0
                                                                                                                      (Just PopState,        _      ) -> state0
                                                                                                                      (Just Unexpected,      _      ) -> state0
                                                                                                                      (action,               Nothing) -> state1 { sAction = action }
                                                                                                                      (_,                    _      ) -> state0
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
  deriving (Ord, Eq, Show)


-- | @dynamic actionFunction mergeFunction initValue machine@ performs dynamic programming to compute values for each @State@.
-- The first @State@ is entered with the /initValue/. This value is modified by the /actionFunction/, yielding the exit value.
-- When the exit value is changed (or set for the first time), it is used as the entry value for updating all @State@s reached through @Transition@s.
-- Since the same @State@ may be reached through multiple paths, the /mergeFunction/ merges a new entry value with an old one.
-- The process repeats until no @State@s change their exit values.
-- The final result is the entry and exit values for each @State@.
dynamic :: (Show a, Eq a) => (Int -> a -> Maybe Action -> a) -> (Int -> a -> a -> a) -> a -> Machine -> Map.Map (Int, Location) a

dynamic actionFunction mergeFunction initValue machine = dynamicUpdate Map.empty [ (0, initValue) ]
  where dynamicUpdate stateData [] = stateData
        dynamicUpdate stateData ((index, entryValue) : updates) = let newEntryValue = case Map.lookup (index, Entry) stateData of
                                                                                           Just oldEntryValue -> trace ("Data: " ++ show stateData) $ mergeFunction index entryValue oldEntryValue
                                                                                           Nothing            -> entryValue
                                                                      Just state = Map.lookup index machine
                                                                      newExitValue = actionFunction index newEntryValue $ state|>sAction
                                                                      stateData' = insertList [ ((index, Entry), newEntryValue), ((index, Exit), newExitValue) ] stateData
                                                                  in if Just newExitValue == Map.lookup (index, Exit) stateData
                                                                        then dynamicUpdate stateData' updates
                                                                        else dynamicUpdate stateData' $ updates ++ map (updateTransition newExitValue) (state|>sTransitions)
        updateTransition value transition = (transition|>tStateIndex, value)

{-

-- | @unendedTokens machine@ computes the set of unended tokens for each state of the /machine/.
unendedTokens :: Machine -> Map.Map (Int, Location) (Set.Set String)

unendedTokens machine = dynamic actionFunction mergeFunction Set.empty machine
  where mergeFunction _ newTokens oldTokens = Set.union newTokens oldTokens
        actionFunction _ tokens (Just (EmptyToken name))
          | "Begin" `isPrefixOf` name = Set.insert (drop 5 name) tokens
          | "End" `isPrefixOf` name = Set.delete (drop 3 name) tokens
        actionFunction _ tokens _ = tokens


-- | @skipUnnecessaryEndTokens machine@ skips all the unnecessary @EndToken@ states in /machine/.
skipUnnecessaryEndTokens :: Machine -> Machine

skipUnnecessaryEndTokens machine = let stateTokens = unendedTokens machine
                                   in Map.mapWithKey (fixState stateTokens) machine
  where fixState stateTokens index state = state { sTransitions = map (fixTransition $ Map.findWithDefault Set.empty (index, Exit) stateTokens) $ state|>sTransitions }
        fixTransition tokens transition = let Just state = Map.lookup (transition|>tStateIndex) machine
                                          in case state of
                                                  State { sAction = Just (EmptyToken name), sTransitions = [ Transition { tStateIndex = index, tClasses = Nothing } ] } ->
                                                    if "End" `isPrefixOf` name && not (Set.member (drop 3 name) tokens)
                                                       then transition { tStateIndex = index }
                                                       else transition
                                                  _ -> transition


-- | @unterminatedTokens machine@ computes for each state of the /machine/ the depth of the unterminated tokens stack when entering the state.
-- An "Unparsed" token is assumed to be automatically started by the implementation of the state machine.
unterminatedTokens :: Machine -> Map.Map (Int, Location) Int

unterminatedTokens machine = dynamic actionFunction mergeFunction 1 machine
  where mergeFunction _ newDepth oldDepth = max newDepth oldDepth
        actionFunction _ depth (Just (BeginToken _)) = depth + 1
        actionFunction _ depth (Just (EndToken _)) = depth - 1
        actionFunction _ depth (Just (EndUnparsed _)) = depth - 1
        actionFunction _ depth _ = depth


-- | @skipUnnecessaryUnparsed machine@ skips all the unnecessary @Unparsed@ states in /machine/.
skipUnnecessaryUnparsed :: Machine -> Machine

skipUnnecessaryUnparsed machine = let stateDepth = unterminatedTokens machine
                                  in foldr (fixState stateDepth) machine $ Map.toList machine
  where fixState stateDepth (index, state) machine = case Map.lookup (index, Exit) stateDepth of
                                                          Nothing    -> machine
                                                          Just depth | depth <= 0 -> Map.insert index (state { sTransitions = map skip $ state|>sTransitions }) machine
                                                          Just depth | depth > 0  -> let (machine', transition') = mapAccumR (split depth) machine $ state|>sTransitions
                                                                                     in Map.insert index (state { sTransitions = transition' }) machine'

        skip transition = let Just state = Map.lookup (transition|>tStateIndex) machine
                              in case state of
                                      State { sAction = Just (EndToken "Unparsed"), sTransitions = [ Transition { tStateIndex = index, tClasses = Nothing } ] } -> transition { tStateIndex = index }
                                      State { sAction = Just (EndUnparsed _),       sTransitions = [ Transition { tStateIndex = index, tClasses = Nothing } ] } -> transition { tStateIndex = index }
                                      _ -> transition
        split depth machine transition = let Just state = Map.lookup (transition|>tStateIndex) machine
                                         in case state of
                                                 State { sAction = Just (EndUnparsed unparsed), sTransitions = [ Transition { tStateIndex = index, tClasses = Nothing } ] } | unparsed /= depth ->
                                                   newState index
                                                 State { sAction = Just (EndToken "Unparsed"), sTransitions = [ Transition { tStateIndex = index, tClasses = Nothing } ] } ->
                                                   newState index
                                                 _ -> (machine, transition)
                                         where newState index = (Map.insert (Map.size machine) (actionState (EndUnparsed depth) index) machine, transition { tStateIndex = Map.size machine })
-}

-- * Clustering.


-- | The machine starts with an implicit "Unparsed" token represented by the -1 fake state index cluster.
startCluster = Set.singleton (-1)

-- | For clarity, wrapping up the implicit "Unparsed" token is done in a separate cluster with the fake index state -2.
finishCluster = Set.singleton (-2)


-- | @clusterName set@ converts the /set/ of state indices to a cluster name.
clusterName :: Set.Set Int -> String

clusterName set = foldr1 combine $ map show $ Set.toAscList set
  where combine left right = left ++ "+" ++ right

-- | @stateClusters machine@ computes for each @State@ in the /machine/ the nested clusters it belongs to.
stateClusters :: Machine -> Map.Map (Int, Location) [ Set.Set Int ]

stateClusters machine = dynamic actionFunction mergeFunction [ startCluster ] machine
  where mergeFunction _ newClusters oldClusters | tail newClusters == tail oldClusters = Set.union (head oldClusters) (head newClusters) : tail oldClusters
        mergeFunction index newClusters oldClusters = error $ "Index: " ++ show index ++ "\nNew: " ++ show newClusters ++ "\nOld: " ++ show oldClusters
        actionFunction index clusters (Just (BeginToken _)) = Set.singleton index : clusters
        actionFunction index [ cluster ] (Just (EndToken _)) | cluster == startCluster = [ finishCluster, startCluster ]
        actionFunction index (head : clusters) (Just (EndToken _)) = clusters
        actionFunction index [ cluster ] (Just (EndUnparsed _)) | cluster == startCluster = [ finishCluster, startCluster ]
        actionFunction index (head : clusters) (Just (EndUnparsed _)) = clusters
        actionFunction index clusters (Just PushState) = Set.singleton index : clusters
        actionFunction index [ cluster ] (Just PopState) | cluster == startCluster = [ finishCluster, startCluster ]
        actionFunction index (head : clusters) (Just PopState) = clusters
        actionFunction index clusters (Just (EmptyToken name)) | "Begin" `isPrefixOf` name = Set.singleton index : clusters
        actionFunction index (head : clusters) (Just (EmptyToken name)) | "End" `isPrefixOf` name = clusters
        actionFunction _ clusters _ = clusters


-- | Represent the overall structure of the state machine as a nested @Tree@ of clusters.
data Tree = Tree String [ Tree ]
  deriving (Show)


-- | @clusterTree sets@ converts the cluster /sets/ of all the @State@s of a @Machine@ into a @Tree@.
clusterTree :: [ [ Set.Set Int ] ] -> Tree

clusterTree stacks = foldr1 mergeTrees $ map (stackTree .reverse) stacks
  where stackTree [ leaf ] = Tree (clusterName leaf) []
        stackTree (head : tail) = Tree (clusterName head) [ stackTree tail ]
        mergeTrees (Tree parent1 nested1) (Tree parent2 nested2) | parent1 == parent2 = Tree parent1 $ mergeNested nested1 nested2
        mergeNested [] nested = nested
        mergeNested nested [] = nested
        mergeNested nested1@(head1@(Tree set1 _) : tail1) nested2@(head2@(Tree set2 _) : tail2)
          | set1 == set2 = mergeTrees head1 head2 : mergeNested tail1 tail2
          | set1 < set2 = head1 : mergeNested tail1 nested2
          | set1 > set2 = head2 : mergeNested nested1 tail2


-- | @innerStates clusters@ converts the /clusters/ of each @State@ into a mapping from cluster name to the list of inner nodes belonging to it.
innerStates :: Map.Map (Int, Location) [ Set.Set Int ] -> Map.Map String [ Int ]

innerStates clusters = Map.fromListWith (++) $ mapMaybe cluster [0 .. (Map.size clusters `div` 2) - 1]
  where cluster index = let entries = fromJust $ Map.lookup (index, Entry) clusters
                            exits = fromJust $ Map.lookup (index, Exit) clusters
                        in if entries == exits
                              then Just (clusterName $ head entries, [ index ])
                              else Nothing


-- | @entryStates clusters@ converts the /clusters/ of each @State@ into a mapping from cluster name to the list of entry nodes belonging to it.
entryStates :: Map.Map (Int, Location) [ Set.Set Int ] -> Map.Map String [ Int ]

entryStates clusters = Map.fromListWith (++) $ mapMaybe cluster [0 .. (Map.size clusters `div` 2) - 1]
  where cluster index = let entries = fromJust $ Map.lookup (index, Entry) clusters
                            exits = fromJust $ Map.lookup (index, Exit) clusters
                        in if entries /= exits && entries `isSuffixOf` exits
                              then Just (clusterName $ head exits, [ index ])
                              else Nothing


-- | @exitStates clusters@ converts the /clusters/ of each @State@ into a mapping from cluster name to the list of exit nodes belonging to it.
exitStates :: Map.Map (Int, Location) [ Set.Set Int ] -> Map.Map String [ Int ]

exitStates clusters = Map.fromListWith (++) $ mapMaybe cluster [0 .. (Map.size clusters `div` 2) - 1]
  where cluster index = let entries = fromJust $ Map.lookup (index, Entry) clusters
                            exits = fromJust $ Map.lookup (index, Exit) clusters
                        in if entries /= exits && exits `isSuffixOf` entries
                              then Just (clusterName $ head entries, [ index ])
                              else Nothing


-- | @clusterDirectives machine entry inner exit tree@ returns cluster directives for the /machine/,
-- given the /entry/, /inner/ and /exit/ nodes for each cluster mentioned in the /tree/.
-- Cluster directives are used solely to structure the graphical presentation of the /machine/.
clusterDirectives :: Machine -> Map.Map String [ Int ] -> Map.Map String [ Int ] -> Map.Map String [ Int ] -> Tree -> String

clusterDirectives machine entry inner exit (Tree set nested) = "BEGIN_CLUSTER(" ++ set ++ ")\n"
                                                              ++ (case Map.lookup set entry of
                                                                       Nothing -> "NO_CLUSTER_ENTRY\n"
                                                                       Just states -> "BEGIN_CLUSTER_ENTRY\n"
                                                                                   ++ (foldr (++) "" $ map stateDirective states)
                                                                                   ++ "END_CLUSTER_ENTRY\n")
                                                              ++ (case Map.lookup set inner of
                                                                       Nothing -> "NO_CLUSTER_INNER\n"
                                                                       Just states -> "BEGIN_CLUSTER_INNER\n"
                                                                                   ++ (foldr (++) "" $ map stateDirective states)
                                                                                   ++ "END_CLUSTER_INNER\n")
                                                              ++ (case Map.lookup set exit of
                                                                       Nothing -> "NO_CLUSTER_EXIT\n"
                                                                       Just states -> "BEGIN_CLUSTER_EXIT\n"
                                                                                   ++ (foldr (++) "" $ map stateDirective states)
                                                                                   ++ "END_CLUSTER_EXIT\n")
                                                              ++ foldr (++) "" (map (clusterDirectives machine entry inner exit) nested)
                                                              ++ "END_CLUSTER(" ++ set ++ ")\n"
                                                                 where stateDirective index = let state = fromJust $ Map.lookup index machine
                                                                                                  nodeDirective = "CLUSTER_NODE(" ++ show index ++ ")\n"
                                                                                                  transitionDirectives = mapMaybe (transitionDirective index) $ zip [0..] $ state|>sTransitions
                                                                                              in foldr (++) nodeDirective transitionDirectives
                                                                       transitionDirective stateIndex (_, Transition { tClasses = Nothing }) = Nothing
                                                                       transitionDirective stateIndex (index, _) = Just $ "CLUSTER_TRANSITION(" ++ show stateIndex ++ ", " ++ show index ++ ")\n"


-- * Directives.


-- | @directives nameds@ converts the /nameds/ machines to directives.
directives :: [ Named ] -> String

directives nameds = concatMap namedDirectives nameds


-- | @namedDirectives named@ converts a /named/ machine to directives.
namedDirectives :: Named -> String

namedDirectives named = "BEGIN_MACHINE(" ++ (named|>nIndex) ++ ", " ++ spoilName (named|>nName) ++ ", `" ++ (named|>nName) ++ "')\n"
                     ++ listDirectives "PARAMETERS" (map parameterDirective $ named|>nParameters)
                     ++ (trace ("Clusters: " ++ show clusters) "")
                     ++ (trace ("Tree: " ++ show tree) "")
                     ++ (trace ("Inner: " ++ show inner) "")
                     ++ (trace ("Entry: " ++ show entry) "")
                     ++ "BEGIN_CLUSTERS\n"
                     ++ clusterDirectives (named|>nMachine) entry inner exit tree
                     ++ "END_CLUSTERS\n"
                     ++ "BEGIN_STATES(" ++ (show $ Map.size $ named|>nMachine) ++ ")\n"
                     ++ (concatMap stateDirectives $ Map.toAscList $ named|>nMachine)
                     ++ "END_STATES(" ++ (show $ Map.size $ named|>nMachine) ++ ")\n"
                     ++ "END_MACHINE(" ++ (named|>nIndex) ++ ", " ++ spoilName (named|>nName) ++ ",  `" ++ (named|>nName) ++ "')\n"
                        where clusters = stateClusters $ named|>nMachine
                              tree = clusterTree $ Map.elems clusters
                              inner = innerStates clusters
                              entry = entryStates clusters
                              exit = exitStates clusters


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
                              ++ (actionDirective state $ state|>sAction)
                              ++ "BEGIN_TRANSITIONS(" ++ (show $ length $ state|>sTransitions) ++ ")\n"
                              ++ (concatMap transitionDirectives $ zip [0..] $ state|>sTransitions)
                              ++ "END_TRANSITIONS(" ++ (show $ length $ state|>sTransitions) ++ ")\n"
                              ++ "END_STATE(" ++ show index ++ ")\n"


-- | @actionDirective action@ converts an /action/ to a directive.
actionDirective :: State -> Maybe Action -> String

actionDirective _ Nothing = "NO_ACTION\n"

actionDirective _ (Just (BeginChoice name)) = "BEGIN_CHOICE(" ++ name ++ ")\n"

actionDirective state (Just (BeginToken name)) = "BEGIN_TOKEN(" ++ name ++ ", " ++ doneTokenIndex state ++ ")\n"

actionDirective state (Just (Commit name)) = "COMMIT(" ++ name ++ ", " ++ doneTokenIndex state ++ ")\n"

actionDirective _ (Just DoneToken) = "NO_ACTION\n"

actionDirective state (Just (EmptyToken name)) = "EMPTY_TOKEN(" ++ name ++ ", " ++ doneTokenIndex state ++ ")\n"

actionDirective state (Just (EndChoice name)) = "END_CHOICE(" ++ name ++ ", " ++ doneTokenIndex state ++ ")\n"

actionDirective state (Just (EndToken name)) = "END_TOKEN(" ++ name ++ ", " ++ doneTokenIndex state ++ ")\n"

actionDirective state (Just (EndUnparsed depth)) = "END_TOKEN(Unparsed, " ++ doneTokenIndex state ++ ")\n"

actionDirective _ (Just IncrementCounter) = "INCREMENT_COUNTER\n"

actionDirective _ (Just NextChar) = "NEXT_CHAR\n"

actionDirective _ (Just NextLine) = "NEXT_LINE\n"

actionDirective state (Just PopState) = "POP_STATE(" ++ doneTokenIndex state ++ ")\n"

actionDirective _ (Just PrevChar) = "PREV_CHAR\n"

actionDirective _ (Just PushState) = "PUSH_STATE\n"

actionDirective _ (Just ResetCounter) = "RESET_COUNTER\n"

actionDirective _ (Just ResetState) = "RESET_STATE\n"

actionDirective state (Just SetState) = "SET_STATE(" ++ doneTokenIndex state ++ ")\n"

actionDirective state (Just Unexpected) = "UNEXPECTED(" ++ doneTokenIndex state ++ ")\n"

actionDirective state action = error $ "actionDirective: " ++ show action


-- | @doneTokenIndex state@ returns the index of the @State@ that immediatly follows the current /state/.
-- This allows the caller to continue from that following @State@ after consuming the token generated in this /state/.
doneTokenIndex :: State -> String

doneTokenIndex (State { sTransitions = [ Transition { tClasses = Nothing, tStateIndex = index } ] }) = show index


-- | @transitionDirectives (index, transition)@ converts a /transition/ with an /index/ to a list of directives.
transitionDirectives :: (Int, Transition) -> String

transitionDirectives (index, transition) = "BEGIN_TRANSITION(" ++ show index ++ ", " ++ show (transition|>tStateIndex) ++ ")\n"
                                        ++ fake_directives
                                        ++ real_directives
                                        ++ "END_TRANSITION(" ++ show index ++ ", " ++ show (transition|>tStateIndex) ++ ")\n"
  where state_index = transition|>tStateIndex
        fake_classes = Set.intersection (fromMaybe Set.empty $ transition|>tClasses) $ Set.fromList specialClasses
        real_classes = Set.difference (fromMaybe Set.empty $ transition|>tClasses) $ Set.fromList specialClasses
        fake_directives = listDirectives "SPECIAL" $ map fakeDirectives $ sort $ Set.toList fake_classes
        real_directives = listDirectives "CLASSES" $ map classDirectives $ sort $ Set.toList real_classes


-- | @classDirectives index@ converts a class with an /index/ to a directive.
classDirectives :: Int -> String

classDirectives index = "CLASS(" ++ show index ++ ")\n"


-- | @fakeDirectives index@ converts a fake class with an /index/ to a directive.
fakeDirectives :: Int -> String

fakeDirectives index
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
