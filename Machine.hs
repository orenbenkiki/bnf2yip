module Machine (
    Machine,
    startOfLine,
    fakeClasses,
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
    sequential,
    alternatives,
    select,
    repeatN,
    uptoExcludingN,
    uptoIncludingN,
    loopN,
    directives
) where


import           Debug.Trace

import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Text.Regex


infixl 9 |>
-- | @record |> field@ is the same as @field record@,  but is more readable.
(|>) :: record -> (record -> value) -> value
record |> field = field record


-- | A state in the machine.
data State = State {
    sAction      :: Maybe Action,
    sTransitions :: [ Transition ]
  }
  deriving (Eq, Ord, Show)


-- | Fake character class for testing for the start of a line.
startOfLine = -1

-- | Fake character class for testing the loop counter against the @n@
-- variable.
counterLessThanN = -1000


-- | Fake character class for testing the loop counter against the @n@
-- variable.
counterLessEqualN = -2000


-- | Set of all fake classes.
fakeClasses = [ startOfLine ]

specialClasses = counterLessThanN : counterLessEqualN : map (+ length fakeClasses) fakeClasses


-- | Action performed when entering a state.
data Action = BeginToken String
            | EndToken String
            | EmptyToken String
            | BeginChoice String
            | EndChoice String
            | Commit String
            | NextChar
            | PrevChar
            | NextLine
            | PushState
            | ResetState
            | PopState
            | ResetCounter
            | IncrementCounter
            | NonPositiveN
            | BadContext
            | Success
            | Failure
  deriving (Eq, Ord, Show)


-- | Transition between states.
data Transition = Transition {
    tClasses    :: Maybe (Set.Set Int),
    tStateIndex :: Int
  }
  deriving (Eq, Ord, Show)


-- | A state machine. The start state is always at index 0.
type Machine = Map.Map Int State


-- | @successState@ is one of the the two final states of every machine. This always
-- has the index @(mSize - 1)@.
successState = State { sAction = Just Success, sTransitions = [] }


-- | @failureState@ is one of the the two final states of every machine. This
-- always has the index @(mSize - 2)@.
failureState = State { sAction = Just Failure, sTransitions = [] }


-- | @gotoTransition index@ is a @Transition@ that always goes to the given
-- @State@ @index@.
gotoTransition :: Int -> Transition

gotoTransition index = Transition { tClasses = Nothing, tStateIndex = index }


-- | @classesTransition classes index@ is a @Transition@ that always goes to
-- the given @State@ @index@ if the current character is any of the given
-- @classes@.
classesTransition :: Set.Set Int -> Int -> Transition

classesTransition classes index = Transition { tClasses = Just classes, tStateIndex = index }


gotoState :: Int -> State

gotoState index = State { sAction = Nothing, sTransitions = [ gotoTransition index ] }


classesState :: Set.Set Int -> Int -> Int -> State

classesState classes onSuccess onFailure = State { sAction = Nothing, sTransitions = [ classesTransition classes onSuccess, gotoTransition onFailure ] }


actionState :: Action -> Int -> State

actionState action index = State { sAction = Just action, sTransitions = [ gotoTransition index ] }


--- | An @empty@ machine.
empty :: Machine

empty = Map.fromList [ (0, gotoState 2), (1, failureState), (2, successState) ]


--- | Fail in unexpected context parameter.
badContext :: Machine

badContext = Map.fromList [ (0, actionState BadContext 1), (1, failureState), (2, successState) ]


-- | @matchClasses classes@ creates a @Machine@ that tests whether the current
-- character belongs to one of the @classes@.
matchClasses :: Set.Set Int -> Machine

matchClasses classes = Map.fromList [ (0, classesState classes 2 1) , (1, failureState) , (2, successState) ]


-- | @zeroOrMoreClasses classes@ creates a @Machine@ that consumes any number
-- of characters of the specified @classes@.
zeroOrMoreClasses :: Set.Set Int -> Machine

zeroOrMoreClasses classes = Map.fromList [ (0, classesState classes 1 3), (1, actionState NextChar 0), (2, failureState), (3, successState) ]


-- | @actionMachine action@ creates a @Machine@ that performs a single
-- @action@.
actionMachine :: Action -> Machine

actionMachine action = Map.fromList [ (0, actionState action 2), (1, failureState), (2, successState) ]


-- | @nextChar@ creates a @Machine@ that consumes the next character.
nextChar :: Machine

nextChar = actionMachine NextChar


-- | @nextLine@ creates a @Machine@ that moves to the start of the next line.
nextLine :: Machine

nextLine = actionMachine NextLine


-- | @beginToken name@ creates a @Machine@ that begins collecting characters
-- for a token with the given @name@.
beginToken :: String -> Machine

beginToken name = actionMachine $ BeginToken name


-- | @endToken name@ creates a @Machine@ that ends collecting characters for a
-- token with the given @name@.
endToken :: String -> Machine

endToken name = actionMachine $ EndToken name


-- | @emptyToken name@ creates a @Machine@ that emits an empty token with the
-- given @name@.
emptyToken :: String -> Machine

emptyToken name = actionMachine $ EmptyToken name


-- | @beginChoice name@ creates a @Machine@ that marks the beginning of a named
-- choice point with the given @name@.
beginChoice :: String -> Machine

beginChoice name = actionMachine $ BeginChoice name


-- | @endChoice name@ creates a @Machine@ that marks the end of a named choice
-- point with the given @name@.
endChoice :: String -> Machine

endChoice name = actionMachine $ EndChoice name


-- | @commit name@ creates a @Machine@ that commits to the choice point of the
-- given @name@.
commit :: String -> Machine

commit name = actionMachine $ Commit name


-- | @pushState@ creates a @Machine@ that saves the current state fo
-- alternatives can backtrack.
pushState :: Machine

pushState = actionMachine PushState


-- | @popState@ creates a @Machine@ that releases the saved state once
-- backtracking is successful.
popState :: Machine

popState = actionMachine PopState


-- | @resetState@ creates a @Machine@ that resets to the recently saved state.
resetState :: Machine

resetState = actionMachine ResetState


-- | @insertList [(key, value)] map@ inserts all the @(key, value)@ pairs into
-- @map@.
insertList :: Ord k => [ (k, v) ] -> Map.Map k v -> Map.Map k v

insertList list map = Map.union (Map.fromList list) map


-- | @sequential [machine]@ returns a new @Machine@ that executes all the
-- specified @machine@s in order.
sequential :: [ Machine ] -> Machine

sequential machines = foldr1 merge machines
  where merge first second =
          let second' = shiftIndices (Map.size first) second
              first' = insertList [ (Map.size first - 1, gotoState $ Map.size first),
                                    (Map.size first - 2, gotoState $ Map.size first + Map.size second - 2) ] first
          in Map.union first' second'


-- | @shiftIndices offset machine@ will shift all the indices of the @machine@
-- @State@s by the @offset@.
shiftIndices :: Int -> Machine -> Machine

shiftIndices offset machine = Map.fromList $ map applyToPair $ Map.toList machine
  where applyToPair (index, state) = (index + offset , applyToState state)
        applyToState state = state { sTransitions = map applyToTransition $ state |>sTransitions }
        applyToTransition transition = transition { tStateIndex = transition|>tStateIndex + offset }


-- | @alternatives [machine]@ returns a new @Machine@ that tries each
-- alternative @machine@ in turn.
alternatives :: [ Machine ] -> Machine

alternatives [ machine ] = machine

alternatives (first : rest) =
  let first' = alternative pushState first
      rest' = map (alternative resetState) rest
  in sequential $ [ foldr1 merge (first' : rest'), popState ]
  where alternative prefix machine
          | machine == empty = empty
          | otherwise = sequential [ prefix, machine ]
        merge first second =
          let second' = shiftIndices (Map.size first) second
              first' = insertList [ (Map.size first - 2, gotoState $ Map.size first),
                                    (Map.size first - 1, gotoState $ Map.size first + Map.size second - 1) ] first
          in Map.union first' second'
        mapInit _ [] = []
        mapInit _ [ x ] = [ x ]
        mapInit f (x : xs) = f x : mapInit f xs


-- | @select [ machine ]@ returns a new @Machine@ that merges all the given
-- @machine@ list entries, since each of them starts with a test for different
-- character classes.
select :: [ (Maybe (Set.Set Int), Machine) ] -> Machine

select alternatives =
  let (successIndices, failureIndices, transitions, machine) = foldl addAlternative ([], [], [], Map.empty) alternatives
      transitions' = if (last transitions)|>tClasses == Nothing
                        then transitions
                        else transitions ++ [ gotoTransition $ 1 + Map.size machine - 2 ]
      states = (0, State { sAction = Nothing, sTransitions = transitions' })
             : ( catMaybes $ map (finalState $ 1 + Map.size machine - 1) successIndices
                          ++ map (finalState $ 1 + Map.size machine - 2) failureIndices )
  in insertList states machine
  where addAlternative (successIndices, failureIndices, transitions, combinedMachine) (classes, machine) =
          let machine' = shiftIndices (1 + Map.size combinedMachine) machine
              combinedMachine' = Map.union combinedMachine machine'
              transitions' = transitions ++ [ Transition { tClasses = classes, tStateIndex = 1 + Map.size combinedMachine } ]
              successIndices' = (1 + Map.size combinedMachine' - 1) : successIndices
              failureIndices' = (1 + Map.size combinedMachine' - 2) : failureIndices
          in (successIndices', failureIndices', transitions', combinedMachine')
        finalState finalIndex stateIndex
          | finalIndex == stateIndex = Nothing
          | otherwise                = Just (stateIndex, gotoState finalIndex)


-- | @repeatN machine@ returns a new @Machine@ that repeats the given
-- @machine@ a specific number of times.
repeatN :: Machine -> Machine

repeatN machine = insertList [ (0, actionState ResetCounter 1),
                               (1, classesState (Set.fromList [ counterLessThanN ]) 2 (3 + Map.size machine + 1)),
                               (2, actionState IncrementCounter 3),
                               (3 + Map.size machine - 2, gotoState $ 3 + Map.size machine + 0),
                               (3 + Map.size machine - 1, gotoState 1),
                               (3 + Map.size machine + 0, failureState),
                               (3 + Map.size machine + 1, successState) ]
                $ shiftIndices 3 machine


-- | @uptoN machine@ returns a new @Machine@ that repeats the given @machine@
-- as long as some @test@ is not satisfied.
uptoTestN :: Int -> Machine -> Machine

uptoTestN test machine = insertList [ (0, actionState ResetCounter 1),
                                      (1, classesState (Set.fromList [ test ]) 2 (3 + Map.size machine + 0)),
                                      (2, actionState IncrementCounter 3),
                                      (3 + Map.size machine - 2, gotoState $ 3 + Map.size machine + 3),
                                      (3 + Map.size machine - 1, classesState (Set.fromList [ test ]) 2 (3 + Map.size machine + 1)),
                                      (3 + Map.size machine + 0, actionState NonPositiveN $ 3 + Map.size machine + 2),
                                      (3 + Map.size machine + 1, actionState PrevChar $ 3 + Map.size machine + 2),
                                      (3 + Map.size machine + 2, failureState),
                                      (3 + Map.size machine + 3, successState) ]
                         $ shiftIndices 3 machine

-- | @uptoN machine@ returns a new @Machine@ that repeats the given @machine@
-- upto (but not including) a specific number of times.
uptoExcludingN :: Machine -> Machine

uptoExcludingN machine = uptoTestN counterLessThanN machine


-- | @uptoIncludingN machine@ returns a new @Machine@ that repeats the given @machine@
-- upto and including a specific number of times.
uptoIncludingN :: Machine -> Machine

uptoIncludingN machine = uptoTestN counterLessEqualN machine


-- @loopN machine less equal@ returns a new @Machine@
loopN :: Machine -> Machine-> Machine -> Machine

loopN machine less equal = insertList [ (0, actionState ResetCounter 1),
                                        (1, classesState (Set.fromList [ counterLessThanN ]) 2 (3 + Map.size machine + Map.size less)),
                                        (2, actionState IncrementCounter 3),
                                        (3 + Map.size machine - 2, gotoState $ 3 + Map.size machine),
                                        (3 + Map.size machine - 1, gotoState 1),
                                        (3 + Map.size machine + Map.size less - 2, gotoState $ 3 + Map.size machine + Map.size less + Map.size equal - 2),
                                        (3 + Map.size machine + Map.size less - 1, gotoState $ 3 + Map.size machine + Map.size less + Map.size equal - 1) ]
                         $ Map.unions [ shiftIndices 3 machine, shiftIndices (3 + Map.size machine) less, shiftIndices (3 + Map.size machine + Map.size less) equal ]


-- | @optimize machine@ returns a smaller equivalent machine. It does not
-- maintain the two final states at the end, but that's OK as this is the last
-- step before emitting the machine as directives.
optimize :: Machine -> Machine

optimize machine = fixpoint (removeUnusedStates . mergeIdenticalStates . mergeConsequtiveStates) machine
  where fixpoint function value =
          let value' = function value
          in if value' == value
                then value
                else fixpoint function value'

-- | If one state always leads to another which has no action, then
-- @mergeConsequtiveStates machine@ will merge them.
mergeConsequtiveStates :: Machine -> Machine

mergeConsequtiveStates machine =
  Map.map merge machine
  where merge state0@(State { sTransitions = [ Transition { tClasses = Nothing, tStateIndex = index1 } ] }) =
          let state1 = fromJust $ Map.lookup index1 machine
          in case (state0|>sAction, state1|>sAction) of
                  (Nothing,         action ) -> state1
                  (action,          Nothing) -> state1 { sAction = action }
                  (_,               _      ) -> state0
        merge state = state

-- | @mergeIdenticalStates machine@ identify identical @State@s, and modify all
-- transitions to point to only one of them.
mergeIdenticalStates :: Machine -> Machine

mergeIdenticalStates machine =
  let statesMap = Map.foldWithKey collect Map.empty machine
      newIndices = Map.mapWithKey (lookup statesMap) machine
  in Map.fromList $ map (applyToPair newIndices) $ Map.toList machine
  where collect index state statesMap = Map.insertWith max state index statesMap
        lookup statesMap _ state = fromJust $ Map.lookup state statesMap
        applyToPair newIndices (index, state) = (index, applyToState newIndices state)
        applyToState newIndices state = state { sTransitions = map (applyToTransition newIndices) $ state |>sTransitions }
        applyToTransition newIndices transition = transition { tStateIndex = fromJust $ Map.lookup (transition|>tStateIndex) newIndices }


-- | @removeUnusedStates machine@ removes all unused @State@s and reindexes all
-- remaining states so there are no gaps in the indices.
removeUnusedStates :: Machine -> Machine

removeUnusedStates machine =
  let usedIndices = collectReachable machine Set.empty 0
      newIndices = foldl (nextIndex usedIndices) Map.empty [0..((Map.size machine) - 1)]
  in Map.fromList $ catMaybes $ map (applyToPair usedIndices newIndices) $ Map.toList machine
  where collectReachable machine reached index =
          if Set.member index reached
             then reached
             else let reached' = Set.insert index reached
                      state = fromJust $ Map.lookup index machine
                  in foldl (collectReachable machine) reached' $ map tStateIndex $ state|>sTransitions
        nextIndex usedIndices newIndices index =
          if Set.member index usedIndices
             then Map.insert index (Map.size newIndices) newIndices
             else newIndices
        applyToPair usedIndices newIndices (index, state) =
          if Set.member index usedIndices
             then Just (fromJust $ Map.lookup index newIndices, applyToState newIndices state)
             else Nothing
        applyToState newIndices state = state { sTransitions = map (applyToTransition newIndices) $ state |>sTransitions }
        applyToTransition newIndices transition = transition { tStateIndex = fromJust $ Map.lookup (transition|>tStateIndex) newIndices }


-- | @directives (name, machine)@ converts a @machine@ with a @name@ to
-- a list of YIP directives.
directives :: (String, String, [ String ], Machine) -> String

directives (index, name, parameters, machine) =
  "BEGIN_MACHINE(" ++ index ++ ", " ++ spoilName name ++ ", `" ++ name ++ "')\n" ++
  listDirectives "PARAMETERS" (map parameterDirective parameters) ++
  "BEGIN_STATES(" ++ (show $ Map.size machine') ++ ")\n" ++
  (concatMap stateDirectives $ Map.toAscList machine') ++
  "END_STATES(" ++ (show $ Map.size machine') ++ ")\n" ++
  "END_MACHINE(" ++ index ++ ", " ++ spoilName name ++ ",  `" ++ name ++ "')\n"
  where machine'' = optimize machine
        machine' = trace ("O: " ++ show machine'') machine''

-- | @parameterDirective name@ converts the parameter @name@ to a YIP
-- directive.
parameterDirective :: String -> String

parameterDirective name = "PARAMETER(" ++ name ++ ")\n"

-- @spoilName name@ converts the \"-\" and \"+\" characters in a production
-- name into \"_\" and \"__\" to make them valid identifiers.
spoilName name   = name''
  where name'  = subRegex (mkRegex "\\+") name "__"
        name'' = subRegex (mkRegex "[ -]") name' "_"


-- | @stateDirectives (index, state)@ converts a @state@ with an @index@ to a
-- list of YIP directives.
stateDirectives :: (Int, State) -> String

stateDirectives (index, state) =
  "BEGIN_STATE(" ++ show index ++ ")\n" ++
  (actionDirective $ state|>sAction) ++
  "BEGIN_TRANSITIONS(" ++ (show $ length $ state|>sTransitions) ++ ")\n" ++
  (concatMap transitionDirectives $ zip [0..] $ state|>sTransitions) ++
  "END_TRANSITIONS(" ++ (show $ length $ state|>sTransitions) ++ ")\n" ++
  "END_STATE(" ++ show index ++ ")\n"


-- | @actionDirective action@ converts an @action@ to a YIP directive.
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

actionDirective (Just ResetState) = "RESET_STATE\n"

actionDirective (Just PopState) = "POP_STATE\n"

actionDirective (Just ResetCounter) = "RESET_COUNTER\n"

actionDirective (Just IncrementCounter) = "INCREMENT_COUNTER\n"

actionDirective (Just NonPositiveN) = "NON_POSITIVE_N\n"

actionDirective (Just BadContext) = "BAD_CONTEXT\n"

actionDirective (Just Success) = "SUCCESS\n"

actionDirective (Just Failure) = "FAILURE\n"


-- | @transitionDirectives (index, transition)@ converts a @transition@ with an
-- @index@ to a list of YIP directives.
transitionDirectives :: (Int, Transition) -> String

transitionDirectives (index, transition) =
  "BEGIN_TRANSITION(" ++ show index
              ++ ", " ++ show state_index ++ ")\n" ++
  fake_directives ++
  real_directives ++
  "END_TRANSITION(" ++ show index
            ++ ", " ++ show state_index ++ ")\n"
  where state_index = transition|>tStateIndex
        fake_classes = Set.intersection (fromMaybe Set.empty $ transition|>tClasses) $ Set.fromList specialClasses
        real_classes = Set.difference (fromMaybe Set.empty $ transition|>tClasses) $ Set.fromList specialClasses
        fake_directives = listDirectives "SPECIAL" $ map fakeDirectives $ sort $ Set.toList fake_classes
        real_directives = listDirectives "CLASSES" $ map classDirectives $ sort $ Set.toList real_classes


-- | @classDirectives index@ converts a class with an @index@ to a YIP
-- directive.
classDirectives :: Int -> String

classDirectives index = "CLASS(" ++ show (index - length fakeClasses) ++ ")\n"


-- | @fakeDirectives index@ converts a fake class with an @index@ to a YIP
-- directive.
fakeDirectives :: Int -> String

fakeDirectives index
  | index == length fakeClasses + startOfLine = "START_OF_LINE\n"
  | index == counterLessThanN  = "COUNTER_LESS_THAN_N\n"
  | index == counterLessEqualN = "COUNTER_LESS_EQUAL_N\n"
  | otherwise = error ("FD: " ++ show index)


-- | @listDirectives none before after directives@ emits @none@ if the list of
-- @directives@ is empty, or the concatenation of @before@, the @directives@,
-- and @after@ if it is not.
listDirectives :: String -> [ String ] -> String

listDirectives name directives =
  if directives == []
     then "NO_" ++ name ++ "\n"
     else "BEGIN_" ++ name ++ "\n" ++ concat directives ++ "END_" ++ name ++ "\n"
