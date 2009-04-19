module Syntax (
    Syntax,
    Production,
    Node(..),
    mapSyntax,
    mapNode,
    listSyntax,
    listNode
) where

import           CharSet
import           Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

-- A syntax is a list of BNF @Production@ nodes.
type Syntax = Map.Map String Production

-- A production has parameters and a pattern.
type Production = ([ Node ], Node)

-- A node in the syntax tree.
data Node = And [ Node ]
          | AndNot Node Node
          | AsInteger
          | Assign [ Node ] Node
          | BadContext
          | BeginToken String
          | Be String Node
          | Call String [ Node ]
          | Case Node [ (Node, Node) ]
          | Chars CharSet
          | Choice String Node
          | Classes (Set.Set Int)
          | Commit String
          | Empty
          | EmptyToken String
          | EndOfFile
          | EndToken String
          | Forbidding Node Node
          | LimitedTo Node Node
          | LoopN Node Node Node Node
          | Max Node Node
          | Minus Node Node
          | NextChar
          | NextLine
          | NonEmpty Node
          | OneOrMore Node
          | Optional Node
          | Or [ Node ]
          | Peek Node
          | Plus Node Node
          | Prev Node
          | Reject Node
          | RepeatN Node Node
          | Results [ Node ]
          | Select [ (Maybe (Set.Set Int), Node) ]
          | StartOfLine
          | Symbol String
          | Token String Node
          | UptoN Node Node
          | Value Int
          | Variable String
          | ZeroOrMore Node
  deriving (Show, Eq);

mapSyntax :: (Node -> Node) -> Syntax -> Syntax

mapSyntax function syntax = Map.map mapProduction syntax
  where mapProduction (parameters, pattern) = (map (mapNode function) parameters, mapNode function pattern)

mapNode :: (Node -> Node) -> Node -> Node

mapNode function (And patterns) = function $ And $ map (mapNode function) patterns

mapNode function (AndNot base subtract) = function $ AndNot (mapNode function base) (mapNode function subtract)

mapNode function (Assign variables call) = function $ Assign (map (mapNode function) variables) (mapNode function call)

mapNode function (Call name arguments) = function $ Call name $ map (mapNode function) arguments
  
mapNode function (Case value alternatives) = function $ Case (mapNode function value) (map mapAlternative alternatives)
  where mapAlternative (value, result) = (mapNode function value, mapNode function result)

mapNode function (Choice name pattern) = function $ Choice name $ mapNode function pattern

mapNode function (Forbidding forbidden pattern) = function $ Forbidding (mapNode function forbidden) (mapNode function pattern)

mapNode function (LimitedTo limit pattern) = function $ LimitedTo (mapNode function limit) (mapNode function pattern)

mapNode function (LoopN times pattern less equal) = function $ LoopN (mapNode function times) (mapNode function pattern) (mapNode function less) (mapNode function equal)

mapNode function (Max left right) = function $ Max (mapNode function left) (mapNode function right)

mapNode function (Minus left right) = function $ Minus (mapNode function left) (mapNode function right)

mapNode function (NonEmpty pattern) = function $ NonEmpty $ mapNode function pattern

mapNode function (OneOrMore pattern) = function $ OneOrMore $ mapNode function pattern

mapNode function (Optional pattern) = function $ Optional $ mapNode function pattern

mapNode function (Or patterns) = function $ Or $ map (mapNode function) patterns

mapNode function (Select alternatives) = function $ Select $ map (mapAlternative function) alternatives
  where mapAlternative function (classes, pattern) = (classes, mapNode function pattern)

mapNode function (Peek pattern) = function $ Peek $ mapNode function pattern

mapNode function (Plus left right) = function $ Plus (mapNode function left) (mapNode function right)

mapNode function (Prev pattern) = function $ Prev $ mapNode function pattern

mapNode function (Reject pattern) = function $ Reject $ mapNode function pattern

mapNode function (RepeatN times pattern) = function $ RepeatN (mapNode function times) (mapNode function pattern)

mapNode function (Results results) = function $ Results $ map (mapNode function) results

mapNode function (Token name pattern) = function $ Token name $ mapNode function pattern

mapNode function (UptoN times pattern) = function $ UptoN (mapNode function times) (mapNode function pattern)

mapNode function (ZeroOrMore pattern) = function $ ZeroOrMore $ mapNode function pattern

mapNode function node = function node


listSyntax :: Syntax -> [ Node ]

listSyntax syntax = concat $ map listProduction $ Map.elems syntax


listProduction :: ([Node], Node) -> [ Node ]

listProduction (parameters, pattern) = concat $ map listNode $ pattern : parameters


listNode :: Node -> [ Node ]

listNode node@(And nodes) = node : (concat $ map listNode nodes)

listNode node@(AndNot base subtract) = node : (listNode base ++ listNode subtract)

listNode node@(Assign variables pattern) = node : (listNode pattern ++ (concat $ map listNode variables))

listNode node@(Call _ parameters) = node : (concat $ map listNode parameters)

listNode node@(Case value alternatives) = node : (listNode value ++ (concat $ map listAlternative alternatives))
  where listAlternative (value, pattern) = value : pattern : (listNode value ++ listNode pattern)

listNode node@(Choice _ parameter) = node : listNode parameter

listNode node@(Forbidding forbidden pattern) = node : (listNode forbidden ++ listNode pattern)

listNode node@(LimitedTo limit pattern) = node : (listNode limit ++ listNode pattern)

listNode node@(LoopN times pattern less equal) = node : (listNode times ++ listNode pattern ++ listNode less ++ listNode equal)

listNode node@(Max left right) = node : (listNode left ++ listNode right)

listNode node@(Minus left right) = node : (listNode left ++ listNode right)

listNode node@(NonEmpty pattern) = node : listNode pattern

listNode node@(OneOrMore pattern) = node : listNode pattern

listNode node@(Optional pattern) = node : listNode pattern

listNode node@(Or nodes) = node : (concat $ map listNode nodes)

listNode node@(Select alternatives) = node : (concat $ map listAlternative alternatives)
  where listAlternative (classes, pattern) = pattern : listNode pattern

listNode node@(Peek pattern) = node : listNode pattern

listNode node@(Plus left right) = node : (listNode left ++ listNode right)

listNode node@(Prev pattern) = node : listNode pattern

listNode node@(Reject pattern) = node : listNode pattern

listNode node@(RepeatN times pattern) = node : (listNode times ++ listNode pattern)

listNode node@(Results values) = node : (concat $ map listNode values)

listNode node@(Token _ pattern) = node : listNode pattern

listNode node@(UptoN times pattern) = node : (listNode times ++ listNode pattern)

listNode node@(ZeroOrMore pattern) = node : listNode pattern

listNode node = [ node ]
