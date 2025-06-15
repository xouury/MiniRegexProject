-- Converts an NFA into a deterministic finite automaton (DFA)
-- using the subset construction algorithm. The DFA is then used
-- to efficiently match input strings.

module DFA (DFA(..), dfaFromNFA, acceptsDFA) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe()
import NFA
import RegexTree()

data DFA = DFA
  { dfaStart :: Set State
  , dfaAccepts :: Set (Set State)
  , dfatransMap :: Map (Set State, Char) (Set State)
  } deriving (Show)

epsilonClosure :: NFA -> Set State -> Set State
epsilonClosure nfa states = go states Set.empty
  where
    go toVisit visited
      | Set.null toVisit = visited
      | otherwise =
          let (s, rest) = Set.deleteFindMin toVisit
              visited' = Set.insert s visited
              epsilons = Set.fromList (Map.findWithDefault [] (s, Nothing) (transitions nfa))
              newStates = Set.difference epsilons visited'
          in go (Set.union rest newStates) visited'

move :: NFA -> Set State -> Char -> Set State
move nfa states c = Set.fromList $
  concat [Map.findWithDefault [] (s, Just c) (transitions nfa) | s <- Set.toList states]

dfaFromNFA :: NFA -> DFA
dfaFromNFA nfa = build initial Set.empty Map.empty
  where
    initial = epsilonClosure nfa (Set.singleton (start nfa))
    allChars = Set.fromList [c | ((_, Just c), _) <- Map.toList (transitions nfa)]

    build :: Set State -> Set (Set State) -> Map (Set State, Char) (Set State) -> DFA
    build startState visited transMap =
        let (visited', transMap') = go [startState] visited transMap
            acceptingStates = Set.filter (\s -> accept nfa `Set.member` s) visited'
        in DFA startState acceptingStates transMap'

    go :: [Set State] -> Set (Set State) -> Map (Set State, Char) (Set State)
       -> (Set (Set State), Map (Set State, Char) (Set State))
    go [] visited transMap = (visited, transMap)
    go (curr:queue) visited transMap
      | curr `Set.member` visited = go queue visited transMap
      | otherwise =
          let visited' = Set.insert curr visited
              (newtransMap, newStates) = unzip
                [ let target = epsilonClosure nfa (move nfa curr c)
                  in ((curr, c, target), target)
                | c <- Set.toList allChars ]
              transMap' = foldr (\(from, c, to) acc -> Map.insert (from, c) to acc) transMap newtransMap
              queue' = queue ++ filter (`Set.notMember` visited') newStates
          in go queue' visited' transMap'
          
acceptsDFA :: DFA -> String -> Bool
acceptsDFA dfa = go (dfaStart dfa) 
  where
    go state [] = state `Set.member` dfaAccepts dfa
    go state (c:cs) =
      case Map.lookup (state, c) (dfatransMap dfa) of
        Just next -> go next cs
        Nothing -> False
