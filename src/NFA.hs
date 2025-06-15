-- Converts the Regex AST into a nondeterministic finite automaton (NFA)
-- using Thompson's construction algorithm.

module NFA (NFA(..), Transition, State, regexToNFA) where

import qualified Data.Map as Map
import Data.Map (Map)
import RegexTree

type State = Int 
type NFAState a = Int -> (a, Int)
type Transition = Map (State, Maybe Char) [State]

data NFA = NFA
  { start :: State
  , accept :: State
  , transitions :: Transition
  } deriving (Show)

regexToNFA :: Regex -> NFA
regexToNFA regex = fst $ build regex 0
  where
    build :: Regex -> NFAState NFA
    build Empty state = 
        let s = state
        in (NFA s s Map.empty, state + 1)

    build (Literal c) state =
        let s1 = state
            s2 = state + 1
            trans = Map.singleton (s1, Just c) [s2]
        in (NFA s1 s2 trans, state + 2)

    build (Concat r1 r2) state =
        let (nfa1, state1) = build r1 state
            (nfa2, state2) = build r2 state1
            trans = Map.unionWith (++) 
                        (transitions nfa1) 
                        (transitions nfa2)
            trans' = Map.insertWith (++) (accept nfa1, Nothing) [start nfa2] trans
        in (NFA (start nfa1) (accept nfa2) trans', state2)

    build (Alt r1 r2) state =
        let s = state
            (nfa1, state1) = build r1 (state + 1)
            (nfa2, state2) = build r2 state1
            f = state2
            trans = Map.unionsWith (++) [
                        transitions nfa1,
                        transitions nfa2,
                        Map.singleton (s, Nothing) [start nfa1, start nfa2],
                        Map.singleton (accept nfa1, Nothing) [f],
                        Map.singleton (accept nfa2, Nothing) [f]
                    ]
        in (NFA s f trans, state2 + 1)

    build (Star r) state =
        let s = state
            (nfa1, state1) = build r (state + 1)
            f = state1
            trans = Map.unionsWith (++) [
                        transitions nfa1,
                        Map.singleton (s, Nothing) [start nfa1, f],
                        Map.singleton (accept nfa1, Nothing) [start nfa1, f]
                    ]
        in (NFA s f trans, state1 + 1)