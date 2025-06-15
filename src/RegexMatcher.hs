-- Provides the interface for users : matchRegex.
-- This function parses a regex, builds NFA, converts to DFA, and performs matching.

module RegexMatcher (matchRegex) where

import RegexTree()
import RegexParser
import NFA
import DFA

matchRegex :: String -> String -> Bool
matchRegex regexStr inputStr =
  case parseRegex regexStr of
    Nothing -> error $ "Invalid regex: " ++ regexStr
    Just ast ->
      let nfa = regexToNFA ast
          dfa = dfaFromNFA nfa
      in acceptsDFA dfa inputStr
