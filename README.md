# MiniRegexProject

## Overview
My project is a simple Haskell application that implements a regular expression matcher using finite automata.
The project has several parts. Firstly, it takes a regular expression on input and parses it into an abstract syntax tree. The next step is to build a nondeterministic automaton (NFA) using Thompson's construction. Then it converts NFA into deterministic finite automata (DFA) via subset construction. The last step is to perform efficient string matching using the DFA.

Each part is implemented in its own file. 

## Supported features 
- Literals: a, b, c;
- Concatenation: a.b;
- Alternation: a|b;
- Kleene star: a*;
- Grouping with parentheses: (a|b)c*.

## How to use 
My project can be used in two different ways.

A first way is directly importing the project into any Haskell program:

```
import RegexMatcher
main :: IO ()
main = do
    let result = matchRegex "a*b" "aaab"
    print result
```

You can call matchRegex with any supported regex string and input string.

The project also includes a simple command-line interface that allows you to interactively enter regex and input strings. Example:

> stack run
Enter regex:
a*b
Enter input string:
aaab
Result: True

## Installation

You need to have **Stack** or **Cabal** installed on your machine (it is recommended to install **Stack**).

# Building the project

1. Clone or download the project:

<git clone https://github.com/xouury/MiniRegexProject.git>

<cd MiniRegexProject>

2. Build using **stack build**.

3. Run the interactive program using **stack run**.

## Limitations 

This project implements a basic subset of regex features for demonstration purposes.
Extensions like +, ?, character classes [abc], and escaped characters \ are not supported in the current version.
