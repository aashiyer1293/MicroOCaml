**MicroCaml Interpreter**


MicroCaml is a dynamically-typed language interpreter implemented in OCaml. This project showcases a simplified version of OCaml, known as MicroCaml, which retains a subset of its features. The interpreter includes core components such as a lexer, parser, evaluator, and type checker, enabling the execution and testing of dynamically-typed scripts.



**Features**

MicroCaml Language: A simplified, dynamically-typed version of OCaml.
Lexer and Parser: Converts source code into tokens and builds a syntax tree.
Evaluator: Executes the syntax tree according to the semantics of MicroCaml.
Type Checker: Ensures that scripts adhere to dynamic type rules.
Mutop (Microtop): An interactive interpreter for testing and running MicroCaml scripts.

**Installation**

To get started with MicroCaml, clone this repository and build the project:

git clone https://github.com/aashiyer1293/MicroOCaml.git

cd MicroOCaml


**Project Structure**
lexer.ml: Lexical analysis for MicroCaml.
parser.ml: Syntax parsing and AST generation.
eval.ml: Evaluation logic for interpreting MicroCaml scripts.
typechecker.ml: Ensures that the dynamically-typed code adheres to expected rules.
mutop.ml: Interactive interpreter for MicroCaml.
