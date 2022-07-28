module SimpleLinker where

import Parser
import X86Builder

main :: IO ()
main = do
  let name = "teste.c"
  s <- readFile name
  let Right (_, prog_ast) = runParser parseProgram s
  let mod = codeGenProgram prog_ast
  let (AsmCode contents _) = emitModule mod (AsmCode "" 0)
  writeFile (name ++ ".s") contents
