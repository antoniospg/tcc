module X86Builder where

import AST
import Data.Char
import Data.Int

----------ASM AST-----------
data Register
  = RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | RSP
  | RBP
  deriving (Show)

data Size
  = I32 Int
  | I64 Int64
  deriving (Show)

data Instructions
  = Ret
  | Movq Size Register
  | Push Register
  | Pop Register
  | Addq Register Register
  | Compq Size Register
  deriving (Show)

data Label =
  Label
    { nameLabel :: String
    , instrs :: [Instructions]
    }
  deriving (Show)

data Module =
  Module
    { labels :: [Label]
    , headers :: [String]
    }
  deriving (Show)

data AsmCode =
  AsmCode
    { code :: String
    , indent :: Int
    }
  deriving (Show)

----------------CONSTANTES-------------
baseAsmCode :: AsmCode
baseAsmCode = (AsmCode "" 0)

baseHeaders :: [String]
baseHeaders = [".globl main"]

-------------CODEGEN-----------------
codeGenProgram :: Program -> Module
codeGenProgram (Program _ _ funcs) =
  Module (map codeGenFunction funcs) baseHeaders

codeGenFunction :: Function -> Label
codeGenFunction (Function _ name _ _ body) =
  Label name $ concat (map codeGenStatement body)

codeGenStatement :: Statement -> [Instructions]
codeGenStatement (Block stats) = concat $ map codeGenStatement stats
codeGenStatement (Return expr) =
  let instr1 = codeGenExpr expr
      instr2 = [Ret]
   in instr1 ++ instr2
codeGenStatement (If epxr stat1 stat2) =
  let sub_expr1 = codeGenExpr expr
      sub_expr2 = [Compl (I32 0) EAX]
   in sub_expr1 ++ sub_expr2

codeGenExpr :: Expr -> [Instructions]
codeGenExpr (Literal val) = [Movq (I32 val) RAX]
codeGenExpr (Binop op expr1 expr2) =
  let sub_expr1 = codeGenExpr expr1
      sub_expr2 = codeGenExpr expr2
      stores_expr = sub_expr1 ++ [(Push RAX)] ++ sub_expr2 ++ [(Pop RCX)]
      op_expr =
        case op of
          Add -> [Addq RCX RAX]
   in stores_expr ++ op_expr

---------------EMITIR ASM---------------
--------------Headers-------
emitHeaders :: [String] -> AsmCode -> AsmCode
emitHeaders headers (AsmCode code indent) =
  let header_code =
        code ++ concat (map (genIndent indent ++) $ map (++ "\n") headers)
   in (AsmCode header_code indent)

emitInstr :: Instructions -> AsmCode -> AsmCode
--------Return-------
emitInstr (Ret) (AsmCode code indent) =
  let instr_code = code ++ (genIndent indent) ++ "ret" ++ "\n"
   in (AsmCode instr_code indent)
-------Movq--------
emitInstr (Movq (I32 val) reg) (AsmCode code indent) =
  let instr_code =
        code ++
        (genIndent indent) ++
        "movq " ++
        "$" ++ (show val) ++ ", %" ++ (map toLower (show reg)) ++ "\n"
   in (AsmCode instr_code indent)
-----Push------
emitInstr (Push reg) (AsmCode code indent) =
  let instr_code =
        code ++
        (genIndent indent) ++ "push " ++ "%" ++ (map toLower (show reg)) ++ "\n"
   in (AsmCode instr_code indent)
------Pop-----
emitInstr (Pop reg) (AsmCode code indent) =
  let instr_code =
        code ++
        (genIndent indent) ++ "pop " ++ "%" ++ (map toLower (show reg)) ++ "\n"
   in (AsmCode instr_code indent)
-------Add-------
emitInstr (Addq src dst) (AsmCode code indent) =
  let instr_code =
        code ++
        (genIndent indent) ++
        "addq " ++
        "%" ++
        (map toLower (show src)) ++ ", %" ++ (map toLower (show dst)) ++ "\n"
   in (AsmCode instr_code indent)

---------Aux-----
emitInstrs :: [Instructions] -> AsmCode -> AsmCode
emitInstrs (i:is) asm = emitInstrs is (emitInstr i asm)
emitInstrs [] asm = asm

emitLabel :: Label -> AsmCode -> AsmCode
emitLabel label (AsmCode code indent) =
  let label_name_code =
        code ++ (genIndent indent) ++ (nameLabel label) ++ ":" ++ "\n"
      (AsmCode instrs_code _) =
        emitInstrs (instrs label) (AsmCode label_name_code (indent + 1))
   in (AsmCode instrs_code indent)

emitLabels :: [Label] -> AsmCode -> AsmCode
emitLabels (l:ls) asm = emitLabels ls (emitLabel l asm)
emitLabels [] asm = asm

emitModule :: Module -> AsmCode -> AsmCode
emitModule mod asm =
  let asm_header = emitHeaders (headers mod) asm
      asm_labels = emitLabels (labels mod) asm_header
   in asm_labels

------------------Aux FUNCS----------------
genIndent :: Int -> String
genIndent num = concat . take num $ repeat "\t"
