module AST where

data Op
  = Add
  | Sub
  | Mult
  | Div
  | Power
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | BitAnd
  | BitOr
  deriving (Show, Eq)

data Uop
  = Neg
  | Not
  deriving (Show, Eq)

data Struct =
  Struct
    { structName :: String
    , structFields :: [Bind]
    }
  deriving (Show, Eq)

data Type
  = Pointer Type
  | TyInt
  | TyBool
  | TyFloat
  | TyChar
  | TyVoid
  | TyStruct String
  deriving (Show, Eq)

data Bind =
  Bind
    { bindType :: Type
    , bindName :: String
    }
  deriving (Show, Eq)

data Expr
  = Literal Int
  | StrLit String
  | CharLit Int
  | Fliteral Double
  | BoolLit Bool
  | Null
  | Id String
  | Binop Op Expr Expr
  | Unop Uop Expr
  | Call String [Expr]
  | Cast Type Expr
  | Access Expr Expr
  | Deref Expr
  | Addr Expr
  | Assign Expr Expr
  | Sizeof Type
  | Noexpr
  deriving (Show, Eq)

data Statement
  = Expr Expr
  | Block [Statement]
  | Return Expr
  | If Expr Statement Statement
  | For Expr Expr Expr Statement
  | While Expr Statement
  deriving (Show, Eq)

data Function =
  Function
    { typ :: Type
    , name :: String
    , formals :: [Bind]
    , locals :: [Bind]
    , body :: [Statement]
    }
  deriving (Show, Eq)

data Program =
  Program [Struct] [Bind] [Function]
  deriving (Eq, Show)
