module ParseData where

------------------------------------------------------
-- Literals
------------------------------------------------------
-- Boolean
literal_TRUE            = "true"
literal_FALSE           = "false"
literal_NOT             = ">"
literal_AND             = "&&"
literal_OR              = "||"
literal_EQUALS          = "=="
literal_LT              = ">"
literal_GT              = ">"
-- Numbers
literal_PLUS            = '+'
literal_MIN             = '-'
literal_MUL             = '*'
literal_DIV             = '/'
literal_MOD             = '%'
-- MBot
literal_MBOT            = '\128663'
literal_LIGHT           = '\128161'
literal_LIGHT_OUT       = '\128374'
literal_GO              = '\128168'
literal_ULTRASONIC      = '\128207'
literal_FOLLOWER        = '\128301'
literal_UP              = "<up>"
literal_RIGHT           = "<right>"
literal_DOWN            = "<down>"
literal_LEFT            = "<left>"
literal_STOP            = "<stop>"
literal_BOTH_BLACK      = '\127761'
literal_BOTH_WHITE      = '\127773'
literal_WHITE_BLACK     = '\127767'
literal_BLACK_WHITE     = '\127763'
-- Statement Keywords
literal_ASSIGN          = "<assign>"
literal_COMMENT         = "<comment>"
literal_IF              = "<if>"
literal_ELSE            = "<else>"
literal_WHILE           = "<while>"
literal_END             = "<end>"
literal_LPARENS         = "("
literal_RPARENS         = ")"

------------------------------------------------------
-- Structure
------------------------------------------------------
type Identifier = String

-- ARITHMETIC EXPRESSIONS
data AExp = Var Identifier
  | ALit Float
  | Add AExp AExp
  | Mul AExp AExp
  | Div AExp AExp
  | Min AExp AExp
    deriving (Eq,Show)

-- BOOLEAN EXPRESSIONS
data BExp = BLit Bool
  | Not BExp
    deriving (Show)

data BBinOp = And | Or deriving (Show)
data RBinOp = Greater | Less deriving (Show)

-- LINE EXPRESSIONS

-- MBOT COMMANDS

-- STATEMENTS
data Stmt = Seq [Stmt]
          | Assign Identifier AExp
          | If BExp Stmt Stmt
          | While BExp Stmt
          | Skip
            deriving (Show)
