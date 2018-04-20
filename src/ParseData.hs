module ParseData where

import MBot
------------------------------------------------------
-- Literals
------------------------------------------------------
-- Tags
_TAG_START              = "<"
_TAG_END                = "</"
-- Boolean
_TRUE                   = "true"
_FALSE                  = "false"
_BOOL                   = "bool>"
_NOT                    = "not>"
_AND                    = "and>"
_OR                     = "or>"
_EQUALS                 = "equals>"
_LT                     = "lesser>"
_GT                     = "greater>"

-- Numbers
_VALUE                  = "value>"
_FLOAT                  = "float>"
_PLUS                   = "plus>"
_MIN                    = "min>"
_MUL                    = "mul>"
_DIV                    = "div>"
_MOD                    = "mod>"
-- Jef
_JEF                    = "jef>"
_LIGHT                  = "light>"
_GO                     = "go>"
_ULTRASONIC             = "sonic"
_FOLLOW                 = "follow"
_FORWARD                = "forward"
_RIGHT                  = "right"
_BACKWARD               = "backward"
_LEFT                   = "left"
_STOP                   = "<stop/>"
_BOTH_BLACK             = "black"
_BOTH_WHITE             = "white"
-- Statement Keywords
_VAR                    = "var>"
_STRING                 = "string>"
_ID                     = "id>"
_ASSIGN                 = "assign>"
_OPEN_COMMENT           = "<!--"
_CLOSE_COMMENT          = "-->"
_PRINT                  = "print>"
_STATEMENT_END          = "end>"
_IFELSE                 = "ifelse>"
_CASE                   = "case>"
_CONDITION              = "conditon>"
_BLOCK                  = "block>"
_WHILE                  = "while>"

------------------------------------------------------
-- Structure
------------------------------------------------------
type Identifier = String

-- ARITHMETIC EXPRESSIONS
data AExp = Var Identifier
  | ALit Int
  | Add AExp AExp
  | Mul AExp AExp
  | Div AExp AExp
  | Min AExp AExp
  | Mod AExp AExp
  | JefSensor
    deriving (Eq,Show)

-- BOOLEAN EXPRESSIONS
data BOp =  And | Or | Equals | Lesser | Greater
    deriving (Show)

data BExp = BLit Bool
  | Not BExp
  | BBool BOp BExp BExp
  | ABool BOp AExp AExp
  | BLine BOp JefLine JefLine
    deriving (Show)

-- PRINT EXPRESSIONS
data PExp = SeqPrint [PExp]
  | APrint AExp
  | BPrint BExp
  | SPrint String
  deriving (Show)

-- JEF COMMANDS
data Direction = Forward | Backward | Left | Right deriving (Show)
data JefLine = JLine Line | Follow deriving (Show)


data JefCommand = SetLight AExp AExp AExp AExp
  | Go Direction
  | Stop
    deriving (Show)

-- STATEMENTS
data Case = BoolBlock BExp Stmt
    deriving (Show)

data Stmt = Seq [Stmt]
  | Assign Identifier AExp
  | Print PExp
  | If [Case]
  | While Case
  | Jef JefCommand
  | Skip
    deriving (Show)
