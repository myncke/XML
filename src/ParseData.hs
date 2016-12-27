module ParseData where


--------------------------------------------------------------------------------
-- STATEMENTS
--------------------------------------------------------------------------------

data Stmt = Seq [Stmt]
          -- | Assign Identifier AExpr
          -- | If BExpr Stmt Stmt
          -- | While BExpr Stmt
          -- | MBot MCommand
          -- | Skip
            deriving (Show)


------------------------------------------------------
-- Literals
------------------------------------------------------
-- Boolean
literal_TRUE            = '\128077'    -- 👍
literal_FALSE           = '\128078'    -- 👎
literal_NOT             = '\128683'    -- 🚫
literal_AND             = '\128591'    -- 🙏
literal_OR              = '\128080'    -- 👐
literal_EQUALS          = '\128145'    -- 💑
literal_LT              = '\128200'    -- 📈
literal_GT              = '\128201'    -- 📉
-- Numbers
literal_PLUS            = '+'          -- +
literal_MIN             = '-'          -- -
literal_MUL             = '*'          -- *
literal_DIV             = '/'          -- /
literal_MOD             = '%'          -- %
-- MBot
literal_MBOT            = '\128663'    -- 🚗
literal_LIGHT           = '\128161'    -- 💡
literal_LIGHT_OUT       = '\128374'    -- 🕶
literal_GO              = '\128168'    -- 💨
literal_ULTRASONIC      = '\128207'    -- 📏
literal_FOLLOWER        = '\128301'    -- 🔭
literal_UP              = '↑'          -- ⬆️
literal_RIGHT           = '→'          -- ➡️
literal_DOWN            = '↓'          -- ⬇️
literal_LEFT            = '←'          -- ⬅️
literal_STOP            = '\9995'      -- ✋
literal_BOTH_BLACK      = '\127761'    -- 🌑
literal_BOTH_WHITE      = '\127773'    -- 🌝
literal_WHITE_BLACK     = '\127767'    -- 🌗
literal_BLACK_WHITE     = '\127763'    -- 🌓
-- Statement Keywords
literal_ASSIGN          = '\8596'      -- ↔
literal_COMMENT         = '\128173'    -- 💭
literal_IF              = '\10067'     -- ❓
literal_ELSE            = '\10071'     -- ❗
literal_WHILE           = '\128259'    -- 🔃
literal_END             = '\128307'    -- 🔳
literal_LPARENS         = '('          -- (
literal_RPARENS         = ')'          -- )

------------------------------------------------------
--                     STRUCTURE                    --
------------------------------------------------------
type Identifier = String

-- BOOLEAN EXPRESSIONS
data BExpr = BConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           | LEquals LExpr LExpr
            deriving (Show)
data BBinOp = And | Or deriving (Show)
data RBinOp = Greater | Less deriving (Show)

-- ARITHMETIC EXPRESSIONS
data AExpr = Var Identifier
           | IntConst Int
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           | MBUltrasonic
             deriving (Show)
data ABinOp = Add | Sub | Mul | Div | Mod deriving (Show)

-- LINE EXPRESSIONS
data LExpr = LineConst Line | LineFollower deriving (Show)

-- MBOT COMMANDS
type Light = Int
data MCommand = SetLight Light Int Int Int
              | Go Direction
              | Stop
                deriving (Show)
data Direction = ForwardD | BackwardD | LeftD | RightD deriving (Show)

-- STATEMENTS
data Stmt = Seq [Stmt]
          | Assign Identifier AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | MBot MCommand
          | Skip
            deriving (Show)
