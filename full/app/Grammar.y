{
module Grammar
  ( parse
  )
where
import System.IO
import Lexer
import Term
}

%name fullSTLC
%tokentype { Token }
%error { parseError }
%right '->'

%token
  unitType    { TUnitType }
  boolType    { TBoolType }
  natType     { TNatType  }
  func        { TFunc     }
  else        { TElse     }
  then        { TThen     }
  unit        { TUnit     }
  let         { TLet      }
  suc         { TSuc      }
  prd         { TPrd      }
  isz         { TIsZ      }
  if          { TIf       }
  in          { TIn       }
  as          { TAs       }
  var         { TVar $$   }
  zero        { TZero     }
  true        { TTrue     }
  false       { TFalse    }
  ';'         { TSemiColon  }
  '='         { TAssign   }
  '('         { TLParen   }
  ')'         { TRParen   }
  '->'        { TArrow    }
  ':'         { TColon    }

%%

Term  : var                         { Var' $1       }
      | Term Term                   { App' $1 $2    }
      | func var ':' Type Term      { Abs' $2 $4 $5 }
      | if Term then Term else Term { Ite' $4 $6 $2 }
      | let var '=' Term in Term    { Lin' $2 $4 $6 }
      | Term ';' Term               { Seq' $1 $3    }
      | prd Term                    { Prd' $2       }
      | suc Term                    { Suc' $2       }
      | isz Term                    { IsZ' $2       }
      | false                       { False'        }
      | true                        { True'         }
      | zero                        { Zero'         }
      | unit                        { Unit'         }
      | '(' Term ')'                { $2            }
      | Term as Type                { Asc' $1 $3    }

Type  : unitType                    { UnitType'     }
      | boolType                    { BoolType'     }
      | natType                     { NatType'      }
      | Type '->' Type              { $1 :=> $3     }
      | '(' Type ')'                { $2            }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> Term
parse = compTerm . fullSTLC . lexer

testFile :: String -> IO Term
testFile file = do
  cont <- readFile file
  print cont
  print $ lexer cont
  print $ fullSTLC $ lexer cont
  return $ parse cont
}
