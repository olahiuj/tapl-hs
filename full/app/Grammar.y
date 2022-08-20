{
module Grammar
  ( parse
  )
where
import System.IO
import Def
import Lexer
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
  letrec      { TLetRec   }
  ';'         { TSemiColon  }
  '='         { TAssign   }
  '('         { TLParen   }
  ')'         { TRParen   }
  '->'        { TArrow    }
  ':'         { TColon    }

%%

Term  : var                         { VarF $1       }
      | Term Term                   { AppF $1 $2    }
      | func var ':' Type Term      { AbsF $2 $4 $5 }
      | if Term then Term else Term { IteF $4 $6 $2 }
      | let var '=' Term in Term    { LinF $2 $4 $6 }
      | Term ';' Term               { SeqF $1 $3    }
      | prd Term                    { PrdF $2       }
      | suc Term                    { SucF $2       }
      | isz Term                    { IsZF $2       }
      | false                       { FalseF        }
      | true                        { TrueF         }
      | zero                        { ZeroF         }
      | unit                        { UnitF         }
      | '(' Term ')'                { $2            }
      | Term as Type                { AscF $1 $3    }
      | letrec var ':' Type
        '=' Term in Term            { LriF $2 $4 $6 $8 }

Type  : unitType                    { UnitTypeF     }
      | boolType                    { BoolTypeF     }
      | natType                     { NatTypeF      }
      | Type '->' Type              { $1 :=> $3     }
      | '(' Type ')'                { $2            }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> FTerm
parse = fullSTLC . lexer

testFile :: String -> IO FTerm
testFile file = do
  cont <- readFile file
  print cont
  print $ lexer cont
  print $ fullSTLC $ lexer cont
  return $ parse cont
}
