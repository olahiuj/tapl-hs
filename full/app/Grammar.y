{
{-# LANGUAGE ScopedTypeVariables #-}
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
  refType     { TRefType  }
  else        { TElse     }
  then        { TThen     }
  unit        { TUnit     }
  fun         { TFun      }
  let         { TLet      }
  suc         { TSuc      }
  prd         { TPrd      }
  isz         { TIsZ      }
  ref         { TRef      }
  if          { TIf       }
  in          { TIn       }
  as          { TAs       }
  id          { TId $$    }
  zero        { TZero     }
  true        { TTrue     }
  false       { TFalse    }
  rec         { TRec      }
  ';'         { TSemiColon  }
  '='         { TBind     }
  '('         { TLParen   }
  ')'         { TRParen   }
  '{'         { TLBrack   }
  '}'         { TRBrack   }
  '->'        { TArrow    }
  ':='        { TAssign   }
  ':'         { TColon    }
  ','         { TComma    }
  '.'         { TDot      }
  '!'         { TExc      }
  int         { TInt $$   }

%%

Term  : id                          { VarF $1       }
      | Term Term                   { AppF $1 $2    }
      | fun id ':' Type Term        { AbsF $2 $4 $5 }
      | if Term then Term else Term { IteF $4 $6 $2 }
      | let id '=' Term in Term     { LinF $2 $4 $6 }
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
      | let rec id ':' Type
        '=' Term in Term            { LriF $3 $5 $7 $9 }
      | '{' Field '}'               { FldF $2       }
      | Term '.' id                 { AccF $1 $3    }
      | int                         { (applyN SucF $1) ZeroF}
      | ref Term                    { RefF $2       }
      | '!' Term                    { DrfF $2       }
      | Term ':=' Term              { AssF $1 $3    }

Field : id '=' Term                 { [($1, $3)]    }
      | id '=' Term ',' Field       { ($1, $3): $5  }

Type  : unitType                    { UnitTypeF     }
      | boolType                    { BoolTypeF     }
      | natType                     { NatTypeF      }
      | refType Type                { RefTypeF $2   }
      | Type '->' Type              { $1 :=> $3     }
      | '{' FldType '}'             { FldTypeF $2   }
      | '(' Type ')'                { $2            }

FldType
      : id ':' Type                 { [($1, $3)]    }
      | id ':' Type ',' FldType     { ($1, $3): $5  }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> FTerm
parse = fullSTLC . lexer

applyN :: forall a. (a -> a) -> Int -> (a -> a)
applyN f = go where
  go :: Int -> (a -> a)
  go 0 = id
  go n = \x -> (go (n - 1)) (f x)

testFile :: String -> IO FTerm
testFile file = do
  cont <- readFile file
  print cont
  print $ lexer cont
  print $ fullSTLC $ lexer cont
  return $ parse cont
}
