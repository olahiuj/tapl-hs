{-# OPTIONS_GHC -w #-}
module Grammar
  ( parse
  )
where
import System.IO
import Lexer
import Term
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,166) ([63744,317,32768,0,0,32768,48892,0,64,0,0,0,1,61384,9,15865,8193,10175,58368,1271,0,0,0,0,0,16384,20350,51200,7151,64768,381,0,0,0,0,0,0,2048,0,4096,0,0,61384,9,0,7168,8192,58368,1271,64640,158,0,0,64502,16386,24574,51200,10735,0,0,0,0,0,28672,32768,0,24576,0,0,56,64,61384,9,15865,1,0,0,0,0,512,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_fullSTLC","Term","Type","unitType","boolType","natType","func","else","then","unit","let","suc","prd","isz","if","in","var","zero","true","false","';'","'='","'('","')'","'->'","':'","%eof"]
        bit_start = st * 29
        bit_end = (st + 1) * 29
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..28]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (9) = happyShift action_4
action_0 (12) = happyShift action_5
action_0 (13) = happyShift action_6
action_0 (14) = happyShift action_7
action_0 (15) = happyShift action_8
action_0 (16) = happyShift action_9
action_0 (17) = happyShift action_10
action_0 (19) = happyShift action_2
action_0 (20) = happyShift action_11
action_0 (21) = happyShift action_12
action_0 (22) = happyShift action_13
action_0 (25) = happyShift action_14
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (19) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (9) = happyShift action_4
action_3 (12) = happyShift action_5
action_3 (13) = happyShift action_6
action_3 (14) = happyShift action_7
action_3 (15) = happyShift action_8
action_3 (16) = happyShift action_9
action_3 (17) = happyShift action_10
action_3 (19) = happyShift action_2
action_3 (20) = happyShift action_11
action_3 (21) = happyShift action_12
action_3 (22) = happyShift action_13
action_3 (23) = happyShift action_23
action_3 (25) = happyShift action_14
action_3 (29) = happyAccept
action_3 (4) = happyGoto action_22
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (19) = happyShift action_21
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_13

action_6 (19) = happyShift action_20
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (9) = happyShift action_4
action_7 (12) = happyShift action_5
action_7 (13) = happyShift action_6
action_7 (14) = happyShift action_7
action_7 (15) = happyShift action_8
action_7 (16) = happyShift action_9
action_7 (17) = happyShift action_10
action_7 (19) = happyShift action_2
action_7 (20) = happyShift action_11
action_7 (21) = happyShift action_12
action_7 (22) = happyShift action_13
action_7 (25) = happyShift action_14
action_7 (4) = happyGoto action_19
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (9) = happyShift action_4
action_8 (12) = happyShift action_5
action_8 (13) = happyShift action_6
action_8 (14) = happyShift action_7
action_8 (15) = happyShift action_8
action_8 (16) = happyShift action_9
action_8 (17) = happyShift action_10
action_8 (19) = happyShift action_2
action_8 (20) = happyShift action_11
action_8 (21) = happyShift action_12
action_8 (22) = happyShift action_13
action_8 (25) = happyShift action_14
action_8 (4) = happyGoto action_18
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (9) = happyShift action_4
action_9 (12) = happyShift action_5
action_9 (13) = happyShift action_6
action_9 (14) = happyShift action_7
action_9 (15) = happyShift action_8
action_9 (16) = happyShift action_9
action_9 (17) = happyShift action_10
action_9 (19) = happyShift action_2
action_9 (20) = happyShift action_11
action_9 (21) = happyShift action_12
action_9 (22) = happyShift action_13
action_9 (25) = happyShift action_14
action_9 (4) = happyGoto action_17
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (9) = happyShift action_4
action_10 (12) = happyShift action_5
action_10 (13) = happyShift action_6
action_10 (14) = happyShift action_7
action_10 (15) = happyShift action_8
action_10 (16) = happyShift action_9
action_10 (17) = happyShift action_10
action_10 (19) = happyShift action_2
action_10 (20) = happyShift action_11
action_10 (21) = happyShift action_12
action_10 (22) = happyShift action_13
action_10 (25) = happyShift action_14
action_10 (4) = happyGoto action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_12

action_12 _ = happyReduce_11

action_13 _ = happyReduce_10

action_14 (9) = happyShift action_4
action_14 (12) = happyShift action_5
action_14 (13) = happyShift action_6
action_14 (14) = happyShift action_7
action_14 (15) = happyShift action_8
action_14 (16) = happyShift action_9
action_14 (17) = happyShift action_10
action_14 (19) = happyShift action_2
action_14 (20) = happyShift action_11
action_14 (21) = happyShift action_12
action_14 (22) = happyShift action_13
action_14 (25) = happyShift action_14
action_14 (4) = happyGoto action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (9) = happyShift action_4
action_15 (12) = happyShift action_5
action_15 (13) = happyShift action_6
action_15 (14) = happyShift action_7
action_15 (15) = happyShift action_8
action_15 (16) = happyShift action_9
action_15 (17) = happyShift action_10
action_15 (19) = happyShift action_2
action_15 (20) = happyShift action_11
action_15 (21) = happyShift action_12
action_15 (22) = happyShift action_13
action_15 (23) = happyShift action_23
action_15 (25) = happyShift action_14
action_15 (26) = happyShift action_28
action_15 (4) = happyGoto action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (9) = happyShift action_4
action_16 (11) = happyShift action_27
action_16 (12) = happyShift action_5
action_16 (13) = happyShift action_6
action_16 (14) = happyShift action_7
action_16 (15) = happyShift action_8
action_16 (16) = happyShift action_9
action_16 (17) = happyShift action_10
action_16 (19) = happyShift action_2
action_16 (20) = happyShift action_11
action_16 (21) = happyShift action_12
action_16 (22) = happyShift action_13
action_16 (23) = happyShift action_23
action_16 (25) = happyShift action_14
action_16 (4) = happyGoto action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (9) = happyShift action_4
action_17 (12) = happyShift action_5
action_17 (13) = happyShift action_6
action_17 (14) = happyShift action_7
action_17 (15) = happyShift action_8
action_17 (16) = happyShift action_9
action_17 (17) = happyShift action_10
action_17 (19) = happyShift action_2
action_17 (20) = happyShift action_11
action_17 (21) = happyShift action_12
action_17 (22) = happyShift action_13
action_17 (23) = happyShift action_23
action_17 (25) = happyShift action_14
action_17 (4) = happyGoto action_22
action_17 _ = happyReduce_9

action_18 (9) = happyShift action_4
action_18 (12) = happyShift action_5
action_18 (13) = happyShift action_6
action_18 (14) = happyShift action_7
action_18 (15) = happyShift action_8
action_18 (16) = happyShift action_9
action_18 (17) = happyShift action_10
action_18 (19) = happyShift action_2
action_18 (20) = happyShift action_11
action_18 (21) = happyShift action_12
action_18 (22) = happyShift action_13
action_18 (23) = happyShift action_23
action_18 (25) = happyShift action_14
action_18 (4) = happyGoto action_22
action_18 _ = happyReduce_7

action_19 (9) = happyShift action_4
action_19 (12) = happyShift action_5
action_19 (13) = happyShift action_6
action_19 (14) = happyShift action_7
action_19 (15) = happyShift action_8
action_19 (16) = happyShift action_9
action_19 (17) = happyShift action_10
action_19 (19) = happyShift action_2
action_19 (20) = happyShift action_11
action_19 (21) = happyShift action_12
action_19 (22) = happyShift action_13
action_19 (23) = happyShift action_23
action_19 (25) = happyShift action_14
action_19 (4) = happyGoto action_22
action_19 _ = happyReduce_8

action_20 (24) = happyShift action_26
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (28) = happyShift action_25
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (9) = happyShift action_4
action_22 (12) = happyShift action_5
action_22 (13) = happyShift action_6
action_22 (14) = happyShift action_7
action_22 (15) = happyShift action_8
action_22 (16) = happyShift action_9
action_22 (17) = happyShift action_10
action_22 (19) = happyShift action_2
action_22 (20) = happyShift action_11
action_22 (21) = happyShift action_12
action_22 (22) = happyShift action_13
action_22 (23) = happyShift action_23
action_22 (25) = happyShift action_14
action_22 (4) = happyGoto action_22
action_22 _ = happyReduce_2

action_23 (9) = happyShift action_4
action_23 (12) = happyShift action_5
action_23 (13) = happyShift action_6
action_23 (14) = happyShift action_7
action_23 (15) = happyShift action_8
action_23 (16) = happyShift action_9
action_23 (17) = happyShift action_10
action_23 (19) = happyShift action_2
action_23 (20) = happyShift action_11
action_23 (21) = happyShift action_12
action_23 (22) = happyShift action_13
action_23 (25) = happyShift action_14
action_23 (4) = happyGoto action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (9) = happyShift action_4
action_24 (12) = happyShift action_5
action_24 (13) = happyShift action_6
action_24 (14) = happyShift action_7
action_24 (15) = happyShift action_8
action_24 (16) = happyShift action_9
action_24 (17) = happyShift action_10
action_24 (19) = happyShift action_2
action_24 (20) = happyShift action_11
action_24 (21) = happyShift action_12
action_24 (22) = happyShift action_13
action_24 (23) = happyShift action_23
action_24 (25) = happyShift action_14
action_24 (4) = happyGoto action_22
action_24 _ = happyReduce_6

action_25 (6) = happyShift action_32
action_25 (7) = happyShift action_33
action_25 (8) = happyShift action_34
action_25 (25) = happyShift action_35
action_25 (5) = happyGoto action_31
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (9) = happyShift action_4
action_26 (12) = happyShift action_5
action_26 (13) = happyShift action_6
action_26 (14) = happyShift action_7
action_26 (15) = happyShift action_8
action_26 (16) = happyShift action_9
action_26 (17) = happyShift action_10
action_26 (19) = happyShift action_2
action_26 (20) = happyShift action_11
action_26 (21) = happyShift action_12
action_26 (22) = happyShift action_13
action_26 (25) = happyShift action_14
action_26 (4) = happyGoto action_30
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (9) = happyShift action_4
action_27 (12) = happyShift action_5
action_27 (13) = happyShift action_6
action_27 (14) = happyShift action_7
action_27 (15) = happyShift action_8
action_27 (16) = happyShift action_9
action_27 (17) = happyShift action_10
action_27 (19) = happyShift action_2
action_27 (20) = happyShift action_11
action_27 (21) = happyShift action_12
action_27 (22) = happyShift action_13
action_27 (25) = happyShift action_14
action_27 (4) = happyGoto action_29
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_14

action_29 (9) = happyShift action_4
action_29 (10) = happyShift action_40
action_29 (12) = happyShift action_5
action_29 (13) = happyShift action_6
action_29 (14) = happyShift action_7
action_29 (15) = happyShift action_8
action_29 (16) = happyShift action_9
action_29 (17) = happyShift action_10
action_29 (19) = happyShift action_2
action_29 (20) = happyShift action_11
action_29 (21) = happyShift action_12
action_29 (22) = happyShift action_13
action_29 (23) = happyShift action_23
action_29 (25) = happyShift action_14
action_29 (4) = happyGoto action_22
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (9) = happyShift action_4
action_30 (12) = happyShift action_5
action_30 (13) = happyShift action_6
action_30 (14) = happyShift action_7
action_30 (15) = happyShift action_8
action_30 (16) = happyShift action_9
action_30 (17) = happyShift action_10
action_30 (18) = happyShift action_39
action_30 (19) = happyShift action_2
action_30 (20) = happyShift action_11
action_30 (21) = happyShift action_12
action_30 (22) = happyShift action_13
action_30 (23) = happyShift action_23
action_30 (25) = happyShift action_14
action_30 (4) = happyGoto action_22
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (9) = happyShift action_4
action_31 (12) = happyShift action_5
action_31 (13) = happyShift action_6
action_31 (14) = happyShift action_7
action_31 (15) = happyShift action_8
action_31 (16) = happyShift action_9
action_31 (17) = happyShift action_10
action_31 (19) = happyShift action_2
action_31 (20) = happyShift action_11
action_31 (21) = happyShift action_12
action_31 (22) = happyShift action_13
action_31 (25) = happyShift action_14
action_31 (27) = happyShift action_38
action_31 (4) = happyGoto action_37
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_15

action_33 _ = happyReduce_16

action_34 _ = happyReduce_17

action_35 (6) = happyShift action_32
action_35 (7) = happyShift action_33
action_35 (8) = happyShift action_34
action_35 (25) = happyShift action_35
action_35 (5) = happyGoto action_36
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (26) = happyShift action_44
action_36 (27) = happyShift action_38
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (9) = happyShift action_4
action_37 (12) = happyShift action_5
action_37 (13) = happyShift action_6
action_37 (14) = happyShift action_7
action_37 (15) = happyShift action_8
action_37 (16) = happyShift action_9
action_37 (17) = happyShift action_10
action_37 (19) = happyShift action_2
action_37 (20) = happyShift action_11
action_37 (21) = happyShift action_12
action_37 (22) = happyShift action_13
action_37 (23) = happyShift action_23
action_37 (25) = happyShift action_14
action_37 (4) = happyGoto action_22
action_37 _ = happyReduce_3

action_38 (6) = happyShift action_32
action_38 (7) = happyShift action_33
action_38 (8) = happyShift action_34
action_38 (25) = happyShift action_35
action_38 (5) = happyGoto action_43
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (9) = happyShift action_4
action_39 (12) = happyShift action_5
action_39 (13) = happyShift action_6
action_39 (14) = happyShift action_7
action_39 (15) = happyShift action_8
action_39 (16) = happyShift action_9
action_39 (17) = happyShift action_10
action_39 (19) = happyShift action_2
action_39 (20) = happyShift action_11
action_39 (21) = happyShift action_12
action_39 (22) = happyShift action_13
action_39 (25) = happyShift action_14
action_39 (4) = happyGoto action_42
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (9) = happyShift action_4
action_40 (12) = happyShift action_5
action_40 (13) = happyShift action_6
action_40 (14) = happyShift action_7
action_40 (15) = happyShift action_8
action_40 (16) = happyShift action_9
action_40 (17) = happyShift action_10
action_40 (19) = happyShift action_2
action_40 (20) = happyShift action_11
action_40 (21) = happyShift action_12
action_40 (22) = happyShift action_13
action_40 (25) = happyShift action_14
action_40 (4) = happyGoto action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (9) = happyShift action_4
action_41 (12) = happyShift action_5
action_41 (13) = happyShift action_6
action_41 (14) = happyShift action_7
action_41 (15) = happyShift action_8
action_41 (16) = happyShift action_9
action_41 (17) = happyShift action_10
action_41 (19) = happyShift action_2
action_41 (20) = happyShift action_11
action_41 (21) = happyShift action_12
action_41 (22) = happyShift action_13
action_41 (23) = happyShift action_23
action_41 (25) = happyShift action_14
action_41 (4) = happyGoto action_22
action_41 _ = happyReduce_4

action_42 (9) = happyShift action_4
action_42 (12) = happyShift action_5
action_42 (13) = happyShift action_6
action_42 (14) = happyShift action_7
action_42 (15) = happyShift action_8
action_42 (16) = happyShift action_9
action_42 (17) = happyShift action_10
action_42 (19) = happyShift action_2
action_42 (20) = happyShift action_11
action_42 (21) = happyShift action_12
action_42 (22) = happyShift action_13
action_42 (23) = happyShift action_23
action_42 (25) = happyShift action_14
action_42 (4) = happyGoto action_22
action_42 _ = happyReduce_5

action_43 (27) = happyShift action_38
action_43 _ = happyReduce_18

action_44 _ = happyReduce_19

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var' happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App' happy_var_1 happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 5 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Abs' happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 6 4 happyReduction_4
happyReduction_4 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Ite' happy_var_4 happy_var_6 happy_var_2
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6 4 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lin' happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Seq' happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Prd' happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Suc' happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (IsZ' happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  4 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn4
		 (False'
	)

happyReduce_11 = happySpecReduce_1  4 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn4
		 (True'
	)

happyReduce_12 = happySpecReduce_1  4 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn4
		 (Zero'
	)

happyReduce_13 = happySpecReduce_1  4 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn4
		 (Unit'
	)

happyReduce_14 = happySpecReduce_3  4 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  5 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn5
		 (UnitType'
	)

happyReduce_16 = happySpecReduce_1  5 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn5
		 (BoolType'
	)

happyReduce_17 = happySpecReduce_1  5 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn5
		 (NatType'
	)

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 :=> happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TUnitType -> cont 6;
	TBoolType -> cont 7;
	TNatType -> cont 8;
	TFunc -> cont 9;
	TElse -> cont 10;
	TThen -> cont 11;
	TUnit -> cont 12;
	TLet -> cont 13;
	TSuc -> cont 14;
	TPrd -> cont 15;
	TIsZ -> cont 16;
	TIf -> cont 17;
	TIn -> cont 18;
	TVar happy_dollar_dollar -> cont 19;
	TZero -> cont 20;
	TTrue -> cont 21;
	TFalse -> cont 22;
	TSemiColon -> cont 23;
	TAssign -> cont 24;
	TLParen -> cont 25;
	TRParen -> cont 26;
	TArrow -> cont 27;
	TColon -> cont 28;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 29 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
fullSTLC tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
