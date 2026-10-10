(* ::Package:: *)

(* ::Title:: *)
(* Restricted reconstruction of commuting coefficient trees *)

(* ::Text:: *)
(* Loaded in CalcFormConverter`Private`. Used only inside the validated
   version-one, no-UpValues import environment. It reads one coefficient, not
   the result file. Rejected attempts return to the general parser, preserving
   ordered diagnostics. All actual factors pass through importFactor and its
   typed dictionary. No result text is interpreted as Wolfram source. *)

(* ::Section:: *)
(* Integer syntax validation *)

(* ::Text:: *)
(* Positive codes identify factors; negative codes denote +, -, *, /, ( and ).
   The WVM checks operand/operator alternation and balanced parentheses before
   any reconstruction. Unary signs are permitted while awaiting an operand.
   The 64-level limit selects the existing fallback for deeper expressions.
   Compilation is lazy, requires no C compiler and retains no import state. *)

(* ::Input::Initialization:: *)
pgCoefficientSyntax[] := pgCoefficientSyntax[] = Compile[
    {{codes, _Integer, 1}},
    Module[{depth = 0, want = True, valid = True, code = 0},
        Do[
            code = codes[[k]];
            Which[
                code > 0,
                    If[!want, valid = False; Break[]]; want = False,
                code == -1 || code == -2,
                    want = True,
                code == -3 || code == -4,
                    If[want, valid = False; Break[]]; want = True,
                code == -5,
                    If[!want || depth >= 64, valid = False; Break[]]; depth++,
                code == -6,
                    If[want || depth == 0, valid = False; Break[]];
                    depth--; want = False,
                True,
                    valid = False; Break[]
            ], {k, Length[codes]}];
        valid && !want && depth == 0
    ],
    CompilationTarget -> "WVM", RuntimeOptions -> "Speed"
];

(* ::Section:: *)
(* Validated factors and held arithmetic reconstruction *)

(* ::Text:: *)
(* First check exact lexical coverage, integer syntax and every distinct factor.
   Then build a NEW string exclusively from numbered slots and six fixed
   arithmetic operators. Original identifiers, numbers and other source text are
   never copied into that string. ToExpression parses this trusted construction
   under HoldComplete; it does not parse FORM output or mapped symbol names.
   Spaces around signs prevent ++/-- from becoming increment/decrement syntax.
   Replace slots with validated values only after obtaining the held tree.

   Division operands are checked from the inside out before releasing the final
   arithmetic, so a zero multiplier cannot hide division by zero. The enclosing
   dispatcher retries failures through the general parser for original failure
   order. This avoids both a six-column node array and repeated level-by-level
   copying of operand lists; no mathematical simplification pass is added. *)

(* ::Input::Initialization:: *)
pgParseCoefficientTree[text_String] := Module[
    {tokens, operators, distinct, codes, decoded, generated, held,
     substitution, operands},
    tokens = StringCases[text, RegularExpression[$flatFactorPattern <> "|[+*/()\\-]"]];
    If[tokens === {} ||
        StringReplace[StringJoin[tokens], WhitespaceCharacter -> ""] =!=
            StringReplace[text, WhitespaceCharacter -> ""], Return[$Failed]];
    operators = <|"+" -> -1, "-" -> -2, "*" -> -3, "/" -> -4, "(" -> -5, ")" -> -6|>;
    distinct = DeleteCases[DeleteDuplicates[tokens], "+" | "-" | "*" | "/" | "(" | ")"];
    codes = Lookup[Join[AssociationThread[distinct, Range[Length[distinct]]], operators], tokens];
    If[!pgCoefficientSyntax[][codes], Return[$Failed]];
    decoded = importFactor /@ distinct;
    If[!FreeQ[decoded, _vectorToken | _indexToken | _caAToken | _caFToken],
        throwFailure["InvalidResult", "A vector or index occurs outside a tensor object."]];
    generated = StringJoin[Lookup[Join[
        AssociationThread[Range[Length[distinct]],
            ("#" <> IntegerString[#] &) /@ Range[Length[distinct]]],
        <|-1 -> " + ", -2 -> " - ", -3 -> "*", -4 -> "/", -5 -> "(", -6 -> ")"|>], codes]];
    held = ToExpression[generated, InputForm, HoldComplete];
    If[!MatchQ[held, HoldComplete[_]], Return[$Failed]];
    substitution = Dispatch[Thread[(Slot /@ Range[Length[distinct]]) -> decoded]];
    (* Cases visits reciprocal operands inside out. Check each inner
       divisor before evaluating any enclosing denominator. *)
    operands = Cases[held,
        HoldPattern[Power[x_, -1]] :> HoldComplete[x], Infinity];
    If[AnyTrue[operands, ReleaseHold[# /. substitution] === 0 &],
        throwFailure["InvalidResult", "Division by zero."]];
    ReleaseHold[held /. substitution]
];
