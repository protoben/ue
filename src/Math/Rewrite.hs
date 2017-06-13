module Math.Rewrite (simplify, simplifyHistory) where

import Data.Expression
import Data.Units

import Data.List

import Math.Rewrite.Engine
import Math.Rewrite.Reductions

rewriteRules :: [RewriteRule]
rewriteRules =
    let (a:b:c:d:e:f:[]) = map (RRIdentifier Any) "abcdef"
        (x:y:z:[]) = map (RRIdentifier Nonliteral) "xyz"
        (m:n:k:[]) = map (RRIdentifier Literal) "mnk" in [
    -- reordering terms to normal form
    (a =* n, n =* a),

    -- simple properties
    (a =- a, csti 0), (a =- csti 0, a), (csti 0 =- a, neg a),
    (neg a =+ a, csti 0), (a =+ (neg a), csti 0),
    (a =+ csti 0, a), (csti 0 =+ a, a),
    (a =* csti 0, csti 0), (csti 0 =* a, csti 0),
    (a =* csti 1, a), (csti 1 =* a, a),
    (a =/ a, csti 1),

    -- unary operations
    (neg $ neg a, a),

    -- addition/subtraction
    (a =+ neg a, csti 0), (a =- neg a, a =+ a),
    (a =+ (b =+ neg a), b), (a =+ (neg a =+ b), b),
    (a =+ (b =+ a), b =+ a =* csti 2), (a =+ (a =+ b), b =+ a =* csti 2),
    ((b =+ a) =+ a, b =+ a =* csti 2), ((a =+ b) =+ a, b =+ a =* csti 2),
    ((a =+ b) =- a, b), ((b =+ a) =- a, b), ((b =- a) =+ a, b),

    -- multiplication/division
    (a =* (b =/ a), b), ((b =/ a) =* a, b), ((a =* b) =/ a, b), ((b =* a) =/ a, b),
    ((n =* a) =/ m, (n =/ m) =* a), ((a =* n) =/ m, (n =/ m) =* a),
    (a =+ a, csti 2 =* a),
    (a =+ b=*a, (b =+ csti 1) =* a), (a =+ a=*b, (b =+ csti 1) =* a),
    (a =- b=*a, (b =- csti 1) =* a), (a =- a=*b, (b =- csti 1) =* a),
    ((neg a) =+ b=*a, (b =- csti 1) =* a), ((neg a) =+ a=*b, (b =- csti 1) =* a),
    -- factoring out coefficients
    (a=*b =+ a=*c, a =* (b =+ c)), (b=*a =+ c=*a, a =* (b =+ c)),
    (a =- (b =+ c), a =+ (neg b =+ neg c)), -- distribute subtraction
    (a =- (b =- c), a =+ (neg b =+ c)), -- distribute subtraction
    ((a =+ b=*c) =+ d=*c, (a =+ c=*(b =+ d))),
    ((a =+ b=*c) =- d=*c, (a =+ c=*(b =- d))),

    -- division
    (a=/b =+ c=/b, (a =+ c) =/ b),

    -- exponentiation
    (a =^ csti 0, csti 1), (a =^ csti 1, a), (a =^ csti (-1), (csti 1) =/ a),
    (csti 1 =^ a, a), (csti 0 =^ a, csti 0),
    (a =* a, a =^ csti 2),
    (a =* a=^b, a =^ (b =+ csti 1)), (a=^b =* a, a =^ (b =+ csti 1)),
    (a=^b =/ a, a =^ (b =- csti 1)), (a=^b =/ a=^c, a =^ (b =- c)),
    (a=^b =* a=^c, a =^ (b =+ c)),
    ((d =* a=^b) =* a=^c, d =* (a =^ (b =+ c))),
    ((a=^b =* d) =* a=^c, d =* (a =^ (b =+ c))),
    ((a =^ b) =* (d =* (a =^ c)), d =* (a =^ (b =+ c))),
    ((a =^ b) =* ((a =^ c) =* d), d =* (a =^ (b =+ c))),

    ((a =^ b) =^ c, a =^ (b =* c)),
    ((a =* b) =^ m, (a =^ m) =* (b =^ m)),
    ((m =^ a) =* (n =^ a), (m =* n) =^ a),

    -- basic equality simplifications
    -- only do this for nonliterals for now, until we can produce boolean values
    (x =: x, csti 1 =: csti 1),
    (n =: x, x =: n), -- constants always on right side of equality
    (a =* b =: a =* c, b =: c), (b =* a =: a =* c, b =: c),
    (a =* b =: c =* a, b =: c), (b =* a =: c =* a, b =: c),
    (a =* b =: a, b =: csti 1), (b =* a =: a, b =: csti 1),
    (a =+ b =: c =+ b, a =: c), (b =+ a =: c =+ b, a =: c), -- subtract both sides
    (a =+ b =: b =+ c, a =: c), (b =+ a =: b =+ c, a =: c), -- subtract both sides
    (a =+ b =: m =* b, a =: b =* (m =- csti 1)),  -- subtract from multiplication
    (a =+ b =: m =* a, b =: a =* (m =- csti 1)),

    -- solve simple algebra
    (n =* a =: m, a =: m =/ n), (n =/ a =: m, a =: m =* n),
    (n =+ a =: m, a =: m =- n), (a =- n =: m, a =: m =+ n),
    (a =* b =: c =* (b =^ d), a =: c =* (b =^ (d =- csti 1))),
    (a =* b =: (b =^ d) =* c, a =: c =* (b =^ (d =- csti 1))),
    (b =* a =: c =* (b =^ d), a =: c =* (b =^ (d =- csti 1))),
    (b =* a =: (b =^ d) =* c, a =: c =* (b =^ (d =- csti 1)))
    ]

reductionRules = [simplifyArithmetic,
                  onChange reorderLikeTerms,
                  rewrite rewriteRules]

simplify :: Expr -> Expr
simplify e = let history = reductions reductionRules e in
    if null history then e else last history

simplifyHistory :: Expr -> [Expr]
simplifyHistory = reductions reductionRules
