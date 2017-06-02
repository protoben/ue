module Data.Units.MKS (mks) where

import Data.Units.Types
import Data.Units.Definition

-- basic units
kg = BaseUnit "kg" "kilogram" Mass
m = BaseUnit "m" "meter" Distance
cd = BaseUnit "cd" "candela" Luminosity
s = BaseUnit "s" "second" Time
k = BaseUnit "K" "Kelvin" Temperature
a = BaseUnit "A" "Ampere" Current

-- the system itself
mks :: UnitSystem
mks = system [kg,m,cd,s,k,a] $ do
    siPrefixesBase m
    siPrefixesBase cd
    siPrefixesBase s
    siPrefixesBase k
    siPrefixesBase a
    hz <- atomic $ siPrefixed $ unit "Hz" "hertz"  $ inv s
    n  <- siPrefixed $ unit "N"  "newton" $ (kg >* m) >/ (s >* s)
    pa <- siPrefixed $ unit "Pa" "pascal" $ n >/ (m >* m)
    j  <- siPrefixed $ unit "J"  "joule"  $ n >* m
    w  <- siPrefixed $ unit "W"  "watt"   $ j >/ s
    c  <- siPrefixed $ unit "C"  "coulomb"$ s >* a
    v  <- siPrefixed $ unit "V"  "volt"   $ w >/ a
    f  <- siPrefixed $ unit "F"  "farad"  $ c >/ v
    ohm<- siPrefixed $ unit "Ω"  "ohm"    $ v >/ a
    siemens <- siPrefixed $ unit "S" "siemens" $ inv ohm
    wb <- siPrefixed $ unit "Wb" "weber"  $ j >/ a
    t  <- siPrefixed $ unit "T"  "tesla"  $ (v >* s) >/ (m >* m)
    h  <- siPrefixed $ unit "H"  "henry"  $ (v >* s) >/ a
    sv <- siPrefixed $ unit "Sv" "sievert"$ j >/ kg
    return ()
