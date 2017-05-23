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
    hz <- unit "Hz" "hertz"  $ inv s
    n  <- unit "N"  "newton" $ (kg >* m) >/ (s >* s)
    pa <- unit "Pa" "pascal" $ n >/ (m >* m)
    j  <- unit "J"  "joule"  $ n >* m
    w  <- unit "W"  "watt"   $ j >/ s
    c  <- unit "C"  "coulomb"$ s >* a
    v  <- unit "V"  "volt"   $ w >/ a
    f  <- unit "F"  "farad"  $ c >/ v
    ohm<- unit "Ω"  "ohm"    $ v >/ a
    siemens <- unit "S" "siemens" $ inv ohm
    wb <- unit "Wb" "weber"  $ j >/ a
    t  <- unit "T"  "tesla"  $ (v >* s) >/ (m >* m)
    h  <- unit "H"  "henry"  $ (v >* s) >/ a
    sv <- unit "Sv" "sievert"$ j >/ kg
    return ()
