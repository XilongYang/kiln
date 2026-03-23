module UT.TestUtils.Colors where

type Color = String

-- ANSI color sequence for green foreground.
colorGreen :: Color
colorGreen = "\ESC[32m"

-- ANSI color sequence for red foreground.
colorRed :: Color
colorRed = "\ESC[31m"

-- ANSI sequence to reset all styles.
colorReset :: Color
colorReset = "\ESC[0m"

-- Wraps a string in ANSI color and appends a reset sequence.
--
-- Parameter order is `(color, text)` to match call sites like:
-- `makeColor colorGreen "[OK]"`.
makeColor :: Color -> String -> String
makeColor color str = color ++ str ++ colorReset
