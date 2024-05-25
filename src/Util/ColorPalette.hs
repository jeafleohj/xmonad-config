module Util.ColorPalette (
    -- Modus Operandi (Light Theme)
    modusOperandiRed,
    modusOperandiGreen,
    modusOperandiBlue,
    modusOperandiYellow,
    modusOperandiCyan,
    modusOperandiMagenta,
    modusOperandiBlack,
    modusOperandiWhite,
    modusOperandiGray,

    -- Modus Vivendi (Dark Theme)
    modusVivendiRed,
    modusVivendiGreen,
    modusVivendiBlue,
    modusVivendiYellow,
    modusVivendiCyan,
    modusVivendiMagenta,
    modusVivendiBlack,
    modusVivendiWhite,
    modusVivendiGray,
    coloredText,

    -- Function to apply color
--    withColor
) where

-- Modus Operandi (Light Theme) Colors
modusOperandiRed :: String
modusOperandiRed = "#e45649"

modusOperandiGreen :: String
modusOperandiGreen = "#50a14f"

modusOperandiBlue :: String
modusOperandiBlue = "#4078f2"

modusOperandiYellow :: String
modusOperandiYellow = "#986801"

modusOperandiCyan :: String
modusOperandiCyan = "#0184bc"

modusOperandiMagenta :: String
modusOperandiMagenta = "#a626a4"

modusOperandiBlack :: String
modusOperandiBlack = "#000000"

modusOperandiWhite :: String
modusOperandiWhite = "#fafafa"

modusOperandiGray :: String
modusOperandiGray = "#d0d0d0"

-- Modus Vivendi (Dark Theme) Colors
modusVivendiRed :: String
modusVivendiRed = "#ff6c6b"

modusVivendiGreen :: String
modusVivendiGreen = "#98be65"

modusVivendiBlue :: String
modusVivendiBlue = "#51afef"

modusVivendiYellow :: String
modusVivendiYellow = "#ecbe7b"

modusVivendiCyan :: String
modusVivendiCyan = "#46d9ff"

modusVivendiMagenta :: String
modusVivendiMagenta = "#c678dd"

modusVivendiBlack :: String
modusVivendiBlack = "#282c34"

modusVivendiWhite :: String
modusVivendiWhite = "#bbc2cf"

modusVivendiGray :: String
modusVivendiGray = "#5B6268"

coloredText :: String -> String -> String
coloredText color str = "<fc=" <> color <> ">" <> str <> "</fc>"
