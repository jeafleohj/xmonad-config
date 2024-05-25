import Util.ColorPalette
import Xmobar
import Util.Plugins

main :: IO ()
main = xmobar config

myTemplate :: String
myTemplate =
  unwords
    [ "%battery%",
      "%UnsafeStdinReader%",
      "}{",
      coloredText modusVivendiMagenta "%cpu%",
      coloredText modusVivendiRed "%memory%",
      coloredText modusVivendiYellow "%Volume%",
      "%dynnetwork%",
      "%date%"
    ]

config :: Config
config =
  defaultConfig
    { font = "DejaVu Sans Mono 12",
      bgColor = modusOperandiBlack,
      fgColor = modusOperandiCyan,
      position = TopH 25,
      alpha = 180,
      sepChar = "%",
      alignSep = "}{",
      iconRoot = "/home/leohj/.xmonad2/icons/",
      commands = myCommands,
      template = myTemplate
    }

myCommands :: [Runnable]
myCommands =
  [ Run $
      DynNetwork
        [ "--template",
          "<txipat><tx>kB/s<rxipat><rx>kB/s",
          "-m", "4",
          "-c", " ",
          "--Low", "1000", -- units: B/s
          "--High", "5000", -- units: B/s
          "--low", modusVivendiGreen,
          "--normal", "darkorange",
          "--high", "#EF2AEF",
          "--",
          "--rx-icon-pattern", "<icon=network/rx/network_rx_%%.xpm/>",
          "--tx-icon-pattern", "<icon=network/tx/network_tx_%%.xpm/>"
        ]
        10,
    Run $ Cpu ["-t", "🧠<total>",
               "-p", "3",
               "--suffix", "True",
               "-H", "50",
               "--high", modusVivendiRed] 10,
    Run $
      Memory
        [ "--template",
          "<icon=mem.xpm/> <usedratio>%",
          "--High",
          "90", -- units: %
          "--high",
          modusVivendiRed
        ]
        10,
    Run $
      Battery
        [ "--template",
          "<leftipat> <acstatus>",
          "--Low",
          "10", -- units: %
          "--High",
          "80", -- units: %
          "--low",
          "red",
          "--normal",
          "darkorange",
          "--high",
          "green",
          "--", -- battery specific options
          "--on-icon-pattern",
          "<icon=battery/on/battery_on_%%.xpm/>",
          "--off-icon-pattern",
          "<icon=battery/off/battery_off_%%.xpm/>",
          "--idle-icon-pattern",
          "<icon=battery/idle/battery_idle_%%.xpm/>",
          "-o",
          "<left><fc=#c5c8c6>%</fc> (<timeleft>)", -- discharging status
          "-O",
          "<left><fc=#c5c8c6>% <timeleft></fc>", -- plugged in status
          "-i",
          coloredText modusVivendiGray "IDLE"
        ]
        50,
    Run $ Date (coloredText modusVivendiCyan"%a, %b %d %Y, %T") "date" 10,
    Run $ Volume "Volume" 4,
    Run UnsafeStdinReader
  ]
