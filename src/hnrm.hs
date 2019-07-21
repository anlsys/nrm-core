{-# LANGUAGE UnicodeSyntax #-}

import           Prelude
import           Xmobar

config :: Config
config = defaultConfig
  { font        = "xft:Hasklig Medium:size=15"
  , allDesktops = True
  , bgColor     = "black"
  , fgColor     = "#dddddd"
  , position    = BottomW L 100
  , commands    =
    [ Run $ Date "<fc=#ECBE7B></fc> %a %b %d %I:%M" "date" 10
    , Run $ DynNetwork
      [ "-t"
      , "<fc=#4db5bd>UP</fc><rx>/<fc=#c678dd>DOWN</fc><tx>"
      , "-H"
      , "200"
      , "-L"
      , "10"
      , "-h"
      , "#bbc2cf"
      , "-l"
      , "#bbc2cf"
      , "-n"
      , "#bbc2cf"
      ]
      50
    , Run $ BatteryP
      ["BAT0"]
      [ "--template"
      , "<acstatus>"
      , "--Low"
      , "10"
      , "--High"
      , "80"
      , "--low"
      , "#fb4934"
      , "--normal"
      , "#bbc2cf"
      , "--high"
      , "#98be65"
      , "--"
      , "-o"
      , "<left>% (<timeleft>)"
      , "-O"
      , "<left>% (<fc=#98be65>Charging</fc>)"
      , "-i"
      , "<fc=#98be65>Charged</fc>"
      ]
      50
    , Run $ Com "bash"
                ["-c", "ls ~/.local/share/mail.queue | wc -l || true"]
                "outbound"
                5
    , Run $ Com
      "bash"
      [ "-c"
      , "NOTMUCH_CONFIG=/run/secrets/notmuch notmuch count 'tag:inbox and ((not tag:spam and not tag:archivedlist) or tag:high)' || true"
      ]
      "local"
      2
    , Run $ Com
      "bash"
      ["-c", "systemctl is-active openvpn-trustzone.service || true"]
      "vpn"
      5
    , Run $ Com "bash"
                ["-c", "systemctl is-active --user getmail.service || true"]
                "getmail"
                5
    , Run $ Com "bash"
                ["-c", "task +in +PENDING count 2>/dev/null || true"]
                "intask"
                10
    , Run $ Com "bash"
                ["-c", "task count +PENDING 2>/dev/null || true"]
                "ttask"
                10
    , Run StdinReader
    ]
  , sepChar     = "%"
  , alignSep    = "}{"
  , template    =
    "%StdinReader% }{ getmail: %getmail% | tasks: total %ttask% new %intask% | mail: new %local% out %outbound% | vpn %vpn% | %battery% | %date% |     "
  }

main :: IO ()
main = xmobar config
