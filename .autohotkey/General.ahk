; Let virtual consoles do their own thing
#IfWinNotActive, ahk_class VirtualConsoleClass

  ^h::
  SendInput {Blind}{Ctrl up}{Backspace}{Ctrl down}
  return

  ^w::Backspace
  return

  !w::
  SendInput ^w
  return

#IfWinNotActive

!q::
SendInput !{f4}