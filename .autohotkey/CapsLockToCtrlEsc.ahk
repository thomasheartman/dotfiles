g_LastCtrlKeyDownTime := 0
g_shouldSendEscape := true
g_ControlRepeatDetected := false

*CapsLock::
    if (g_ControlRepeatDetected)
    {
        return
    }

    SendInput,{Ctrl down}
    g_LastCtrlKeyDownTime := A_TickCount
    g_shouldSendEscape := true
    g_ControlRepeatDetected := true

    return

*CapsLock Up::
    SendInput,{Ctrl up}
    g_ControlRepeatDetected := false
    if (!g_shouldSendEscape)
    {
        return
    }
    current_time := A_TickCount
    time_elapsed := current_time - g_LastCtrlKeyDownTime
    if (time_elapsed <= 200)
    {
        SendInput {Esc}
    }
    return

~*^a::
~*^b::
~*^c::
~*^d::
~*^e::
~*^f::
~*^g::
~*^h::
~*^i::
~*^j::
~*^k::
~*^l::
~*^m::
~*^n::
~*^o::
~*^p::
~*^q::
~*^r::
~*^s::
~*^t::
~*^u::
~*^v::
~*^w::
~*^x::
~*^y::
~*^z::
~*^1::
~*^2::
~*^3::
~*^4::
~*^5::
~*^6::
~*^7::
~*^8::
~*^9::
~*^0::
~*^Space::
~*^Backspace::
~*^Delete::
~*^Insert::
~*^Home::
~*^End::
~*^PgUp::
~*^PgDn::
~*^Tab::
~*^Return::
~*^,::
~*^.::
~*^/::
~*^;::
~*^'::
~*^[::
~*^]::
~*^\::
~*^-::
~*^=::
~*^`::
~*^F1::
~*^F2::
~*^F3::
~*^F4::
~*^F5::
~*^F6::
~*^F7::
~*^F8::
~*^F9::
~*^F10::
~*^F11::
~*^F12::
    g_shouldSendEscape := false
return
