g_LastReturnCtrlKeyDownTime := 0
g_AbortSendRet := false
g_ControlReturnRepeatDetected := false

*Enter::
    if (g_ControlReturnRepeatDetected)
    {
        return
    }

    send,{Ctrl down}
    g_LastReturnCtrlKeyDownTime := A_TickCount
    g_AbortSendRet := false
    g_ControlReturnRepeatDetected := true

    return

*Enter Up::
    send,{Ctrl up}
    g_ControlReturnRepeatDetected := false
    if (g_AbortSendRet)
    {
        return
    }
    current_time := A_TickCount
    time_elapsed := current_time - g_LastReturnCtrlKeyDownTime
    if (time_elapsed <= 250)
    {
        SendInput {Enter}
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
    g_AbortSendRet := true
return
