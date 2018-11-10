g_LastRetCtrlKeyDownTime := 0
g_shouldSendRet := true
g_ControlRetRepeatDetected := false

*Enter::
    if (g_ControlRetRepeatDetected)
    {
        return
    }

    SendInput,{Ctrl down}
    g_LastRetCtrlKeyDownTime := A_TickCount
    g_shouldSendRet := true
    g_ControlRetRepeatDetected := true

    return

*Enter Up::
    SendInput,{Ctrl up}
    g_ControlRetRepeatDetected := false
    if (!g_shouldSendRet)
    {
        return
    }
    current_time := A_TickCount
    time_elapsed := current_time - g_LastRetCtrlKeyDownTime
    if (time_elapsed <= 200)
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
    g_shouldSendRet := false
return
