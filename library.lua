--reaper.ClearConsole()

function sign(n)
    if n < 0 then
        return -1
    elseif n > 0 then
        return 1
    else
        return 0
    end
end

function themecol(name)
  return reaper.ImGui_ColorConvertNative(reaper.GetThemeColor(name,0))<<8 | 0xFF
end

function random_string(length)
    local str = {}
    local ch=''
    for i=1, length do
      if math.random() < 0.5 then
          ch = math.random(65,90)
      else
          ch = math.random(48,57)
      end
      str[#str+1] = string.char(ch)
    end
    return table.concat(str)    
end

function random_id(name)
    if name ~= nil then
        return string.format(
              "%s_%s_%s_%s",
              string.upper(name),
              os.date("%Y%m%d"),
              os.clock(),
              random_string(8)
        )
    else
        return string.format(
              "%s_%s_%s",
              os.date("%Y%m%d"),
              os.clock(),
              random_string(8)
        )
    end
end

function dec(v1)
    return v1 - 1
end
function inc(v1)
    return v1 + 1
end

function clamp(v,lo,hi)
    lo = lo or 0
    hi = hi or 1.0
    if v < lo then
        return lo
    elseif v > hi then
        return hi
    end
    return v
end

function Error_unknown_key(t,e)
    error("Unknown key: ".. e)
end

function LockKeys(o) 
    local mt = getmetatable(o) 
    if mt == nil then
        mt = {}
        setmetatable(o, mt)
    end

    mt.__index = Error_unknown_key
    mt.__newindex = Error_unknown_key
    return o
end
    
function nilvalue()
    return 'nil'
end

function nilencode(v)
    return v or nilvalue()
end

function nildecode(v)
    if v == nilvalue() then
        return nil
    end
    return v
end

function cout(txt)
    reaper.ShowConsoleMsg(txt.."\n")
end
function cout2(txt)
    reaper.ShowConsoleMsg(tostring(txt).."\n")
end

function coutkv(k,v)
    reaper.ShowConsoleMsg(tostring(k).."="..tostring(v).."\n")
end

function MakeTable(...)
    local t={}
    local args = {...}
    for i=1,#args do
        t[args[i]] = nilvalue()
    end
    return t
end

function MakeBoolFalseTable(...)
    local t={}
    local args = {...}
    for i=1,#args do
        t[args[i]] = false
    end
    return t
end

-- Returns true if any is true
function any(t)
    for k,v in pairs(t) do
        if v == true then
            return true
        end
    end
    return false 
end

function all(t)
    for k,v in pairs(t) do
        if v == false then
            return false
        end
    end
    return true
end



