-- @description Velocity Histogram MU
-- @author signoc (Sigge Eriksson)
-- @links
--    Author URI https://forum.cockos.com/member.php?u=10082
-- @version 0.9.1-beta
-- @changelog
--    Stopped actions that need at least 2 notes selected from starting.
--    Implemented a check for invalid integers that exits with error message
--    instead of crash. Please report if that happens. Making this check fool proof
--    is a larger refactor, so now this check that is performed for example
--    before notes are updated.
-- @provides
--    [main=midi_editor]signoc_velocity_histogram_mu.lua 
--    [nomain]../library_mu.lua
-- @donation https://www.paypal.com/donate/?hosted_button_id=FARB5QU9C8VT8
-- @about
--    # Beta
--
--    Consider the script to be in beta state. There is some unknown behaviour that I have 
--    trouble to replicate that changes the midi events somehow.
--    The current scripts tries to detect this when reading the events and compares
--    the analyzed data to the original data. If there is a difference an error should occur.
--    Also if there is a difference in expected message length when on note on/off the
--    script tries to detect this and throw an error.
--
--    # Velocity Histogram
--
--    Velocity histogram is a utility to deal with on and off velocity. It displays a histogram
--    of the selected notes velocitues in the main section of its interface. Far left is 
--    velocity = 1, and far right is velocity = 127. If the velocity area is hovered then
--    it displays the notes velocitues and relative position as circles/dots over the histogram.
--
--    The main section(histogram) can be activeted by clicking and holding the left button. 
--    Moving the mouse up/down offsets the velocities in either direction. Moving left/right
--    scales the velocities around the mean.
--
--    Buttons with double movement, thats is left/right and up/down can be locked to single
--    axis movement by holding shift and dragging in the desired direction.
--    holding ctrl will scale the influence of the movement allowing for finer adjustments
--
--    ## Tilt button
--
--    Press and hold left mouse button to activate.
--
--    Left/right movement - tilts/angles the velocities from the center of the selection.
--    Up/down - Offset velocities. This is useful if the tilting hits the bottom or top
--
--    ## Ramp
--
--    Ramp takes the lowest velocity at the start and the highest velocity at the end
--    and creates a linear interpolation between them ramping up the velocities if
--    you click the left mouse button.
--
--    Press and hold the left button and drag up/down to change the ramp curvature.
--
--    ## Chaos
--
--    This function adds random offsets to each note.
--
--    Press and hold left mouse button to activate.
--    Make a  new random series by clicking right button while holding left button pressed.
--
--    Up/Down movement to scale the offsets added.
--
--    ## Straighten
--
--    Press and hold left mouse button and drag left/right to scale velocities away or towards the
--    trend line of the selected velocities.
--
--    If you have an overall ramping trend but to much "chaos" in the velocities this helps
--    scaling it down while maintaining the overall ramp.
--
--    ## Shift  < > 
--
--    The shift button shift note velocities to previous or next note. 
--    This function may be replaced, and was added to utilise the space since the 
--    development of another function was put on hold.
--
--    ## On or Off
--    
--    Switch between note on velocity and note off velocity.
--

local library = table.concat({reaper.GetResourcePath(),"Scripts","signoc_scripts","library.lua"},package.config:sub(1,1))
dofile(library)


-- get the package path to MIDIUtils in my repository
package.path = reaper.GetResourcePath() .. '/Scripts/sockmonkey72 Scripts/MIDI/?.lua'
local mu = require 'MIDIUtils'

-- true by default, enabling argument type-checking, turn off for 'production' code
-- mu.ENFORCE_ARGS = false -- or use mu.MIDI_InitializeTake(), see below

-- enable overlap correction
mu.CORRECT_OVERLAPS = false
-- by default, note-ons take precedence when calculating overlap correction
-- turn this on to favor selected notes' note-off instead
-- mu.CORRECT_OVERLAPS_FAVOR_SELECTION = true

-- return early if something is missing (currently no dependencies)
if not mu.CheckDependencies('My Script') then return end


--------------------------------------------------------------------------------------------------------------
-- All app vars
--------------------------------------------------------------------------------------------------------------

local theme_colors = LockKeys({
        window_bg=0xdcdedeff, -- themecol("genlist_bg"),
        button=0xf0f0f0ff, --themecol("genlist_seliabg"),
        button_txt=0x000000ff, --themecol("genlist_seliafg"),
        dblbutton_text_on=0xffffffff,
        dblbutton_on = 0x339887ff,
        dblbutton_on_hover = 0x66cbbaff,
        dblbutton_on_hover_txt = 0xffffffff,
        button_active=0xffffffff, --themecol("genlist_selfg"),
        button_hovered=0xffffffff ,-- themecol("genlist_selfg"),
        histogram_bars_inactive=0x339887ff, --themecol("midi_editcurs"),
        histogram_bars=0xff,--0x3398878f, --themecol("midi_editcurs"),
        histogram_bg=0x333333ff, -- , themecol("col_main_bg2"),
        histogram_txt=0x000000ff, --themecol("genlist_seliafg"),
        title_bg=0x333333ff, -- themecol("col_main_bg2"),
        title_bg_txt= 0xabb1b1ff,  -- themecol("col_main_bg"),
        title_bg_active=0xabb1b1ff, --themecol("col_main_bg"),
        title_bg_active_txt=0x333333ff, --themecol("col_main_bg2"),
        active_velocity = 0x44a998ff,
        active_velocity_outline = 0xff,
    })   
--cout(string.format("0x%x",theme_darker3.title_bg_active_txt & 0xFFFFFFFF))
local all_vars = LockKeys({
    window_id="SIGNOC"..random_string(12), 
    colors=theme_colors,
    -- reaper 
    environment=nilvalue(),
    extstate_section="signoc velocity histogram",
    -- imgui
    window_has_focus=true,
    ctx = nilvalue(),  
    font = {
        default = nilvalue(),
        monospace = nilvalue()
    },
    plot_type=nilvalue(),
    window_title = "Velocity Histogram",
    btn_width = 140,
    btn_height = 32,
    -- misc

    t_noteon_src = nilvalue(),
    t_noteoff_src = nilvalue(),
    t_note_src = nilvalue(),

    current_type="on",

    statistics = LockKeys({
        start_ppq = 0,      -- First ppq_pos in selection
        end_ppq   = 0,      -- Last_ppq in pos
        mean = 0,       -- working mean
        min = 0,        -- No need to have midi variants, since they only need clamping.
        max = 0,        -- No need to have midi variants, since they only need clamping.
        midi_mean = 0   -- mean in the displayed data, 
    }),
    
    gui = LockKeys({
        velocity_histogram= nilvalue(),
        action_button_tilt = nilvalue(),
        action_button_straighten = nilvalue(),
        action_button_ramp = nilvalue(),
        action_button_chaos = nilvalue(),
        action_dblbutton_onoff = nilvalue(),
        action_dblbutton_shift = nilvalue(),
    })
    
})

function get_reaper_environment(env) 
    local changed = LockKeys(MakeBoolFalseTable("midieditor","take","mediaitem","itemname","trackname","midihash"))
    
    local t = LockKeys(MakeTable("midieditor","take","mediaitem","itemname","trackname","midihash","changed"))
    t.changed = changed
    if env == nil or env == nilvalue() then
        env = LockKeys(MakeTable("midieditor","take","mediaitem","itemname","trackname","midihash","changed"))
    end
    
    t.midieditor = nilencode(reaper.MIDIEditor_GetActive())
    
    changed.midieditor = (t.midieditor ~= env.midieditor)
     if t.midieditor == nilvalue() then
        return false, t
    end
    
    t.take = nilencode(reaper.MIDIEditor_GetTake( t.midieditor))
    changed.take = (t.take ~= env.take)

    if t.take == nilvalue() then
        return false, t
    end
    
      -- acquire events from take (can pass true/false as 2nd arg to enable/disable ENFORCE_ARGS)
      -- you will want to do this once per defer cycle in persistent scripts to ensure that the
      -- internal state matches the current take state
    mu.MIDI_InitializeTake(t.take)
    
    
    t.mediaitem = nilencode(reaper.GetMediaItemTake_Item(t.take))
    changed.mediaitem = (t.mediaitem ~= env.mediaitem)

    if t.mediaitem == nilvalue() then 
        return false, t
    end

    local _, itemname  = reaper.GetSetMediaItemTakeInfo_String(t.take,"P_NAME","", false)
    local track = reaper.GetMediaItem_Track(t.mediaitem)
    local _ , trackname = reaper.GetTrackName(track)
    
    -- Names does not trigger changed, since it does not affect note buffer
    t.itemname = itemname
    t.trackname = trackname
    
    local _, mh = reaper.MIDI_GetHash(t.take, false)
    t.midihash = mh
    changed.midihash = (t.midihash ~= env.midihash)
    return true, t
end

function setup_imgui(vars)
    vars.ctx = reaper.ImGui_CreateContext("VELOCITY HISTOGRAM")
    vars.font.default =  reaper.ImGui_CreateFont('Arial', 16)
--    vars.font.monospace = reaper.ImGui_CreateFont('Consolas', 16)
--    vars.font.monospace = reaper.ImGui_CreateFont('Lucida Console', 16)
    vars.font.monospace = reaper.ImGui_CreateFont('Courier New', 16)
--  reaper.ShowConsoleMsg(tostring(vars.font.monospace));
--    vars.font.monospace = reaper.ImGui_CreateFont('Monaco', 16)
    reaper.ImGui_Attach(vars.ctx, vars.font.default) 
    reaper.ImGui_Attach(vars.ctx, vars.font.monospace) 
    vars.plot_type = reaper.GetExtState(vars.extstate_section, "type") 
    if vars.plot_type == '' then
        vars.plot_type = 'lines'
        reaper.SetExtState(vars.extstate_section, "type", "lines", true)
    end
end

--------------------------------------------------------------------------------------------------------------
-- Early setup
--------------------------------------------------------------------------------------------------------------

local rv, env, chng = get_reaper_environment()
if rv == false then
    -- Error this early can only mean no midieditor open or
    -- no take/was deleted, and that makes no sense since 
    -- this can only be activated from midieditor
    return
end

all_vars.environment = env

function Exit(vars)
    if vars.environment.midieditor ~= nilvalue() then reaper.BR_Win32_SetForegroundWindow(vars.environment.midieditor) end
end

reaper.atexit(function () Exit(all_vars) end)


function not_finite(value)
  local is_inf = (value == math.huge or value == -math.huge)
  local is_nan = (value ~= value)
  return is_inf or is_nan
end


--------------------------------------------------------------------------------------------------------------
-- MuNoteEvt
--------------------------------------------------------------------------------------------------------------
-- Holds on or off events depending on need
-- idx = index into the Note event table where on/off byte velocity is stored
-- ppq = running ppq position
-- vel = velocity 

MuNoteEvt = {}

function MuNoteEvt:new(idx, ppq, vel)
    local o = {
        idx = idx,
        ppq = ppq,
        vel = vel
    }
    setmetatable(o, self)
    self.__index = self
    return o
end

function MuNoteEvt:clone()
    return MuNoteEvt:new(self.idx, self.ppq, self.vel)
end

-- Returns velocity clamped(1-127) and rounded
function MuNoteEvt:get_midi_vel()
    if not_finite(self.vel) then
        error("Velocity is invalid (not a finite value)")
    end 
    if self.vel < 1 then
        return 1
    elseif self.vel > 127 then
        return 127
    else
        return math.floor(self.vel+0.5)
    end
end

--------------------------------------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------------------------------------

-- Set active velocity type to on velocity
function set_current_type_on(vars)
    vars.current_type = "on"
    vars.t_note_src = vars.t_noteon_src
end
function is_type_on(vars)
    return vars.current_type == "on"
end

-- Set active velocity type to off velocity(release velocity)
function set_current_type_off(vars)
    vars.current_type = "off"
    vars.t_note_src = vars.t_noteoff_src
end
function is_type_off(vars)
    return vars.current_type == "off"
end
----------------------
function MU_read_selected_notes(vars)
    local sel_on =  {}
    local sel_off = {}
    local take = vars.environment.take
    
    local current_index =  mu.MIDI_EnumSelNotes(take, -1);
    while current_index >= 0 do
        rv, selected, muted, ppqpos, endppqpos, chan, pitch, vel, relvel = mu.MIDI_GetNote(take, current_index)
        if selected then
          sel_on[#sel_on + 1] = MuNoteEvt:new(current_index, ppqpos, vel);
          sel_off[#sel_off + 1] = MuNoteEvt:new(current_index, ppqpos, relvel);
        end
        current_index =  mu.MIDI_EnumSelNotes(take, current_index);
    end 
    
    vars.t_noteon_src = sel_on
    vars.t_noteoff_src = sel_off    
end

-----------------------
function read_all_events(vars) 
    if vars.environment.take == nilvalue() then
        vars.t_noteon_src = {}
        vars.t_noteoff_src = {}
        vars.t_note_src = {}
        return
    end
    MU_read_selected_notes(vars)
    if is_type_on(vars) then
        set_current_type_on(vars)
    elseif is_type_off(vars) then
        set_current_type_off(vars)
    else
        error("Unknown current type")
    end
end

--------------------------------------------------------------------------------------------------------------
-- Note actions
--------------------------------------------------------------------------------------------------------------

--[[
f(t) = p1 + (1-t)^2(p0-p1)+t^2(p2-p1)
p2=(1,1) och p0=(0,0)

0<=k<=1 , 0.5 = linear
p1 = k, (-1k+1)
]]--
--[[
  X = current x-point on curve 0<=X<=1 where you want to find out Y for
--]]
-- gui_curve goes from -1 to 1 wheres real curve goes from 0 to 1
-- so gui curve of zero, is equal to real curve 0.5
-- which results in a division zero du to the curve being a
-- linear function. which in case that happends, Y=X
function prep_bezier(gui_curve)
    if gui_curve == 0 then 
        return nilvalue()
    end
    local curve = 0.5 + gui_curve/2.0
    local p1 = {curve, -curve+1}
    local x,y = p1[1],p1[2]
    local two_x = 2*x
    local t = 1.0 - two_x
    local p = two_x/t 
    local p_half = p/2.0
    local sign = 1
    if gui_curve > 0 then
        sign = -1
    end
    
    return LockKeys({
        t=t,
        p_half=p_half,
        p_half2=p_half*p_half,
        sign = sign,
        y=y
    })
end 

-- b = prep_bezier
function calc_y(X,b)
    if b == nilvalue() then
        return X
    end
    local q = -X/b.t
    local t = -b.p_half + b.sign*math.sqrt(b.p_half2-q)
    local y = b.y
    y = y+((1.0-t)^2*(-y))+((t^2)*(1-y))
    return y
end

--------------------------------------------------------------------------------------------------------------
-- Misc functions
--------------------------------------------------------------------------------------------------------------
function MU_update_selected_notes(vars)
    local sel_on = vars.t_noteon_src
    local sel_off = vars.t_noteoff_src 
    local take = vars.environment.take
    
    mu.MIDI_OpenWriteTransaction(take)
    for i=1, #sel_on do
        local idx = sel_on[i].idx
        local vel = sel_on[i]:get_midi_vel();
        local relvel = sel_off[i]:get_midi_vel();
        local rv = mu.MIDI_SetNote(take, idx, nil, nil, nil, nil, nil, nil, vel, relvel)
    end
    -- commit the transaction to the take
    --   by default, this won't reacquire the MIDI events and update the
    --   take data in memory, pass 'true' as a 2nd argument if you want that
    mu.MIDI_CommitWriteTransaction(take)
    reaper.MarkTrackItemsDirty(
       reaper.GetMediaItemTake_Track(take),
       reaper.GetMediaItemTake_Item(take))
end

function update_all_events(vars)
    MU_update_selected_notes(vars)
end

function clone_notes(notes)
    local t = {}
    for i=1,#notes do
        t[i] = notes[i]:clone()
    end
    return t
end

function stats_tostring(o)
    return string.format(
          "min=% 9.4f, max=% 9.4f, mean=% 9.4f, midi_mean=%3i, start_ppq=%010d, end_ppq=%010d",
          o.min,o.max, o.mean, o.midi_mean, o.start_ppq, o.end_ppq
          )
end

function calculate_note_statistics(notes)
    local t=MakeTable("min","max", "mean","midi_mean","start_ppq", "end_ppq", "mean_ppq","stddev_ppq", "stddev_vel", "regressable")
    setmetatable(t,{__tostring=stats_tostring})
    LockKeys(t)
    
    if #notes == 0 then
        return t
    end
    
    t.regressable = true
    
    local n = notes[1]
    local mi = n.vel
    local mx = mi
    local sum = mi
    local midi_sum = n:get_midi_vel()
    local pmi = n.ppq
    local pmx = n.ppq
    local ppq_sum = n.ppq
    
    for i=2,#notes do
        n = notes[i]
        sum = sum + n.vel
        midi_sum = midi_sum + n:get_midi_vel()
        ppq_sum = ppq_sum + n.ppq
        if n.vel < mi then
            mi = n.vel
        elseif n.vel > mx then
            mx = n.vel
        end
        
        if n.ppq < pmi then
            pmi = n.ppq
        elseif n.ppq > pmx then
            pmx = n.ppq
        end
    end
    
    t.min = mi
    t.max = mx
    t.mean = sum/#notes
    t.midi_mean = math.floor(midi_sum/#notes +0.5)
    t.start_ppq = pmi
    t.end_ppq = pmx
    t.mean_ppq = ppq_sum/#notes
    
    if #notes > 1 then
        local stddev_ppq = 0
        local stddev_vel = 0
        local q = 0
        for i =1, #notes do
            q = notes[i].ppq - t.mean_ppq
            stddev_ppq = stddev_ppq + q*q
            q = notes[i].vel - t.mean
            stddev_vel = stddev_vel + q*q
        end
        t.stddev_ppq = math.sqrt(stddev_ppq/(#notes -1))
        t.stddev_vel = math.sqrt(stddev_vel/(#notes -1))
        if t.stddev_ppq == 0 or t.stddev_vel == 0 then
          t.regressable = false
        else
            t.regressable = true
        end 
    end
    return t
end
--------------------------------------------------------------------------------------------------------------
-- MouseDragger
--------------------------------------------------------------------------------------------------------------

MouseDragger = {}

function MouseDragger:new()
    local o = {
        x = 0,
        y = 0,
        lock = false,
        dir=0,
        update=self.update,
        reset=self.reset,
        detect_dir = false,
        detect_dir_x = 0,
        detect_dir_y = 0,
    }
    setmetatable(o, self)
    self.__index = self    
    LockKeys(o)
    return o
end

function MouseDragger:update(ctx)
    local dx,dy = reaper.ImGui_GetMouseDelta(ctx)
    if reaper.ImGui_IsKeyDown(ctx,reaper.ImGui_Mod_Shift()) then
        if not self.lock then
            if not self.detect_dir then
                self.detect_dir = true
                detect_dir_x = dx
                detect_dir_y = dy
            else
                detect_dir_x = detect_dir_x + dx
                detect_dir_y =detect_dir_y + dy
            end
            local x = math.abs(detect_dir_x)
            local y = math.abs(detect_dir_y)
            
            if (x-y) > 16 then
                self.dir=0
                self.lock = true
                self.detect_dir=false
            elseif (y-x) > 16 then
                self.dir = 1
                self.lock = true
                self.detect_dir=false
            end
        end
    else
        self.lock = false
        self.detect_dir=false
    end 
    
    if not self.detect_dir then
        if reaper.ImGui_IsKeyDown(ctx, reaper.ImGui_Mod_Ctrl()) then
            dx = dx/5.0
            dy = dy/5.0
        end
        if not self.lock then
            self.x = self.x + dx 
            self.y = self.y + dy
        elseif self.dir == 0 then
            self.x = self.x + dx
        elseif self.dir == 1 then
            self.y = self.y + dy
        else
            error("Illegal value in lock direction")
        end

    end
end

function MouseDragger:reset()
    self.x = 0
    self.y = 0
    self.lock = false
end

--------------------------------------------------------------------------------------------------------------
-- ActionButton
--------------------------------------------------------------------------------------------------------------

ActionButton = {}

function ActionButton:new()
    local o = {
        id = random_id("SIGNOCACTIONBUTTON"), 
        text_callback = self.text_callback,
        callback = self.callback,
        is_active = false,
        is_hovered = false,
        is_left_clicked = true,
        is_right_clicked = true, 
        action_active = false,
    }
    setmetatable(o, self)
    self.__index = self    
    return o
end

function ActionButton:render(vars)
    local txt=""
    if nilencode(self.text_callback) == nilvalue() then
        txt ="NO CALLBACK"
    else
        txt=self:text_callback(vars)
    end
    txt = txt..'###'..self.id
    reaper.ImGui_PushStyleColor(vars.ctx, reaper.ImGui_Col_Text(), vars.colors.button_txt)
    reaper.ImGui_PushStyleVar(vars.ctx, reaper.ImGui_StyleVar_FrameBorderSize(),1.0)
    reaper.ImGui_PushStyleVar(vars.ctx, reaper.ImGui_StyleVar_FrameRounding(),6.0)

    reaper.ImGui_Button(vars.ctx, txt, vars.btn_width, vars.btn_height)
    reaper.ImGui_PopStyleVar(vars.ctx,2)
    reaper.ImGui_PopStyleColor(vars.ctx)
    self.is_active = reaper.ImGui_IsItemActive(vars.ctx)
    self.is_hovered = reaper.ImGui_IsItemHovered(vars.ctx)
    self.is_left_clicked = reaper.ImGui_IsItemClicked(vars.ctx, reaper.ImGui_MouseButton_Left())
    self.is_right_clicked = reaper.ImGui_IsItemClicked(vars.ctx, reaper.ImGui_MouseButton_Right())
    
    if nilencode(self.callback) ~= nilvalue() then
        if #vars.t_note_src > 0 then 
            local changed = self:callback(vars)
            if changed then
                local _, mh = reaper.MIDI_GetHash(vars.environment.take, false)
                vars.environment.midihash = mh 
            end
        end
    end
end

--------------------------------------------------------------------------------------------------------------
-- ActionChaos
--------------------------------------------------------------------------------------------------------------

ActionChaos = {}

function ActionChaos:new()
    local o = ActionButton:new()
    o.text_callback = self.text_callback
    o.callback = self.callback
    o.inflict_chaos = self.inflict_chaos
    o.value = 0
    o.start_notes = {}
    o.seed = os.time()+math.random()*10000.0
    return o
end

function ActionChaos:text_callback(vars)
    if self.action_active then
        return string.format("Amount %7.2f", self.value*100)
    else
        return string.format("Chaos")
    end
end

function ActionChaos:inflict_chaos(src,dst, seed, scale)
    math.randomseed(seed)
    local r = math.random
    for i=1, #src do
        dst[i].vel = src[i].vel + 2.0*(r()-0.5)*scale*16.0
    end
end

function ActionChaos:callback(vars)

    if self.is_active then
        if not self.action_active then
            self.action_active = true
            self.start_notes = clone_notes(vars.t_note_src)
            self.seed = os.time() + math.random()*10000.0

        end

        if reaper.ImGui_IsMouseClicked(vars.ctx, reaper.ImGui_MouseButton_Right()) then
            self.seed = os.time() + math.random()*10000.0
        end
        
        local dragx, dragy = reaper.ImGui_GetMouseDragDelta(vars.ctx,0, 0,nil, 32)
        self.value = 0.0 + dragx/200
        self:inflict_chaos(self.start_notes,vars.t_note_src, self.seed, self.value)
        vars.statistics = calculate_note_statistics(vars.t_note_src)
        update_all_events(vars)
        
        return true
    else
        if self.action_active then
            read_all_events(vars)
            vars.statistics = calculate_note_statistics(vars.t_note_src)
            reaper.Undo_OnStateChange_Item(0, "Velocity chaos", vars.environment.mediaitem)
        end
        self.action_active = false
        self.value = 0.0
        self.start_notes = {}
        return false
    end
end

--------------------------------------------------------------------------------------------------------------
-- ActionStraighten
--------------------------------------------------------------------------------------------------------------

ActionStraighten = {}

function ActionStraighten:new()
    local o = ActionButton:new()
    o.text_callback = self.text_callback
    o.callback = self.callback
    o.do_regression = self.do_regression
    o.value = 0
    o.start_notes = {}
    o.dragger = MouseDragger:new()
    o.e_y = {}
    o.m = {}
    o.b = {}
    o.xm = 0
    o.s_e = 1   -- Scale factor for residuals
    o.m_t = 0   -- Tilt factor for regressione line along data middle point
    o.valid_regression = true
    return o
end

function ActionStraighten:text_callback(vars)
    if self.action_active then
        return string.format("Amount%7.1f%%", (1-self.value)*100)
    else
        return string.format("Straighten")
    end
end

function ActionStraighten:do_regression(vars, s_e)
    local src = self.start_notes
    local dst = vars.t_note_src
    local ey = self.e_y
    local b = self.b
    local m_t = self.m_t
    local xm = self.xm
    local m = self.m
    -- Since the note positions is static we can use current statistics
    local range = vars.statistics.end_ppq - vars.statistics.start_ppq 
    m_t = m_t/range

    for i=1, #src do
        dst[i].vel = src[i].ppq*m + b + m_t*(src[i].ppq - xm) + ey[i]*s_e
    end

end
function ActionStraighten:callback(vars)
    
    if self.is_active then
        if #vars.t_note_src < 2 then
            self.action_active = true
            return false
        end
        

        if not self.action_active then

            self.dragger:reset()
            self.action_active = true
            self.start_notes = clone_notes(vars.t_note_src)

            -- Calculate regression line ------------------------
            -- Calculate stddev
            local ey = {}
            local ex = {}
            local sy = 0
            local sx = 0
            local ym = vars.statistics.mean
            local xm = vars.statistics.mean_ppq 

            local src = self.start_notes
            
            --local a1 = {}
            local a2 = {}
            local a3 = {}
            
            local numerator = 0 
            local denominator = 0
            local x
            local y
            for i=1, #src do
                x = (src[i].ppq-xm)
                y = (src[i].vel-ym)
                numerator = numerator + x*y
                a2[i] = x
                a3[i] = y

                sy = sy + (y*y)
                sx = sx + (x*x)
            end 
         
            sx = math.sqrt(sx/(#src-1))
            sy = math.sqrt(sy/(#src-1))

            if sx == 0 then
                self.valid_regression = false
            elseif sy == 0 then
                self.m = 0
                self.xm = xm
                self.b = ym
                self.valid_regression = true
                for i=1, #src do
                    ey[i] = 0
                end
                self.e_y = ey 
            else
                self.valid_regression = true
                local r_xy = 0
                for i=1, #src do
                    r_xy = r_xy + (a2[i]/sx)*(a3[i]/sy)
                end
                -- Correlation formula
                -- http://www.stat.yale.edu/Courses/1997-98/101/correl.htm
                r_xy = r_xy * (1/(#src -1))
    
                -- calculate slope 
                self.m = r_xy * (sy/sx)
                
                -- calculate b
                self.b = ym - self.m*xm
                
                -- Calculate residials
                for i=1, #src do
                    local pred = src[i].ppq*self.m + self.b
                    ey[i] = src[i].vel - pred
                end
                self.e_y = ey
                
                self.s_e = 1   -- Scale factor for residuals
                self.m_t = 0   -- Tilt factor for regressione line along data middle point
                self.xm = xm
            end

        end

        self.dragger:update(vars.ctx)
        self.value = 1.0 - self.dragger.x/1000.0
        if self.value < 0 then
            self.value = 0
        end

        self.m_t = 0 --+ self.dragger.y/2.0

        if self.valid_regression then
            self:do_regression(vars, self.value)
            vars.statistics = calculate_note_statistics(vars.t_note_src)
            update_all_events(vars)
        end
        
        return true
    else
        if self.action_active then
            read_all_events(vars)
            vars.statistics = calculate_note_statistics(vars.t_note_src)
            reaper.Undo_OnStateChange_Item(0, "Velocity straighten", vars.environment.mediaitem)
        end
        self.action_active = false
        self.value = 0.0
        self.start_notes = {}
        self.valid_regression = false
        return false
    end
    
    
    return false
end

--------------------------------------------------------------------------------------------------------------
-- ActionTilt
--------------------------------------------------------------------------------------------------------------

ActionTilt = {}

function ActionTilt:new()
    local o = ActionButton:new()
    o.text_callback = self.text_callback
    o.callback = self.callback
    o.do_tilt = self.do_tilt
    o.value = 0
    o.offset = 0 
    o.start_notes = {}
    o.start_stats = {}
    o.dragger = MouseDragger:new()
    return o
end

function ActionTilt:text_callback(vars)
    if self.action_active then
        local l = (vars.statistics.end_ppq - vars.statistics.start_ppq)
        local a = math.atan(self.value)
        a = a*180/math.pi
        return string.format("Tilt %7.2f", a)
    else
        return string.format("Tilt")
    end
end

function ActionTilt:do_tilt (src,dst, stats, tilt, offset)
--[[
  Tilt velocity(on/off). The tilt-factor is the slope of the line that and the lines
  origin is in the center of the mininmum and maximum ppq och the used events.
--]]
    local m = (stats.end_ppq+ stats.start_ppq)/2.0  -- middle time
    for i=1,#src do
        dst[i].vel = src[i].vel + (src[i].ppq - m) * tilt + offset
    end
end


function ActionTilt:callback(vars)

    if self.is_active then

        if not self.action_active then
            self.action_active = true
            self.start_notes = clone_notes(vars.t_note_src)
            self.start_stats = vars.statistics
            self.dragger:reset()
            self.value = 0
            self.offset = 0
        end

        self.dragger:update(vars.ctx)
        local len = (vars.statistics.end_ppq - vars.statistics.start_ppq)
        if len > 0 then
            self.value = -self.dragger.x/len
            self.offset = -self.dragger.y/4.0
            self:do_tilt(self.start_notes,vars.t_note_src, self.start_stats, self.value, self.offset)            
            vars.statistics = calculate_note_statistics(vars.t_note_src)
            update_all_events(vars)
            return true
        else
            return false
        end
    else
        if self.action_active then
            read_all_events(vars)
            vars.statistics = calculate_note_statistics(vars.t_note_src)
            reaper.Undo_OnStateChange_Item(0, "Velocity tilt", vars.environment.mediaitem)
        end
        self.action_active = false
        self.value = 0.0
        self.start_notes = {}
        return false
    end
end

--------------------------------------------------------------------------------------------------------------
-- ActionRamp
--------------------------------------------------------------------------------------------------------------

ActionRamp = {}

function ActionRamp:new()
    local o = ActionButton:new()
    o.text_callback = self.text_callback
    o.callback = self.callback
    o.value = 0
    o.start_notes = {}
    o.start_stats = {}
    o.gui_curve = 0
    o.do_ramp = self.do_ramp
    return o
end

function ActionRamp:text_callback(vars)
    if self.action_active then
        return string.format("Curve %5.2f", self.gui_curve)
    else
        return string.format("Ramp")
    end
end

function ActionRamp:do_ramp(src, dst, gui_curve)
    local b = prep_bezier(gui_curve)
    local tab = {}
    local keys = {}
    local ppq=0
    for i=1, #src do
        ppq = src[i].ppq
        if tab[ppq] ==  nil then
            tab[ppq] = {src[i]}
            keys[#keys +1] = ppq
        else
            table.insert(tab[ppq],src[i])
        end
    end
    table.sort(keys)
    local s=0
    local e=0
    local w=0
    local n=0
    
    local s_ppq = keys[1] -- earliest time
    local e_ppq = keys[#keys] -- last time
    local w_ppq = e_ppq - s_ppq  -- for scaling ppq to 0-1 range
    local start_vel = 127
    local t = tab[s_ppq]
    for i=1,#t do
        if t[i].vel < start_vel then
          start_vel = t[i].vel
        end
    end
    
    local t = tab[e_ppq]
    local end_vel = 1
    for i=1,#t do
        if t[i].vel > end_vel then
          end_vel = t[i].vel
        end
    end
    
    -- Use the lowest velocity from start ppq, and the heighest from the end ppq.
    local height = end_vel-start_vel
    local X
    for i=1, #keys do
        ppq = keys[i]
        X = (ppq-s_ppq)/w_ppq   -- normalized ppq position
        local k = tab[ppq]
        local new_vel =  start_vel + calc_y(X, b)*height
        for j=1, #k do
            k[j].vel = new_vel
        end
    end
    for i=1, #src do
      dst[i].vel = src[i].vel
    end
end


function ActionRamp:callback(vars)
    if self.is_active then
        if #vars.t_note_src < 2 then
            self.action_active = false
            return false
        end
        if not self.action_active then
            self.action_active = true
            self.start_notes = clone_notes(vars.t_note_src)
            self.start_stats = vars.statistics
        end
        local dragx, dragy = reaper.ImGui_GetMouseDragDelta(vars.ctx,0, 0,nil, 32)
        self.value = clamp(0.0 + dragx/200,-1,1)
        self.gui_curve = self.value

        local notes =  clone_notes(self.start_notes)
        --action_ramp(notes, self.gui_curve)
        self:do_ramp(notes, vars.t_note_src, self.gui_curve)
--        vars.t_note_src = notes
        vars.statistics = calculate_note_statistics(vars.t_note_src)
        update_all_events(vars)
        return true
    
    else
        if self.action_active then
            read_all_events(vars)
            vars.statistics = calculate_note_statistics(vars.t_note_src)
            reaper.Undo_OnStateChange_Item(0, "Velocity ramp", vars.environment.mediaitem)
        end
        self.action_active = false
        self.value = 0.0
        self.start_notes = {}
        return false
    end
end

------------------------------------------------------------------------------------------------------------------------
-- misc
------------------------------------------------------------------------------------------------------------------------

function missing_take(env)
    return env.take == nilvalue()
end

function midieditor_closed(env)
    return env.midieditor == nilvalue()
end

------------------------------------------------------------------------------------------------------------------------
-- VelocityHistogram
------------------------------------------------------------------------------------------------------------------------

VelocityHistogram = {}

function VelocityHistogram:new()
    local o = {
        id = random_id("SIGNOCVELHIST"), 
        is_active = false,
        is_hovered = false,
        action_active = false,
        value = 0,
        start_notes = {},
        scale_start_stats = {},
        scale_value = 1,
        render = self.render,
        main_area_active = false,
        dragger = MouseDragger:new(),
        data = reaper.new_array(127),
        -- drawing stuff.
        dlist = nilvalue(),
        x = 0,
        y = 0,
        cw = 0,
        ch = 0,
        histo_width = 0,
        histo_height = 127,
        bgcol = 0,
        histcol = 0,
        px = 5,   -- Padding
        py = 3,   -- Padding
        has_hovered_or_active_components = false,
        
    }
    setmetatable(o, self)
    self.__index = self    
    return o
end

function VelocityHistogram:offset_and_scale (src, dst, offset, scale, stats)
    for i=1, #src do
        dst[i].vel = (src[i].vel - stats.mean)*scale + stats.mean + offset
    end
end

function VelocityHistogram:clean_histogram_array()
    local h = self.data
    for i=1, #h do
        h[i] = 0.0
    end
end

function VelocityHistogram:update_histogram_data(vars)
    self:clean_histogram_array()
    local n = vars.t_note_src
    local h = self.data
    local v=0
    for k=1, #n do
        v = n[k]:get_midi_vel()
        h[v] = h[v] + 1
    end
end 
function VelocityHistogram:setup_drawing(vars)
    self.dlist = reaper.ImGui_GetWindowDrawList(vars.ctx)
    self.x,self.y = reaper.ImGui_GetCursorScreenPos(vars.ctx)
    self.cw,self.ch = reaper.ImGui_GetContentRegionAvail(vars.ctx)
    self.histo_width = self.cw
    self.histo_height = 127     -- Histogram height
    self.bgcol = vars.colors.histogram_bg
    self.px = 5   -- Padding
    self.py = 3   -- Padding
    
    -- check if anything is active
    self.has_hovered_or_active_components = (
        vars.gui.action_button_tilt.is_hovered 
        or vars.gui.action_button_tilt.is_active
        or vars.gui.action_button_straighten.is_hovered
        or vars.gui.action_button_straighten.is_active
        or vars.gui.action_button_ramp.is_hovered
        or vars.gui.action_button_ramp.is_active  
        or vars.gui.action_button_chaos.is_hovered
        or vars.gui.action_button_chaos.is_active
--        or vars.gui.action_dblbutton_onoff:is_any_hovered()
        or vars.gui.action_dblbutton_shift:is_any_hovered()
        or self.action_active
        or self.is_hovered
    )
    
    -- setup colors
    if self.has_hovered_or_active_components then
        self.histcol =  vars.colors.histogram_bars
    else
        self.histcol = vars.colors.histogram_bars_inactive    
    end  

end
function VelocityHistogram:draw_histogram(vars)
    -- Histogram background
    reaper.ImGui_DrawList_AddRectFilled(
        self.dlist,
        self.x,
        self.y,
        self.x+self.histo_width,
        self.y+self.histo_height,
        self.bgcol
    )

    local data = self.data
    -- Histogram bars ----------------------------------------------------------------------------------------
    
    for i=1,127 do
        local c = data[i]
        if c > 0 then
            local x1 =  math.floor(self.histo_width*(i-1)/127.0+0.5)
            local x2 = math.floor(self.histo_width*(i)/127.0+0.5)
            if c == 1 then
                c = 0.25
            elseif c == 2 then
                c = 0.5
            elseif c == 3 then
                c = 0.75
            elseif c > 3 then
                c = 1.0
            end 
            local b= (self.histo_height*c)
            reaper.ImGui_DrawList_AddRectFilled(
                self.dlist,
                self.x + x1,
                self.y + self.histo_height - b,
                self.x + x2,
                self.y + self.histo_height,
                self.histcol
            )
        end
    end 
end

function VelocityHistogram:draw_text(vars)
    -- Calculate text ----------------------------------------------------------------------------------------

    -- Statistics text
    local mi_txt = '---'
    local mx_txt = '---'
    local mn_txt = '---'

    if vars.statistics.min ~= nilvalue() then
        mi = math.floor(vars.statistics.min+0.5)
        mx = math.floor(vars.statistics.max+0.5)
        mn = math.floor(vars.statistics.midi_mean+0.5)
        mi = clamp(mi,1,127)
        mx = clamp(mx,1,127)
        mn = clamp(mn,1,127)
        mi_txt=string.format("Min %3i",mi)
        mx_txt=string.format("Max %3i",mx)
        mn_txt=string.format("Mean %3i", mn)
    end

    -- Calculate text location below histogram -------------------------------------------------------------------------
    local mi_txt_w, mi_txt_h = reaper.ImGui_CalcTextSize(vars.ctx, mi_txt, 200,self.histo_height)
    local mn_txt_w, mn_txt_h = reaper.ImGui_CalcTextSize(vars.ctx, mn_txt, 200,self.histo_height)
    local mx_txt_w, mx_txt_h = reaper.ImGui_CalcTextSize(vars.ctx, mx_txt, 200,self.histo_height)
    
    -- px, py = padding
    local mi_txt_x = self.px
    local mi_txt_y = self.py

    local mn_txt_x = (self.histo_width - mn_txt_w)/2
    local mn_txt_y = self.py
    
    local mx_txt_x = self.histo_width-mx_txt_w-self.px
    local mx_txt_y = self.py

    xpadd, ypadd = reaper.ImGui_GetStyleVar(vars.ctx, reaper.ImGui_StyleVar_ItemSpacing() )
    reaper.ImGui_PushStyleVar(vars.ctx, reaper.ImGui_StyleVar_ItemSpacing(),0,0)
    
    -- Check if histogram area is activated
    reaper.ImGui_InvisibleButton(vars.ctx, self.id, self.histo_width,self.histo_height)
    self.is_hovered = reaper.ImGui_IsItemHovered(vars.ctx)
    self.main_area_active = reaper.ImGui_IsItemActive(vars.ctx)
    
    do
        -- draw min/mean/max text 
        local py = self.py + self.py
        local x,y = reaper.ImGui_GetCursorScreenPos(vars.ctx)
        local cw,ch = reaper.ImGui_GetContentRegionAvail(vars.ctx)
        local mi_txt_x = self.px
        local mi_txt_y = py
    
        local mn_txt_x = (self.histo_width - mn_txt_w)/2
        local mn_txt_y = py
        
        local mx_txt_x = self.histo_width-mx_txt_w-self.px
        local mx_txt_y = py

        reaper.ImGui_DrawList_AddText(self.dlist, x+mi_txt_x, y+mi_txt_y, vars.colors.histogram_txt, mi_txt)
        reaper.ImGui_DrawList_AddText(self.dlist, x+mn_txt_x, y+mn_txt_y, vars.colors.histogram_txt, mn_txt)
        reaper.ImGui_DrawList_AddText(self.dlist, x+mx_txt_x, y+mx_txt_y, vars.colors.histogram_txt, mx_txt)
        reaper.ImGui_InvisibleButton(vars.ctx, self.id.."histo_stats", self.histo_width, mi_txt_h+py*2)
        
    end
    reaper.ImGui_PopStyleVar(vars.ctx)

end

function VelocityHistogram:draw_velocities(vars)
    -- Draw notes --------------------------------------------------------------------------------------------
    local r = 8 -- outline radius
    local pr = r/2 -- point radius
    local rr = r+r 
    local s = 12 --segments
    if #vars.t_note_src > 0  then
        local s = vars.statistics.start_ppq
        local l = vars.statistics.end_ppq - vars.statistics.start_ppq

        for i=1, #vars.t_note_src do
            local xx = r+(self.histo_width-rr)*(vars.t_note_src[i].ppq-s)/l
            local yy = 127-r-(self.histo_height-rr)*vars.t_note_src[i]:get_midi_vel()/127
            if self.has_hovered_or_active_components then 
                reaper.ImGui_DrawList_AddCircleFilled(self.dlist,self.x+xx,self.y+yy,r, vars.colors.active_velocity_outline, s)
            end
        end
        for i=1, #vars.t_note_src do
            local xx = r+(self.histo_width-rr)*(vars.t_note_src[i].ppq-s)/l
            local yy = 127-r-(self.histo_height-rr)*vars.t_note_src[i]:get_midi_vel()/127
            if self.has_hovered_or_active_components then
                reaper.ImGui_DrawList_AddCircleFilled(self.dlist,self.x+xx, self.y+yy, pr, vars.colors.active_velocity, s)
            end
        end
    end
end

function VelocityHistogram:render(vars)
    self:update_histogram_data(vars)
    self:setup_drawing(vars)
    self:draw_histogram(vars)
    self:draw_text(vars)
    self:draw_velocities(vars)
    
    -- Interactive -------------------------------------------------------------------------------------------
    
    if  self.main_area_active then
        if not self.action_active then
            -- First time activated
            self.action_active = true
            self.start_notes = clone_notes(vars.t_note_src)
            self.scale_start_stats = vars.statistics
        end
        self.dragger:update(vars.ctx)

        -- Velocity offset
        self.value = math.floor(-self.dragger.y/5.0+0.5)
        
        -- Velocity range scale
        self.scale_value = 1.0+self.dragger.x*0.01
        
        -- Calculate new velocities
        self:offset_and_scale(self.start_notes, vars.t_note_src,self.value,self.scale_value, self.scale_start_stats)

        vars.statistics = calculate_note_statistics(vars.t_note_src)
        
        update_all_events(vars)
        
        -- Calculate hash so we can now if update happends outside our control
        local _, mh = reaper.MIDI_GetHash(vars.environment.take, false)
        
        vars.environment.midihash = mh         
        
    else
        -- Histogram is not active
        if self.action_active then
            -- Was active before
            read_all_events(vars)
            vars.statistics = calculate_note_statistics(vars.t_note_src)
            self.action_active = false
            self.dragger:reset()
            reaper.Undo_OnStateChange_Item(0, "Velocity offset & scale", vars.environment.mediaitem)
        end
        self.value = 0
        self.start_notes = {}
    end
end

--------------------------------------------------------------------------------------------------------------
-- DblButton
--------------------------------------------------------------------------------------------------------------

DblButton = {}

function DblButton:new()
    local o = {
        id = random_id("SIGNOCDBLBUTTON"), 
        left_callback = self.left_callback,
        left_text_callback = self.left_text_callback,
        left_is_on = false,
        left_is_active = false,
        left_is_hovered = false, 
        left_is_clicked = false,
        right_callback = self.right_callback,
        right_text_callback = self.right_text_callback,
        right_is_active = false,
        right_is_hovered = false, 
        right_is_clicked = false,
        right_is_on = false,
        is_any_hovered=self.is_any_hovered,
        
        update_button_state = self.update_button_state,
    }
    setmetatable(o, self)
    self.__index = self    
    return o
end

function DblButton:is_any_hovered()
    return self.left_is_hovered or self.right_is_hovered
end

function DblButton:render(vars)

    -- Gui
    local w = vars.btn_width*0.5
    local h = 32
    local dl = reaper.ImGui_GetWindowDrawList(vars.ctx)
    local cx,cy = reaper.ImGui_GetCursorScreenPos(vars.ctx)
    
    self:update_button_state(vars)

    -- Left --------------------------------------------------------------------------------------------------
    local left_txt=""

    if nilencode(self.left_text_callback) == nilvalue() then
        left_txt ="NO CALLBACK"
    else
        left_txt=self:left_text_callback(vars)
    end
    
    reaper.ImGui_InvisibleButton(vars.ctx, self.id.."-left", w, 32)
    self.left_is_hovered = reaper.ImGui_IsItemHovered(vars.ctx)
    self.left_is_clicked = reaper.ImGui_IsItemClicked(vars.ctx)
    
    local leftcolor = vars.colors.button
    local leftcolortxt = vars.colors.button_txt
    if self.left_is_on then
        if self.left_is_hovered then
            leftcolor = vars.colors.dblbutton_on_hover
            leftcolortxt = vars.colors.dblbutton_on_hover_txt
        else
            leftcolor = vars.colors.dblbutton_on
            leftcolortxt = vars.colors.dblbutton_text_on
        end
    elseif self.left_is_hovered then
        leftcolor = vars.colors.button_hovered
        leftcolortxt = vars.colors.button_txt
    end 
    
    -- Right -------------------------------------------------------------------------------------------------
    local right_txt=""
    if nilencode(self.right_text_callback) == nilvalue() then
        right_txt ="NO CALLBACK"
    else
        right_txt=self:right_text_callback(vars)
    end
    
    reaper.ImGui_SameLine(vars.ctx,nil,0)
    reaper.ImGui_InvisibleButton(vars.ctx,self.id.."-right", w, 32)
    self.right_is_hovered = reaper.ImGui_IsItemHovered(vars.ctx)
    self.right_is_clicked = reaper.ImGui_IsItemClicked(vars.ctx)
    
    local rightcolor = vars.colors.button
    local rightcolortxt = vars.colors.button_txt


    if self.right_is_on then
        if self.right_is_hovered then
            rightcolor = vars.colors.dblbutton_on_hover
            rightcolortxt = vars.colors.dblbutton_on_hover_txt
        else
            rightcolor = vars.colors.dblbutton_on
            rightcolortxt = vars.colors.dblbutton_text_on
        end
    elseif self.right_is_hovered then
        rightcolor = vars.colors.button_hovered
        rightcolortxt = vars.colors.button_txt
    end 

    -- Left & Right ------------------------------------------------------------------------------------------
    
    reaper.ImGui_DrawList_AddRectFilled(dl,cx,cy,cx+w,cy+h,0x00000032,8,reaper.ImGui_DrawFlags_RoundCornersLeft())
    reaper.ImGui_DrawList_AddRectFilled(dl,cx+w,cy,cx+w*2,cy+h,0x00000032,8,reaper.ImGui_DrawFlags_RoundCornersRight())
    reaper.ImGui_DrawList_AddRectFilled(dl,cx+1,cy+1,cx+w,cy+h-1,leftcolor,8,reaper.ImGui_DrawFlags_RoundCornersLeft())
    reaper.ImGui_DrawList_AddRectFilled(dl,cx+w,cy+1,cx+w*2-1,cy+h-1,rightcolor,8,reaper.ImGui_DrawFlags_RoundCornersRight())
    local tw,th = reaper.ImGui_CalcTextSize(vars.ctx, left_txt, w,h)
    reaper.ImGui_DrawList_AddText(dl,cx+w/2-tw/2,cy+h/2 -th/2,leftcolortxt, left_txt) 
    tw,th = reaper.ImGui_CalcTextSize(vars.ctx, right_txt, w,h)
    reaper.ImGui_DrawList_AddText(dl,cx+w*1.5-tw/2,cy+h/2 -th/2,rightcolortxt, right_txt) 
 
    ----------------------------------------------------------------------------------------------------------
    
    if #vars.t_note_src > 0 then 
        local changed = false
        if nilencode(self.left_is_clicked) ~= nilvalue() and self.left_is_clicked then
            changed = self:left_callback(vars)
        elseif nilencode(self.right_is_clicked) ~= nilvalue() and self.right_is_clicked then
            changed = self:right_callback(vars)
        end 
        if changed then
            local _, mh = reaper.MIDI_GetHash(vars.environment.take, false)
            vars.environment.midihash = mh 
        end
    end
    
end
--------------------------------------------------------------------------------------------------------------
-- DblButtonOnOff
--------------------------------------------------------------------------------------------------------------

DblButtonOnOff = {}

function DblButtonOnOff:new() 
    local o = DblButton:new() 
    o.left_callback = self.left_callback
    o.left_text_callback = self.left_text_callback
    o.right_callback = self.right_callback
    o.right_text_callback = self.right_text_callback
    o.update_button_state = self.update_button_state
    return o
end

function DblButtonOnOff:update_button_state(vars)
    self.left_is_on = is_type_on(vars)
    self.right_is_on = is_type_off(vars) 
end

function DblButtonOnOff:left_callback(vars)
    set_current_type_on(vars)
    return true
end

function DblButtonOnOff:left_text_callback(vars)
    return "On"
end

function DblButtonOnOff:right_callback(vars)
    set_current_type_off(vars)
    return true
end

function DblButtonOnOff:right_text_callback(vars)
    return "Off"
end

--------------------------------------------------------------------------------------------------------------
-- DblButtonShift
--------------------------------------------------------------------------------------------------------------

DblButtonShift = {}

function DblButtonShift:new() 
    local o = DblButton:new() 
    o.left_callback = self.left_callback
    o.left_text_callback = self.left_text_callback
    o.right_callback = self.right_callback
    o.right_text_callback = self.right_text_callback
    o.update_button_state = self.update_button_state
    return o
end

function DblButtonShift:update_button_state(vars)
    self.left_is_on = false
    self.right_is_on = false
end

function DblButtonShift:left_callback(vars)
    local last = vars.t_note_src[1].vel
    local curr
    for i=1, #vars.t_note_src do
        local j = #vars.t_note_src + 1 - i
        curr = vars.t_note_src[j].vel 
        vars.t_note_src[j].vel = last
        last = curr
    end
    
    vars.statistics = calculate_note_statistics(vars.t_note_src)
    update_all_events(vars)
    reaper.Undo_OnStateChange_Item(0, "Velocity shift left", vars.environment.mediaitem)
    return true
end

function DblButtonShift:left_text_callback(vars)
    return "<"
end

function DblButtonShift:right_callback(vars)
    local last = vars.t_note_src[#vars.t_note_src].vel
    local curr
    for i=1, #vars.t_note_src do
        curr = vars.t_note_src[i].vel 
        vars.t_note_src[i].vel = last
        last = curr
    end
        
    vars.statistics = calculate_note_statistics(vars.t_note_src)
    update_all_events(vars)
    reaper.Undo_OnStateChange_Item(0, "Velocity shift right", vars.environment.mediaitem)
    return true
end

function DblButtonShift:right_text_callback(vars)
    return ">"
end

--------------------------------------------------------------------------------------------------------------
-- app
--------------------------------------------------------------------------------------------------------------

function apploop()
    local vars = all_vars
    local got_all_env_vars, env  = get_reaper_environment(vars.environment)
    local changed_env_vars = any(env.changed)
    -- Failed to get essential environment info ---------------------------------
    if not got_all_env_vars then
        if midieditor_closed(env) then
            -- Critical fail, midieditor is closed
            -- stop apploop
             return
        elseif missing_take(env) and changed_env_vars then
            -- Not critical, take was deleted but midieditor
            -- is still open.. maybee keep open
--            cout("Take was deleted")
            -- Ful hack för tillfället 
            vars.t_noteon_src = {}
            vars.t_noteoff_src = {}
        end
    end
    
    -- Names can change which isn't flagged as a change since its not critical,
    -- so we always update. Changed var deals with the need of reloading midi data
    local old_env =  vars.environment
    vars.environment = env 
    -- Some essential value changed
    if changed_env_vars then
--        if env.changed.midihash then
            read_all_events(vars)
            vars.statistics = calculate_note_statistics(vars.t_note_src)
            
--        end
    end    
    
    --------------------------------------------------------------------------------------------------------------------
    -- GUI 
    --------------------------------------------------------------------------------------------------------------------

    local l_name =  string.upper(vars.environment.trackname) ..' - '.. string.upper((vars.environment.itemname)) .. '###SIGNOCVELHIST'..vars.window_id

    local color_count=7

    reaper.ImGui_PushStyleColor(vars.ctx, reaper.ImGui_Col_WindowBg(),      vars.colors.window_bg)
    reaper.ImGui_PushStyleColor(vars.ctx, reaper.ImGui_Col_Button(),        vars.colors.button)
    reaper.ImGui_PushStyleColor(vars.ctx, reaper.ImGui_Col_ButtonActive(),  vars.colors.button_active)
    reaper.ImGui_PushStyleColor(vars.ctx, reaper.ImGui_Col_ButtonHovered(), vars.colors.button_hovered)
    reaper.ImGui_PushStyleColor(vars.ctx, reaper.ImGui_Col_TitleBg(), vars.colors.title_bg)
    reaper.ImGui_PushStyleColor(vars.ctx, reaper.ImGui_Col_TitleBgActive(), vars.colors.title_bg_active)
    if vars.window_has_focus then
        reaper.ImGui_PushStyleColor(vars.ctx, reaper.ImGui_Col_Text(), vars.colors.title_bg_active_txt)        
    else
        reaper.ImGui_PushStyleColor(vars.ctx, reaper.ImGui_Col_Text(), vars.colors.title_bg_txt)
    end
    reaper.ImGui_PushFont(vars.ctx, vars.font.default)

    local px, py = reaper.ImGui_GetStyleVar(vars.ctx, reaper.ImGui_StyleVar_WindowPadding())
    reaper.ImGui_PushStyleVar(vars.ctx, reaper.ImGui_StyleVar_FramePadding(),px,py)
    
    local visible, open = reaper.ImGui_Begin(vars.ctx, l_name, true, reaper.ImGui_WindowFlags_TopMost() | reaper.ImGui_WindowFlags_NoScrollbar() | reaper.ImGui_WindowFlags_NoResize() | reaper.ImGui_WindowFlags_NoSavedSettings() | reaper.ImGui_WindowFlags_NoCollapse())
    reaper.ImGui_PopStyleVar(vars.ctx)
    reaper.ImGui_PopFont(vars.ctx)

    local focus = reaper.ImGui_IsWindowFocused(vars.ctx)
    vars.window_has_focus=focus
    
    if visible then
        vars.gui.velocity_histogram:render(vars)
        reaper.ImGui_PushFont(vars.ctx, vars.font.monospace)
        vars.gui.action_button_tilt:render(vars)
        reaper.ImGui_SameLine(vars.ctx,nil,8)
        vars.gui.action_button_ramp:render(vars)

        reaper.ImGui_SameLine(vars.ctx,nil,8)
        vars.gui.action_button_chaos:render(vars)

        -- Line 2
        vars.gui.action_button_straighten:render(vars)

        reaper.ImGui_SameLine(vars.ctx,nil,8)
        vars.gui.action_dblbutton_shift:render(vars)
        
        reaper.ImGui_SameLine(vars.ctx,nil,8)        
        vars.gui.action_dblbutton_onoff:render(vars)

        reaper.ImGui_PopFont(vars.ctx)
        reaper.ImGui_End(vars.ctx)
    end
        reaper.ImGui_PopStyleColor(vars.ctx,color_count)    
    if open then
        if not reaper.ImGui_IsKeyPressed(vars.ctx, reaper.ImGui_Key_Escape()) then
            reaper.defer(apploop)
        end 
    end    
    
end
------------------------------------------------------------------------------------------------------------------------
-- First start setup 
------------------------------------------------------------------------------------------------------------------------

do
    read_all_events(all_vars)
    setup_imgui(all_vars)
    all_vars.statistics = calculate_note_statistics(all_vars.t_note_src)
    local win_w = all_vars.btn_width*3+4*8
    local win_h = 235+(32)
    local x,y  =  reaper.GetMousePosition()
    local xp, yp = math.floor(x-win_w/2.0) , math.floor(y-win_h/2.0)
    reaper.ImGui_SetNextWindowSize(all_vars.ctx, win_w,win_h,reaper.ImGui_Cond_Once())
    reaper.ImGui_SetNextWindowPos(all_vars.ctx, xp, yp, reaper.ImGui_Cond_Once())
    
    all_vars.gui.velocity_histogram = VelocityHistogram:new()
    all_vars.gui.action_button_tilt = ActionTilt:new()
    all_vars.gui.action_button_straighten = ActionStraighten:new()
    all_vars.gui.action_button_ramp =  ActionRamp:new()
    all_vars.gui.action_button_chaos =  ActionChaos:new()
    all_vars.gui.action_dblbutton_onoff = DblButtonOnOff:new()
    all_vars.gui.action_dblbutton_shift =  DblButtonShift:new()
end
reaper.defer(apploop)
