-- @description Adjust project grid(mousewheel)
-- @author signoc (Sigge Eriksson)
-- @links
--    Author URI https://forum.cockos.com/member.php?u=10082
-- @version 1.0
-- @changelog
--      First version
-- @provides
--    [main]signoc_adjust_project_grid(mousewheel).lua
-- @donation https://www.paypal.com/donate/?hosted_button_id=FARB5QU9C8VT8
-- @about
--    # Adjust project grid
--
--    Action to bind mouse wheel to be able to adjust project grid setting.
--    For example shortcut ctrl+alt+mousewheel

function sign(n)
     if n < 0 then
         return -1
     elseif n > 0 then
         return 1
     else
         return 0
     end
 end
 
function main()
    local _,d,_,_ = reaper.GetSetProjectGrid(0, false)
    local _, _, _, _, _, _, mwheel, _  = reaper.get_action_context()
    local s = sign(mwheel)
    local out = d*((1/2)^s)
    if out >= 1/128 and out <= 1 then
        reaper.GetSetProjectGrid(0, true, out) 
    end
end

reaper.defer(main)
