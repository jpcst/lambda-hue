--hs.hotkey.bind({}, "F14", function()
--  hs.execute("/Users/jpcst/bin/lambda-hue")
--end)

local function runAsync(cmd)
  hs.task.new("/bin/bash", nil, {"-c", cmd}):start()
end

hs.hotkey.bind({}, "F14", function()
  hs.osascript.applescript([[
    tell application "iTerm"
      if not (exists window 1) then
        create window with default profile
      else
        create tab with default profile
      end if
      tell current session of current window
        write text "/Users/jpcst/bin/lambda-hue"
      end tell
    end tell
  ]])
end)

hs.hotkey.bind({}, "F15", function()
  hs.execute("/Users/jpcst/bin/lambda-hue c")
end)

hs.hotkey.bind({"shift"}, "F15", function()
  hs.execute("/Users/jpcst/bin/lambda-hue c 100")
end)

hs.hotkey.bind({}, "F16", function()
  hs.execute("/Users/jpcst/bin/lambda-hue d")
end)

hs.hotkey.bind({"shift"}, "F16", function()
  hs.execute("/Users/jpcst/bin/lambda-hue d 100")
end)

hs.hotkey.bind({}, "F17", function()
  hs.execute("/Users/jpcst/bin/lambda-hue b")
end)

hs.hotkey.bind({"shift"}, "F17", function()
  hs.execute("/Users/jpcst/bin/lambda-hue b 100")
end)

hs.hotkey.bind({"ctrl"}, "F14", function()
  runAsync("/Users/jpcst/bin/lambda-hue inc " .. -10)
end)

hs.hotkey.bind({"ctrl"}, "F15", function()
  runAsync("/Users/jpcst/bin/lambda-hue inc " .. 10)
end)

hs.hotkey.bind({"ctrl-shift"}, "F14", function()
  hs.execute("/Users/jpcst/bin/lambda-hue bri 100")
end)

hs.hotkey.bind({}, "F20", function()
  hs.execute("/Users/jpcst/bin/lambda-hue d & /Users/jpcst/bin/lambda-hue b")
end)

hs.hotkey.bind({"ctrl", "shift"}, "F15", function()
  hs.execute("/Users/jpcst/bin/lambda-hue c1")
end)

hs.hotkey.bind({"ctrl", "shift"}, "F16", function()
  hs.execute("/Users/jpcst/bin/lambda-hue c2")
end)

hs.hotkey.bind({"ctrl", "shift"}, "F17", function()
  hs.execute("/Users/jpcst/bin/lambda-hue c3")
end)

hs.hotkey.bind({"ctrl", "shift"}, "F20", function()
  hs.execute("/Users/jpcst/bin/lambda-hue c4")
end)

hs.hotkey.bind({"ctrl", "alt"}, "F15", function()
  hs.execute("/Users/jpcst/bin/lambda-hue clr br")
end)

hs.hotkey.bind({"ctrl", "alt"}, "F16", function()
  hs.execute("/Users/jpcst/bin/lambda-hue clr am")
end)

hs.hotkey.bind({"ctrl", "alt"}, "F17", function()
  hs.execute("/Users/jpcst/bin/lambda-hue clr blue")
end)

hs.hotkey.bind({"ctrl", "alt"}, "F20", function()
  hs.execute("/Users/jpcst/bin/lambda-hue clr indigo")
end)
