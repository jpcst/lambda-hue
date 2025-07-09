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
  hs.alert.show("Toggle ceiling")
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

hs.hotkey.bind({"ctrl"}, "F19", function()
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
  hs.execute("/Users/jpcst/bin/lambda-hue clr indigo")
end)

hs.hotkey.bind({"ctrl", "alt"}, "F20", function()
  hs.execute("/Users/jpcst/bin/lambda-hue nox")
end)

--hs.hotkey.bind({"alt"}, "F16", function()
--  hs.execute("/Users/jpcst/bin/lambda-hue bri 25")
--end)

--hs.hotkey.bind({"alt"}, "F17", function()
--  hs.execute("/Users/jpcst/bin/lambda-hue bri 75")
--end)

--hs.hotkey.bind({"alt"}, "F19", function()
--  hs.execute("/Users/jpcst/bin/lambda-hue clr green")
--end)

-- RGB state
local rgb = { r = 0, g = 0, b = 0 }
local componentIndex = 1  -- 1 = r, 2 = g, 3 = b
local componentNames = {"r", "g", "b"}

-- Store alert ID to reuse for replacing the alert
local alertId = nil

-- Helper to show alert cleanly (replaces previous)
local function showAlert(msg)
  if alertId then
    hs.alert.closeSpecific(alertId)
  end
  alertId = hs.alert.show(msg, {}, hs.screen.mainScreen(), 0.5)
end

-- Helper to update and run command
local function updateRGB(delta)
  local comp = componentNames[componentIndex]
  rgb[comp] = math.max(0, math.min(255, rgb[comp] + delta))

  -- Show alert first for faster UI response
  showAlert(string.format("RGB → %d %d %d", rgb.r, rgb.g, rgb.b), 0.5)

  -- Build and run command
  local command = string.format("/Users/jpcst/bin/lambda-hue rgb %d %d %d", rgb.r, rgb.g, rgb.b)
  hs.task.new("/bin/bash", nil, {"-c", command}):start()
end

-- ALT + F16 → decrease current component
hs.hotkey.bind({"alt"}, "F16", function()
  updateRGB(-10)
end)

-- ALT + F17 → increase current component
hs.hotkey.bind({"alt"}, "F17", function()
  updateRGB(10)
end)

-- ALT + F19 → cycle RGB component
hs.hotkey.bind({"alt"}, "F19", function()
  componentIndex = (componentIndex % 3) + 1
  showAlert("Component: " .. componentNames[componentIndex]:upper(), 0.5)
end)
