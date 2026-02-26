{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bridge
import Network
import Payloads
import Math
import Types

import System.Environment (getArgs)
import Utils (runCommand)

import System.Directory (doesFileExist)
import Network.HTTP.Client (Manager)
import Data.List (isInfixOf, (\\))
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (void)
import System.Info (os)
import System.Process (callCommand)

clearScreen :: IO ()
clearScreen = callCommand $ if os == "mingw32" then "cls" else "clear"

-- ============================================================================
-- ALIAS SETUP AND LOADING
-- ============================================================================

-- Parse alias file format: 
-- Individual lights: "customIndex alias"
-- Groups: "GROUP groupAlias member1,member2,member3"
-- Original index is inferred from line position (0, 1, 2...)
parseAliasFile :: String -> ([(String, Int)], [Int], [(String, [String])])
parseAliasFile content = 
  let linesOfFile = lines content
      parseLine (lineIdx, line) = case words line of
        ["GROUP", groupAlias, members] -> 
          Left (groupAlias, map (filter (/= ' ')) $ splitOn ',' members)
        [customIdx, alias] -> 
          Right (alias, lineIdx, read customIdx)
        _ -> Left ("", [])  -- Invalid line, ignore
      parsed = map parseLine (zip [0..] linesOfFile)
      groups = [g | Left g <- parsed, fst g /= ""]
      lights = [l | Right l <- parsed]
      aliasMap = [(a, i) | (a, i, _) <- lights]
      customIdxs = [c | (_, _, c) <- lights]
  in (aliasMap, customIdxs, groups)
  where
    splitOn _ [] = []
    splitOn c s = let (chunk, rest) = break (== c) s
                  in chunk : case rest of
                       [] -> []
                       (_:rs) -> splitOn c rs

-- Interactive alias setup
setupAliases :: [String] -> IO ([(String, Int)], [Int], [(String, [String])])
setupAliases names = do
  clearScreen
  putStrLn "╔════════════════════════════════════════════════════════╗"
  putStrLn "║           ALIAS SETUP - FIRST TIME CONFIGURATION       ║"
  putStrLn "╚════════════════════════════════════════════════════════╝\n"
  
  putStrLn "Lights found from API in original order:\n"
  mapM_ (\(idx, name) -> 
    putStrLn $ "  [" ++ show (idx :: Int) ++ "] " ++ name
    ) (zip [0..] names)
  
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
  putStrLn "Set up aliases for each light. This is what you will type to control them"
  putStrLn "Type an alias without spaces (e.g., 'd', 'b', 'c1', 'c2') and press ENTER. Must be done for all lights\n"
  
  -- Get aliases for each light
  aliasMap <- mapM (\(idx, name) -> do
      -- putStr $ "[" ++ show (idx :: Int) ++ "] " ++ name ++ " → alias: "
      alias <- getLine
      if null alias
        then do
          putStrLn "Warning: Empty alias! Please enter something\n"
          putStr $ "[" ++ show (idx :: Int) ++ "] " ++ name ++ " → alias: "
          -- putStr "→ "
          alias' <- getLine
          return (alias', idx)
        else return (alias, idx)
    ) (zip [0..] names)
  
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
  putStrLn "Aliases configured:\n"
  mapM_ (\(alias, idx) -> 
    putStrLn $ "  [" ++ show idx ++ "] " ++ (names !! idx) ++ " → " ++ alias
    ) aliasMap
  
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "\nDo you want to customize the order of how the lights appears on the terminal? Y/n ('n' for default)"
  -- putStrLn "This changes the order of how lights appear on the terminal ('n' for default)\n"
  -- putStr "→ "
  customizeOrder <- getLine

  
  customIdxs <- if customizeOrder == "y" || customizeOrder == "Y"
    then do
      putStrLn "\nEnter the display order (space-separated indices)"
      putStrLn $ "Example: 4 2 5 0 3 1  (to show lights in this order)"
      putStrLn $ "Available indices: " ++ unwords (map show [0..length names - 1])
      putStrLn ""
      orderInput <- getLine
      let indices = map read (words orderInput) :: [Int]
      if length indices == length names
        then return indices
        else do
          putStrLn "Warning: Wrong number of indices. Using default order."
          return [0..length names - 1]
    else return [0..length names - 1]
  
  -- putStrLn "Configuration complete\n"
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
  putStrLn "Custom display order:"
  mapM_ (\idx -> 
    putStrLn $ "  " ++ show (idx :: Int) ++ " → " ++ (names !! idx)
    ) customIdxs

  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "\nGroups allows you control multiple lights at once with one alias"
  putStrLn "Examples:"
  putStrLn "  * 'c' to control all lights at the ceiling"
  putStrLn "  * 'bedroom' to control all lights in the room"
  putStrLn "Default groups (hardcoded):\n * 'all' for all lights"
  putStrLn "\nDo you want to create light groups? Y/n"
  createGroups <- getLine
  
  groups <- if createGroups == "y" || createGroups == "Y"
    then do
      -- putStrLn ""
      -- putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
      -- putStrLn "GROUP SETUP"
      -- putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
      -- putStrLn ""
      putStrLn "\nHow many groups do you want to create?"
      -- putStrLn "Example: 2 (if you want 'c' for ceilings and 'db' for desk+bed)"
      -- putStr "→ "
      numGroupsStr <- getLine
      let numGroups = read numGroupsStr :: Int
      
      putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

      putStrLn "\nSTEP 1: Type the name of the group, this is what you will use later to control the group. Press ENTER"
      putStrLn "  Examples:\n    * 'c'\n    * 'db'\n    * 'bedroom'"
      putStrLn "Step 2: List the lights that will be added, using the aliases that you defined above. Separate with commas and NO SPACES. Press ENTER"
      putStrLn "  Examples:\n    * 'c1,c2,c3,c4'\n    * 'd,b'\n"
      
      mapM (\n -> do
        putStrLn $ "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        putStrLn $ "GROUP " ++ show (n :: Int) ++ " of " ++ show numGroups
        putStrLn $ "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
        -- putStrLn ""
        -- putStr "  Group alias → "
        groupAlias <- getLine
        
        -- putStrLn ""
        -- putStrLn ""
        putStrLn $ "  * Available aliases: " ++ unwords (map fst aliasMap)
        -- putStrLn ""
        -- putStr "  Members → "
        membersInput <- getLine
        let members = map (filter (/= ' ')) $ 
                      case break (== ',') membersInput of
                        (m, "") -> [m]
                        (m, rest) -> m : splitMembers (tail rest)
        
        -- putStrLn ""
        putStrLn $ "\nGroup '" ++ groupAlias ++ "' created with: " ++ unwords members
        putStrLn ""
        
        return (groupAlias, members)
        ) [1..numGroups]
    else return []
  
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "\nAll done! Press ENTER to continue..."
  _ <- getLine
  
  return (aliasMap, customIdxs, groups)
  where
    splitMembers "" = []
    splitMembers s = case break (== ',') s of
      (m, "") -> [m]
      (m, rest) -> m : splitMembers (tail rest)

-- Save aliases to file (format: "customIdx alias" per line, groups as "GROUP alias members")
-- Original index is the line number (0, 1, 2...)
saveAliasFile :: FilePath -> [(String, Int)] -> [Int] -> [(String, [String])] -> IO ()
saveAliasFile filePath aliasMap customIdxs groups = do
  let lightLines = [show customIdx ++ " " ++ alias
                   | ((alias, _), customIdx) <- zip aliasMap customIdxs]
      groupLines = ["GROUP " ++ groupAlias ++ " " ++ intercalate "," members
                   | (groupAlias, members) <- groups]
      content = unlines (lightLines ++ groupLines)
  writeFile filePath content
  where
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Load or setup aliases (OPTIONAL - only if file exists)
loadAliasesIfExists :: FilePath -> [String] -> IO (Maybe ([(String, Int)], [Int], [(String, [String])]))
loadAliasesIfExists aliasFilePath names = do
  fileExists <- doesFileExist aliasFilePath
  if fileExists
    then do
      content <- readFile aliasFilePath
      let (aliasMap, customIdxs, groups) = parseAliasFile content
      -- Validate that we have all lights
      if length aliasMap == length names && length customIdxs == length names
        then do
          -- putStrLn $ "Loaded aliases from " ++ aliasFilePath
          return $ Just (aliasMap, customIdxs, groups)
        else do
          putStrLn "Alias file incomplete or mistyped. Use 'setAlias' to reconfigure."
          return Nothing
    else do
      -- No file = show warning
      putStrLn "Aliases undefined!"
      putStrLn "Use 'l1', 'l2', ... ,'ln' to control lights"
      putStrLn "Type 'setAlias' to setup custom aliases\n"
      return Nothing

-- ============================================================================
-- MAIN LOOP
-- ============================================================================

-- Read or create a file
readOrCreateFile :: FilePath -> IO String
readOrCreateFile filePath = do
  fileExists <- doesFileExist filePath
  if fileExists then readFile filePath else writeFile filePath "" >> return ""

main :: IO ()
main = do
  let homeDirPath = "/Users/jpcst/bin/"
      ipFilePath = homeDirPath ++ "ip.txt"
      keyFilePath = homeDirPath ++ "key.txt"
      aliasFilePath = homeDirPath ++ "alias.txt"

  -- Read or create ip.txt file
  ipFileContent <- readOrCreateFile ipFilePath
  ip <- if ipFileContent == "" -- File does not exist or is empty
          then do
            bridge <- getMainBridgeIp -- Gets the ip from the API
            case bridge of
              Left err  -> putStrLn err >> return "" -- Some error, return nothing
              Right ip' -> writeFile ipFilePath ip' >> return ip' -- IP was found, save it to the file
          else return ipFileContent
  putStrLn $ "Found IP: " ++ ip ++ " at " ++ ipFilePath

  -- Read or create key.txt file
  keyFileContent <- readOrCreateFile keyFilePath
  key <- if keyFileContent == "" -- file does not exist or is empty
           then genKey ip keyFilePath -- user must press the Hue Bridge link button to generate a key and continue
           else return keyFileContent -- key was generated or already existed
  putStrLn $ "Found Key: " ++ key ++ " at " ++ keyFilePath

  args <- getArgs
  manager <- getInsecureManager -- HTTP request SSL disabled

  if not (null args)
    then do
      Right body <- sendGetRequest manager key (apiHueUrl ip "/light")
      let ids    = extractIdsFromBody    body
          states = extractStatesFromBody body
          bri    = extractBriFromBody    body
      runCommand manager key ip args ids states bri
    else
      loop manager key ip aliasFilePath

loop :: Manager -> String -> String -> FilePath -> IO ()
loop manager key ip aliasFilePath = do
  let hueUrl = apiHueUrl ip "/light" -- https://<ip>/clip/v2/resource/ ++ light
  result <- sendGetRequest manager key hueUrl
  case result of
    Left err -> do
      putStrLn $ "GET failed: " ++ err
      if "HttpExceptionRequest" `isInfixOf` err || "InvalidUrlException" `isInfixOf` err
        then do
          putStrLn "Detected invalid Bridge IP. Rewriting ip.txt..."
          writeFile "ip.txt" ""
      else if "Invalid Hue key" `isInfixOf` err || "no lighting" `isInfixOf` err
        then do
          putStrLn "Detected invalid Hue key. Rewriting key.txt..."
          writeFile "key.txt" ""
      else
        putStrLn "Unknown error, no file rewritten."
      putStrLn "Restarting main..."
      main


    Right body -> do
      let ids    = extractIdsFromBody body
          names  = extractNamesFromBody body
          states = extractStatesFromBody body
          bri    = extractBriFromBody body

      -- Try to load aliases (optional)
      maybeAliases <- loadAliasesIfExists aliasFilePath names
      
      -- Use aliases if available, otherwise create simple 1:1 mapping from API
      let (aliasMap, customIdxs, groups) = case maybeAliases of
            Just (aMap, cIdxs, grps) -> (aMap, cIdxs, grps)
            Nothing -> 
              -- DEFAULT: No aliases configured, use API order as-is
              -- Create simple numeric aliases: "l0", "l1", "l2", etc.
              -- Display in same order as API returns them
              let numLights = length names
                  defaultAliases = [("l" ++ show (i+1), i) | i <- [0..numLights-1]]
                  defaultOrder = [0..numLights-1]
              in (defaultAliases, defaultOrder, [])  -- No groups by default
      
      -- Helper to get index from alias
      -- let getIdx alias = case lookup alias aliasMap of
                           -- Just idx -> idx
                           -- Nothing  -> error $ "Unknown alias: " ++ alias
      
      -- Helper to get group members
      let getGroup groupAlias = case lookup groupAlias groups of
                                  Just members -> 
                                    [idx | m <- members, Just idx <- [lookup m aliasMap]]
                                  Nothing -> []
      
      -- Define light groups
      let allIdxs = [0..length names - 1]
      
      let maxNameLength = maximum (map length names)
      let maxBriLength = 3  -- Fixed width for the 'bri' column
      let maxStateLength = 5  -- Fixed width for the 'state' column ('True' or 'False')
      putStrLn " |--- NAME ---|- STATE -|- BRI -|"
      putStrLn " |            |         |       |"
      mapM_ (\idx ->
          let name = names !! idx
              state = show (states !! idx)
              bri' = if states !! idx
                     then show (round (bri !! idx) :: Int)
                     else "==="
              -- Pad the name to the max length, left-align it
              paddedName = name ++ replicate (maxNameLength - length name) ' '
              -- Adjust state padding, ensuring fixed width for the state column
              paddedState = replicate (maxStateLength - length state) ' ' ++ state  -- Right-align 'state' in a 5-character wide column
              -- Adjust bri padding, ensuring fixed width for the bri column
              paddedBri = replicate (maxBriLength - length bri') ' ' ++ bri' -- Right-align 'bri' in a 5-character wide column
          in putStrLn $ " | " ++ paddedName ++ "  |  " ++ paddedState ++ "  |  " ++ paddedBri ++ "  |  "
        ) customIdxs
      putStrLn " |            |         |       |"
      putStrLn " |---|λHue|---|--|0.3|--|--|?|--|"

      let lightsIdx     = getListIndex ids
          lightsOnList  = getTrueIndexes states
          lightsOffList = lightsIdx \\ lightsOnList -- difference of Sets operator (\\) e.g. l = [1,2,3] and l' = [2] .:. l \\ l' = [1,3]

      -- putStrLn $ "\n" ++ "light id, idx"
      -- print $ zip ids lightsIdx
      -- print lightsIdxs
      -- putStrLn $ "\n" ++ "idx on list"
      -- print lightsOnList
      -- putStrLn $ "\n" ++ "idx off list"
      -- print lightsOffList

      let toggleLight position                = void $ sendPutRequest manager
                                                                      key
                                                                      (apiLightUrl hueUrl (ids !! position)) $
                                                                      buildJsonPayloadLightsToggle $ not $ states !! position
          setLightBri tt briPerc position     = void $ sendPutRequest manager
                                                                      key
                                                                      (apiLightUrl hueUrl (ids !! position)) $
                                                                      buildJsonPayloadSetBri tt briPerc
          setToggleAndBri tt briPerc position = void $ sendPutRequest manager
                                                                      key
                                                                      (apiLightUrl hueUrl (ids !! position)) $
                                                                      buildJsonPayloadToggleAndBri tt (not $ states !! position) briPerc
          setLightXy tt (x,y) position        = void $ sendPutRequest manager
                                                                      key
                                                                      (apiLightUrl hueUrl (ids !! position)) $
                                                                      buildJsonPayloadSetXy tt (x,y)

      userInput <- getLine
      clearScreen
      rawInput <- handleInput userInput
      case rawInput of

      -------------------------------------------
      --   Hue Light Order   -- This is what the Hue app shows originally
      -------------------------------------------
      --      names          idx       alises  --
      --     CEILING 2        0         C2     --
      --     CEILING 4        1         C4     --
      --     DESK             2         D      --
      --     CEILING 3        3         C3     --
      --     BED              4         B      --
      --     CEILING 1        5         C1  ]  --
      -------------------------------------------

        -- Try to use alias, fall back to numeric index
        S alias | Just idx <- lookup alias aliasMap -> toggleLight idx
        
        -- Try group commands
        S groupAlias | not (null (getGroup groupAlias)) -> 
          mapM_ toggleLight (getGroup groupAlias)

        S s | s == "all" || s == "on"  -> mapM_ toggleLight lightsOffList
        S s | s == "off" || s == "nox" -> mapM_ toggleLight lightsOnList
        -- S s | s == "all" || s == "on"   -> mapConcurrently_ toggleLight lightsOffList
        -- S s | s == "nox" || s == "off" -> mapConcurrently_ toggleLight lightsOnList
        S "br"  -> mapM_ (setLightXy 0 (0.3   , 0.3))    (lightsOnList \\ [4]) -- I need to manually remove the BED light because the white LEDS are broken
        S "am"  -> mapM_ (setLightXy 0 (0.5203, 0.4141)) lightsOnList -- .5435, .430
        S "red" -> mapM_ (setLightXy 0 $ convertRgbToXy (256, 0, 0)) lightsOnList
        S "green" -> mapM_ (setLightXy 0 $ convertRgbToXy (0, 256, 0)) lightsOnList
        S "blue" -> mapM_ (setLightXy 0 $ convertRgbToXy (0, 0, 256)) lightsOnList

        I briPerc  -> mapConcurrently_ (setLightBri 0 briPerc) lightsOnList

        -- SI with any alias
        SI alias briPerc | Just idx <- lookup alias aliasMap ->
          if states !! idx
            then setLightBri 0 briPerc idx
            else setToggleAndBri 0 briPerc idx
        
        -- Group commands with brightness
        SI groupAlias briPerc | not (null (getGroup groupAlias)) ->
          mapConcurrently_ (\i ->
            if states !! i
              then setLightBri     0 briPerc i
              else setToggleAndBri 0 briPerc i
          ) (getGroup groupAlias)
        
        SI "all" briPerc ->
          mapConcurrently_ (\i ->
            if states !! i
              then setLightBri     0 briPerc i
              else setToggleAndBri 0 briPerc i
          ) allIdxs


        -- SI "c"  briPerc -> mapConcurrently_ (setLightBri 0 briPerc) [5,0,3,1]
        -- SI "db" briPerc -> mapConcurrently_ (setLightBri 0 briPerc) [2,4]
        
        -- SS with any alias
        SS alias "br" | Just idx <- lookup alias aliasMap -> 
          setLightXy 0 (0.3, 0.3) idx
        SS alias "am" | Just idx <- lookup alias aliasMap -> 
          setLightXy 0 (0.5203, 0.4141) idx
        
        -- SS group commands
        SS groupAlias "br" | not (null (getGroup groupAlias)) -> 
          mapM_ (setLightXy 0 (0.3, 0.3)) (getGroup groupAlias)
        SS groupAlias "am" | not (null (getGroup groupAlias)) -> 
          mapM_ (setLightXy 0 (0.5203, 0.4141)) (getGroup groupAlias)
        SS groupAlias "." | not (null (getGroup groupAlias)) -> 
          mapM_ (setToggleAndBri 0 100) (getGroup groupAlias)

        III r g b -> mapM_ (setLightXy 0 $ convertRgbToXy (r,g,b)) lightsOnList
        
        S "setAlias" -> do
          putStrLn "\n╔═══════════════════════════════════════════════╗"
          putStrLn "║       ALIAS CONFIGURATION                     ║"
          putStrLn "╚═══════════════════════════════════════════════╝\n"
          (newAliasMap, newCustomIdxs, newGroups) <- setupAliases names
          saveAliasFile aliasFilePath newAliasMap newCustomIdxs newGroups
          putStrLn "Aliases saved. Restarting loop...\n"
        
        _ -> return ()

      loop manager key ip aliasFilePath