-- ロード順によっては本来読み込みたい Cache2.lua ではなくこっちが読み込まれてしまう
-- 細工して正しいファイルを読み込みなおす
package.loaded["Cache2"] = nil
local origpath = package.path
local origcpath = package.cpath
require("PSDToolKit")
package.path = require("PSDToolKitBridge").getscriptpath() .. "?.lua"
package.cpath = require("PSDToolKitBridge").getscriptpath() .. "?.dll"
local p = require("Cache2")
package.path = origpath
package.cpath = origcpath
return p
