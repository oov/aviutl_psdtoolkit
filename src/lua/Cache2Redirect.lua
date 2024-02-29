-- ロード順によっては本来読み込みたい Cache2.lua ではなくこっちが読み込まれてしまう
-- 細工して正しいファイルを読み込みなおす
package.loaded["Cache2"] = nil
local origpath = package.path
package.path = obj.getinfo("script_path") .. "?.lua"
local p = require("Cache2")
package.path = origpath
return p
