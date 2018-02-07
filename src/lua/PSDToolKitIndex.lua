-- このファイルは AviUtl のテキストオブジェクトやスクリプト制御フィルタで
-- require("PSDToolKit") をした時に読み込まれるファイル
-- このファイルが読み込まれるということは正しいファイルが読み込めていないので、
-- 一旦キャッシュを無効化しパスを通した上で改めて読み込む
package.loaded["PSDToolKit"] = nil
local origpath = package.path
package.path = obj.getinfo("script_path") .. "PSDToolKit\\?.lua"
local p = require("PSDToolKit")
package.path = origpath
return p
