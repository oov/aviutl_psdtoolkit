-- ���[�h���ɂ���Ă͖{���ǂݍ��݂��� Cache2.lua �ł͂Ȃ����������ǂݍ��܂�Ă��܂�
-- �׍H���Đ������t�@�C����ǂݍ��݂Ȃ���
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
