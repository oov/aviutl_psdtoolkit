-- ���[�h���ɂ���Ă͖{���ǂݍ��݂��� Cache2.lua �ł͂Ȃ����������ǂݍ��܂�Ă��܂�
-- �׍H���Đ������t�@�C����ǂݍ��݂Ȃ���
package.loaded["Cache2"] = nil
local origpath = package.path
package.path = obj.getinfo("script_path") .. "?.lua"
local p = require("Cache2")
package.path = origpath
return p
