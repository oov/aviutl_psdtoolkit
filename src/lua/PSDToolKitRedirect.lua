-- ���̃t�@�C���� AviUtl �̃e�L�X�g�I�u�W�F�N�g��X�N���v�g����t�B���^��
-- require("PSDToolKit") ���������ɓǂݍ��܂��t�@�C��
-- ���̃t�@�C�����ǂݍ��܂��Ƃ������Ƃ͐������t�@�C�����ǂݍ��߂Ă��Ȃ��̂ŁA
-- ��U�L���b�V���𖳌������p�X��ʂ�����ŉ��߂ēǂݍ���
package.loaded["PSDToolKit"] = nil
local origpath = package.path
local origcpath = package.cpath
package.path = obj.getinfo("script_path") .. "PSDToolKit\\?.lua"
package.cpath = obj.getinfo("script_path") .. "PSDToolKit\\?.dll"
local p = require("PSDToolKit")
require("PSDToolKitBridge")
package.path = origpath
package.cpath = origcpath
return p
