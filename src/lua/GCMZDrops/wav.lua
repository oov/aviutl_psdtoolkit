local P = {}

P.name = "*.wav �h���b�v���ɒǉ��̃I�u�W�F�N�g�𐶐�"

P.priority = 0

local avoiddupP = require("avoiddup")

local function getextension(filepath)
  return filepath:match(".[^.]+$"):lower()
end

local function trimextension(filepath)
  local ext = getextension(filepath)
  return filepath:sub(1, #filepath - #ext)
end

local function fileexists(filepath)
  local f = io.open(filepath, "rb")
  if f ~= nil then
    f:close()
    return true
  end
  return false
end

local function fileread(filepath)
  local f = io.open(filepath, "rb")
  if f == nil then
    return nil
  end
  local b = f:read("*all")
  f:close()
  return b
end

local function postprocesssubtitle(subtitle, encoding, setting)
  -- BOM ������ꍇ�͂������ɃG���R�[�f�B���O�ݒ���㏑�����A
  -- ���ł� BOM ���J�b�g����
  if subtitle:sub(1, 3) == "\239\187\191" then
    encoding = "utf8"
    subtitle = subtitle:sub(4)
  elseif GCMZDrops.isutf8(subtitle) then
    -- BOM ���Ȃ��Ă��e�L�X�g�� UTF-8 �Ƃ��Đ������G���R�[�h����Ă���Ȃ� UTF-8 �Ƃ��Ĉ���
    encoding = "utf8"
  elseif subtitle:sub(1, 2) == "\255\254" then
    encoding = "utf16le"
    subtitle = subtitle:sub(3)
  elseif subtitle:sub(1, 2) == "\254\255" then
    encoding = "utf16be"
    subtitle = subtitle:sub(3)
  end
  if encoding ~= "utf8" then
    -- �����̕ێ���Ԃ� UTF-8 �ɓ��ꂷ��
    subtitle = GCMZDrops.convertencoding(subtitle, encoding, "utf8")
  end

  -- �u���p�������Ăяo��
  subtitle = setting:wav_subtitle_replacer(subtitle)

  return subtitle
end

local function readsubtitle(filepath, encoding, setting)
  local subtitle = fileread(filepath)
  if subtitle == nil then
    return nil
  end
  return postprocesssubtitle(subtitle, encoding, setting)
end

local function requireLocal(pkg)
  local origpath = package.path
  local origcpath = package.cpath
  package.path = GCMZDrops.scriptdir() .. "..\\script\\PSDToolKit\\?.lua"
  package.cpath = GCMZDrops.scriptdir() .. "..\\script\\PSDToolKit\\?.dll"
  local ok, r = pcall(require, pkg)
  package.path = origpath
  package.cpath = origcpath
  if not ok then
    error(r)
  end
  return r
end

local function loadjsonstr(jsonstr)
  if jsonstr:sub(1, 3) == "\239\187\191" then
    jsonstr = jsonstr:sub(4)
  end
  local j = requireLocal("json").decode(jsonstr)
  if j == nil then
    return nil
  end
  return j
end

local function loadjson(filepath)
  local jsonstr = fileread(filepath)
  if jsonstr == nil then
    return nil
  end
  return loadjsonstr(jsonstr)
end

function P.resolvepath(filepath, finder, setting)
  if finder == 1 then
    return filepath:match("([^\\]+)[\\][^\\]+$")
  elseif finder == 2 then
    return filepath:match("([^\\]+)%.[^.]+$")
  elseif finder == 3 then
    return filepath:match("([^\\]+)%.[^.]+$"):match("^[^_]+")
  elseif finder == 4 then
    return filepath:match("([^\\]+)%.[^.]+$"):match("^[^_]+_([^_]+)")
  elseif finder == 5 then
    return filepath:match("([^\\]+)%.[^.]+$"):match("^[^_]+_[^_]+_([^_]+)")
  elseif type(finder) == "function" then
    return finder(setting, filepath)
  end
  return nil
end

function P.loadsetting()
  if P.setting ~= nil then
    return P.setting
  end
  local origpath = package.path
  package.path = GCMZDrops.scriptdir() .. "..\\script\\PSDToolKit\\?.lua"
  local ok, gui = pcall(require, "setting-gui")
  if not ok then gui = {} end
  local ok, user = pcall(require, "setting")
  if not ok then user = {} end
  P.setting = setmetatable(user, {__index = setmetatable(gui, {__index = require("default")})})
  package.path = origpath
  return P.setting
end

function P.ondragenter(files, state)
  for i, v in ipairs(files) do
    local ext = getextension(v.filepath)
    if ext == ".wav" or ext == ".txt" or ext == ".exo" then
      -- �t�@�C���̊g���q�� .wav �� .txt �� .exo �̃t�@�C�����������珈���ł��邩������Ȃ��̂� true
      return true
    end
  end
  return false
end

function P.ondragover(files, state)
  -- ondragenter �ŏ����ł������Ȃ��̂� ondragover �ł������ł������Ȃ̂Œ��ׂ� true
  return true
end

function P.ondragleave()
end

function P.exaread(filepath, postfix)
  local basepath = GCMZDrops.scriptdir() .. "..\\script\\PSDToolKit\\exa\\"
  local inistr = nil
  if filepath ~= nil then
    filepath = basepath .. filepath .. "_" .. postfix .. ".exa"
    inistr = fileread(filepath)
    if inistr == nil then
      debug_print("�ǂݍ��ݎ��s: " .. filepath)
    end
  end
  if inistr == nil then
    filepath = basepath .. postfix .. ".exa"
    inistr = fileread(filepath)
  end
  if inistr ~= nil then
    debug_print("�g�p����G�C���A�X�t�@�C��: " .. filepath)
  else
    error("cannot read: " .. filepath)
  end
  return GCMZDrops.inistring(inistr)
end

function P.findexatype(ini)
  if ini:sectionexists("vo") then
    return "vo"
  elseif ini:sectionexists("ao") then
    return "ao"
  end
  error("unexpected alias file format")
end

function P.numitemsections(ini)
  local prefix = P.findexatype(ini)  
  local n = 0
  while ini:sectionexists(prefix .. "." .. n) do
    n = n + 1
  end
  return n
end

function P.insertexa(destini, srcini, index, layer)
  local prefix = P.findexatype(srcini)
  destini:set(index, "layer", layer)
  destini:set(index, "overlay", 1)
  for _, key in ipairs(srcini:keys(prefix)) do
    if key ~= "length" then
      destini:set(index, key, srcini:get(prefix, key, ""))
    end
  end
  if prefix == "ao" then
    destini:set(index, "audio", 1)
  end

  for i = 0, P.numitemsections(srcini) - 1 do
    local exosection = index .. "." .. i
    local section = prefix .. "." .. i
    for _, key in ipairs(srcini:keys(section)) do
      destini:set(exosection, key, srcini:get(section, key, ""))
    end
  end
end

function P.parseexo(filepath)
  local exo = fileread(filepath)
  if exo == nil then
    return nil
  end
  local ini = GCMZDrops.inistring(exo)
  local wav, txt, j = nil, nil, nil
  local i = 0
  while 1 do
    if ini:get(i, "start", "") ~= "1" then
      break
    end
    if i == 10 then
      -- ���ꂾ������Ώ\��
      return nil
    end
    local name = ini:get(i .. ".0", "_name", "")
    if wav == nil and ((name == "�����t�@�C��")or(name == "Audio file")) then
      wav = ini:get(i .. ".0", "file", nil)
      if j == nil or j == '' then
        j = ini:get(i .. ".0", "__json", nil)
      end
    elseif txt == nil and ((name == "�e�L�X�g")or(name == "Text")) then
      txt = ini:get(i .. ".0", "text", nil)
      if j == nil or j == '' then
        j = ini:get(i .. ".0", "__json", nil)
      end
    end
    i = i + 1
  end
  if wav == nil or txt == nil then
    return nil
  end
  txt = GCMZDrops.decodeexotextutf8(txt)
  if j ~= nil and j ~= '' then
    j = loadjsonstr(GCMZDrops.decodeexotextutf8(j))
  end
  return wav, txt, j
end

function P.fire(files, state)
  local setting = P.loadsetting()

  -- setting.wav_firemode �ɓK�����邩�`�F�b�N
  for i, v in ipairs(files) do
    if getextension(v.filepath) == ".wav" then
      local firemode = setting.wav_firemode
      if v.overridefiremode ~= nil then
        -- ���̃X�N���v�g���� overridefiremode ������ǉ�����Ă����ꍇ��
        -- �ݒ���e�Ɋւ�炸������̔������[�h���̗p����
        firemode = v.overridefiremode
      end
      -- ���t�@�C���Ɠ����ꏊ�ɂ���e�L�X�g�t�@�C����ǂݍ��ށi������Ȃ���� nil�j
      local subtitle = readsubtitle(
        trimextension(v.orgfilepath or v.filepath) .. ".txt",
        v.overridesubtitleencoding or setting.wav_subtitle_encoding,
        setting)
      local exabase = P.resolvepath(
        v.orgfilepath or v.filepath,
        setting.wav_exafinder,
        setting)
      local j = loadjson(trimextension(v.orgfilepath or v.filepath) .. ".json")
      if firemode == 0 then
        if state.shift then
          if subtitle == nil then
            subtitle = ""
          end
          return v.filepath, subtitle, exabase, j
        end
      elseif firemode == 1 then
        if (subtitle ~= nil)and(not state.shift) then
          return v.filepath, subtitle, exabase, j
        end
      elseif firemode == 2 then
        return v.filepath, subtitle, exabase, j
      end
    end
  end

  -- setting.wav_firemode_wavtxt �ɓK�����邩�`�F�b�N
  if setting.wav_firemode_wavtxt == 1 then
    local wav, txt = nil, nil
    for i, v in ipairs(files) do
      if getextension(v.filepath) == ".wav" then
        local wavname = trimextension(v.orgfilepath or v.filepath)
        for i2, v2 in ipairs(files) do
          if getextension(v2.filepath) == ".txt" and wavname == trimextension(v2.orgfilepath or v2.filepath) then
            wav, txt = v, v2
            break
          end
        end
        if wav ~= nil and txt ~= nil then
          break
        end
      end
    end
    if wav ~= nil and txt ~= nil then
      local subtitle = readsubtitle(
        txt.orgfilepath or txt.filepath,
        wav.overridesubtitleencoding or setting.wav_subtitle_encoding,
        setting)
      local exabase = P.resolvepath(
        wav.orgfilepath or wav.filepath,
        setting.wav_exafinder,
        setting)
      local j = loadjson(trimextension(wav.orgfilepath or wav.filepath) .. ".json")
      return wav.filepath, subtitle, exabase, j
    end
  end

  -- setting.wav_firemode_exo �ɓK�����邩�`�F�b�N
  if setting.wav_firemode_exo == 1 then
    for i, v in ipairs(files) do
      if getextension(v.filepath) == ".exo" then
        local orgwav, txt, j = P.parseexo(v.filepath)
        if orgwav ~= nil and fileexists(orgwav) then
          local newwav = orgwav
          if newwav ~= nil and GCMZDrops.needcopy(newwav) then
            newwav = avoiddupP.getfile(newwav)
            if newwav == '' then
              newwav = nil
            end
          end
          if newwav ~= nil and txt ~= nil then
            local subtitle = postprocesssubtitle(txt, "utf8", setting)
            local exabase = P.resolvepath(orgwav, setting.wav_exafinder, setting)
            if j == nil then
              j = loadjson(trimextension(orgwav) .. ".json")
            end
            return newwav, subtitle, exabase, j
          end
        end
      end
    end
  end

  return nil
end

function P.firetext(files, state)
  local setting = P.loadsetting()

  for i, v in ipairs(files) do
    if getextension(v.filepath) == ".txt" then
      local encoding = setting.wav_subtitle_encoding
      if v.mediatype == "text/plain; charset=Shift_JIS" then
        encoding = "sjis"
      elseif v.mediatype == "text/plain; charset=UTF-8" then
        encoding = "utf8"
      end
      local subtitle = readsubtitle(
        v.filepath,
        v.overridesubtitleencoding or encoding,
        setting)
      local exabase = P.resolvepath(
        v.orgfilepath or v.filepath,
        setting.wav_exafinder,
        setting)
      local j = loadjson(trimextension(v.orgfilepath or v.filepath) .. ".json")
      if state.shift then
        return subtitle, exabase, j
      end
    end
  end
  return nil
end

function P.ondrop(files, state)
  local wavfilepath, subtitle, exabase, j = P.fire(files, state)
  if wavfilepath ~= nil then
    -- �v���W�F�N�g�ƃt�@�C���̏����擾����
    local proj = GCMZDrops.getexeditfileinfo()
    local fi = GCMZDrops.getfileinfo(wavfilepath)
    -- ���������݂̃v���W�F�N�g�ŉ��t���[��������̂����v�Z����
    local wavlen = math.ceil((fi.audio_samples * proj.rate) / (proj.audio_rate * proj.scale))
    return P.generateexo(wavfilepath, wavlen, subtitle, exabase, state, j)
  end
  subtitle, exabase, j = P.firetext(files, state)
  if subtitle ~= nil then
    return P.generateexo(nil, 64, subtitle, exabase, state, j)
  end
  return false
end

function P.generateexo(wavfilepath, wavlen, subtitle, exabase, state, j)
  local setting = P.loadsetting()
  -- �e���v���[�g�p�ϐ�������
  local values = {
    WAV_START = 1,
    WAV_END = 0,
    WAV_PATH = wavfilepath,
    LIPSYNC_START = 1,
    LIPSYNC_END = 0,
    LIPSYNC_PATH = wavfilepath,
    MPSLIDER_START = 1,
    MPSLIDER_END = 0,
    SUBTITLE_START = 1,
    SUBTITLE_END = 0,
    SUBTITLE_TEXT = subtitle,
    USER = j or {},
  }
  local modifiers = {
    ENCODE_TEXT = function(v)
      return GCMZDrops.encodeexotextutf8(v)
    end,
    ENCODE_LUA_STRING = function(v)
      v = GCMZDrops.convertencoding(v, "sjis", "utf8")
      v = GCMZDrops.encodeluastring(v)
      v = GCMZDrops.convertencoding(v, "utf8", "sjis")
      return v
    end,
  }

  -- �����𔽉f
  values.WAV_END = values.WAV_END + wavlen
  values.LIPSYNC_END = values.LIPSYNC_END + wavlen
  values.MPSLIDER_END = values.MPSLIDER_END + wavlen
  values.SUBTITLE_END = values.SUBTITLE_END + wavlen

  -- �I�t�Z�b�g�ƃ}�[�W���𔽉f
  values.LIPSYNC_START = values.LIPSYNC_START + setting.wav_lipsync_offset
  values.LIPSYNC_END = values.LIPSYNC_END + setting.wav_lipsync_offset
  values.MPSLIDER_START = values.MPSLIDER_START - setting.wav_mpslider_margin_left
  values.MPSLIDER_END = values.MPSLIDER_END + setting.wav_mpslider_margin_right
  values.SUBTITLE_START = values.SUBTITLE_START - setting.wav_subtitle_margin_left
  values.SUBTITLE_END = values.SUBTITLE_END + setting.wav_subtitle_margin_right

  -- �}�C�i�X�����ɐi��ł��܂�������߂�
  local ofs = math.min(values.LIPSYNC_START, values.MPSLIDER_START, values.SUBTITLE_START) - 1
  values.WAV_START = values.WAV_START - ofs
  values.WAV_END = values.WAV_END - ofs
  values.LIPSYNC_START = values.LIPSYNC_START - ofs
  values.LIPSYNC_END = values.LIPSYNC_END - ofs
  values.MPSLIDER_START = values.MPSLIDER_START - ofs
  values.MPSLIDER_END = values.MPSLIDER_END - ofs
  values.SUBTITLE_START = values.SUBTITLE_START - ofs
  values.SUBTITLE_END = values.SUBTITLE_END - ofs

  -- exo �t�@�C���̃w�b�_������g�ݗ���
  local proj = GCMZDrops.getexeditfileinfo()
  local oini = GCMZDrops.inistring("")
  local totallen = math.max(values.WAV_END, values.LIPSYNC_END, values.MPSLIDER_END, values.SUBTITLE_END)
  oini:set("exedit", "width", proj.width)
  oini:set("exedit", "height", proj.height)
  oini:set("exedit", "rate", proj.rate)
  oini:set("exedit", "scale", proj.scale)
  oini:set("exedit", "length", totallen)
  oini:set("exedit", "audio_rate", proj.audio_rate)
  oini:set("exedit", "audio_ch", proj.audio_ch)

  -- �I�u�W�F�N�g�̑}��
  local index = 0

  -- ������g�ݗ���
  if wavfilepath ~= nil then
    local aini = P.exaread(exabase, "wav")
    setting:wav_examodifler_wav(aini, values, modifiers)
    P.insertexa(oini, aini, index, index + 1)
    index = index + 1
  end

  if setting.wav_mergedprep > 0 then
    -- �����I�u�W�F�N�g��g�ݗ���
    local aini = GCMZDrops.inistring("")
    setting:wav_examodifler_mergedprep(aini, values, modifiers)
    P.insertexa(oini, aini, index, index + 1)
    index = index + 1
  else
    -- ���p�N������g�ݗ���
    if wavfilepath ~= nil and setting.wav_lipsync == 1 then
      local aini = P.exaread(exabase, "lipsync")
      setting:wav_examodifler_lipsync(aini, values, modifiers)
      P.insertexa(oini, aini, index, index + 1)
      index = index + 1
    end

    -- ���ړI�X���C�_�[��g�ݗ���
    if setting.wav_mpslider > 0 then
      local aini = GCMZDrops.inistring("")
      setting:wav_examodifler_mpslider(aini, values, modifiers)
      P.insertexa(oini, aini, index, index + 1)
      index = index + 1
    end

    -- ����������g�ݗ���
    if setting.wav_subtitle > 0 then
      local aini = P.exaread(exabase, "subtitle")
      setting:wav_examodifler_subtitle(aini, values, modifiers)
      P.insertexa(oini, aini, index, index + 1)
      index = index + 1
    end
  end

  local filepath = GCMZDrops.createtempfile("wav", ".exo")
  f, err = io.open(filepath, "wb")
  if f == nil then
    error(err)
  end
  f:write(tostring(oini))
  f:close()
  debug_print("["..P.name.."] ���h���b�v���ꂽ�t�@�C���� exo �t�@�C���ɍ����ւ��܂����B")

  if state.frameadvance ~= nil and state.frameadvance > 0 then
    state.frameadvance = totallen
    if values.USER and values.USER.padding then
      state.frameadvance = state.frameadvance + values.USER.padding
    end
  end

  return {{filepath=filepath}}, state
end

return P
