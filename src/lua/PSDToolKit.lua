local P = {}
local PSDToolKitBridge = require("PSDToolKitBridge")

local function print(obj, msg)
  obj.load("figure", "\148\119\140\105", 0, 1, 1)
  obj.alpha = 0.75
  obj.draw()
  obj.setfont("MS UI Gothic", 16, 0, "0xffffff", "0x000000")
  obj.load("text", "<s,,B>" .. msg)
  obj.draw()
  -- �e�L�X�g�̂ڂ₯�h�~
  obj.ox = obj.w % 2 == 1 and 0.5 or 0
  obj.oy = obj.h % 2 == 1 and 0.5 or 0
end

local function getpixeldata(obj, width, height)
  local maxw, maxh = obj.getinfo("image_max")
  if width > maxw then
    width = maxw
  end
  if height > maxh then
    height = maxh
  end
  obj.setoption("drawtarget", "tempbuffer", width, height)
  obj.load("tempbuffer")
  return obj.getpixeldata("work")
end

local function fileexists(filepath)
  local f = io.open(filepath, "rb")
  if f ~= nil then
    f:close()
    return true
  end
  return false
end

local function getrenderindex()
  local frame, totalframe, render_index = PSDToolKitBridge.getcurrentframe()
  return render_index
end

local function isdead(subobj)
  local render_index = getrenderindex()
  if subobj.used == nil then
    subobj.used = render_index
    return false
  elseif subobj.used == render_index then
    return false
  end
  return true
end

local function wordwrap(text, options)
  if PSDToolKitBridge.wordwrap ~= nil then
    return PSDToolKitBridge.wordwrap(text, options)
  end
  return text
end

local PSDState = {}

-- �X�N���v�g����Ăяo���p
function PSDState.init(obj, o)
  local r = PSDState.new(
    (o.scene or 0)*1000+obj.layer,
    o.ptkf ~= "" and o.ptkf or nil,
    o.tag or 0,
    {
      layer = o.ptkl ~= "" and o.ptkl or nil,
      lipsync = o.lipsync ~= 0 and o.lipsync or nil,
      mpslider = o.mpslider ~= 0 and o.mpslider or nil,

      ls_locut = o.ls_locut ~= nil and o.ls_locut or 100,
      ls_hicut = o.ls_hicut ~= nil and o.ls_hicut or 1000,
      ls_threshold = o.ls_threshold ~= nil and o.ls_threshold or 20,
      ls_sensitivity = o.ls_sensitivity ~= nil and o.ls_sensitivity or 1,
    }
  )
  -- �����o�͂��Ȃ��ƒ���̃A�j���[�V�������ʈȊO�K�p����Ȃ�����
  -- ����ɑ΂��郏�[�N�A���E���h
  mes(" ")

  local subobj
  if o.mpslider ~= 0 then
    subobj = r.valueholder or P.emptysubobj
  elseif o.lipsync ~= 0 then
    subobj = r.talkstate ~= nil and r.talkstate.wavfile ~= "" and r.talkstate or P.emptysubobj
  else
    subobj = P.emptysubobj
  end
  return r, subobj
end

PSDState.cachekeys = {}

-- PSD�I�u�W�F�N�g
-- id - �ŗL���ʔԍ�
-- file - PSD�t�@�C���ւ̃p�X
-- tag - �ŗL���ʔԍ�(PSDToolKit �E�B���h�E�p)
-- opt - �ǉ��̐ݒ荀��
-- opt �ɂ͈ȉ��̂悤�ȃI�u�W�F�N�g��n��
-- {
--   layer = "���C���[�̏������",
--   lipsync = 2,
--   mpslider = 3,
-- }
function PSDState.new(id, file, tag, opt)
  local self = setmetatable({
    id = id,
    file = file,
    tag = tag,
    layer = {opt.layer or "L.0"},
    scale = 1,
    offsetx = 0,
    offsety = 0,
    valueholder = nil,
    valueholderindex = nil,
    talkstate = nil,
    talkstateindex = nil,
    rendered = false,
  }, {__index = PSDState})
  if opt.lipsync ~= nil then
    self.talkstate = P.talk:get(opt.lipsync)
    if self.talkstate ~= nil then
      self.talkstate.deflocut = opt.ls_locut
      self.talkstate.defhicut = opt.ls_hicut
      self.talkstate.defthreshold = opt.ls_threshold
      self.talkstate.defsensitivity = opt.ls_sensitivity
    end
    self.talkstateindex = opt.lipsync
  end
  if opt.mpslider ~= nil then
    self.valueholder = P.valueholder:get(opt.mpslider)
    self.valueholderindex = 1
  end
  return self
end

function PSDState:addstate(layer, index)
  -- index ���w�肳��Ă��Ȃ��ꍇ�� layer �̓��e�𒼐ڒǉ�
  -- (layer �� type �� ������)
  if index == nil then
    if layer ~= nil and layer ~= "" then
      table.insert(self.layer, layer)
    end
    return
  end

  -- index ���w�肳��Ă���ꍇ�� layer ���̍��ڂ̂ЂƂ����蓖�Ă邪�A
  -- ���� valueholder �����݂���ꍇ�� index ���㏑������
  if self.valueholder ~= nil then
    index = self.valueholder:get(index, self.valueholderindex, 0)
    self.valueholderindex = self.valueholderindex + 1
  end
  -- �l���͈͊O�łȂ���Ί��蓖��
  if 0 < index and index <= #layer then
    table.insert(self.layer, layer[index])
  end
end

function PSDState:adjustcenter(obj)
  local w, h = obj.getpixel()
  obj.ox = w % 2 == 1 and 0.5 or 0
  obj.oy = h % 2 == 1 and 0.5 or 0
end

function PSDState:render(obj)
  if self.rendered then
    error("already rendered")
  end
  if self.file == nil then
    error("no image")
  end
  self.rendered = true
  if #self.layer > 0 then
    local layer = {}
    for i, v in ipairs(self.layer) do
      local typ = PSDToolKitBridge.type(v)
      if typ == "string" then
        table.insert(layer, v)
      elseif typ == "table" and PSDToolKitBridge.type(v.getstate) == "function" then
        table.insert(layer, v:getstate(self, obj))
      end
    end
    self.layer = table.concat(layer, " ")
  end
  local modified, cachekey, width, height = PSDToolKitBridge.setprops(self.id, self.file, self)
  local cacheid = "cache:"..self.id.." "..self.file
  if (not modified)or((PSDState.cachekeys[cacheid] or 0) == cachekey) then
    if obj.copybuffer("obj", cacheid) then
      self:adjustcenter(obj)
      return
    end
    local data, w, h = getpixeldata(obj, width, height)
    if pcall(PSDToolKitBridge.getcache, cacheid, data, w * 4 * h) then
      obj.putpixeldata(data)
      obj.copybuffer(cacheid, "obj")
      self:adjustcenter(obj)
      return
    end
  end
  local data, w, h = getpixeldata(obj, width, height)
  PSDToolKitBridge.draw(self.id, self.file, data, w, h)
  PSDToolKitBridge.putcache(cacheid, data, w * 4 * h, false)
  obj.putpixeldata(data)
  obj.copybuffer(cacheid, "obj")
  self:adjustcenter(obj)
  PSDState.cachekeys[cacheid] = cachekey
end

local Blinker = {}

-- �u���A�j���[�^�[
-- patterns - {'��', '�قڕ�', '���J��', '�قڊJ��', '�J��'} �̃p�^�[�����������z��i�قڕ��A���ځA�قڊJ���͏ȗ��j
-- interval - �A�j���[�V�����Ԋu(�b)
-- speed - �A�j���[�V�������x
-- offset - �A�j���[�V�����J�n�ʒu
function Blinker.new(patterns, interval, speed, offset)
  if #patterns > 3 then
    -- 3�R�}�ȏ゠��Ȃ�擪�Ɂu�قڊJ���v�����̂��̂�}������
    -- �J�����قڊJ���������قڕ������ځ��قڊJ�����J���@�̂悤��
    -- ���n�߂�A�j���[�V�����̒���A���Ɉڍs����悤�ɂ���
    table.insert(patterns, 1, patterns[#patterns-1])
  end
  return setmetatable({
    patterns = patterns,
    interval = interval,
    speed = speed,
    offset = offset
  }, {__index = Blinker})
end

function Blinker:getstate(psd, obj)
  if #self.patterns < 2 then
    error("�ڃp�`�ɂ͏��Ȃ��Ƃ��u�J���v�u���v�̃p�^�[���ݒ肪�K�v�ł�")
  end
  local interval = self.interval * obj.framerate + self.speed * #self.patterns*2;
  local basetime = obj.frame + interval + self.offset
  local blink = basetime % interval
  local blink2 = (basetime + self.speed*#self.patterns) % (interval * 5)
  for i, v in ipairs(self.patterns) do
    local l = self.speed*i
    local r = l + self.speed
    if (l <= blink and blink < r)or(l <= blink2 and blink2 < r) then
      return v
    end
  end
  return self.patterns[#self.patterns]
end

local LipSyncSimple = {}

-- ���p�N�i�J�̂݁j
-- patterns - {'��', '�قڕ�', '���J��', '�قڊJ��', '�J��'} �̃p�^�[�����������z��i�قڕ��A���ځA�قڊJ���͏ȗ��j
-- speed - �A�j���[�V�������x
-- alwaysapply - ���p�N�����̃f�[�^���Ȃ��Ă�����K�p����
function LipSyncSimple.new(patterns, speed, alwaysapply)
  return setmetatable({
    patterns = patterns,
    speed = speed,
    alwaysapply = alwaysapply,
  }, {__index = LipSyncSimple})
end

LipSyncSimple.states = {}

function LipSyncSimple:getstate(psd, obj)
  if #self.patterns < 2 then
    error("���p�N�ɂ͏��Ȃ��Ƃ��u�J���v�u���v�̃p�^�[���ݒ肪�K�v�ł�")
  end
  if psd.talkstateindex == nil then
    error("���p�N���������郌�C���[�ԍ����w�肵�Ă�������")
  end

  local stat = LipSyncSimple.states[obj.layer] or {time = obj.time, n = -1, pat = 0}
  if stat.time > obj.time or stat.time + 1 < obj.time then
    -- �����߂��Ă�����A���܂�ɐ�ɐi��ł���悤�Ȃ�A�j���[�V�����̓��Z�b�g����
    -- �v���r���[�ŃR�}��т���ꍇ�͐����������������Ȃ��̂ŁA1�b�̗P�\����������
    stat.n = -1
    stat.pat = 0
  end
  stat.n = stat.n + 1
  stat.time = obj.time
  if stat.n >= self.speed then
    local volume = psd.talkstate ~= nil and psd.talkstate:getvolume() or 0
    if volume >= 1.0 then
      if stat.pat < #self.patterns - 1 then
        stat.pat = stat.pat + 1
        stat.n = 0
      end
    else
      if stat.pat > 0 then
        stat.pat = stat.pat - 1
        stat.n = 0
      end
    end
  end
  LipSyncSimple.states[obj.layer] = stat
  if psd.talkstate == nil and not self.alwaysapply then
    return ""
  end
  return self.patterns[stat.pat + 1]
end

local LipSyncLab = {}

-- ���p�N�i�����������j
-- patterns - {'a'='��', 'e'='��', 'i'='��', 'o'='��','u'='��', 'N'='��'}
-- mode - �q���̏������[�h
-- alwaysapply - ���p�N�����̃f�[�^���Ȃ��Ă�����K�p����
function LipSyncLab.new(patterns, mode, alwaysapply)
  if patterns.A == nil then patterns.A = patterns.a end
  if patterns.E == nil then patterns.E = patterns.e end
  if patterns.I == nil then patterns.I = patterns.i end
  if patterns.O == nil then patterns.O = patterns.o end
  if patterns.U == nil then patterns.U = patterns.u end
  return setmetatable({
    patterns = patterns,
    mode = mode,
    alwaysapply = alwaysapply,
  }, {__index = LipSyncLab})
end

LipSyncLab.states = {}

function LipSyncLab:getstate(psd, obj)
  local pat = self.patterns
  if pat.a == nil or pat.e == nil or pat.i == nil or pat.o == nil or pat.u == nil or pat.N == nil then
    error("���p�N�ɂ́u���v�u���v�u���v�u���v�u���v�u��v�S�Ẵp�^�[���ݒ肪�K�v�ł�")
  end
  if psd.talkstateindex == nil then
    error("���p�N���������郌�C���[�ԍ����w�肵�Ă�������")
  end
  local ts = psd.talkstate
  if ts == nil then
    -- �f�[�^��������Ȃ������ꍇ�͕���Ԃɂ���
    return self.alwaysapply and pat.N or ""
  end

  if ts.cur == "" then
    -- ���f��񂪂Ȃ����͉��ʂɉ����āu���v�̌`���g��
    -- �ilab �t�@�C�����g�킸�Ɂu���p�N�@�����������v���g���Ă���ꍇ�̑[�u�j
    if ts:getvolume() >= 1.0 then
      return pat.a
    end
    return pat.N
  end

  if self.mode == 0 then
    -- �q�������^�C�v0 -> �S�āu��v
    if ts:curisvowel() ~= 0 then
      -- �ꉹ�͐ݒ肳�ꂽ�`�����̂܂܎g��
      return pat[ts.cur]
    end
    return pat.N
  elseif self.mode == 1 then
    -- �q�������^�C�v1 -> �������q���ȊO�͑O�̕ꉹ�������p��
    local stat = LipSyncLab.states[obj.layer] or {frame = obj.frame-1, p = "N"}
    if stat.frame >= obj.frame or stat.frame + obj.framerate < obj.frame then
      -- �����߂��Ă�����A���܂�ɐ�ɐi��ł���悤�Ȃ�A�j���[�V�����̓��Z�b�g����
      -- �v���r���[�ŃR�}��т���ꍇ�͐����������������Ȃ��̂ŁA1�b�̗P�\����������
      stat.p = "N"
    end
    stat.frame = obj.frame
    if ts:curisvowel()  == 1 then
      -- �ꉹ�͐ݒ肳�ꂽ�`�����̂܂܎g���i�������ꉹ�͏��O�j
      stat.p = ts.cur
    elseif ts.cur == "pau" or ts.cur == "N" or ts.cur == "cl" then
      -- pau / �� / �����i���j
      stat.p = "N"
    else
      -- ����ȊO�̎q���ł͂��̂܂܈����p��
    end
    LipSyncLab.states[obj.layer] = stat
    return pat[stat.p]
  elseif self.mode == 2 then
    -- �q�������^�C�v2 -> �������q���ȊO�͑O��̕ꉹ�̌`��菬�������̂ŕ��
    if ts:curisvowel() ~= 0 then
      -- �ꉹ�͐ݒ肳�ꂽ�`�����̂܂܎g��
      return pat[ts.cur]
    end
    if ts.cur == "pau" or ts.cur == "N" or ts.cur == "m" or ts.cur == "p" or ts.cur == "b" or ts.cur == "v" then
      -- pau / �� / �q���i�܁E�ρE�΁E���s�j
      return pat.N
    end
    if ts.cur == "cl" then
      -- �����i���j
      if ts.progress < 0.5 then
        -- �ЂƂO���ꉹ�ŁA���A�������ꏊ�ɑ��݂��Ă���Ȃ�O���͂��̕ꉹ�̌`�������p��
        if ts:previsvowel() ~= 0 and ts.prev_end == ts.cur_start then
          return pat[ts.prev]
        end
        return pat.N
      else
        -- �㔼�́u���v�̌`�ň����p��
        return pat.u
      end
    end
    -- ��������Ȃ������S�Ă̎q���̃f�t�H���g����
    -- �אڂ���O��̕ꉹ�Ɉˑ����Č`�����肷��
    if ts.progress < 0.5 then
      -- �O���͑O�̕ꉹ�������p��
      if ts:previsvowel() ~= 0 and ts.prev_end == ts.cur_start then
        -- �O�̕ꉹ���Ȃ�ׂ��������J�����ɂȂ�悤��
        if ts.prev == "a" or ts.prev == "A" then
          return pat.o
        elseif ts.prev == "i" or ts.prev == "I" then
          return pat.i
        else
          return pat.u
        end
      end
      return pat.N
    else
      -- �㔼�͌��̕ꉹ���s������
      if ts:nextisvowel() ~= 0 and ts.next_start == ts.cur_end then
        -- �O�̕ꉹ���Ȃ�ׂ��������J�����ɂȂ�悤��
        if ts.next == "a" or ts.next == "A" then
          return pat.o
        elseif ts.next == "i" or ts.next == "I" then
          return pat.i
        else
          return pat.u
        end
      end
      return pat.N
    end
  end
  error("unexpected consonant processing mode")
end

local TalkState = {}

function TalkState.isvowel(p)
  if p == "a" or p == "e" or p == "i" or p == "o" or p == "u" then
    return 1
  end
  if p == "A" or p == "E" or p == "I" or p == "O" or p == "U" then
    return -1
  end
  return 0
end

function TalkState.new(frame, time, totalframe, totaltime)
  return setmetatable({
    used = nil,
    frame = frame,
    time = time,
    totalframe = totalframe,
    totaltime = totaltime,
    wavfile = "",
    cstate = nil,
    locut = 0,
    hicut = 0,
    threshold = 0,
    sensitivity = 0,
    deflocut = 0,
    defhicut = 0,
    defthreshold = 0,
    defsensitivity = 0,
    progress = 0,
    cur = "",
    cur_start = 0,
    cur_end = 0,
    prev = "",
    prev_start = 0,
    prev_end = 0,
    next = "",
    next_start = 0,
    next_end = 0
  }, {__index = TalkState})
end

function TalkState:curisvowel()
  return TalkState.isvowel(self.cur)
end

function TalkState:previsvowel()
  return TalkState.isvowel(self.prev)
end

function TalkState:nextisvowel()
  return TalkState.isvowel(self.next)
end

function TalkState:getvolume()
  local locut = self.locut ~= 0 and self.locut or self.deflocut
  local hicut = self.hicut ~= 0 and self.hicut or self.defhicut
  local v = PSDToolKitBridge.getspeaklevel(self.wavfile, self.time, locut, hicut) * 100
  local threshold = self.threshold ~= 0 and self.threshold or self.defthreshold
  local sensitivity = self.sensitivity ~= 0 and self.sensitivity or self.defsensitivity
  return self.cstate:getvolume(v, self.time, sensitivity) / threshold
end

function TalkState:setfile(wavfile, locut, hicut, threshold, sensitivity)
  self.wavfile = wavfile
  self.locut = locut
  self.hicut = hicut
  self.threshold = threshold
  self.sensitivity = sensitivity
end

function TalkState:setphoneme(labfile, time)
  time = time * 10000000
  local line
  local f = io.open(labfile, "r")
  if f == nil then
    error("file not found: " .. labfile)
  end
  for line in f:lines() do
    local st, ed, p = string.match(line, "([0-9.]+) ([0-9.]+) (.+)")
    if st == nil then
      return nil -- unexpected format
    end
    st = st + 0
    ed = ed + 0
    if st <= time then
      if time < ed then
        if self.cur == "" then
          self.progress = (time - st)/(ed - st)
          self.cur = p
          self.cur_start = st
          self.cur_end = ed
        end
      else
        self.prev = p
        self.prev_start = st
        self.prev_end = ed
      end
    else
      self.next = p
      self.next_start = st
      self.next_end = ed
      f:close()
      return
    end
  end
  f:close()
end

local TalkContinuousState = {}

function TalkContinuousState.new()
  return setmetatable({
    time = -1,
    vols = {},
  }, {__index = TalkContinuousState})
end

function TalkContinuousState:getvolume(volume, time, sensitivity)
  if self.time == time then
    self.vols[#self.vols] = volume
    return volume/sensitivity
  end
  if self.time > time or self.time + 1 < time then
    self.vols = {}
    self.user = {}
  end
  table.insert(self.vols, volume)
  if #self.vols > sensitivity then
    table.remove(self.vols, #self.vols - sensitivity)
  end
  self.time = time
  local vol = 0
  for _, v in pairs(self.vols) do
    vol = vol + v
  end
  return vol/sensitivity
end

local TalkStates = {}

function TalkStates.new()
  return setmetatable({
    states = {},
    cstates = {},
  }, {__index = TalkStates})
end

function TalkStates:set(obj, srcfile, locut, hicut, threshold, sensitivity)
  local wav, lab = PSDToolKitBridge.getwavlabpath(srcfile)
  local t = TalkState.new(obj.frame, obj.time, obj.totalframe, obj.totaltime)
  if wav ~= nil then
    t:setfile(wav, locut, hicut, threshold, sensitivity)
  end
  if lab ~= nil then
    t:setphoneme(lab, obj.time)
  end

  local cstate = self.cstates[obj.layer]
  if cstate == nil then
    cstate = TalkContinuousState.new()
    self.cstates[obj.layer] = cstate
  end
  t.cstate = cstate

  self.states[obj.layer] = t
end

function TalkStates:setphoneme(obj, phonemestr)
  local t = TalkState.new(obj.frame, obj.time, obj.totalframe, obj.totaltime)
  t.progress = obj.time / obj.totaltime
  t.cur = phonemestr
  t.cur_start = 1
  t.cur_end = obj.totaltime + 1

  local cstate = self.cstates[obj.layer]
  if cstate == nil then
    cstate = TalkContinuousState.new()
    self.cstates[obj.layer] = cstate
  end
  t.cstate = cstate

  self.states[obj.layer] = t
end

function TalkStates:get(index)
  local ts = self.states[index]
  if ts == nil or isdead(ts) then
    self.states[index] = nil
    return nil
  end
  return ts
end

local SubtitleState = {}

function SubtitleState.new(text, x, y, z, frame, time, totalframe, totaltime, unescape)
  if unescape then
    text = text:gsub("([\128-\160\224-\255]\092)\092", "%1")
  end
  return setmetatable({
    used = nil,
    text = text,
    x = x,
    y = y,
    z = z,
    frame = frame,
    time = time,
    totalframe = totalframe,
    totaltime = totaltime
  }, {__index = SubtitleState})
end

function SubtitleState:mes(obj, opts)
  -- v0.2.0beta64 �ȑO
  --   opts = nil -- �����Ȃ�
  -- v0.2.0beta65 �ȍ~
  --   opts = {
  --     wordwrap = {
  --       mode = 0,
  --       width = 0,
  --     },
  --   }
  local text = self.text
  if opts.wordwrap ~= nil then
    text = wordwrap(text, opts.wordwrap)
  end
  obj.mes(text)
end

function SubtitleState:mesfast(obj, opts)
  -- v0.2.0beta64 �ȑO
  --   opts = 0 -- �L���b�V�����[�h
  -- v0.2.0beta65 �ȍ~
  --   opts = {
  --     cache = 0,
  --     wordwrap = {
  --       mode = 0,
  --       width = 0,
  --     },
  --   }
  if PSDToolKitBridge.type(opts) == "number" then
    -- v0.2.0beta64 �ȑO�̌`���͕ϊ�����
    opts = {cache = opts}
  end
  local text = self.text
  if opts.wordwrap ~= nil then
    text = wordwrap(text, opts.wordwrap)
  end
  require("CacheText").rawmes(text, opts.cache or 0)
end

local SubtitleStates = {}

function SubtitleStates.new()
  return setmetatable({
    states = {}
  }, {__index = SubtitleStates})
end

function SubtitleStates:set(text, obj, unescape)
  if obj.frame > obj.totalframe then
    -- ���̏ꍇ�̓V�[���`�F���W�ɂ���ăI�u�W�F�N�g�������I�Ɉ����L�΂���Ă���Ǝv���邪�A
    -- �����炭�命���̃P�[�X�Ŏ����͈����L�΂��ĕ\������邱�Ƃ��Ӑ}���Ă��Ȃ��͂��Ȃ̂ŁA�����őł��؂�
    -- ����ɔ�������p�Ŏ����\�����V�[���`�F���W���g���ăt�F�[�h�A�E�g������ł��Ȃ��Ȃ邪�A
    -- �n��g�AYouTube�ANetflix �ȂǑ命���̉f���z�M�ɂ����Ă͎����͓���Ƃ͕ʂ̎d�g�݂Ŏ�������Ă���A
    -- �����Ɖf���͈ꏏ�ɃN���X�t�F�[�h�����肵�Ȃ��̂���ʓI�Ǝv����̂Ŗ��ɂȂ�Ȃ��͂�
    return
  end
  self.states[obj.layer] = SubtitleState.new(
    text,
    obj.x,
    obj.y,
    obj.z,
    obj.frame,
    obj.time,
    obj.totalframe,
    obj.totaltime,
    unescape
  )
end

function SubtitleStates:get(index)
  return self.states[index]
end

function SubtitleStates:getlive(index, obj)
  local s = self.states[index]
  if s == nil or isdead(s) then
    self.states[index] = nil
    return P.emptysubobj
  end
  return s
end

function SubtitleStates:mes(index, obj, opts)
  local s = self:getlive(index, obj)
  if s.notfound then
    return s
  end
  s:mes(obj, opts)
  return s
end

function SubtitleStates:mesfast(index, obj, opts)
  local s = self:getlive(index, obj)
  if s.notfound then
    return s
  end
  s:mesfast(obj, opts)
  return s
end

local ValueHolder = {}

function ValueHolder.new(x, y, z, frame, time, totalframe, totaltime)
  return setmetatable({
    used = nil,
    created = getrenderindex(),
    values = {},
    x = x,
    y = y,
    z = z,
    frame = frame,
    time = time,
    totalframe = totalframe,
    totaltime = totaltime
  }, {__index = ValueHolder})
end

function ValueHolder:add(value)
  table.insert(self.values, value)
end

function ValueHolder:get(defvalue, vhindex, unusedvalue)
  if (vhindex < 1)or(vhindex > #self.values) then
    return defvalue
  end
  local v = self.values[vhindex]
  if v == unusedvalue then
    return defvalue
  end
  return v
end

local MultipleValueHolder = {}

function MultipleValueHolder.new(holders, x, y, z, frame, time, totalframe, totaltime)
  return setmetatable({
    used = nil,
    holders = holders,
    x = x,
    y = y,
    z = z,
    frame = frame,
    time = time,
    totalframe = totalframe,
    totaltime = totaltime
  }, {__index = MultipleValueHolder})
end

function MultipleValueHolder:get(defvalue, vhindex, unusedvalue)
  local idx = -1
  for i in ipairs(self.holders) do
    idx = self.holders[i]:get(-1, vhindex, unusedvalue)
    if idx ~= -1 then
      return idx
    end
  end
  return defvalue
end

local ValueHolderStates = {}

function ValueHolderStates.new()
  return setmetatable({
    states = {}
  }, {__index = ValueHolderStates})
end

function ValueHolderStates:set(index, values, obj)
  local vh = self.states[index]
  if vh == nil or vh.created ~= getrenderindex() then
    vh = ValueHolder.new(
      obj.x,
      obj.y,
      obj.z,
      obj.frame,
      obj.time,
      obj.totalframe,
      obj.totaltime
    )
    self.states[index] = vh
  end
  for i in ipairs(values) do
    vh:add(values[i])
  end
end

function ValueHolderStates:get(index)
  local indices
  if PSDToolKitBridge.type(index) == "table" then
    indices = index
  else
    indices = {index}
  end
  local vhs = {}
  local first = P.emptysubobj
  for i in ipairs(indices) do
    local vh = self.states[indices[i]]
    if vh == nil or isdead(vh) then
      self.states[indices] = nil
    else
      table.insert(vhs, vh)
      if i == 1 then
        first = vh
      end
    end
  end
  if #vhs == 0 then
    return nil
  else
    return MultipleValueHolder.new(
      vhs,
      first.x,
      first.y,
      first.z,
      first.frame,
      first.time,
      first.totalframe,
      first.totaltime
    )
  end
end

local PrepObject = {}

function PrepObject.new()
  return setmetatable({
    o = {},
    layer = 0,
    frame = 0,
    totalframe = 0,
    framerate = 30,
  }, {__index = PrepObject})
end

-- �X�N���v�g����Ăяo���p
function PrepObject.init(o, obj, text)
  P.prep:set(o, obj)
  if text ~= "" then
    local st = P.prep:getst(obj)
    if st ~= nil then
      P.subtitle:set(text, st, true)
    end
  end

  -- �����o�͂��Ȃ��ƒ���̃A�j���[�V�������ʈȊO�K�p����Ȃ�����
  -- ����ɑ΂��郏�[�N�A���E���h
  mes(" ")
end

function PrepObject:fakeobj(mgl, mgr)
  local totalframe = self.totalframe - mgl - mgr
  local frame = self.frame - mgl
  if totalframe < 0 or frame < 0 or frame > totalframe then
    return nil
  end
  return {
    layer = self.layer,
    x = self.x,
    y = self.y,
    z = self.z,
    frame = frame,
    time = frame / self.framerate,
    totalframe = totalframe,
    totaltime = totalframe / self.framerate,
  }
end

function PrepObject:set(o, obj)
  self.o = o
  self.layer = obj.layer
  self.created = getrenderindex()
  self.x = obj.x
  self.y = obj.y
  self.z = obj.z
  self.frame = obj.frame
  self.totalframe = obj.totalframe
  self.framerate = obj.framerate
end

function PrepObject:getst(obj)
  if self.layer ~= obj.layer or self.created ~= getrenderindex() then
    return nil
  end
  return self:fakeobj(self.o.st_mgl or 0, self.o.st_mgr or 0)
end

function PrepObject:getls(obj)
  if self.layer ~= obj.layer or self.created ~= getrenderindex() then
    return nil
  end
  return self:fakeobj(self.o.ls_mgl or 0, self.o.ls_mgr or 0)
end

function PrepObject:getsl(obj)
  if self.layer ~= obj.layer or self.created ~= getrenderindex() then
    return nil
  end
  return self:fakeobj(self.o.sl_mgl or 0, self.o.sl_mgr or 0)
end

P.talk = TalkStates.new()
P.subtitle = SubtitleStates.new()
P.valueholder = ValueHolderStates.new()
P.prep = PrepObject.new()

P.emptysubobj = {
  x = 0,
  y = 0,
  z = 0,
  frame = 0,
  time = 0,
  totalframe = 1,
  totaltime = 1,
  notfound = true
}

P.print = print
P.wordwrap = wordwrap
P.PSDState = PSDState
P.Blinker = Blinker
P.LipSyncSimple = LipSyncSimple
P.LipSyncLab = LipSyncLab
return P
