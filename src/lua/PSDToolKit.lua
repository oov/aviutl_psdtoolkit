local P = {}
local PSDToolKitBridge = require("PSDToolKitBridge")

local function print(obj, msg)
  obj.load("figure", "\148\119\140\105", 0, 1, 1)
  obj.alpha = 0.75
  obj.draw()
  obj.setfont("MS UI Gothic", 16, 0, "0xffffff", "0x000000")
  obj.load("text", "<s,,B>" .. msg)
  obj.draw()
  -- テキストのぼやけ防止
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

local PSDState = {}

-- スクリプトから呼び出す用
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
  -- 何も出力しないと直後のアニメーション効果以外適用されないため
  -- それに対するワークアラウンド
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

-- PSDオブジェクト
-- id - 固有識別番号
-- file - PSDファイルへのパス
-- tag - 固有識別番号(PSDToolKit ウィンドウ用)
-- opt - 追加の設定項目
-- opt には以下のようなオブジェクトを渡す
-- {
--   layer = "レイヤーの初期状態",
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
  -- index が指定されていない場合は layer の内容を直接追加
  -- (layer の type が 文字列)
  if index == nil then
    if layer ~= nil and layer ~= "" then
      table.insert(self.layer, layer)
    end
    return
  end

  -- index が指定されている場合は layer 内の項目のひとつを割り当てるが、
  -- もし valueholder が存在する場合は index を上書きする
  if self.valueholder ~= nil then
    index = self.valueholder:get(index, self.valueholderindex, 0)
    self.valueholderindex = self.valueholderindex + 1
  end
  -- 値が範囲外でなければ割り当て
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

-- 瞬きアニメーター
-- patterns - {'閉じ', 'ほぼ閉じ', '半開き', 'ほぼ開き', '開き'} のパターンが入った配列（ほぼ閉じ、半目、ほぼ開きは省略可）
-- interval - アニメーション間隔(秒)
-- speed - アニメーション速度
-- offset - アニメーション開始位置
function Blinker.new(patterns, interval, speed, offset)
  if #patterns > 3 then
    -- 3コマ以上あるなら先頭に「ほぼ開き」相当のものを挿入して
    -- 開き→ほぼ開き→閉じ→ほぼ閉じ→半目→ほぼ開き→開き　のように
    -- 閉じ始めるアニメーションの直後、閉じに移行するようにする
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
    error("目パチには少なくとも「開き」「閉じ」のパターン設定が必要です")
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

-- 口パク（開閉のみ）
-- patterns - {'閉じ', 'ほぼ閉じ', '半開き', 'ほぼ開き', '開き'} のパターンが入った配列（ほぼ閉じ、半目、ほぼ開きは省略可）
-- speed - アニメーション速度
-- alwaysapply - 口パク準備のデータがなくても閉じを適用する
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
    error("口パクには少なくとも「開き」「閉じ」のパターン設定が必要です")
  end
  if psd.talkstateindex == nil then
    error("口パク準備があるレイヤー番号を指定してください")
  end

  local stat = LipSyncSimple.states[obj.layer] or {time = obj.time, n = -1, pat = 0}
  if stat.time > obj.time or stat.time + 1 < obj.time then
    -- 巻き戻っていたり、あまりに先に進んでいるようならアニメーションはリセットする
    -- プレビューでコマ飛びする場合は正しい挙動を示せないので、1秒の猶予を持たせる
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

-- 口パク（あいうえお）
-- patterns - {'a'='あ', 'e'='え', 'i'='い', 'o'='お','u'='う', 'N'='ん'}
-- mode - 子音の処理モード
-- alwaysapply - 口パク準備のデータがなくても閉じを適用する
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
    error("口パクには「あ」「い」「う」「え」「お」「ん」全てのパターン設定が必要です")
  end
  if psd.talkstateindex == nil then
    error("口パク準備があるレイヤー番号を指定してください")
  end
  local ts = psd.talkstate
  if ts == nil then
    -- データが見つからなかった場合は閉じ状態にする
    return self.alwaysapply and pat.N or ""
  end

  if ts.cur == "" then
    -- 音素情報がない時は音量に応じて「あ」の形を使う
    -- （lab ファイルを使わずに「口パク　あいうえお」を使っている場合の措置）
    if ts:getvolume() >= 1.0 then
      return pat.a
    end
    return pat.N
  end

  if self.mode == 0 then
    -- 子音処理タイプ0 -> 全て「ん」
    if ts:curisvowel() ~= 0 then
      -- 母音は設定された形をそのまま使う
      return pat[ts.cur]
    end
    return pat.N
  elseif self.mode == 1 then
    -- 子音処理タイプ1 -> 口を閉じる子音以外は前の母音を引き継ぐ
    local stat = LipSyncLab.states[obj.layer] or {frame = obj.frame-1, p = "N"}
    if stat.frame >= obj.frame or stat.frame + obj.framerate < obj.frame then
      -- 巻き戻っていたり、あまりに先に進んでいるようならアニメーションはリセットする
      -- プレビューでコマ飛びする場合は正しい挙動を示せないので、1秒の猶予を持たせる
      stat.p = "N"
    end
    stat.frame = obj.frame
    if ts:curisvowel()  == 1 then
      -- 母音は設定された形をそのまま使う（無声化母音は除外）
      stat.p = ts.cur
    elseif ts.cur == "pau" or ts.cur == "N" or ts.cur == "cl" then
      -- pau / ん / 促音（っ）
      stat.p = "N"
    else
      -- それ以外の子音ではそのまま引き継ぐ
    end
    LipSyncLab.states[obj.layer] = stat
    return pat[stat.p]
  elseif self.mode == 2 then
    -- 子音処理タイプ2 -> 口を閉じる子音以外は前後の母音の形より小さいもので補間
    if ts:curisvowel() ~= 0 then
      -- 母音は設定された形をそのまま使う
      return pat[ts.cur]
    end
    if ts.cur == "pau" or ts.cur == "N" or ts.cur == "m" or ts.cur == "p" or ts.cur == "b" or ts.cur == "v" then
      -- pau / ん / 子音（ま・ぱ・ば・ヴ行）
      return pat.N
    end
    if ts.cur == "cl" then
      -- 促音（っ）
      if ts.progress < 0.5 then
        -- ひとつ前が母音で、かつ連続した場所に存在しているなら前半はその母音の形を引き継ぐ
        if ts:previsvowel() ~= 0 and ts.prev_end == ts.cur_start then
          return pat[ts.prev]
        end
        return pat.N
      else
        -- 後半は「う」の形で引き継ぐ
        return pat.u
      end
    end
    -- 処理されなかった全ての子音のデフォルト処理
    -- 隣接する前後の母音に依存して形を決定する
    if ts.progress < 0.5 then
      -- 前半は前の母音を引き継ぐ
      if ts:previsvowel() ~= 0 and ts.prev_end == ts.cur_start then
        -- 前の母音よりなるべく小さい開け方になるように
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
      -- 後半は後ろの母音を先行させる
      if ts:nextisvowel() ~= 0 and ts.next_start == ts.cur_end then
        -- 前の母音よりなるべく小さい開け方になるように
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

function SubtitleState:mes(obj)
  obj.mes(self.text)
end

function SubtitleState:mesfast(obj, mode)
  require("CacheText").rawmes(self.text, mode)
end

local SubtitleStates = {}

function SubtitleStates.new()
  return setmetatable({
    states = {}
  }, {__index = SubtitleStates})
end

function SubtitleStates:set(text, obj, unescape)
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

function SubtitleStates:mes(index, obj)
  local s = self:getlive(index, obj)
  if s.notfound then
    return s
  end
  s:mes(obj)
  return s
end

function SubtitleStates:mesfast(index, obj, mode)
  local s = self:getlive(index, obj)
  if s.notfound then
    return s
  end
  s:mesfast(obj, mode)
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

-- スクリプトから呼び出す用
function PrepObject.init(o, obj, text)
  P.prep:set(o, obj)
  if text ~= "" then
    local st = P.prep:getst(obj)
    if st ~= nil then
      P.subtitle:set(text, st, true)
    end
  end

  -- 何も出力しないと直後のアニメーション効果以外適用されないため
  -- それに対するワークアラウンド
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
P.PSDState = PSDState
P.Blinker = Blinker
P.LipSyncSimple = LipSyncSimple
P.LipSyncLab = LipSyncLab
return P
