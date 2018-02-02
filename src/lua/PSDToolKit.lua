PSDToolKit = PSDToolKit or {}

PSDToolKit.psd = {
  id = 0,
  file = "",
  layer = {},
  faview = {},
  scale = 1,
  offsetx = 0,
  offsety = 0,
  rendered = false,
  init = function(self, id, file, scale, offsetx, offsety)
    self.id = id
    self.file = file
    self.layer = {}
    self.faview = {}
    self.scale = scale
    self.offsetx = offsetx
    self.offsety = offsety
    self.rendered = false
  end,
  addstate = function(self, layer)
    table.insert(self.layer, layer)
  end,
  render = function(self)
    if self.rendered then
      self:msg("[PSDToolKit] ALREADY RENDERED")
      return
    end
    self.rendered = true
    require("PSDToolKitBridge")
    if self.file == "" then
      self:msg("[PSDToolKit] NO IMAGE")
      return
    end
    if #self.faview > 0 then
      local empty = true
      for i, v in ipairs(self.faview) do
        if v == -1 then
          self.faview[i] = ""
        else
          empty = false
        end
      end
      if not empty then
        self:addstate("S." .. table.concat(self.faview, "_"))
      end
    end
    if #self.layer > 0 then
      self.layer = table.concat(self.layer, " ")
    end
    local ok, modified, width, height = PSDToolKitBridge.setprops(self.id, self.file, self)
    if not ok then
      self:msg("[PSDToolKit] CANNOT LOAD\n\n"..modified)
      return
    end
    if not modified then
      local data, w, h = self:getpixeldata(width, height)
      if PSDToolKitBridge.getcache("cache:"..self.id.." "..self.file, data, w * 4 * h) then
        obj.putpixeldata(data)
        obj.cx = w % 2 == 1 and 0.5 or 0
        obj.cy = h % 2 == 1 and 0.5 or 0
        return
      end
    end
    local data, w, h = self:getpixeldata(width, height)
    local ok, msg = PSDToolKitBridge.draw(self.id, self.file, data, w, h)
    if not ok then
      self:msg("[PSDToolKit] CANNOT RENDER\n\n"..msg)
      return
    end
    PSDToolKitBridge.putcache("cache:"..self.id.." "..self.file, data, w * 4 * h, false)
    obj.putpixeldata(data)
    obj.cx = w % 2 == 1 and 0.5 or 0
    obj.cy = h % 2 == 1 and 0.5 or 0
  end,
  getpixeldata = function(self, width, height)
    local maxw, maxh = obj.getinfo("image_max")
    if width > maxw then
      width = maxw
    end
    if height > maxh then
      height = maxh
    end
    obj.setoption("drawtarget", "tempbuffer", width, height)
    obj.copybuffer("obj", "tmp")
    return obj.getpixeldata()
  end,
  msg = function(self, msg)
    obj.load("figure", "\148\119\140\105", 0, 1, 1)
    obj.alpha = 0.75
    obj.draw()
    obj.setfont("Arial", 16, 0, "0xffffff", "0x000000")
    obj.load("text", "<s,,B>" .. msg)
    obj.draw()
    obj.cx = obj.w % 2 == 1 and 0.5 or 0
    obj.cy = obj.h % 2 == 1 and 0.5 or 0
  end
}

PSDToolKit.talking = function(buf, rate, lo, hi, thr)
  if thr == 0 then
    return 0
  end
  local n = #buf
  local hzstep = rate / 2 / 1024
  local v, d, hz = 0, 0, 0
  for i in ipairs(buf) do
    hz = math.pow(2, 10*((i-1)/n))*hzstep
    if lo < hz then
      if hz > hi then
        break
      end
      v = v + buf[i]
      d = d + 1
    end
  end
  if d > 0 then
    v = v / d
  end
  return v / thr
end

PSDToolKit.talkingphoneme = function(labfile, time)
  time = time * 10000000
  local line
  local f = io.open(labfile, "r")
  local r = {
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
  }
  for line in f:lines() do
    local st, ed, p = string.match(line, "(%d+) (%d+) (%a+)")
    if st == nil then
      return nil -- unexpected format
    end
    st = st + 0
    ed = ed + 0
    if st <= time then
      if time < ed then
        if r.cur == "" then
          r.progress = (time - st)/(ed - st)
          r.cur = p
          r.cur_start = st
          r.cur_end = ed
        end
      else
        r.prev = p
        r.prev_start = st
        r.prev_end = ed
      end
    else
      r.next = p
      r.next_start = st
      r.next_end = ed
      f:close()
      return r
    end
  end
  f:close()
  return r
end

PSDToolKit.isvowel = function(p)
  if p == "a" or p == "e" or p == "i" or p == "o" or p == "u" then
    return 1
  end
  if p == "A" or p == "E" or p == "I" or p == "O" or p == "U" then
    return -1
  end
  return 0
end

local function fileexists(filepath)
  local f = io.open(filepath, "rb")
  if f ~= nil then
    f:close()
    return true
  end
  return false
end

PSDToolKit.settalking = function(obj, src, locut, hicut, threshold)
  local v, p = 0, nil
  if src == nil then
    local n, rate, buf = obj.getaudio(nil, "audiobuffer", "spectrum", 32)
    v = PSDToolKit.talking(buf, rate, locut, hicut, threshold)
    PSDToolKit.settalkingraw(obj, v, p)
    return
  end

  local ext = string.lower(string.sub(src, -4))
  if ext == ".lab" then
    p = PSDToolKit.talkingphoneme(src, obj.time)
    local wav = string.sub(src, 1, #src - 3) .. "wav"
    if fileexists(wav) then
      local n, rate, buf = obj.getaudio(nil, wav, "spectrum", 32)
      v = PSDToolKit.talking(buf, rate, locut, hicut, threshold)
    else
      v = (p ~= nil and #p.cur > 0) and 1 or 0
    end
    PSDToolKit.settalkingraw(obj, v, p)
    return
  end
  if ext == ".wav" then
    local n, rate, buf = obj.getaudio(nil, src, "spectrum", 32)
    v = PSDToolKit.talking(buf, rate, locut, hicut, threshold)
    PSDToolKit.settalkingraw(obj, v, p)
    return
  end
  PSDToolKit.settalkingraw(obj, v, p)
end

PSDToolKit.settalkingraw = function(obj, volume, phoneme)
  local o = {
    v = volume,
    p = phoneme
  }
  PSDToolKit.phoneme["latest"] = o
  PSDToolKit.phoneme[obj.layer] = o
end

PSDToolKit.settalkingcur = function(obj, curphoneme)
  PSDToolKit.settalkingraw(obj, 0, {
    progress = obj.time/obj.totaltime,
    cur = curphoneme,
    cur_start = 1,
    cur_end = obj.totaltime+1,
    prev = "",
    prev_start = 0,
    prev_end = 0,
    next = "",
    next_start = 0,
    next_end = 0
  })
end

PSDToolKit.gettalking = function(index)
  if index == 0 then
    index = "latest"
  end
  local o = PSDToolKit.phoneme[index]
  if o == nil then
    return 0, nil
  end
  PSDToolKit.phoneme[index] = nil
  return o.v, o.p
end

PSDToolKit.peektalking = function(index)
  if index == 0 then
    index = "latest"
  end
  local o = PSDToolKit.phoneme[index]
  if o == nil then
    return 0, nil
  end
  return o.v, o.p
end

PSDToolKit.phoneme = PSDToolKit.phoneme or {}

PSDToolKit.talkstat = PSDToolKit.talkstat or {}

PSDToolKit.text = PSDToolKit.text or {}

PSDToolKit.settext = function(text, obj, unescape)
  if unescape then
    text = text:gsub("([\128-\160\224-\255]\092)\092", "%1")
  end
  local o = {
    s = text,
    f = obj.frame,
    t = obj.time,
    tf = obj.totalframe,
    tt = obj.totaltime
  }
  PSDToolKit.text["latest"] = o
  PSDToolKit.text[obj.layer] = o
end

PSDToolKit.gettext = function(index)
  local s = ''
  if index == 0 then
    index = "latest"
  end
  if PSDToolKit.text[index] ~= nil then
    s = PSDToolKit.text[index].s
    PSDToolKit.text[index] = nil
  end
  return s
end

PSDToolKit.gettextdata = function(index)
  local o = nil
  if index == 0 then
    index = "latest"
  end
  if PSDToolKit.text[index] ~= nil then
    o = PSDToolKit.text[index]
    PSDToolKit.text[index] = nil
  end
  return o
end

return PSDToolKit
