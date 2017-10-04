PSDToolKitLib = PSDToolKitLib or {}

PSDToolKitLib.psd = {
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
    require("PSDToolKit")
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
    local ok, modified, width, height = PSDToolKit.setprops(self.id, self.file, self)
    if not ok then
      self:msg("[PSDToolKit] CANNOT LOAD\n\n"..modified)
      return
    end
    if not modified then
      local data, w, h = self:getpixeldata(width, height)
      if PSDToolKit.getcache("cache:"..self.id.." "..self.file, data, w * 4 * h) then
        obj.putpixeldata(data)
        obj.cx = w % 2 == 1 and 0.5 or 0
        obj.cy = h % 2 == 1 and 0.5 or 0
        return
      end
    end
    local data, w, h = self:getpixeldata(width, height)
    local ok, msg = PSDToolKit.draw(self.id, self.file, data, w, h)
    if not ok then
      self:msg("[PSDToolKit] CANNOT RENDER\n\n"..msg)
      return
    end
    PSDToolKit.putcache("cache:"..self.id.." "..self.file, data, w * 4 * h, false)
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

PSDToolKitLib.talking = function(buf, rate, lo, hi, thr)
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
  return v > thr
end

PSDToolKitLib.talkingphoneme = function(labfile, time)
  time = time * 10000000
  local line
  local f = io.open(labfile, "r")
  for line in f:lines() do
    local st, ed, p = string.match(line, "(%d+) (%d+) (%a+)")
    if st == nil then
      return "" -- unexpected format
    end
    if st+0 < time and time < ed+0 then
      f:close()
      return p
    end
  end
  f:close()
  return ""
end

PSDToolKitLib.phoneme = PSDToolKitLib.phoneme or ""

PSDToolKitLib.talkstat = PSDToolKitLib.talkstat or {}

PSDToolKitLib.text = PSDToolKitLib.text or {}

PSDToolKitLib.settext = function(text, obj, unescape)
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
  PSDToolKitLib.text["latest"] = o
  PSDToolKitLib.text[obj.layer] = o
end

PSDToolKitLib.gettext = function(index)
  local s = ''
  if index == 0 then
    index = "latest"
  end
  if PSDToolKitLib.text[index] ~= nil then
    s = PSDToolKitLib.text[index].s
    PSDToolKitLib.text[index] = nil
  end
  return s
end

PSDToolKitLib.gettextdata = function(index)
  local o = nil
  if index == 0 then
    index = "latest"
  end
  if PSDToolKitLib.text[index] ~= nil then
    o = PSDToolKitLib.text[index]
    PSDToolKitLib.text[index] = nil
  end
  return o
end

return PSDToolKitLib
