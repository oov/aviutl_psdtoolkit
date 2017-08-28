PSDToolIPCLib = {}

PSDToolIPCLib.psd = {
  id = 0,
  file = "",
  layer = "",
  layeradd = "",
  faview = {},
  scale = 1,
  offsetx = 0,
  offsety = 0,
  init = function(self, id, file, layer, scale, offsetx, offsety)
    self.id = id
    self.file = file
    self.layer = layer
    self.layeradd = ""
    self.faview = {}
    self.scale = scale
    self.offsetx = offsetx
    self.offsety = offsety
  end,
  cleanup = function(self)
    self:init(0, "", "", 1, 0, 0)
  end,
  render = function(self)
    require("PSDToolIPC")
    if self.file == "" then
      self:msg("[PSDToolIPC] NO IMAGE")
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
        self.layeradd = " S." .. table.concat(self.faview, "_") .. self.layeradd
      end
    end
    if self.layeradd ~= "" then
      self.layer = self.layer .. self.layeradd
    end
    local ok, modified, width, height = PSDToolIPC.setprops(self.id, self.file, self)
    if not ok then
      self:msg("[PSDToolIPC] CANNOT LOAD\n\n"..modified)
      return
    end
    if not modified then
      local data, w, h = self:getpixeldata(width, height)
      if PSDToolIPC.getcache("cache:"..self.id.." "..self.file, data, w * 4 * h) then
        obj.putpixeldata(data)
        obj.copybuffer("cache:"..self.id.." "..self.file, "obj")
        return
      end
    end
    local data, w, h = self:getpixeldata(width, height)
    local ok, msg = PSDToolIPC.draw(self.id, self.file, data, w, h)
    if not ok then
      self:msg("[PSDToolIPC] CANNOT RENDER\n\n"..msg)
      return
    end
    PSDToolIPC.putcache("cache:"..self.id.." "..self.file, data, w * 4 * h, false)
    obj.putpixeldata(data)
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
  end
}

PSDToolIPCLib.talking = function(buf, rate, lo, hi, thr)
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

PSDToolIPCLib.talkingphoneme = function(labfile, time)
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

PSDToolIPCLib.phoneme = ""

PSDToolIPCLib.talkstat = {}

PSDToolIPCLib.retnil = function()
  return nil
end

return PSDToolIPCLib
