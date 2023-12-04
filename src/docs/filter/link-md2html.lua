function encode(str)
  return string.gsub(str, "([^0-9a-zA-Z*._-])", function(c)
    return string.format(".%02X", string.byte(c))
  end)
end

return {
  {
    Header = function (elem)
      local text = ""
      pandoc.walk_block(elem, {
        Code = function(elm)
          text = text .. elm.text:gsub(" ", "_")
          return elm
        end,
        Space = function(elm)
          text = text .. "_"
          return elm
        end,
        Str = function(elm)
          text = text .. elm.text
          return elm
        end
      })
      if text ~= "" then
        elem.identifier = encode(text)
      end
      return elem
    end,
    Link = function (elem)
      local hash = elem.target:match("#[^#]+$") or ""
      local other = elem.target:sub(1, #elem.target - #hash)
      if other:match("[^.]+$") == "md" and other:match("^https?://") == nil and other:sub(1, 1) ~= "/" then
        elem.target = other:sub(1, #other - 2) .. "html"
        if #hash > 0 then
          elem.target = elem.target .. "#" .. encode(hash:sub(2))
        end
      else
        elem.target = other
        if #hash > 0 then
          elem.target = elem.target .. "#" .. hash:sub(2)
        end
      end
      return elem
    end
  }
}