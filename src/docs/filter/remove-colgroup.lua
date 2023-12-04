return {
  {
    Table = function (elem)
      if elem.widths ~= nil then
        for k, v in ipairs(elem.widths) do
          elem.widths[k]=0
        end
      end
      return elem
    end
  }
}