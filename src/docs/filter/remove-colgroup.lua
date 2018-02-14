return {
  {
    Table = function (elem)
      for k, v in ipairs(elem.widths) do
        elem.widths[k]=0
      end
      return elem
    end
  }
}