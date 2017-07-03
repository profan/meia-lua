local function thing_operation(op, a, b, c)
  return op(a), op(b), op(c)
end

local x, y, z = thing(function(v) return v + 1 end, 1, 2, 3)
print(x, y, z)
