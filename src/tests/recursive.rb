
def cur depth
  if depth <= 0
    0
  else
    cur (depth - 1)
  end
end
