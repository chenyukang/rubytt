def demo value = 1
  return value if value <= 1
  demo(value-1) + demo(value-2)
end
