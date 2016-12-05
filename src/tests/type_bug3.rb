a = 1
b = "hello#{a}"
c = 2

x = a == b
y = a <= c
y2 = a >= c
z = a != b
m = !a
h = a || b

puts "m class: #{m.class}"
puts "h class: #{h.class}"
