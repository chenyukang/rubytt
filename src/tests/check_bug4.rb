a = 1

b = [2]
b.each{ |x|
  puts a
  puts "now: #{a}"
}
