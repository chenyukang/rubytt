f = File.read("./src/test.rb")

f = f.encode('utf-8', { :undef => :replace,
                      :invalid => :replace,
                      :universal_newline => true })
f.split.each{ |c|
  puts "now: #{c.length}"
}
