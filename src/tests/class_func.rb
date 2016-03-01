class Base

  def initialize
    @a = 1
  end

  def base_base_func
    [1, 2, 3]
  end
end

class Hello < Base
  def base_func
    "here"
  end
end

class Demo < Hello
  def initialize
    puts "initialize"
    @a = 1
  end

  def test
    puts "hello"
    puts "@a: #{@a}"
  end

  def hello
    1
  end

end


demo = Demo.new
demo.test
res = demo.base_func
puts res
hello = demo.hello
puts hello

base = demo.base_base_func
puts base
