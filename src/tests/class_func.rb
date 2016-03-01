class Demo

  def initialize
    puts "initialize"
    @a = 1
  end

  def test
    puts "hello"
    puts "@a: #{@a}"
  end

end


demo = Demo.new
demo.test
