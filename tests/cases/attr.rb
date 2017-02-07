class Demo
  b = 1

  def initialize
    @attr = 1
  end

  def test a, b = 1
    puts a
  end
end

a = Demo.new
a.test 1, 2
puts a
