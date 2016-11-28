class Demo

  XX = "hello"

  def func
    a = 1
    b = a
    puts Demo::XX
  end

end

a = Demo.new
a.func
