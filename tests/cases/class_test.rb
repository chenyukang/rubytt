class Demo

  def func_a
    1
  end

  def self.class_func
    "a"
  end

  def func_b
    a = self.func_a
    b = Demo.class_func
    puts a
    puts b
  end

end

a = Demo.new
a.func_b
