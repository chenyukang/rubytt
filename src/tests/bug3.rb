class Demo
  def data
    @data
  end

  def now
    @data
  end
end

a = Demo.new
puts a.data
puts a.data.class
puts a.instance_variables

res = a.now
# res = a.xx

