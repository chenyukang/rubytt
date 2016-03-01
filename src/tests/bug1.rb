
class Order

  def order_gateway
    @order_gateway ||= 1
  end

end


a = Order.new
puts a.order_gateway.class
