a = 1
res = begin
        div = a / 0
      rescue
        'div error'
      end
puts res

