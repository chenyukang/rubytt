def eat(meal)
  if block_given?
    meal.each {|food| yield(food)}
  end
  'delicious!'
end


