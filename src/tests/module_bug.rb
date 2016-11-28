C = 1

module A
  C = "In A"
end

module A
  module B
    puts Module.nesting # => [A::B, A]
    puts C              # => "In A"
    a = C
  end
end

module A::B
  puts Module.nesting # => [A::B]
  puts C              # => "At the top level"
  a = C
end
